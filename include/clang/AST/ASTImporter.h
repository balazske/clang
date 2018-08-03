//===--- ASTImporter.h - Importing ASTs from other Contexts -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the ASTImporter class which imports AST nodes from one
//  context into another context.
//
//===----------------------------------------------------------------------===//
#ifndef LLVM_CLANG_AST_ASTIMPORTER_H
#define LLVM_CLANG_AST_ASTIMPORTER_H

#include "clang/AST/DeclarationName.h"
#include "clang/AST/Type.h"
#include "clang/Basic/SourceLocation.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Optional.h"

namespace clang {

  class ASTContext;
  class CXXCtorInitializer;
  class CXXBaseSpecifier;
  class Decl;
  class DeclContext;
  class DiagnosticsEngine;
  class Expr;
  class FileManager;
  class IdentifierInfo;
  class NestedNameSpecifier;
  class Stmt;
  class TypeSourceInfo;

  /// \brief Kind of error when importing a declaration.
  /// The term 'inherited' is used to indicate that the current object could
  /// not be imported because import of a (direct) dependency has failed.
  /// The real source of the error in this case is not the current object,
  /// instead one of its dependencies.
  enum class ImportErrorKind {
      NameConflict, /// Naming ambiguity (likely ODR violation).
      UnsupportedConstruct, /// Not supported node or case.
      TypeFailed, /// A needed type or type info could not be imported.
      ExprFailed, /// A needed expression could not be imported.
      StmtFailed, /// A needed statement could not be imported.
      LocationFailed /// Source location could not be imported.
  };

  // \brief Returns with a list of declarations started from the canonical decl
  // then followed by subsequent decls in the translation unit.
  // This gives a canonical list for each entry in the redecl chain.
  // `Decl::redecls()` gives a list of decls which always start from the
  // previous decl and the next item is actually the previous item in the order
  // of source locations.  Thus, `Decl::redecls()` gives different lists for
  // the different entries in a given redecl chain.
  llvm::SmallVector<Decl*, 2> getCanonicalForwardRedeclChain(Decl* D);

  /// \brief Imports selected nodes from one AST context into another context,
  /// merging AST nodes where appropriate.
  class ASTImporter {
  public:
    typedef llvm::DenseSet<std::pair<Decl *, Decl *> > NonEquivalentDeclSet;
    typedef llvm::DenseMap<const CXXBaseSpecifier *, CXXBaseSpecifier *>
    ImportedCXXBaseSpecifierMap;

  private:
    /// \brief The contexts we're importing to and from.
    ASTContext &ToContext, &FromContext;
    
    /// \brief The file managers we're importing to and from.
    FileManager &ToFileManager, &FromFileManager;

    /// \brief Whether to perform a minimal import.
    bool Minimal;

    /// \brief Whether the last diagnostic came from the "from" context.
    bool LastDiagFromFrom;
    
    /// \brief Mapping from the already-imported types in the "from" context
    /// to the corresponding types in the "to" context.
    llvm::DenseMap<const Type *, const Type *> ImportedTypes;
    
    /// \brief Mapping from the already-imported declarations in the "from"
    /// context to the corresponding declarations in the "to" context.
    llvm::DenseMap<Decl *, Decl *> ImportedDecls;

    /// \brief Mapping from the already-imported declarations in the "from"
    /// context to the error status of the import of that declaration.
    /// This map contains only the declarations that were not correctly
    /// imported. The same declaration may or may not be included in
    /// ImportedDecls. This map is updated continuously during imports and never
    /// cleared (like ImportedDecls).
    llvm::DenseMap<Decl *, ImportErrorKind> ImportDeclErrors;

    /// Counter for errors encountered during import.
    /// It can be used to detect what errors are found during import of a
    /// specific Decl, by using the counter reset and get functions.
    /// An import is considered as failed if the import directly failed (Decl
    /// node could not be created correctly) or if a previous import of the
    /// Decl node has failed. It is possible that import of a Decl node is
    /// successful but some of its "subnodes" is not successful (for example at
    /// members of a namespace, some members can be missed because import
    /// failure but the import of the whole namespace itself still succeeds).
    llvm::DenseMap<int, unsigned int> ImportDeclErrorCount;

    /// Current error during import of a Decl.
    /// This error code will be used for any Decl for which Import returns
    /// nullptr. It must be set in any Import function that can cause a Decl
    /// import to return nullptr, including import functions for non-decl
    /// types.
    llvm::Optional<ImportErrorKind> CurrentImportDeclError;

    /// \brief Mapping from the already-imported statements in the "from"
    /// context to the corresponding statements in the "to" context.
    llvm::DenseMap<Stmt *, Stmt *> ImportedStmts;

    /// \brief Mapping from the already-imported FileIDs in the "from" source
    /// manager to the corresponding FileIDs in the "to" source manager.
    llvm::DenseMap<FileID, FileID> ImportedFileIDs;

    /// \brief Mapping from the already-imported CXXBasesSpecifier in
    ///  the "from" source manager to the corresponding CXXBasesSpecifier
    ///  in the "to" source manager.
    ImportedCXXBaseSpecifierMap ImportedCXXBaseSpecifiers;

    /// \brief Declaration (from, to) pairs that are known not to be equivalent
    /// (which we have already complained about).
    NonEquivalentDeclSet NonEquivalentDecls;

  public:
    /// \brief Create a new AST importer.
    ///
    /// \param ToContext The context we'll be importing into.
    ///
    /// \param ToFileManager The file manager we'll be importing into.
    ///
    /// \param FromContext The context we'll be importing from.
    ///
    /// \param FromFileManager The file manager we'll be importing into.
    ///
    /// \param MinimalImport If true, the importer will attempt to import
    /// as little as it can, e.g., by importing declarations as forward
    /// declarations that can be completed at a later point.
    ASTImporter(ASTContext &ToContext, FileManager &ToFileManager,
                ASTContext &FromContext, FileManager &FromFileManager,
                bool MinimalImport);
    
    virtual ~ASTImporter();
    
    /// \brief Whether the importer will perform a minimal import, creating
    /// to-be-completed forward declarations when possible.
    bool isMinimalImport() const { return Minimal; }
    
    /// \brief Import the given type from the "from" context into the "to"
    /// context.
    ///
    /// \returns the equivalent type in the "to" context, or a NULL type if
    /// an error occurred.
    QualType Import(QualType FromT);

    /// \brief Import the given type source information from the
    /// "from" context into the "to" context.
    ///
    /// \returns the equivalent type source information in the "to"
    /// context, or NULL if an error occurred.
    TypeSourceInfo *Import(TypeSourceInfo *FromTSI);

    /// \brief Import the given declaration from the "from" context into the 
    /// "to" context.
    ///
    /// \returns the equivalent declaration in the "to" context, or a NULL type 
    /// if an error occurred.
    Decl *Import(Decl *FromD);
    Decl *Import(const Decl *FromD);

    /// \brief Return the copy of the given declaration in the "to" context if
    /// it has already been imported from the "from" context.  Otherwise return
    /// NULL.
    Decl *GetAlreadyImportedOrNull(Decl *FromD);

    /// \brief Return the declaration of the built-in type "__va_list_tag" from
    /// the ASTContext instead of importing it.
    Decl *GetVAListTag(Decl *FromD);

    /// \brief Import the given declaration context from the "from"
    /// AST context into the "to" AST context.
    ///
    /// \returns the equivalent declaration context in the "to"
    /// context, or a NULL type if an error occurred.
    DeclContext *ImportContext(DeclContext *FromDC);
    
    /// \brief Import the given expression from the "from" context into the
    /// "to" context.
    ///
    /// \returns the equivalent expression in the "to" context, or NULL if
    /// an error occurred.
    Expr *Import(Expr *FromE);

    /// \brief Import the given statement from the "from" context into the
    /// "to" context.
    ///
    /// \returns the equivalent statement in the "to" context, or NULL if
    /// an error occurred.
    Stmt *Import(Stmt *FromS);

    /// \brief Import the given nested-name-specifier from the "from"
    /// context into the "to" context.
    ///
    /// \returns the equivalent nested-name-specifier in the "to"
    /// context, or NULL if an error occurred.
    NestedNameSpecifier *Import(NestedNameSpecifier *FromNNS);

    /// \brief Import the given nested-name-specifier from the "from"
    /// context into the "to" context.
    ///
    /// \returns the equivalent nested-name-specifier in the "to"
    /// context.
    NestedNameSpecifierLoc Import(NestedNameSpecifierLoc FromNNS);

    /// \brief Import the goven template name from the "from" context into the
    /// "to" context.
    TemplateName Import(TemplateName From);
    
    /// \brief Import the given source location from the "from" context into
    /// the "to" context.
    ///
    /// \returns the equivalent source location in the "to" context, or an
    /// invalid source location if an error occurred.
    SourceLocation Import(SourceLocation FromLoc);

    /// \brief Import the given source range from the "from" context into
    /// the "to" context.
    ///
    /// \returns the equivalent source range in the "to" context, or an
    /// invalid source location if an error occurred.
    SourceRange Import(SourceRange FromRange);

    /// \brief Import the given declaration name from the "from"
    /// context into the "to" context.
    ///
    /// \returns the equivalent declaration name in the "to" context,
    /// or an empty declaration name if an error occurred.
    DeclarationName Import(DeclarationName FromName);

    /// \brief Import the given identifier from the "from" context
    /// into the "to" context.
    ///
    /// \returns the equivalent identifier in the "to" context.
    IdentifierInfo *Import(const IdentifierInfo *FromId);

    /// \brief Import the given Objective-C selector from the "from"
    /// context into the "to" context.
    ///
    /// \returns the equivalent selector in the "to" context.
    Selector Import(Selector FromSel);

    /// \brief Import the given file ID from the "from" context into the 
    /// "to" context.
    ///
    /// \returns the equivalent file ID in the source manager of the "to"
    /// context.
    FileID Import(FileID);

    /// \brief Import the given C++ constructor initializer from the "from"
    /// context into the "to" context.
    ///
    /// \returns the equivalent initializer in the "to" context.
    CXXCtorInitializer *Import(CXXCtorInitializer *FromInit);

    /// \brief Import the given CXXBaseSpecifier from the "from" context into
    /// the "to" context.
    ///
    /// \returns the equivalent CXXBaseSpecifier in the source manager of the
    /// "to" context.
    CXXBaseSpecifier *Import(const CXXBaseSpecifier *FromSpec);

    /// \brief Import the definition of the given declaration, including all of
    /// the declarations it contains.
    ///
    /// This routine is intended to be used 
    void ImportDefinition(Decl *From);

    /// \brief Cope with a name conflict when importing a declaration into the
    /// given context.
    ///
    /// This routine is invoked whenever there is a name conflict while 
    /// importing a declaration. The returned name will become the name of the
    /// imported declaration. By default, the returned name is an empty name.
    /// If the original name is returned the conflict remains unresolved such
    /// that name lookup for this name is likely to find an ambiguity later.
    /// This can cause various assertions or problems later
    /// and should be avoided.
    ///
    /// Subclasses may override this routine to resolve the conflict, e.g., by
    /// renaming the declaration being imported.
    ///
    /// \param Name the name of the declaration being imported, which conflicts
    /// with other declarations.
    ///
    /// \param DC the declaration context (in the "to" AST context) in which 
    /// the name is being imported.
    ///
    /// \param IDNS the identifier namespace in which the name will be found.
    ///
    /// \param Decls the set of declarations with the same name as the
    /// declaration being imported.
    ///
    /// \param NumDecls the number of conflicting declarations in \p Decls.
    ///
    /// \returns the name that the newly-imported declaration should have.
    /// If empty the current import is forced to fail with name conflict error.
    virtual DeclarationName HandleNameConflict(DeclarationName Name,
                                               DeclContext *DC,
                                               unsigned IDNS,
                                               NamedDecl **Decls,
                                               unsigned NumDecls);
    
    /// \brief Retrieve the context that AST nodes are being imported into.
    ASTContext &getToContext() const { return ToContext; }
    
    /// \brief Retrieve the context that AST nodes are being imported from.
    ASTContext &getFromContext() const { return FromContext; }
    
    /// \brief Retrieve the file manager that AST nodes are being imported into.
    FileManager &getToFileManager() const { return ToFileManager; }

    /// \brief Retrieve the file manager that AST nodes are being imported from.
    FileManager &getFromFileManager() const { return FromFileManager; }
    
    /// \brief Report a diagnostic in the "to" context.
    DiagnosticBuilder ToDiag(SourceLocation Loc, unsigned DiagID);
    
    /// \brief Report a diagnostic in the "from" context.
    DiagnosticBuilder FromDiag(SourceLocation Loc, unsigned DiagID);
    
    /// \brief Return the set of declarations that we know are not equivalent.
    NonEquivalentDeclSet &getNonEquivalentDecls() { return NonEquivalentDecls; }

    /// \brief Called for ObjCInterfaceDecl, ObjCProtocolDecl, and TagDecl.
    /// Mark the Decl as complete, filling it in as much as possible.
    ///
    /// \param D A declaration in the "to" context.
    virtual void CompleteDecl(Decl* D);
    
    /// Subclasses can override this function to observe all of the \c From ->
    /// \c To declaration mappings as they are imported.
    virtual Decl *Imported(Decl *From, Decl *To) { return To; }

    /// Store and assign the imported declaration to its counterpart.
    Decl *MapImported(Decl *From, Decl *To);

    /// \brief Called by StructuralEquivalenceContext.  If a RecordDecl is
    /// being compared to another RecordDecl as part of import, completing the
    /// other RecordDecl may trigger importation of the first RecordDecl. This
    /// happens especially for anonymous structs.  If the original of the second
    /// RecordDecl can be found, we can complete it without the need for
    /// importation, eliminating this loop.
    virtual Decl *GetOriginalDecl(Decl *To) { return nullptr; }
    
    /// \brief Determine whether the given types are structurally
    /// equivalent.
    bool IsStructurallyEquivalent(QualType From, QualType To,
                                  bool Complain = true);

    /// Return if import of the given declaration has failed and if yes
    /// the kind of the problem. This gives the first error encountered with
    /// the node.
    llvm::Optional<ImportErrorKind> getImportDeclErrorIfAny(Decl *FromD) const;

    /// Get if there was any import error since last counter reset.
    bool hasImportErrorCount() const;

    /// Get count of error type after last count reset.
    unsigned int getImportErrorCount(ImportErrorKind Error) const;

    /// Increment count of error type.
    void incrementImportErrorCount(ImportErrorKind Error);

    /// Reset import error counts.
    void resetImportErrorCount();

    /// Mark (newly) imported declaration with error.
    void setImportDeclError(Decl *From, ImportErrorKind Error);
    
    /// Set current error for Decl import result, only if not set already.
    /// Can be used at places where a specific error reason is known (there was
    /// no last import operation that failed already).
    void setCurrentImportDeclError(ImportErrorKind Error);
    
    /// Clear current error for Decl import result.
    /// Should be used if an import operation succeeds even if it had Decl
    /// import operations that have failed.
    void resetCurrentImportDeclError();
  };
}

#endif // LLVM_CLANG_AST_ASTIMPORTER_H
