//===- unittest/AST/ASTImporterTest.cpp - AST node import test ------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Tests for the correct import of AST nodes from one AST context to another.
//
//===----------------------------------------------------------------------===//

#include "MatchVerifier.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTImporter.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Tooling/Tooling.h"
#include "gtest/gtest.h"

namespace clang {
namespace ast_matchers {

typedef std::vector<std::string> StringVector;

void getLangArgs(Language Lang, StringVector &Args) {
  switch (Lang) {
  case Lang_C:
    Args.insert(Args.end(), { "-x", "c", "-std=c99" });
    break;
  case Lang_C89:
    Args.insert(Args.end(), { "-x", "c", "-std=c89" });
    break;
  case Lang_CXX:
    Args.push_back("-std=c++98");
    break;
  case Lang_CXX11:
    Args.push_back("-std=c++11");
    break;
  case Lang_OpenCL:
  case Lang_OBJCXX:
    break;
  }
}

template<typename NodeType, typename MatcherType>
testing::AssertionResult
testImport(const std::string &FromCode, Language FromLang,
           const std::string &ToCode, Language ToLang,
           MatchVerifier<NodeType> &Verifier,
           const MatcherType &AMatcher) {
  StringVector FromArgs, ToArgs;
  getLangArgs(FromLang, FromArgs);
  getLangArgs(ToLang, ToArgs);

  const char *const InputFileName = "input.cc";
  const char *const OutputFileName = "output.cc";

  std::unique_ptr<ASTUnit>
      FromAST = tooling::buildASTFromCodeWithArgs(
        FromCode, FromArgs, InputFileName),
      ToAST = tooling::buildASTFromCodeWithArgs(ToCode, ToArgs, OutputFileName);

  ASTContext &FromCtx = FromAST->getASTContext(),
      &ToCtx = ToAST->getASTContext();

  // Add input.cc to virtual file system so importer can 'find' it
  // while importing SourceLocations.
  vfs::OverlayFileSystem *OFS = static_cast<vfs::OverlayFileSystem *>(
        ToCtx.getSourceManager().getFileManager().getVirtualFileSystem().get());
  vfs::InMemoryFileSystem *MFS = static_cast<vfs::InMemoryFileSystem *>(
        OFS->overlays_begin()->get());
  MFS->addFile(InputFileName, 0, llvm::MemoryBuffer::getMemBuffer(FromCode));

  ASTImporter Importer(ToCtx, ToAST->getFileManager(),
                       FromCtx, FromAST->getFileManager(), false);

  IdentifierInfo *ImportedII = &FromCtx.Idents.get("declToImport");
  assert(ImportedII && "Declaration with 'declToImport' name"
                       "should be specified in test!");
  DeclarationName ImportDeclName(ImportedII);
  SmallVector<NamedDecl *, 4> FoundDecls;
  FromCtx.getTranslationUnitDecl()->localUncachedLookup(
        ImportDeclName, FoundDecls);

  if (FoundDecls.size() == 0)
    return testing::AssertionFailure() << "No declarations were found!";

  if (FoundDecls.size() > 1)
    return testing::AssertionFailure() << "Multiple declarations were found!";

  // Sanity check: the node being imported should match in the same way as
  // the result node.
  EXPECT_TRUE(Verifier.match(FoundDecls.front(), AMatcher));

  auto Imported = Importer.Import(FoundDecls.front());
  if (!Imported)
    return testing::AssertionFailure() << "Import failed, nullptr returned!";

  // This should dump source locations and assert if some source locations
  // were not imported
  SmallString<1024> ImportChecker;
  llvm::raw_svector_ostream ToNothing(ImportChecker);
  ToCtx.getTranslationUnitDecl()->print(ToNothing);

  return Verifier.match(Imported, AMatcher);
}

struct Fixture : ::testing::Test {

  std::unique_ptr<ASTUnit> FromAST, ToAST;
  const char *const InputFileName = "input.cc";
  const char *const OutputFileName = "output.cc";

  std::tuple<Decl *, Decl *>
  getImportedDecl(const std::string &FromCode, Language FromLang,
                  const std::string &ToCode, Language ToLang,
                  const char *const Identifier = "declToImport") {
    StringVector FromArgs, ToArgs;
    getLangArgs(FromLang, FromArgs);
    getLangArgs(ToLang, ToArgs);

    FromAST =
        tooling::buildASTFromCodeWithArgs(FromCode, FromArgs, InputFileName),
    ToAST = tooling::buildASTFromCodeWithArgs(ToCode, ToArgs, OutputFileName);

    ASTContext &FromCtx = FromAST->getASTContext(),
               &ToCtx = ToAST->getASTContext();

    // Add input.cc to virtual file system so importer can 'find' it
    // while importing SourceLocations.
    vfs::OverlayFileSystem *OFS = static_cast<vfs::OverlayFileSystem *>(
        ToCtx.getSourceManager().getFileManager().getVirtualFileSystem().get());
    vfs::InMemoryFileSystem *MFS =
        static_cast<vfs::InMemoryFileSystem *>(OFS->overlays_begin()->get());
    MFS->addFile(InputFileName, 0, llvm::MemoryBuffer::getMemBuffer(FromCode));

    ASTImporter Importer(ToCtx, ToAST->getFileManager(), FromCtx,
                         FromAST->getFileManager(), false);

    IdentifierInfo *ImportedII = &FromCtx.Idents.get(Identifier);
    assert(ImportedII && "Declaration with the given identifier "
                         "should be specified in test!");
    DeclarationName ImportDeclName(ImportedII);
    SmallVector<NamedDecl *, 4> FoundDecls;
    FromCtx.getTranslationUnitDecl()->localUncachedLookup(ImportDeclName,
                                                          FoundDecls);

    assert(FoundDecls.size() == 1);

    Decl *Imported = Importer.Import(*FoundDecls.begin());
    assert(Imported);
    return std::make_tuple(*FoundDecls.begin(), Imported);
  }

  TranslationUnitDecl *getTuDecl(const std::string &Code, Language Lang) {
    StringVector Args;
    getLangArgs(Lang, Args);
    FromAST = tooling::buildASTFromCodeWithArgs(Code, Args, InputFileName);
    return FromAST->getASTContext().getTranslationUnitDecl();
  }

  Decl *Import(Decl *From, Language ToLang) {
    assert(FromAST);
    StringVector ToArgs;
    getLangArgs(ToLang, ToArgs);
    ToAST = tooling::buildASTFromCodeWithArgs("", ToArgs, OutputFileName);

    ASTContext &FromCtx = FromAST->getASTContext(),
               &ToCtx = ToAST->getASTContext();
    ASTImporter Importer(ToCtx, ToAST->getFileManager(), FromCtx,
                         FromAST->getFileManager(), false);
    return Importer.Import(From);
  }
};

TEST(ImportExpr, ImportStringLiteral) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(testImport("void declToImport() { \"foo\"; }",
                         Lang_CXX, "", Lang_CXX, Verifier,
                         functionDecl(
                           hasBody(
                             compoundStmt(
                               has(
                                 stringLiteral(
                                   hasType(
                                     asString("const char [4]")))))))));
  EXPECT_TRUE(testImport("void declToImport() { L\"foo\"; }",
                         Lang_CXX, "", Lang_CXX, Verifier,
                         functionDecl(
                           hasBody(
                             compoundStmt(
                               has(
                                 stringLiteral(
                                   hasType(
                                     asString("const wchar_t [4]")))))))));
  EXPECT_TRUE(testImport("void declToImport() { \"foo\" \"bar\"; }",
                         Lang_CXX, "", Lang_CXX, Verifier,
                         functionDecl(
                           hasBody(
                             compoundStmt(
                               has(
                                 stringLiteral(
                                   hasType(
                                     asString("const char [7]")))))))));
}

TEST(ImportExpr, ImportGNUNullExpr) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(testImport("void declToImport() { __null; }",
                         Lang_CXX, "", Lang_CXX, Verifier,
                         functionDecl(
                           hasBody(
                             compoundStmt(
                               has(
                                 gnuNullExpr(
                                   hasType(isInteger()))))))));
}

TEST(ImportExpr, ImportCXXNullPtrLiteralExpr) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(testImport("void declToImport() { nullptr; }",
                         Lang_CXX11, "", Lang_CXX11, Verifier,
                         functionDecl(
                           hasBody(
                             compoundStmt(
                               has(
                                 cxxNullPtrLiteralExpr()))))));
}


TEST(ImportExpr, ImportFloatinglLiteralExpr) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(testImport("void declToImport() { 1.0; }",
                         Lang_CXX, "", Lang_CXX, Verifier,
                         functionDecl(
                           hasBody(
                             compoundStmt(
                               has(
                                 floatLiteral(
                                   equals(1.0),
                                   hasType(asString("double")))))))));
  EXPECT_TRUE(testImport("void declToImport() { 1.0e-5f; }",
                         Lang_CXX, "", Lang_CXX, Verifier,
                         functionDecl(
                           hasBody(
                             compoundStmt(
                               has(
                                 floatLiteral(
                                   equals(1.0e-5f),
                                   hasType(asString("float")))))))));
}

TEST(ImportExpr, ImportCompoundLiteralExpr) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(
        testImport(
          "void declToImport() {"
          "  struct s { int x; long y; unsigned z; }; "
          "  (struct s){ 42, 0L, 1U }; }",
          Lang_CXX, "", Lang_CXX, Verifier,
          functionDecl(
            hasBody(
              compoundStmt(
                has(
                  compoundLiteralExpr(
                    hasType(asString("struct s")),
                    has(initListExpr(
                      hasType(asString("struct s")),
                      has(integerLiteral(
                            equals(42), hasType(asString("int")))),
                      has(integerLiteral(
                            equals(0), hasType(asString("long")))),
                      has(integerLiteral(
                            equals(1),
                            hasType(asString("unsigned int"))))
                      )))))))));
}

TEST(ImportExpr, ImportCXXThisExpr) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(
        testImport("class declToImport { void f() { this; } };",
                   Lang_CXX, "", Lang_CXX, Verifier,
                   cxxRecordDecl(
                     hasMethod(
                       hasBody(
                         compoundStmt(
                           has(
                             cxxThisExpr(
                               hasType(
                                 asString("class declToImport *"))))))))));
}

TEST(ImportExpr, ImportAtomicExpr) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(testImport(
      "void declToImport() { int *ptr; __atomic_load_n(ptr, 1); }", Lang_CXX,
      "", Lang_CXX, Verifier,
      functionDecl(hasBody(compoundStmt(has(atomicExpr(
          has(ignoringParenImpCasts(
              declRefExpr(hasDeclaration(varDecl(hasName("ptr"))),
                          hasType(asString("int *"))))),
          has(integerLiteral(equals(1), hasType(asString("int")))))))))));
}

TEST(ImportExpr, ImportLabelDeclAndAddrLabelExpr) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(
        testImport(
          "void declToImport() { loop: goto loop; &&loop; }",
          Lang_CXX, "", Lang_CXX, Verifier,
          functionDecl(
            hasBody(
              compoundStmt(
                has(labelStmt(hasDeclaration(labelDecl(hasName("loop"))))),
                has(addrLabelExpr(hasDeclaration(labelDecl(hasName("loop")))))
                )))));
}

AST_MATCHER_P(TemplateDecl, hasTemplateDecl,
              internal::Matcher<NamedDecl>, InnerMatcher) {
  const NamedDecl *Template = Node.getTemplatedDecl();
  return Template && InnerMatcher.matches(*Template, Finder, Builder);
}

TEST(ImportExpr, ImportParenListExpr) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(
        testImport(
          "template<typename T> class dummy { void f() { dummy X(*this); } };"
          "typedef dummy<int> declToImport;"
          "template class dummy<int>;",
          Lang_CXX, "", Lang_CXX, Verifier,
          typedefDecl(
            hasType(
              templateSpecializationType(
                hasDeclaration(
                  classTemplateDecl(
                    hasTemplateDecl(
                      cxxRecordDecl(
                        hasMethod(
                        allOf(
                          hasName("f"),
                          hasBody(
                            compoundStmt(
                              has(
                                declStmt(
                                  hasSingleDecl(
                                    varDecl(
                                      hasInitializer(
                                        parenListExpr(
                                          has(
                                            unaryOperator(
                                              hasOperatorName("*"),
                                              hasUnaryOperand(cxxThisExpr())
                                              )))))))))))))))))))));
}

TEST(ImportExpr, ImportStmtExpr) {
  MatchVerifier<Decl> Verifier;
  // NOTE: has() ignores implicit casts, using hasDescendant() to match it
  EXPECT_TRUE(
        testImport(
          "void declToImport() { int b; int a = b ?: 1; int C = ({int X=4; X;}); }",
          Lang_CXX, "", Lang_CXX, Verifier,
          functionDecl(
            hasBody(
              compoundStmt(
                has(
                  declStmt(
                    hasSingleDecl(
                      varDecl(
                        hasName("C"),
                        hasType(asString("int")),
                        hasInitializer(
                          stmtExpr(
                            hasAnySubstatement(
                              declStmt(
                                hasSingleDecl(
                                  varDecl(
                                    hasName("X"),
                                    hasType(asString("int")),
                                    hasInitializer(
                                      integerLiteral(equals(4))))))),
                            hasDescendant(
                              implicitCastExpr()
                              ))))))))))));
}

TEST(ImportExpr, ImportConditionalOperator) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(
        testImport(
          "void declToImport() { true ? 1 : -5; }",
          Lang_CXX, "", Lang_CXX, Verifier,
          functionDecl(
            hasBody(
              compoundStmt(
                has(
                  conditionalOperator(
                    hasCondition(cxxBoolLiteral(equals(true))),
                    hasTrueExpression(integerLiteral(equals(1))),
                    hasFalseExpression(
                      unaryOperator(hasUnaryOperand(integerLiteral(equals(5))))
                      ))))))));
}

TEST(ImportExpr, ImportBinaryConditionalOperator) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(
        testImport(
          "void declToImport() { 1 ?: -5; }",
          Lang_CXX, "", Lang_CXX, Verifier,
          functionDecl(
            hasBody(
              compoundStmt(
                has(
                  binaryConditionalOperator(
                    hasCondition(
                      implicitCastExpr(
                        hasSourceExpression(
                          opaqueValueExpr(
                            hasSourceExpression(integerLiteral(equals(1))))),
                        hasType(booleanType()))),
                    hasTrueExpression(
                      opaqueValueExpr(hasSourceExpression(
                                        integerLiteral(equals(1))))),
                    hasFalseExpression(
                      unaryOperator(hasOperatorName("-"),
                                    hasUnaryOperand(integerLiteral(equals(5)))))
                      )))))));
}

TEST(ImportExpr, ImportDesignatedInitExpr) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(testImport("void declToImport() {"
                         "  struct point { double x; double y; };"
                         "  struct point ptarray[10] = "
                                "{ [2].y = 1.0, [2].x = 2.0, [0].x = 1.0 }; }",
                         Lang_C, "", Lang_C, Verifier,
                         functionDecl(
                           hasBody(
                             compoundStmt(
                               has(
                                 declStmt(
                                   hasSingleDecl(
                                     varDecl(
                                       hasInitializer(
                                         initListExpr(
                                           hasSyntacticForm(
                                             initListExpr(
                                               has(
                                                 designatedInitExpr(
                                                   designatorCountIs(2),
                                                   has(floatLiteral(
                                                         equals(1.0))),
                                                   has(integerLiteral(
                                                         equals(2))))),
                                               has(
                                                 designatedInitExpr(
                                                   designatorCountIs(2),
                                                   has(floatLiteral(
                                                         equals(2.0))),
                                                   has(integerLiteral(
                                                         equals(2))))),
                                               has(
                                                 designatedInitExpr(
                                                   designatorCountIs(2),
                                                   has(floatLiteral(
                                                         equals(1.0))),
                                                   has(integerLiteral(
                                                         equals(0)))))
                                               )))))))))))));
}


TEST(ImportExpr, ImportPredefinedExpr) {
  MatchVerifier<Decl> Verifier;
  // __func__ expands as StringLiteral("declToImport")
  EXPECT_TRUE(testImport("void declToImport() { __func__; }",
                         Lang_CXX, "", Lang_CXX, Verifier,
                         functionDecl(
                           hasBody(
                             compoundStmt(
                               has(
                                 predefinedExpr(
                                   hasType(
                                     asString("const char [13]")),
                                   has(
                                     stringLiteral(
                                       hasType(
                                         asString("const char [13]")))))))))));
}

TEST(ImportExpr, ImportInitListExpr) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(
        testImport(
          "void declToImport() {"
          "  struct point { double x; double y; };"
          "  point ptarray[10] = { [2].y = 1.0, [2].x = 2.0,"
          "                        [0].x = 1.0 }; }",
          Lang_CXX, "", Lang_CXX, Verifier,
          functionDecl(
            hasBody(
              compoundStmt(
                has(
                  declStmt(
                    hasSingleDecl(
                      varDecl(
                        hasInitializer(
                          initListExpr(
                            has(
                              cxxConstructExpr(
                                requiresZeroInitialization())),
                            has(
                              initListExpr(
                                hasType(asString("struct point")),
                                has(floatLiteral(equals(1.0))),
                                has(implicitValueInitExpr(
                                      hasType(asString("double")))))),
                            has(
                              initListExpr(
                                hasType(asString("struct point")),
                                has(floatLiteral(equals(2.0))),
                                has(floatLiteral(equals(1.0)))))
                              )))))))))));
}


const internal::VariadicDynCastAllOfMatcher<Expr, VAArgExpr> vaArgExpr;

TEST(ImportExpr, ImportVAArgExpr) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(
        testImport(
          "void declToImport(__builtin_va_list list, ...) {"
          "  (void)__builtin_va_arg(list, int); }",
          Lang_CXX, "", Lang_CXX, Verifier,
          functionDecl(
            hasBody(
              compoundStmt(
                has(
                  cStyleCastExpr(
                    hasSourceExpression(
                      vaArgExpr()))))))));
}

TEST(ImportExpr, CXXTemporaryObjectExpr) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(testImport(
      "struct C {};"
      "void declToImport() { C c = C(); }",
      Lang_CXX, "", Lang_CXX, Verifier,
      functionDecl(hasBody(compoundStmt(
          has(declStmt(has(varDecl(has(exprWithCleanups(has(cxxConstructExpr(
              has(materializeTemporaryExpr(has(implicitCastExpr(
                  has(cxxTemporaryObjectExpr()))))))))))))))))));
}

TEST(ImportType, ImportAtomicType) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(testImport("void declToImport() { typedef _Atomic(int) a_int; }",
                         Lang_CXX11, "", Lang_CXX11, Verifier,
                         functionDecl(
                           hasBody(
                             compoundStmt(
                               has(
                                 declStmt(
                                   has(
                                     typedefDecl(
                                       has(atomicType()))))))))));
}

const internal::VariadicDynCastAllOfMatcher<Expr, CXXDependentScopeMemberExpr>
    cxxDependentScopeMemberExpr;

TEST(ImportExpr, ImportCXXDependentScopeMemberExpr) {
  MatchVerifier<Decl> Verifier;
  testImport("template <typename T> struct C { T t; };"
             "template <typename T> void declToImport() {"
             "  C<T> d;"
             "  d.t;"
             "}"
             "void instantiate() { declToImport<int>(); }",
             Lang_CXX, "", Lang_CXX, Verifier,
             functionTemplateDecl(has(functionDecl(
                 has(compoundStmt(has(cxxDependentScopeMemberExpr())))))));
  testImport("template <typename T> struct C { T t; };"
             "template <typename T> void declToImport() {"
             "  C<T> d;"
             "  (&d)->t;"
             "}"
             "void instantiate() { declToImport<int>(); }",
             Lang_CXX, "", Lang_CXX, Verifier,
             functionTemplateDecl(has(functionDecl(
                 has(compoundStmt(has(cxxDependentScopeMemberExpr())))))));
}

TEST(ImportType, ImportTypeAliasTemplate) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(testImport("template <int K>"
                         "struct dummy { static const int i = K; };"
                         "template <int K> using dummy2 = dummy<K>;"
                         "int declToImport() { return dummy2<3>::i; }",
                         Lang_CXX11, "", Lang_CXX11, Verifier,
                         functionDecl(
                           hasBody(
                             compoundStmt(
                               has(
                                 returnStmt(
                                   has(
                                     implicitCastExpr(
                                       has(
                                         declRefExpr()))))))))));
}


TEST(ImportType, ImportPackExpansion) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(testImport("template <typename... Args>"
                         "struct dummy {"
                         "  dummy(Args... args) {}"
                         "  static const int i = 4;"
                         "};"
                         "int declToImport() { return dummy<int>::i; }",
                         Lang_CXX11, "", Lang_CXX11, Verifier,
                         functionDecl(
                           hasBody(
                             compoundStmt(
                               has(
                                 returnStmt(
                                   has(
                                     implicitCastExpr(
                                       has(
                                         declRefExpr()))))))))));
}

const internal::VariadicDynCastAllOfMatcher<Type,
                                            DependentTemplateSpecializationType>
    dependentTemplateSpecializationType;

TEST(ImportType, ImportDependentTemplateSpecialization) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(testImport(
      "template<typename T>"
      "struct A;"
      "template<typename T>"
      "struct declToImport {"
      "  typename A<T>::template B<T> a;"
      "};",
      Lang_CXX, "", Lang_CXX, Verifier,
      classTemplateDecl(has(cxxRecordDecl(
          has(fieldDecl(hasType(dependentTemplateSpecializationType()))))))));
}

const internal::VariadicDynCastAllOfMatcher<Stmt, SizeOfPackExpr>
    sizeOfPackExpr;

TEST(ImportExpr, ImportSizeOfPackExpr) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(
      testImport("template <typename... Ts>"
                 "void declToImport() {"
                 "  const int i = sizeof...(Ts);"
                 "};",
                 Lang_CXX11, "", Lang_CXX11, Verifier,
                 functionTemplateDecl(has(functionDecl(hasBody(
                     compoundStmt(has(declStmt(has(varDecl(hasInitializer(
                         implicitCastExpr(has(sizeOfPackExpr()))))))))))))));
  EXPECT_TRUE(testImport(
      "template <typename... Ts>"
      "using X = int[sizeof...(Ts)];"
      "template <typename... Us>"
      "struct Y {"
      "  X<Us..., int, double, int, Us...> f;"
      "};"
      "Y<float, int> declToImport;",
      Lang_CXX11, "", Lang_CXX11, Verifier,
      varDecl(hasType(classTemplateSpecializationDecl(has(fieldDecl(hasType(
          hasUnqualifiedDesugaredType(constantArrayType(hasSize(7)))))))))));
}

/// \brief Matches __builtin_types_compatible_p:
/// GNU extension to check equivalent types
/// Given
/// \code
///   __builtin_types_compatible_p(int, int)
/// \endcode
//  will generate TypeTraitExpr <...> 'int'
const internal::VariadicDynCastAllOfMatcher<Stmt, TypeTraitExpr> typeTraitExpr;

TEST(ImportExpr, ImportTypeTraitExpr) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(testImport("void declToImport() { "
                         "  __builtin_types_compatible_p(int, int);"
                         "}",
                         Lang_C, "", Lang_C, Verifier,
                         functionDecl(
                           hasBody(
                             compoundStmt(
                               has(
                                 typeTraitExpr(hasType(asString("int")))))))));
}

TEST(ImportExpr, ImportTypeTraitExprValDep) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(testImport("template<typename T> struct declToImport {"
                         "  void m() { __is_pod(T); };"
                         "};",
                         Lang_CXX11, "", Lang_CXX11, Verifier,
                         classTemplateDecl(
                           has(
                             cxxRecordDecl(
                               has(
                                 functionDecl(
                                   hasBody(
                                     compoundStmt(
                                       has(
                                         typeTraitExpr(
                                           hasType(asString("_Bool"))
                                           )))))))))));
}

TEST(ImportDecl, ImportRecordDeclInFuncParams) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(
        testImport(
          "int declToImport(struct data_t{int a;int b;} *d){ return 0; }",
          Lang_CXX, "", Lang_CXX, Verifier,
          functionDecl()));
}


TEST(ImportDecl, ImportFunctionTemplateDecl) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(
        testImport(
          "template <typename T> void declToImport() { };",
          Lang_CXX, "", Lang_CXX, Verifier,
          functionTemplateDecl()));
  EXPECT_TRUE(testImport(
      "template<typename Y> int a() { return 1; }"
      "template<typename Y, typename D> int a(){ return 2; }"
      "void declToImport() { a<void>(); }",
      Lang_CXX, "", Lang_CXX, Verifier,
      functionDecl(has(compoundStmt(has(callExpr(has(ignoringParenImpCasts(
          declRefExpr(to(functionDecl(hasBody(compoundStmt(
              has(returnStmt(has(integerLiteral(equals(1)))))))))))))))))));
  EXPECT_TRUE(testImport(
      "template<typename Y> int a() { return 1; }"
      "template<typename Y, typename D> int a() { return 2; }"
      "void declToImport() { a<void,void>(); }",
      Lang_CXX, "", Lang_CXX, Verifier,
      functionDecl(has(compoundStmt(has(callExpr(has(ignoringParenImpCasts(
          declRefExpr(to(functionDecl(hasBody(compoundStmt(
              has(returnStmt(has(integerLiteral(equals(2)))))))))))))))))));
}

TEST(ImportExpr, ImportClassTemplatePartialSpecialization) {
  MatchVerifier<Decl> Verifier;
  auto Code =  R"s(
struct declToImport {
  template <typename T0>
  struct X;
   template <typename T0>
  struct X<T0*> {};
};
                   )s";

  EXPECT_TRUE(testImport(Code, Lang_CXX, "", Lang_CXX, Verifier, recordDecl()));
}

TEST(ImportExpr, ImportClassTemplatePartialSpecializationComplex) {
  MatchVerifier<Decl> Verifier;
  auto Code = R"s(
// excerpt from <functional>

namespace declToImport {

template <typename _MemberPointer>
class _Mem_fn;

template <typename _Tp, typename _Class>
_Mem_fn<_Tp _Class::*> mem_fn(_Tp _Class::*);

template <typename _Res, typename _Class>
class _Mem_fn<_Res _Class::*> {
    template <typename _Signature>
    struct result;

    template <typename _CVMem, typename _Tp>
    struct result<_CVMem(_Tp)> {};

    template <typename _CVMem, typename _Tp>
    struct result<_CVMem(_Tp&)> {};
};

} // namespace
                  )s";

  EXPECT_TRUE(testImport(Code, Lang_CXX, "", Lang_CXX, Verifier,
                         namespaceDecl()));
}

const internal::VariadicDynCastAllOfMatcher<Expr, UnresolvedMemberExpr>
    unresolvedMemberExpr;
TEST(ImportExpr, ImportUnresolvedMemberExpr) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(testImport("struct S { template <typename T> void mem(); };"
                         "template <typename U> void declToImport() {"
                         "S s;"
                         "s.mem<U>();"
                         "}",
                         Lang_CXX, "", Lang_CXX, Verifier,
                         functionTemplateDecl(has(functionDecl(has(compoundStmt(
                             has(callExpr(has(unresolvedMemberExpr()))))))))));
}

const internal::VariadicDynCastAllOfMatcher<Expr, DependentScopeDeclRefExpr>
    dependentScopeDeclRefExpr;
TEST(ImportExpr, ImportDependentScopeDeclRefExpr) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(testImport("template <typename T> struct S;"
                         "template <typename T> void declToImport() {"
                         "S<T>::foo;"
                         "}",
                         Lang_CXX, "", Lang_CXX, Verifier,
                         functionTemplateDecl(has(functionDecl(has(compoundStmt(
                             has(dependentScopeDeclRefExpr()))))))));

  EXPECT_TRUE(testImport("template <typename T> struct S;"
                         "template <typename T> void declToImport() {"
                         "S<T>::template foo;"
                         "}",
                         Lang_CXX, "", Lang_CXX, Verifier,
                         functionTemplateDecl(has(functionDecl(has(compoundStmt(
                             has(dependentScopeDeclRefExpr()))))))));

  EXPECT_TRUE(testImport("template <typename T> struct S;"
                         "template <typename T> void declToImport() {"
                         "S<T>::template foo<>;"
                         "}",
                         Lang_CXX, "", Lang_CXX, Verifier,
                         functionTemplateDecl(has(functionDecl(has(compoundStmt(
                             has(dependentScopeDeclRefExpr()))))))));

  EXPECT_TRUE(testImport("template <typename T> struct S;"
                         "template <typename T> void declToImport() {"
                         "S<T>::template foo<T>;"
                         "}",
                         Lang_CXX, "", Lang_CXX, Verifier,
                         functionTemplateDecl(has(functionDecl(has(compoundStmt(
                             has(dependentScopeDeclRefExpr()))))))));
}
const internal::VariadicDynCastAllOfMatcher<Type, DependentNameType>
    dependentNameType;
TEST(ImportExpr, DependentNameType) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(testImport("template <typename T> struct declToImport {"
                         "typedef typename T::type dependent_name;"
                         "};",
                         Lang_CXX11, "", Lang_CXX11, Verifier,
                         classTemplateDecl(has(cxxRecordDecl(
                             has(typedefDecl(has(dependentNameType()))))))));
}

TEST(ImportExpr, DependentSizedArrayType) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(testImport("template<typename T, int Size> class declToImport {"
                         "  T data[Size];"
                         "};",
                         Lang_CXX, "", Lang_CXX, Verifier,
                         classTemplateDecl(has(cxxRecordDecl(has(fieldDecl(
                             hasType(dependentSizedArrayType()))))))));
}

TEST(ImportExpr, CXXOperatorCallExpr) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(testImport("class declToImport {"
                         "  void f() { *this = declToImport(); }"
                         "};",
                         Lang_CXX, "", Lang_CXX, Verifier,
                         cxxRecordDecl(has(cxxMethodDecl(hasBody(compoundStmt(
                             has(exprWithCleanups(
                                 has(cxxOperatorCallExpr()))))))))));
}

TEST(ImportExpr, ImportUnresolvedLookupExpr) {
  MatchVerifier<Decl> Verifier;
  testImport("template<typename T> int foo();"
             "template <typename T> void declToImport() {"
             "  ::foo<T>;"
             "  ::template foo<T>;"
             "}"
             "void instantiate() { declToImport<int>(); }",
             Lang_CXX, "", Lang_CXX, Verifier,
             functionTemplateDecl(has(functionDecl(
                 has(compoundStmt(has(unresolvedLookupExpr())))))));
}

TEST(ImportExpr, ImportCXXUnresolvedConstructExpr) {
  MatchVerifier<Decl> Verifier;
  testImport("template <typename T> struct C { T t; };"
             "template <typename T> void declToImport() {"
             "  C<T> d;"
             "  d.t = T();"
             "}"
             "void instantiate() { declToImport<int>(); }",
             Lang_CXX, "", Lang_CXX, Verifier,
             functionTemplateDecl(has(functionDecl(has(compoundStmt(has(
                 binaryOperator(has(cxxUnresolvedConstructExpr())))))))));
  testImport("template <typename T> struct C { T t; };"
             "template <typename T> void declToImport() {"
             "  C<T> d;"
             "  (&d)->t = T();"
             "}"
             "void instantiate() { declToImport<int>(); }",
             Lang_CXX, "", Lang_CXX, Verifier,
             functionTemplateDecl(has(functionDecl(has(compoundStmt(has(
                 binaryOperator(has(cxxUnresolvedConstructExpr())))))))));
}

/// Check that function "declToImport()" (which is the templated function
/// for corresponding FunctionTemplateDecl) is not added into DeclContext.
/// Same for class template declarations.
TEST(ImportDecl, ImportTemplatedDeclForTemplate) {
  MatchVerifier<Decl> Verifier;
  testImport("template <typename T> void declToImport() { T a = 1; }"
             "void instantiate() { declToImport<int>(); }",
             Lang_CXX, "", Lang_CXX, Verifier,
             functionTemplateDecl(hasAncestor(translationUnitDecl(
                 unless(has(functionDecl(hasName("declToImport"))))))));
  testImport("template <typename T> struct declToImport { T t; };"
             "void instantiate() { declToImport<int>(); }",
             Lang_CXX, "", Lang_CXX, Verifier,
             classTemplateDecl(hasAncestor(translationUnitDecl(
                 unless(has(cxxRecordDecl(hasName("declToImport"))))))));
}

TEST_F(Fixture, TUshouldNotContainTemplatedDeclOfFunctionTemplates) {

  Decl *From, *To;
  std::tie(From, To) =
      getImportedDecl("template <typename T> void declToImport() { T a = 1; }"
                      "void instantiate() { declToImport<int>(); }",
                      Lang_CXX, "", Lang_CXX);

  auto Check = [](Decl *D) -> bool {
    auto TU = D->getTranslationUnitDecl();
    for (auto Child : TU->decls()) {
      if (FunctionDecl *FD = dyn_cast<FunctionDecl>(Child)) {
        if (FD->getNameAsString() == "declToImport") {
          GTEST_NONFATAL_FAILURE_(
              "TU should not contain any FunctionDecl with name declToImport");
          TU->dump();
          return false;
        }
      }
    }
    return true;
  };

  assert(Check(From));
  Check(To);
}

TEST_F(Fixture, TUshouldNotContainTemplatedDeclOfClassTemplates) {

  Decl *From, *To;
  std::tie(From, To) =
      getImportedDecl("template <typename T> struct declToImport { T t; };"
                      "void instantiate() { declToImport<int>(); }",
                      Lang_CXX, "", Lang_CXX);

  auto Check = [](Decl *D) -> bool {
    auto TU = D->getTranslationUnitDecl();
    for (auto Child : TU->decls()) {
      if (CXXRecordDecl *RD = dyn_cast<CXXRecordDecl>(Child)) {
        if (RD->getNameAsString() == "declToImport") {
          GTEST_NONFATAL_FAILURE_(
              "TU should not contain any CXXRecordDecl with name declToImport");
          TU->dump();
          return false;
        }
      }
    }
    return true;
  };

  assert(Check(From));
  Check(To);
}

TEST_F(Fixture, TUshouldNotContainTemplatedDeclOfTypeAlias) {

  Decl *From, *To;
  std::tie(From, To) =
      getImportedDecl(
          "template <typename T> struct X {};"
          "template <typename T> using declToImport = X<T>;"
          "void instantiate() { declToImport<int> a; }",
                      Lang_CXX11, "", Lang_CXX11);

  auto Check = [](Decl *D) -> bool {
    auto TU = D->getTranslationUnitDecl();
    for (auto Child : TU->decls()) {
      if (TypeAliasDecl *AD = dyn_cast<TypeAliasDecl>(Child)) {
        if (AD->getNameAsString() == "declToImport") {
          GTEST_NONFATAL_FAILURE_(
              "TU should not contain any TypeAliasDecl with name declToImport");
          TU->dump();
          return false;
        }
      }
    }
    return true;
  };

  assert(Check(From));
  Check(To);
}

TEST_F(
    Fixture,
    TUshouldNotContainClassTemplateSpecializationOfImplicitInstantiation) {

  Decl *From, *To;
  std::tie(From, To) = getImportedDecl(
      R"(
        template<class T>
        class Base {};
        class declToImport : public Base<declToImport> {};
    )",
      Lang_CXX, "", Lang_CXX);

  // Check that the ClassTemplateSpecializationDecl is NOT the child of the TU
  auto Pattern =
      translationUnitDecl(unless(has(classTemplateSpecializationDecl())));
  ASSERT_TRUE(
      MatchVerifier<Decl>{}.match(From->getTranslationUnitDecl(), Pattern));
  EXPECT_TRUE(
      MatchVerifier<Decl>{}.match(To->getTranslationUnitDecl(), Pattern));

  // Check that the ClassTemplateSpecializationDecl is the child of the
  // ClassTemplateDecl
  Pattern = translationUnitDecl(has(classTemplateDecl(
      hasName("Base"), has(classTemplateSpecializationDecl()))));
  ASSERT_TRUE(
      MatchVerifier<Decl>{}.match(From->getTranslationUnitDecl(), Pattern));
  EXPECT_TRUE(
      MatchVerifier<Decl>{}.match(To->getTranslationUnitDecl(), Pattern));
}

TEST_F(
    Fixture,
    TUshouldContainClassTemplateSpecializationOfExplicitInstantiation) {

  Decl *From, *To;
  std::tie(From, To) = getImportedDecl(
      R"(
        namespace NS {
          template<class T>
          class X {};
          template class X<int>;
        }
    )",
      Lang_CXX, "", Lang_CXX, "NS");

  // Check that the ClassTemplateSpecializationDecl is NOT the child of the
  // ClassTemplateDecl
  auto Pattern = namespaceDecl(has(classTemplateDecl(
      hasName("X"), unless(has(classTemplateSpecializationDecl())))));
  ASSERT_TRUE(MatchVerifier<Decl>{}.match(From, Pattern));
  EXPECT_TRUE(MatchVerifier<Decl>{}.match(To, Pattern));

  // Check that the ClassTemplateSpecializationDecl is the child of the
  // NamespaceDecl
  Pattern = namespaceDecl(has(classTemplateSpecializationDecl(hasName("X"))));
  ASSERT_TRUE(MatchVerifier<Decl>{}.match(From, Pattern));
  EXPECT_TRUE(MatchVerifier<Decl>{}.match(To, Pattern));
}


TEST_F(Fixture, CXXRecordDeclFieldsShouldBeInCorrectOrder) {


  Decl *From, *To;
  std::tie(From, To) =
      getImportedDecl(
          "struct declToImport { int a; int b; };",
                      Lang_CXX11, "", Lang_CXX11);

  MatchVerifier<Decl> Verifier;
  ASSERT_TRUE(Verifier.match(From, cxxRecordDecl(has(fieldDecl()))));
  ASSERT_TRUE(Verifier.match(To, cxxRecordDecl(has(fieldDecl()))));

  auto Check = [](Decl *D) -> bool {
    std::array<const char*, 2> FieldNamesInOrder{{"a", "b"}};
    int i = 0;
    for (auto Child : cast<DeclContext>(D)->decls()) {
      if (FieldDecl *FD = dyn_cast<FieldDecl>(Child)) {
        if (FD->getNameAsString() != FieldNamesInOrder[i++]) {
          GTEST_NONFATAL_FAILURE_(
              "Fields are in wrong order");
          cast<DeclContext>(D)->dumpDeclContext();
          D->dump();
          return false;
        }
      }
    }
    return true;
  };

  assert(Check(From));
  Check(To);
}

TEST_F(Fixture,
    CXXRecordDeclFieldsShouldBeInCorrectOrderEvenWhenWeImportFirstTheLastDecl) {

  Decl *From, *To;
  std::tie(From, To) = getImportedDecl(
      // The original recursive algorithm of ASTImporter first imports 'c' then
      // 'b' and lastly 'a'.  Therefore we must restore the order somehow.
      R"s(
      struct declToImport {
          int a = c + b;
          int b = 1;
          int c = 2;
      };
      )s",
      Lang_CXX11, "", Lang_CXX11);

  MatchVerifier<Decl> Verifier;
  ASSERT_TRUE(Verifier.match(From, cxxRecordDecl(has(fieldDecl()))));
  ASSERT_TRUE(Verifier.match(To, cxxRecordDecl(has(fieldDecl()))));

  auto Check = [](Decl *D) -> bool {
    std::array<const char*, 3> FieldNamesInOrder{{"a", "b", "c"}};
    int i = 0;
    for (auto Child : cast<DeclContext>(D)->decls()) {
      if (FieldDecl *FD = dyn_cast<FieldDecl>(Child)) {
        if (FD->getNameAsString() != FieldNamesInOrder[i++]) {
          GTEST_NONFATAL_FAILURE_(
              "Fields are in wrong order");
          cast<DeclContext>(D)->dumpDeclContext();
          D->dump();
          return false;
        }
      }
    }
    return true;
  };

  assert(Check(From));
  Check(To);
}

TEST_F(Fixture, ShouldImportImplicitCXXRecordDecl) {
  Decl *From, *To;
  std::tie(From, To) = getImportedDecl(
    R"(
    template <typename U>
    struct declToImport {
    };
    )",Lang_CXX, "", Lang_CXX);

  MatchVerifier<Decl> Verifier;
  // matches the implicit decl
  auto Matcher = classTemplateDecl(has(cxxRecordDecl(has(cxxRecordDecl()))));
  ASSERT_TRUE(Verifier.match(From, Matcher));
  EXPECT_TRUE(Verifier.match(To, Matcher));
}

TEST_F(Fixture, IDNSOrdinary) {
  Decl *From, *To;
  std::tie(From, To) = getImportedDecl(
    R"(
    void declToImport() {}
    )",Lang_CXX, "", Lang_CXX);

  MatchVerifier<Decl> Verifier;
  auto Matcher = functionDecl();
  ASSERT_TRUE(Verifier.match(From, Matcher));
  EXPECT_TRUE(Verifier.match(To, Matcher));
  EXPECT_EQ(From->getIdentifierNamespace(), To->getIdentifierNamespace());
}

// Matcher class to retrieve the last matched node under a given AST.
template <typename NodeType>
class LastDeclMatcher : public MatchFinder::MatchCallback {
  NodeType *Node = nullptr;
  void run(const MatchFinder::MatchResult &Result) override {
    Node = const_cast<NodeType *>(Result.Nodes.getNodeAs<NodeType>(""));
  }

public:
  // Returns the last matched node under the tree rooted in `D`.
  template <typename MatcherType>
  NodeType *match(const Decl *D, const MatcherType &AMatcher) {
    MatchFinder Finder;
    Finder.addMatcher(AMatcher.bind(""), this);
    Finder.matchAST(D->getASTContext());
    assert(Node);
    return Node;
  }
};

TEST_F(Fixture, IDNSOfNonmemberOperator) {
  Decl *FromTU = getTuDecl(
    R"(
    struct X {};
    void operator<<(int, X);
    )",Lang_CXX);
  Decl* From = LastDeclMatcher<Decl>{}.match(FromTU, functionDecl());
  const Decl* To = Import(From, Lang_CXX);
  EXPECT_EQ(From->getIdentifierNamespace(), To->getIdentifierNamespace());
}

TEST(ImportExpr, CXXTypeidExpr) {
  MatchVerifier<Decl> Verifier;
  EXPECT_TRUE(testImport("namespace std { struct type_info {}; }"
                         "class C {}; void declToImport() { typeid(C); }",
                         Lang_CXX11, "", Lang_CXX11, Verifier,
                         functionDecl(hasBody(compoundStmt(
                            has(cxxTypeidExpr()))))));
  EXPECT_TRUE(testImport("namespace std { struct type_info {}; }"
                         "class C {};"
                         "void declToImport() { C c; typeid(c); }",
                         Lang_CXX11, "", Lang_CXX11, Verifier,
                         functionDecl(hasBody(compoundStmt(
                            has(expr(allOf(cxxTypeidExpr(),
                               has(declRefExpr(to(
                                  varDecl(allOf(hasName("c"),
                                                hasType(cxxRecordDecl(
                                                   hasName("C"))))))))))))))));
}

} // end namespace ast_matchers
} // end namespace clang
