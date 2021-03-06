//===- ClangFnMapGen.cpp -----------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===--------------------------------------------------------------------===//
//
// Clang tool which creates a list of defined functions and the files in which
// they are defined.
//
//===--------------------------------------------------------------------===//

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/GlobalDecl.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Index/USRGeneration.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Signals.h"
#include <sstream>
#include <string>
#include <vector>

using namespace llvm;
using namespace clang;
using namespace clang::tooling;

static cl::OptionCategory ClangFnMapGenCategory("clang-fnmapgen options");

class MapFunctionNamesConsumer : public ASTConsumer {
public:
  MapFunctionNamesConsumer(ASTContext &Context) : Ctx(Context) {}

  ~MapFunctionNamesConsumer() {
    // Flush results to standard output.
    llvm::outs() << DefinedFuncsStr.str();
  }

  virtual void HandleTranslationUnit(ASTContext &Ctx) {
    handleDecl(Ctx.getTranslationUnitDecl());
  }

private:
  std::string getLookupName(const FunctionDecl *FD);
  void handleDecl(const Decl *D);

  ASTContext &Ctx;
  std::stringstream DefinedFuncsStr;
  std::string CurrentFileName;
};

void MapFunctionNamesConsumer::handleDecl(const Decl *D) {
  if (!D)
    return;

  if (const auto *FD = dyn_cast<FunctionDecl>(D)) {
    if (FD->isThisDeclarationADefinition()) {
      if (const Stmt *Body = FD->getBody()) {
        SmallString<128> LookupName;
        bool Res = index::generateUSRForDecl(D, LookupName);
        assert(!Res);
        (void)Res;
        const SourceManager &SM = Ctx.getSourceManager();
        if (CurrentFileName.empty()) {
          CurrentFileName =
              SM.getFileEntryForID(SM.getMainFileID())->tryGetRealPathName();
          if (CurrentFileName.empty())
            CurrentFileName = "invalid_file";
        }

        switch (FD->getLinkageInternal()) {
        case ExternalLinkage:
        case VisibleNoLinkage:
        case UniqueExternalLinkage:
          if (SM.isInMainFile(Body->getLocStart()))
            DefinedFuncsStr << LookupName.str().str() << " " << CurrentFileName
                            << "\n";
        default:
          break;
        }
      }
    }
  }

  if (const auto *DC = dyn_cast<DeclContext>(D))
    for (const Decl *D : DC->decls())
      handleDecl(D);
}

class MapFunctionNamesAction : public ASTFrontendAction {
protected:
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 llvm::StringRef) {
    std::unique_ptr<ASTConsumer> PFC(
        new MapFunctionNamesConsumer(CI.getASTContext()));
    return PFC;
  }
};

static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);

int main(int argc, const char **argv) {
  // Print a stack trace if we signal out.
  sys::PrintStackTraceOnErrorSignal(argv[0], false);
  PrettyStackTraceProgram X(argc, argv);

  const char *Overview = "\nThis tool collects the USR name and location "
                         "of all functions definitions in the source files "
                         "(excluding headers).\n";
  CommonOptionsParser OptionsParser(argc, argv, ClangFnMapGenCategory,
                                    cl::ZeroOrMore, Overview);

  ClangTool Tool(OptionsParser.getCompilations(),
                 OptionsParser.getSourcePathList());
  Tool.run(newFrontendActionFactory<MapFunctionNamesAction>().get());
  return 0;
}
