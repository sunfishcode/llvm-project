//===- WebAssembly.cpp ----------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "ABIInfoImpl.h"
#include "TargetInfo.h"

using namespace clang;
using namespace clang::CodeGen;

static bool passTypeAsValues(QualType T, ASTContext &Context, size_t &Num);
static bool passRecordTypeAsValues(QualType T, ASTContext &Context, size_t &Num);

//===----------------------------------------------------------------------===//
// WebAssembly ABI Implementation
//
// This is a very simple ABI that relies a lot on DefaultABIInfo.
//===----------------------------------------------------------------------===//

class WebAssemblyABIInfo final : public ABIInfo {
  DefaultABIInfo defaultInfo;
  WebAssemblyABIKind Kind;

public:
  explicit WebAssemblyABIInfo(CodeGen::CodeGenTypes &CGT,
                              WebAssemblyABIKind Kind)
      : ABIInfo(CGT), defaultInfo(CGT), Kind(Kind) {}

private:
  ABIArgInfo classifyReturnType(QualType RetTy) const;
  ABIArgInfo classifyArgumentType(QualType Ty) const;

  // DefaultABIInfo's classifyReturnType and classifyArgumentType are
  // non-virtual, but computeInfo and EmitVAArg are virtual, so we
  // overload them.
  void computeInfo(CGFunctionInfo &FI) const override {
    if (!getCXXABI().classifyReturnType(FI))
      FI.getReturnInfo() = classifyReturnType(FI.getReturnType());
    for (auto &Arg : FI.arguments())
      Arg.info = classifyArgumentType(Arg.type);
  }

  Address EmitVAArg(CodeGenFunction &CGF, Address VAListAddr,
                    QualType Ty) const override;
};

class WebAssemblyTargetCodeGenInfo final : public TargetCodeGenInfo {
public:
  explicit WebAssemblyTargetCodeGenInfo(CodeGen::CodeGenTypes &CGT,
                                        WebAssemblyABIKind K)
      : TargetCodeGenInfo(std::make_unique<WebAssemblyABIInfo>(CGT, K)) {
    SwiftInfo =
        std::make_unique<SwiftABIInfo>(CGT, /*SwiftErrorInRegister=*/false);
  }

  void setTargetAttributes(const Decl *D, llvm::GlobalValue *GV,
                           CodeGen::CodeGenModule &CGM) const override {
    TargetCodeGenInfo::setTargetAttributes(D, GV, CGM);
    if (const auto *FD = dyn_cast_or_null<FunctionDecl>(D)) {
      if (const auto *Attr = FD->getAttr<WebAssemblyImportModuleAttr>()) {
        llvm::Function *Fn = cast<llvm::Function>(GV);
        llvm::AttrBuilder B(GV->getContext());
        B.addAttribute("wasm-import-module", Attr->getImportModule());
        Fn->addFnAttrs(B);
      }
      if (const auto *Attr = FD->getAttr<WebAssemblyImportNameAttr>()) {
        llvm::Function *Fn = cast<llvm::Function>(GV);
        llvm::AttrBuilder B(GV->getContext());
        B.addAttribute("wasm-import-name", Attr->getImportName());
        Fn->addFnAttrs(B);
      }
      if (const auto *Attr = FD->getAttr<WebAssemblyExportNameAttr>()) {
        llvm::Function *Fn = cast<llvm::Function>(GV);
        llvm::AttrBuilder B(GV->getContext());
        B.addAttribute("wasm-export-name", Attr->getExportName());
        Fn->addFnAttrs(B);
      }
    }

    if (auto *FD = dyn_cast_or_null<FunctionDecl>(D)) {
      llvm::Function *Fn = cast<llvm::Function>(GV);
      if (!FD->doesThisDeclarationHaveABody() && !FD->hasPrototype())
        Fn->addFnAttr("no-prototype");
    }
  }

  /// Return the WebAssembly externref reference type.
  virtual llvm::Type *getWasmExternrefReferenceType() const override {
    return llvm::Type::getWasm_ExternrefTy(getABIInfo().getVMContext());
  }
  /// Return the WebAssembly funcref reference type.
  virtual llvm::Type *getWasmFuncrefReferenceType() const override {
    return llvm::Type::getWasm_FuncrefTy(getABIInfo().getVMContext());
  }
};

/// Classify argument of given type \p Ty.
ABIArgInfo WebAssemblyABIInfo::classifyArgumentType(QualType Ty) const {
  Ty = useFirstFieldIfTransparentUnion(Ty);

  if (isAggregateTypeForABI(Ty)) {
    // Records with non-trivial destructors/copy-constructors should not be
    // passed by value.
    if (auto RAA = getRecordArgABI(Ty, getCXXABI()))
      return getNaturalAlignIndirect(Ty, RAA == CGCXXABI::RAA_DirectInMemory);
    // Ignore empty structs/unions.
    if (isEmptyRecord(getContext(), Ty, true))
      return ABIArgInfo::getIgnore();
    // Lower single-element structs to just pass a regular value. TODO: We
    // could do reasonable-size multiple-element structs too, using getExpand(),
    // though watch out for things like bitfields.
    if (const Type *SeltTy = isSingleElementStruct(Ty, getContext()))
      return ABIArgInfo::getDirect(CGT.ConvertType(QualType(SeltTy, 0)));
    // For the experimental multivalue ABI, fully expand all other aggregates
    if (Kind == WebAssemblyABIKind::ExperimentalMV) {
      const RecordType *RT = Ty->getAs<RecordType>();
      assert(RT);
      bool HasBitField = false;
      for (auto *Field : RT->getDecl()->fields()) {
        if (Field->isBitField()) {
          HasBitField = true;
          break;
        }
      }
      if (!HasBitField)
        return ABIArgInfo::getExpand();
    } else if (Kind == WebAssemblyABIKind::ExperimentalA) {
      size_t Num = 16;
      if (passRecordTypeAsValues(Ty, getContext(), Num)) {
        return ABIArgInfo::getExpand();
      }
    } else if (Kind == WebAssemblyABIKind::ExperimentalB) {
      size_t Num = 16;
      if (passRecordTypeAsValues(Ty, getContext(), Num)) {
        return ABIArgInfo::getExpand();
      }
    } else if (Kind == WebAssemblyABIKind::ExperimentalC) {
      size_t Num = 64;
      if (passRecordTypeAsValues(Ty, getContext(), Num)) {
        return ABIArgInfo::getExpand();
      }
    } else if (Kind == WebAssemblyABIKind::ExperimentalD) {
      size_t Num = 4;
      if (passRecordTypeAsValues(Ty, getContext(), Num)) {
        return ABIArgInfo::getExpand();
      }
    }
  }

  // Otherwise just do the default thing.
  return defaultInfo.classifyArgumentType(Ty);
}

ABIArgInfo WebAssemblyABIInfo::classifyReturnType(QualType RetTy) const {
  if (isAggregateTypeForABI(RetTy)) {
    // Records with non-trivial destructors/copy-constructors should not be
    // returned by value.
    if (!getRecordArgABI(RetTy, getCXXABI())) {
      // Ignore empty structs/unions.
      if (isEmptyRecord(getContext(), RetTy, true))
        return ABIArgInfo::getIgnore();
      // Lower single-element structs to just return a regular value. TODO: We
      // could do reasonable-size multiple-element structs too, using
      // ABIArgInfo::getDirect().
      if (const Type *SeltTy = isSingleElementStruct(RetTy, getContext()))
        return ABIArgInfo::getDirect(CGT.ConvertType(QualType(SeltTy, 0)));
      // For the experimental multivalue ABI, return all other aggregates
      if (Kind == WebAssemblyABIKind::ExperimentalMV)
        return ABIArgInfo::getDirect();
      if (Kind == WebAssemblyABIKind::ExperimentalA) {
        size_t Num = 4;
        if (passRecordTypeAsValues(RetTy, getContext(), Num)) {
          return ABIArgInfo::getDirect();
        }
      } else if (Kind == WebAssemblyABIKind::ExperimentalB) {
        size_t Num = 16;
        if (passRecordTypeAsValues(RetTy, getContext(), Num)) {
          return ABIArgInfo::getDirect();
        }
      } else if (Kind == WebAssemblyABIKind::ExperimentalC) {
        size_t Num = 4;
        if (passRecordTypeAsValues(RetTy, getContext(), Num)) {
          return ABIArgInfo::getDirect();
        }
      } else if (Kind == WebAssemblyABIKind::ExperimentalD) {
        size_t Num = 4;
        if (passRecordTypeAsValues(RetTy, getContext(), Num)) {
          return ABIArgInfo::getDirect();
        }
      }
    }
  }

  // Otherwise just do the default thing.
  return defaultInfo.classifyReturnType(RetTy);
}

// Test if `T` can be passed by value, with `Num` flat argument values left.
// Decrement `Num` by the number of flat values used.
static bool passTypeAsValues(QualType T, ASTContext &Context, size_t &Num) {
  if (isAggregateTypeForABI(T)) {
    return passRecordTypeAsValues(T, Context, Num);
  }

  // If we don't have any flat values left, we can't pass values by value.
  if (Num == 0) {
    return false;
  }

  // Use one flat value.
  --Num;
  return true;
}

// Test if `T`, which is an aggregate type, can be passed by value, with `Num`
// flat argument values left. Decrement `Num` by the number of flat values used.
static bool passRecordTypeAsValues(QualType T, ASTContext &Context, size_t &Num) {
  const RecordType *RT = T->getAs<RecordType>();
  if (!RT)
    return false;

  const RecordDecl *RD = RT->getDecl();
  if (RD->hasFlexibleArrayMember())
    return false;

  // If this is a C++ record, check the bases first.
  if (const CXXRecordDecl *CXXRD = dyn_cast<CXXRecordDecl>(RD)) {
    for (const auto &I : CXXRD->bases()) {
      // Ignore empty records.
      if (isEmptyRecord(Context, I.getType(), true))
        continue;

      // If this is non-empty and not a single element struct, the composite
      // cannot be a single element struct.
      bool Found = passRecordTypeAsValues(I.getType(), Context, Num);
      if (!Found)
        return false;
    }
  }

  if (const RecordType *UT = RT->getAsUnionType()) {
    // Determine the union arm that uses the most flat values.
    size_t MinNum = Num;
    for (const auto *FD : UT->getDecl()->fields()) {
      size_t FieldNum = Num;
      QualType FT = FD->getType();

      // Ignore empty fields.
      if (isEmptyField(Context, FD, true))
        continue;

      // Treat single element arrays as the element.
      while (const ConstantArrayType *AT = Context.getAsConstantArrayType(FT)) {
        if (AT->getSize().getZExtValue() != 1)
          break;
        FT = AT->getElementType();
      }

      bool Found = passTypeAsValues(FT, Context, FieldNum);
      if (!Found)
        return false;

      if (FieldNum < MinNum)
        MinNum = FieldNum;
    }

    Num = MinNum;
    return true;
  }

  // Check for single element.
  for (const auto *FD : RD->fields()) {
    QualType FT = FD->getType();

    // Ignore empty fields.
    if (isEmptyField(Context, FD, true))
      continue;

    // Treat single element arrays as the element.
    while (const ConstantArrayType *AT = Context.getAsConstantArrayType(FT)) {
      if (AT->getSize().getZExtValue() != 1)
        break;
      FT = AT->getElementType();
    }

    bool Found = passTypeAsValues(FT, Context, Num);
    if (!Found)
      return false;
  }

  return true;
}

Address WebAssemblyABIInfo::EmitVAArg(CodeGenFunction &CGF, Address VAListAddr,
                                      QualType Ty) const {
  bool IsIndirect = isAggregateTypeForABI(Ty) &&
                    !isEmptyRecord(getContext(), Ty, true) &&
                    !isSingleElementStruct(Ty, getContext());
  return emitVoidPtrVAArg(CGF, VAListAddr, Ty, IsIndirect,
                          getContext().getTypeInfoInChars(Ty),
                          CharUnits::fromQuantity(4),
                          /*AllowHigherAlign=*/true);
}

std::unique_ptr<TargetCodeGenInfo>
CodeGen::createWebAssemblyTargetCodeGenInfo(CodeGenModule &CGM,
                                            WebAssemblyABIKind K) {
  return std::make_unique<WebAssemblyTargetCodeGenInfo>(CGM.getTypes(), K);
}
