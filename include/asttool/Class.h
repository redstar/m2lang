//===--- Class.h - ASTtool data type for classes ----------------*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the Class data structure.
///
//===----------------------------------------------------------------------===//

#ifndef ASTTOOL_CLASS_H
#define ASTTOOL_CLASS_H

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"

namespace asttool {

#if 0
#define AST_DECLARATION
#include "asttool/ast.inc"
#else
class Member {
public:
  enum MemberKind { MK_Enum, MK_Field };

private:
  const MemberKind Kind;
  llvm::SMLoc Loc;

protected:
  Member(MemberKind Kind, llvm::SMLoc Loc) : Kind(Kind), Loc(Loc) {}

public:
  llvm::SMLoc getLoc() const { return Loc; }
  MemberKind getKind() const { return Kind; }
};

class Enum : public Member {
  llvm::StringRef Name;
  llvm::StringRef Code;

public:
  Enum(llvm::SMLoc Loc, llvm::StringRef Name, llvm::StringRef Code)
      : Member(MK_Enum, Loc), Name(Name), Code(Code) {}

  llvm::StringRef getName() { return Name; };
  llvm::StringRef getCode() { return Code; };

  static bool classof(const Member *M) { return M->getKind() == MK_Enum; }
};

class Field : public Member {
public:
  enum Property { In = 0x01, Out = 0x02 };

private:
  unsigned Properties;
  llvm::StringRef Name;
  llvm::StringRef TypeName;
  bool TypeIsList;

public:
  Field(llvm::SMLoc Loc, unsigned Properties, llvm::StringRef Name,
        llvm::StringRef TypeName, bool TypeIsList)
      : Member(MK_Field, Loc), Properties(Properties), Name(Name),
        TypeName(TypeName), TypeIsList(TypeIsList) {}

  unsigned getProperties() { return Properties; }
  llvm::StringRef getName() { return Name; };
  llvm::StringRef getTypeName() { return TypeName; }
  bool getTypeIsList() { return TypeIsList; }

  static bool classof(const Member *M) { return M->getKind() == MK_Field; }
};

class Class {
public:
  enum ClassType { Plain, Base, Node };

private:
  ClassType Type;
  llvm::SMLoc Loc;
  llvm::StringRef Name;
  llvm::StringRef SuperClass;
  llvm::SmallVector<Member *, 8> Members;
  llvm::SmallVector<Class *, 8> SubClasses;

public:
  Class(ClassType Type, llvm::SMLoc Loc, llvm::StringRef Name,
        llvm::StringRef SuperClass, llvm::SmallVectorImpl<Member *> &Members)
      : Type(Type), Loc(Loc), Name(Name), SuperClass(SuperClass),
        Members(std::move(Members)) {}

  ClassType getType() const { return Type; }
  llvm::SMLoc getLoc() const { return Loc; }
  llvm::StringRef getName() const { return Name; };
  llvm::StringRef getSuperClass() const { return SuperClass; };
  const llvm::SmallVectorImpl<Member *> &getMembers() const { return Members; }
  llvm::SmallVectorImpl<Class *> &getSubClasses() { return SubClasses; }
};
#endif
} // namespace asttool
#endif
