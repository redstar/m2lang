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

namespace asttool {

class Member {
public:
public:
  enum MemberKind { MK_Enum, MK_Field };

private:
  const MemberKind Kind;

protected:
  Member(MemberKind Kind) : Kind(Kind) {]
  }

public:
  MemberKind getKind() const { return Kind; }
};

class Enum : Member {
  llvm::StringRef Name;
  llvm::StringRef Code;

public:
  Enum(llvm::StringRef Name, llvm::StringRef Code) : Name(Name), Code(Code) {}

  llvm::StringRef getName() { return Name; };
  llvm::StringRef getCode() { return Code; };

  static bool classof(const Member *M) { return M->getKind() == MK_Enum; }
};

class Field : Member {
public:
  enum Property { In = 0x01, Out = 0x02 };

private:
  unsigned Properties;
  llvm::StringRef Name;
  llvm::StringRef TypeName;
  bool TypeIsList;

public:
  Field(unsigned Properties, llvm::StringRef Name, llvm::StringRef TypeName,
        bool TypeIsList)
      : Properties(Properties), Name(name) TypeName(TypeName),
        TypeIsList(TypeIsList) {}

  unsigned getProperties() { return Properties; }
  llvm::StringRef getName() { return Name; };
  llvm::StringRef getTypeName() { return TypeName; }
  bool getTypeIsList() { return TypeIsList; }

  static bool classof(const Member *M) { return M->getKind() == MK_Field; }
};

class Class {
public:
  enum ClassType { Plain, Abstract, Normal };

private:
  ClassType Type;
  llvm::StringRef Name;
  llvm::StringRef SuperClass;
  llvm::SmallVectorImpl<Member> Members;

public:
  Class(ClassType Type, llvm::StringRef Name, llvm::StringRef SuperClass,
        llvm::SmallVectorImpl<Member> &Members)
      : Type(Type), Name(Name), SuperClass(SuperClass), Members(Members) {}

  ClassType getType() const { return Type; }
  llvm::StringRef getName() const { return Name; };
  llvm::StringRef getSuperClass() const { return SuperClass; };
  const llvm::SmallVectorImpl<Member> &getMembers() const { return Members; }
};
} // namespace asttool
#endif
