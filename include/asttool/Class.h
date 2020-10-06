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
  enum Property { In = 0x01, Out = 0x02 };

private:
  Property Properties;
  llvm::StringRef Name;
  llvm::StringRef TypeName;
  bool TypeIsList;

public:
  Member() {}

  Property getProperties() { return Properties; }
  llvm::StringRef getName() { return Name; };
  llvm::StringRef getTypeName() { return TypeName; }
  bool getTypeIsList() { return TypeIsList; }
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
  Class() {}

  ClassType getType() { return Type; }
  llvm::StringRef getName() { return Name; };
};
} // namespace asttool
#endif
