ASTtool - An abstract syntax tree generator
===========================================

Introduction
------------

ASTtool generates the C++ classes representing an abstract syntax tree (AST),
using a very concise syntax.

Examples
--------

A value class with a location and an identifier:

    %plain Identifer =
      %in Loc : SMLoc,
      %in Name : StringRef
    ;

A base class and a derived class:

    %base TypeDenoter ;

    %node PervasiveType <: TypeDenoter =
      %in TypeKind : PervasiveTypeKind
    ;

Use of a list type:

    %node RecordType <: TypeDenoter =
      %in Fields : %list FixedRecordField
    ;

Providing a default value:

    %base Expression =
      %in %out TypeDenoter : TypeDenoter,
      %in Const : bool
    ;

    %node IntegerLiteral <: Expression =
      %in Value : APInt,
      %let Const = {true}
    ;
