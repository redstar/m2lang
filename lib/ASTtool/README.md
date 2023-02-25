# ASTtool - An abstract syntax tree generator

ASTtool generates the C++ classes representing an abstract syntax tree (AST),
using a very concise syntax.

## Introduction

When creating the classes for an AST then you typically deal with value classes,
tree nodes, and sequences. ASTtool is designed to generate those classes for you.

The input for ASTtool is devided into 2 sections, a header and the definitions.
The syntax is loosely inspired by yacc.

Let's make a simple example. A common element of an AST is an identifer. It
consits of a location and a name. You can define it as a value class:

    %plain Identifer =
      %in Loc : SMLoc,
      %in Name : StringRef
    ;

The keyword `%plain` indicates that you define a value class. The name of the
class is `Identifier`, and the class has 2 members `Loc` and `Name`. The used
types are LLVM types, but the tool knows nothing about them. To use those
external types you have to introduce them via a type definition. The complete
input file looks like this:

    %language "c++"
    %typedef Loc {llvm::SMLoc}
    %typedef StringRef {llvm::StringRef}
    %%

    %plain Identifier =
      %in Loc : Loc,
      %in String : StringRef
    ;

ASTtool generates the following C++ source code based on this input:

    class Identifier {
      llvm::SMLoc _Loc;
      llvm::StringRef _String;

    public:
      Identifier(llvm::SMLoc _Loc, llvm::StringRef _String)
        : _Loc(_Loc), _String(_String) {}
      Identifier() = default;

      llvm::SMLoc getLoc() {
        return _Loc;
      }

      llvm::StringRef getString() {
        return _String;
      }
    };

Now let's have a look at AST nodes. The difference to value classes is that AST
nodes are dynamically allocated, and that they support LLVM RTTI if used in a
class hierarchy.

Very often, programming languages use declarations to introduce entities.
Usually, there are many variations of declations, all of them having a name. A
natural way to model this as an AST is to create a class hierarchy. In this case,
the base class cannot be used as is, it is really an abstract class. In the
ASTtool language, you can express it in the following way:

    %base Declaration =
       %in Name : Identifier
    ;

    %node VarDeclaration <: Declaration =
       %in Type : StringRef
    ;

With the keyword `%node` you introduce an AST node. In contrast to a value class,
an AST node can have a super class, which follows after the 'derived from' operator
`<:`. A node defined with `%base` is similar to `%node` except that no public
constructor and no RTTI kind member are generated. The generated C++ code looks
as follows:

    class Declaration {
      friend class VarDeclaration;
    public:
      enum class __KindType : unsigned {
        VarDeclaration,
        __Last = VarDeclaration
      };
    protected:
      const __KindType __Kind;
    private:
      Identifier _Name;

    protected:
      Declaration(__KindType __Kind, Identifier _Name)
        : __Kind(__Kind), _Name(_Name) {}
    public:

      Identifier getName() {
        return _Name;
      }

      __KindType kind() { return __Kind; }
    };

    class VarDeclaration : public Declaration {
      llvm::StringRef _Type;

    public:
      VarDeclaration(Identifier _Name, llvm::StringRef _Type)
        : Declaration(__KindType::VarDeclaration, _Name), _Type(_Type) {}

      llvm::StringRef getType() {
        return _Type;
      }

      static bool classof(const Declaration* T) {
        return T->__Kind == __KindType::VarDeclaration;
      }
    };

If you change the `Declaration` node from `%base` to `%node`, then the following
code is generated:

    class Declaration {
      friend class VarDeclaration;
    public:
      enum class __KindType : unsigned {
        Declaration,
        VarDeclaration,
        __Last = VarDeclaration
      };
    protected:
      const __KindType __Kind;
    private:
      Identifier _Name;

    protected:
      Declaration(__KindType __Kind, Identifier _Name)
        : __Kind(__Kind), _Name(_Name) {}
    public:
      Declaration(Identifier _Name)
        : __Kind(__KindType::Declaration), _Name(_Name) {}

      Identifier getName() {
        return _Name;
      }

      __KindType kind() { return __Kind; }
    };

Note some differences and similarities:

 - The enumeration `__Kind` has now a member for `Declaration`
 - A public constructor is generated since this is not an abstract class
 - Like before, no `classof()` is generated because this does not make sense -
   the class is the root of the class hierarchy

Another often required feature is to have a sequence of elements. For example,
an `EnumDeclaration` may need to hold the names of the enum members:

    %node EnumDeclaration <: Declaration =
       %in Members : %list Identifier
    ;

This generates the following code:

    class EnumDeclaration : public Declaration {
      llvm::SmallVector<Identifier, 4> _Members;

    public:
      EnumDeclaration(Identifier _Name, const llvm::SmallVector<Identifier, 4> &_Members)
        : Declaration(__KindType::EnumDeclaration, _Name), _Members(_Members) {}

      const llvm::SmallVector<Identifier, 4> &getMembers() {
        return _Members;
      }

      static bool classof(const Declaration* T) {
        return T->__Kind == __KindType::EnumDeclaration;
      }
    };

You can also use an AST node class:

    %node EnumDeclaration <: Declaration =
      %in Members : %list Declaration
    ;

AST nodes are dynamically allocated, so a pointer type is used in the generated code:

    class EnumDeclaration : public Declaration {
      llvm::SmallVector<Declaration *, 4> _Members;

    public:
      EnumDeclaration(Identifier _Name, const llvm::SmallVector<Declaration *, 4> &_Members)
        : Declaration(__KindType::EnumDeclaration, _Name), _Members(_Members) {}

      const llvm::SmallVector<Declaration *, 4> &getMembers() {
        return _Members;
      }

      static bool classof(const Declaration* T) {
        return T->__Kind == __KindType::EnumDeclaration;
      }
    };

The definition of a field can have a prefix. In the examples so far I used the
prefix `%in` which means that this fiels is initialized with the constructor,
and only a get method to retrieve the value is generated. If you leave out the
value, then only a get and a set method for the field is generated. You can
think of ab `%in` field as an inherited attribute - the value is derived from
the context of the node and available at the time the node is generated.
Synthesized attributes are compued later, so there must be a possibility to set
that value after the node is constructed. Occasionally you want to be able to
change the value recorded at construction time. You can use the prefix `%in %out`
to achieve this.

For example assume you creates an AST hierarchy to model expressions, and you have a
field `IsConst` which describes if this part is constant or not. Often, the value is
available when you creat the AST node. But a later analysis may come to a different
result, so you need to be able to overwrite the value: this is the use case for the
`%in %out` properties. For an integer literal, the value is always `true` at
construction time, so you can provide that value using `%let`:

    %base Expression =
      %in %out IsConst : bool
    ;

    %node IntLiteral <: Expression =
      %let IsConst = {true}
    ;

In the generated code, the constructor for class `IntLiteral` has no argument for
`IsConst`, and the value `true` is used when initializing the super class.

    class Expression {
      friend class IntLiteral;
    public:
      enum class __KindType : unsigned {
        IntLiteral,
        __Last = IntLiteral
      };
    protected:
      const __KindType __Kind;
    private:
      bool _IsConst;

    protected:
      Expression(__KindType __Kind, bool _IsConst)
        : __Kind(__Kind), _IsConst(_IsConst) {}
    public:

      bool isIsConst() {
        return _IsConst;
      }

      void setIsConst(bool _IsConst) {
        this->_IsConst = _IsConst;
      }

      __KindType kind() { return __Kind; }
    };

    class IntLiteral : public Expression {
    public:
      IntLiteral()
        : Expression(__KindType::IntLiteral, true) {}

      static bool classof(const Expression* T) {
        return T->__Kind == __KindType::IntLiteral;
      }
    };

## Customizing the generated Code

In the header you can define several values to change the generated source code.
For example, to use `yy` instead of `_` as a prefix:

    %define api.prefix "yy"


## Future development

- There a couple other useful declarations which could be added, e.g. `%using` (like in C++) and
`%union` (a tagged union of AST nodes).
- Integration with LLtool, to automatically generate an AST at parse time.