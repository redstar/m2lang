# LLtool
A recursive-descent parser generator for C++.

## Purpose

LLtool generates the body of a parser from a context free grammar written in EBNF.
The generated code fragment can be mixed into C++ source.

## Interface

The generated code makes some assumptions about the environment:

- It assumes there are functions `expect()`, `consume()` and `advance()`.
- It assumes that functions `expect()` and `consume()` return `true` in case of error.
- It assumes there exists an enumeration `TokenKind`.
- It assumes there is a member/variable `tok`. The type of `tok` is not important.
  Only a member/property `kind` (of type `TokenKind`) is required.

## Syntax

The input for syntax LLtool is similar to yacc/bison. It has the following specification:

    %token identifier, code, argument, string
    %start lltool
    %%
    lltool : ( header )? ( rule )+ ;

    header : ( "%start" identifier | "%token" tokenlist | "%language" string | "%eoi" identifier )* "%%" ;

    tokenlist : tokendecl ("," tokendecl )* ;

    tokendecl : (identifier | string) ( "=" identifier )? ;

    rule : nonterminal "=" rhs "." ;

    nonterminal : identifier ( argument )? ;

    rhs : sequence ( "|" sequence )* ;

    sequence : ( group | identifier ( argument)? | string | code | "%if" code )* ;

    group : "(" rhs ( ")" | ")?" | ")*" | ")+" ) ;

This specification uses the following tokens:

- `identifier`: a sequence of letters and digits. First element must be a letter.
  Only ASCII characters are supported.
- `string`: an arbitrary sequence of characters, enclosed by `"` and `"` or `'` and `'`.
- `code`: an arbitrary sequence of characters, enclosed by `{.` and `.}`.
- `argument`: an arbitrary sequence of characters, enclosed by `<` and `>` or `<.` and `.>`.

Single-line comments start with `//` and run until the end of line.
Multi-line comments use `/*` and `*/` as delimiters. Multi-line comments may not
be nested.

## Influencing the parsing process

Consider the following example which is a simple version of a `import` statement
with the possibility to use an alias:

    %token id
    %start import
    %%
    import :
      "import" (id ":=")? id;

Because the optional group `(id "=")?` begins with the same token as the symbol
after it (both are `id`), the parser generator can't decide if parsing must
continue with the optional group or with the symbol after the group  if the next
token is `id`. This is an example of an LL(1) conflict. To solve this conflict
it is possible to insert a resolver. A resolver is a `bool` expression, e.g.
`bool isAlias()`. The resolver is inserted at the place of the LL(1) conflict:

    %token id
    %start import
    %%
    import :
      "import" (%if {. isAlias() .} id ":=")? id;

In this case the implementation of the resolver is trivial. It only has to look
one token further in the token range:

    bool isAlias()
    {
        // LL(1) conflict can be resolved with a look ahead of two
        return lexer.save.moveFront.kind == TokenKind.ColonEqual;
    }

Other LL(1) conflicts can be solved in a similar way. The resolver can be as
complex as required, as long as a `bool` value is returned.

Now consider that the language has evolved over time. The original version did
not support the alias name. That was later introduced in version 2. To support
both versions, the grammar now look like:

    %token id
    %start import
    %%
    import :
      "import" id (":=" id)?;

This rule can parse an import with or without an alias name. To support both
language versions with one parser, you can add a predicate to differentiate
between both versions. Like a resolver a predicate must return a `bool` value.
Here the flag `isV2` is used as predicate:

    %token id
    %start import
    %%
    import :
      "import" id (%if {. isV2 .} ":=" id)?;

A predicate can be inserted at the beginning of an optional group or at the
beginning of an sequence in case the sequence itself can derive epsilon or is
embedded in an optional group.

Another approach to solving this task is to deliberately create an LL (1)
conflict and then use a resolver:

    %token id
    %start import
    %%
    import :
      "import" ( %if {. isAlias() && isV2 .} id ":=" id
               | id
               ) ;

LLtools checks if resolver and predicates are placed correctly. Incorrectly
placed resolvers and predicates are ignored and a warning is printed.

## Error handling

A simple local error handling scheme based on _FOLLOW_ sets is implemented. It
uses the so-called panic mode approach.

## Open tasks

- Integration with ASTtool
