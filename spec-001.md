# Specification: Modern PHP Parsing Library in Idiomatic Haskell

## Problem Statement

Haskell developers building developer tooling, static analysis engines, linters, refactoring tools, and code formatters for PHP have no modern, idiomatic parser library available. Existing Haskell PHP parsing libraries are abandoned, target legacy PHP 5.x or 7.x syntax, or rely on imperative parser-generator machinery that yields partial, opaque, or untyped syntax representations. 

Over recent years, the PHP language has evolved dramatically. Active and security-supported PHP releases—specifically PHP 8.2, 8.3, 8.4, and 8.5—introduce syntax constructs such as property hooks, asymmetric visibility, Disjunctive Normal Form (DNF) types, the pipe operator (`|>`), clone-with syntax, direct class dereferencing on instantiation, dynamic class constant fetch, typed class constants, standalone null/false/true types, and attributes. Tool authors currently cannot parse, analyze, or transform modern PHP code in Haskell without encountering parse failures on contemporary, standard PHP syntax.

## Solution

A purely functional, idiomatic Haskell library for parsing modern PHP source code: mathematically principled, declarative, elegant, and composed of algebraic building blocks. 

The library exposes an expressive, parameterized Abstract Syntax Tree (AST) that represents the complete grammar of all currently supported released versions of PHP (PHP 8.2, 8.3, 8.4, and 8.5). It provides a clean, total public API that accepts PHP source text and returns either precise diagnostic parse errors with source positions or fully annotated syntax trees. The library is accompanied by an algebraic pretty-printer supporting round-trip invariants and recursion schemes for seamless traversal and transformation.

## User Stories

1. As a static analysis tool developer, I want to parse complete PHP source files into a strongly typed AST, so that I can analyze PHP codebases reliably.
2. As a linter author, I want parse errors to include precise source span locations (file, line, column), so that I can provide actionable feedback to users.
3. As a developer, I want the parser to support the PHP 8.2 Disjunctive Normal Form (DNF) types (e.g., `(A&B)|C`), so that I can analyze modern type signatures.
4. As a developer, I want the parser to support PHP 8.2 readonly classes, so that immutable domain models are correctly represented in the AST.
5. As a developer, I want the parser to support PHP 8.2 standalone `null`, `false`, and `true` type declarations, so that modern scalar type definitions parse without errors.
6. As a developer, I want the parser to support PHP 8.2 constants defined within traits, so that trait-based constant declarations are captured in the AST.
7. As a developer, I want the parser to support PHP 8.3 typed class constants (e.g., `public const string ROLE = "admin"`), so that constant types are accurately reflected in the AST.
8. As a developer, I want the parser to support PHP 8.3 dynamic class constant fetch (e.g., `ClassName::{$var}`), so that dynamic constant lookups parse cleanly.
9. As a developer, I want the parser to support PHP 8.3 anonymous readonly classes, so that inline immutable class expressions parse without errors.
10. As a developer, I want the parser to support PHP 8.4 property hooks (`get` and `set` blocks and arrow expressions), so that virtual and hooked properties are fully represented in the AST.
11. As a developer, I want the parser to support PHP 8.4 asymmetric property visibility (e.g., `public private(set) string $title`), so that distinct read and write visibilities are captured in the AST.
12. As a developer, I want the parser to support PHP 8.4 new-without-parentheses member dereferencing (e.g., `new Service()->process()`, `new Config()::KEY`), so that chained instantiations parse without error.
13. As a developer, I want the parser to support the PHP 8.5 pipe operator (`|>`), so that forward-flowing functional pipelines parse correctly.
14. As a developer, I want the parser to support the PHP 8.5 clone-with syntax (e.g., `clone($obj, ['prop' => $val])`), so that immutable record updates parse cleanly.
15. As a developer, I want the parser to support PHP 8.5 asymmetric visibility on static properties, so that static access qualifiers are correctly reflected in the AST.
16. As a developer, I want the parser to support PHP attributes (e.g., `#[AttributeName(arg: 1)]`) on classes, methods, functions, parameters, properties, and class constants, so that metadata annotations are preserved in the AST.
17. As a developer, I want the parser to support PHP 8 match expressions with pattern arms and default branches, so that pattern-matching constructs are captured as first-class expressions.
18. As a developer, I want the parser to support constructor property promotion (e.g., `public function __construct(private string $name)`), so that promoted properties are represented in method parameter definitions.
19. As a developer, I want the parser to support first-class callable syntax (e.g., `strlen(...)`, `$this->method(...)`), so that callable references are parsed into dedicated AST nodes.
20. As a developer, I want the parser to support union types (e.g., `int|string`) and intersection types (e.g., `Countable&Iterator`), so that composite type expressions are faithfully represented.
21. As a developer, I want the parser to support PHP enumerations (pure enums and backed enums with `int` or `string` backing types), so that enumeration declarations and cases parse into distinct AST structures.
22. As a developer, I want the parser to support named arguments in function and method invocations (e.g., `foo(name: $val, count: 42)`), so that argument labels are captured in call nodes.
23. As a developer, I want the parser to support nullsafe operator chains (e.g., `$user?->getProfile()?->name`), so that null-tolerant property and method accesses are distinct in the AST.
24. As a developer, I want the parser to support non-capturing `catch` statements (e.g., `catch (SpecificException)` without variable binding), so that anonymous exception catches parse cleanly.
25. As a developer, I want the parser to support `throw` expressions in expression contexts (such as arrow functions, ternary expressions, and coalescing operations), so that throw expressions are treated as valid expressions.
26. As a developer, I want the parser to support array unpacking with both integer and string keys, so that array spread operations parse accurately.
27. As a developer, I want the parser to handle PHP opening tags (`<?php`), short echo tags (`<?=`), and inline HTML content outside PHP tags, so that mixed template files parse into coherent AST representations.
28. As a developer, I want the parser to parse Heredoc and Nowdoc string literals, including flexible indented Heredoc/Nowdoc syntax, so that complex multiline strings are captured correctly.
29. As a developer, I want the parser to recognize and parse numeric literals in all PHP formats (decimal, hexadecimal `0x`, octal `0o`/`0O`, binary `0b`, floats, and numeric underscores like `1_000_000`), so that literal values are parsed without truncation or syntax errors.
30. As a developer, I want the parser to support generator expressions and statements (`yield` and `yield from`), so that asynchronous and iterative functions are correctly represented.
31. As a developer, I want the parser to handle closure and arrow function (`fn(...) => ...`) declarations with parameter types, return types, and lexical capture clauses, so that anonymous functions are captured in the AST.
32. As a developer, I want the parser to support namespace declarations (both bracketed and unbracketed syntax) and grouped `use` imports, so that package hierarchies and import aliases are properly structured.
33. As a developer, I want the parser to parse trait usage, trait adaptations (method aliasing `as` and conflict resolution `insteadof`), so that trait composition is fully preserved.
34. As a developer, I want the parser to represent AST nodes as parameterized types (e.g., parameterized over source annotations), so that I can enrich the AST with custom metadata such as type information or metrics without re-parsing.
35. As a developer, I want total, pure parsing functions that never throw runtime exceptions or use partial functions, so that parser invocation is completely safe and referentially transparent.
36. As a functional programmer, I want recursion schemes (such as catamorphisms and folds) over the AST types, so that tree transformations and queries can be expressed concisely without manual recursive boilerplate.
37. As a developer, I want an algebraic pretty-printer that outputs formatted PHP source code from an AST, so that I can re-serialize modified ASTs into valid PHP code.
38. As a developer, I want round-trip verification properties (`parse (prettyPrint ast) == Right ast`), so that I can rely on the structural fidelity of AST manipulations.
39. As a developer, I want the parser to preserve comments and docblocks (PHPDoc) associated with AST declarations, so that documentation-aware analysis tools can inspect annotations.
40. As a developer, I want the parser to offer parsing at different granularities (e.g., full program, individual statements, and standalone expressions), so that interactive tools and REPLs can parse partial snippets.
41. As a library consumer, I want clean, comprehensive Haddock documentation and idiomatic exports, so that the library is intuitive to integrate and use.
42. As a test author, I want property-based generators (QuickCheck / Hedgehog) for AST nodes, so that I can fuzz and property-test downstream tools.
43. As a developer, I want the parser to support `declare(...)` directive statements (both file-level declarations and block-scoped declarations), so that execution directives like `strict_types`, `ticks`, and `encoding` are represented in the AST.
44. As a developer, I want the parser to support `goto` statements and label statements (e.g., `goto end;` and `end:`), so that jump instructions and destination labels are captured in the AST.
45. As a developer, I want the parser to support `unset(...)` statements with multiple variable and array targets, so that variable unsetting operations are preserved in the AST.

## Implementation Decisions

- **Composability and Functional Architecture**: The parser will be designed around purely functional, composable parser combinators built on top of standard Haskell algebraic abstractions (`Functor`, `Applicative`, `Monad`, `Alternative`). The implementation emphasizes mathematical clarity, total functions, and compositional elegance rather than imperative parser-generator state machines.
- **Parametric AST Representation**: Core AST structures will be parameterized over an annotation type (e.g., node location metadata). This pattern decouples syntactic structure from metadata, allowing consumers to strip annotations, attach source spans, or attach semantic analysis results without duplicating AST definitions.
- **Strict, Idiomatic Algebraic Data Types**: AST nodes will be implemented using strict data fields with precise Sum and Product types that make syntactically invalid states unrepresentable. Common idioms such as non-empty sequences (e.g., for match arms or parameter lists where required by grammar) will be utilized to encode language invariants into types.
- **Unified Modern PHP Grammar**: Rather than maintaining separate diverging parsers for minor PHP versions, the library will provide a unified parser covering PHP 8.2 through PHP 8.5 with optional configuration flags for version-specific dialect restrictions where applicable.
- **Single Public API Seam**: The library exposes its functionality through a single cohesive public interface module that provides high-level pure functions for parsing programs, statements, and expressions from strict text or lazy text into either structured parse error values or annotated syntax trees.
- **Explicit Trivia and Comment Attachment**: Comments and PHPDoc blocks will be retained and associated with adjacent declaration nodes within the annotated AST, providing full fidelity for linting and refactoring tools.
- **Algebraic Pretty-Printing and Folds**: Alongside the parser, an algebraic printer module will be constructed following the Wadler/Leijen pretty-printing algebra, guaranteeing that any parsed AST can be pretty-printed back into syntactically valid PHP.
- **Total Diagnostic Error System**: Parse errors will not be simple strings; they will be structured algebraic values containing source span locations, expected tokens, and context descriptions to allow downstream applications to format custom error diagnostics.

## Testing Decisions

- **External Behavior Focus**: Tests will only exercise the parser via its public API entry points. Tests will verify that input PHP source strings yield the expected AST representations or appropriate diagnostic error values. No tests will assert against internal intermediate parser combinators or private lexer state.
- **Target Seams and Modules Tested**:
  - The public parsing interface (parsing complete PHP scripts, isolated statements, and expressions).
  - The public pretty-printer interface (ensuring printed outputs re-parse to equivalent AST structures).
  - The public traversal and folding utilities (ensuring structural folds visit all nodes in order).
- **PHP Version Conformance Suites**:
  - Dedicated specification suites validating each major language feature introduced in PHP 8.2 (readonly classes, DNF types, standalone types, trait constants).
  - Dedicated specification suites for PHP 8.3 (typed class constants, dynamic class constant fetches, anonymous readonly classes).
  - Dedicated specification suites for PHP 8.4 (property hooks with `get`/`set`, asymmetric property visibility, new without parentheses).
  - Dedicated specification suites for PHP 8.5 (pipe operator `|>`, clone-with expressions, static asymmetric visibility).
  - Ingestion of selected official PHP Zend engine language tests (`.phpt` test cases) covering grammar and syntax to verify real-world compatibility.
- **Property-Based Testing**:
  - Round-trip property testing (`parse . prettyPrint . parse == parse`) using Hedgehog or QuickCheck over generated syntax trees to guarantee parser-printer isomorphism.
  - Negative property tests ensuring invalid syntax fails gracefully with structured errors and never causes runtime crashes or non-termination.
- **Prior Art**: Greenfield implementation adhering to the repository's `CODING_STANDARDS.md`.

## Out of Scope

- **Runtime Evaluation & Interpretation**: Executing, evaluating, or interpreting PHP code is explicitly out of scope.
- **Semantic Analysis & Type Checking**: Performing static type checking, type inference, class hierarchy resolution, or symbol table generation beyond AST construction is out of scope.
- **Legacy PHP Syntax Compatibility**: Backward compatibility for syntax constructs deprecated and removed prior to PHP 8.0 (such as old-style PHP 4 constructors, PHP 5 call-time pass-by-reference, ASP tags `<% %>`, or script tags `<script language="php">`) is out of scope.
- **Bytecode & Opcache Compilation**: Emitting Zend opcodes or compiling PHP to bytecode or machine code is out of scope.
- **Language Server Protocol (LSP) Server**: Providing a JSON-RPC / LSP daemon implementation is out of scope; the library provides the AST and parser foundation upon which an LSP server can be built.

## Further Notes

- **PHP Release Lifecycle**: As of September 2026, PHP 8.2 and PHP 8.3 are in their security-fix support lifecycle, while PHP 8.4 and PHP 8.5 are in active support. Targeting PHP 8.2 through PHP 8.5 comprehensively covers all currently supported released versions of the language.
- **Foundational References**: The design draws inspiration from foundational works, including Graham Hutton and Erik Meijer's monadic parser combinators, Philip Wadler's pretty-printer algebra, and modern recursion scheme formulations for abstract syntax trees.
- **Extensibility**: Parameterizing AST nodes over an annotation functor allows future extensions, such as sourcemap generation, comments-as-nodes, or static analysis tags, without altering the core grammar definition.
