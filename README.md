# PFL-PROJECT-2

**T02_G11**

**Team Members:**
- Diogo Carvalho Pintado (up202108875@fe.up.pt)
- Tiago Tavares Simões (up202108857@fe.up.pt)

## Introduction

In our project, we've implemented a parser as a critical component of the compiler for an imperative programming language. The parser's role is to transform a string representation of a program into a structured format.

## Data Types

Defining appropriate data types was a key decision in our project. We wanted to ensure that each type (like `Aexp`, `Bexp`, `Stm`) accurately represents a segment of the programming language structure. These data types serve as the building blocks for our parser and compiler, making it easier to manipulate and process different parts of the program. Careful consideration was given to ensure that the data types align with the language's syntax and semantics, allowing for precise parsing and compilation of programs.


## Lexer and Tokenization

The foundation of our parsing strategy starts with the lexer, implemented as the `lexer` function. This function breaks down the input string into a list of tokens. By converting the input string into tokens, we significantly simplify the parsing process.

## Parsing Arithmetic Expressions - `parseA`

The `parseA` function is responsible for understanding the arithmetic parts of the program. It looks at sequences of tokens that represent numbers and arithmetic operations (like addition, subtraction, and multiplication).

## Parsing Boolean Expressions - `parseB`

Similarly, `parseB` deals with the true/false parts of the program. It identifies boolean values and logical operators (like "and", "or", "not").

The `parseSimpleBexp` function is a part of this process. It deals with simpler boolean expressions and basic comparisons. For instance, it can easily interpret expressions like `True` or `x == y`.

## Parsing Statements - `parseStm`

`parseStm` reads various kinds of statements like assignments, conditional statements, and loops. This function takes the tokens related to these constructs and prepares them for the next steps in the compilation process.

The `parseStatements` function complements `parseStm`. It's designed to handle multiple statements, accumulating the results from `parseStm`. This is particularly useful for parsing sequences of instructions, ensuring that they are processed in the correct order and structure.


## Overall Strategy

Our strategy is about breaking down the program into manageable chunks and organizing these chunks in a way that our compiler can later turn into instructions for the computer to execute. Each function (`parseA`, `parseB`, `parseStm`) handles a specific aspect of the program, making the process orderly and manageable.
