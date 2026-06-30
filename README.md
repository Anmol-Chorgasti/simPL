# simPL

`simPL` is a lightweight, strongly typed functional programming language implemented in OCaml. It is designed to illustrate key concepts in programming language theory, including environment-based evaluation, lexical closures, and Hindley-Milner type inference.

The system includes a parser built with Menhir, a lexer built with OCamllex, and a test suite powered by `OUnit2`.

## Features

*   **Lexical Closures:** Functions evaluate to closures that capture their definition-time environment, ensuring lexical scoping even when variables are shadowed later.
*   **Hindley-Milner Type Inference:** Automatically infers the most general type of any valid expression (Algorithm W-style constraint generation followed by unification) without requiring manual type annotations.
*   **Let-Polymorphism:** Supports polymorphic values through let-bindings. A let-bound value is generalized into a type scheme, so it can be instantiated at different types at each use rather than being locked to the type of its first use.
*   **Precedence & Associativity:** Correctly parses mathematical operations (multiplication binds tighter than addition; function application is left-associative).
*   **Occurs Check:** Prevents recursive/infinite types during unification. Example: `fun x -> x x`, where typing the self-application would require a type that contains itself, raising a static type error instead of looping.

---

## Language Syntax & Expressions

The language supports standard functional constructs. Below is the Abstract Syntax Tree (AST):

```ocaml
type bop =
  | Add  (* + *)
  | Mult (* * *)
  | Leq  (* <= *)

type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Binop of bop * expr * expr
  | Let of string * expr * expr
  | If of expr * expr * expr
  | Fun of string * expr
  | App of expr * expr
```

---

## How Inference Works

Type checking runs as a gate in front of evaluation: a program is only evaluated if it first typechecks. Inference proceeds in three stages.

1.  **Constraint generation.**
A single recursive pass over the AST produces an inferred type together with a set of constraints. A constraint is represented as (type 1, type 2) and this can be interpreted as type 1 MUST BE EQUAL to type 2. For the inferred type to be true, all the generated constraints must be solved by the unification algorithm below. Primitive expressions and forms like `if` and application contribute fixed constraints (e.g. the guard of an `if` must be `bool`, the two branches must have the same inferred types); fresh type variables stand in for anything not yet determined. The inferred type is sort of like a guess with certain conditions, and the constraints are the conditions that must be satisfied.

2.  **Unification.**
A solver consumes the constraint set and produces an ordered list of substitutions. Each step that solves a constraint of the form `'a = t` eliminates the variable `'a` from the remaining problem, so the number of unsolved variables strictly decreases guaranteeing the termination of the algorithm. The occurs check guards each such step: it saves us from replacing an unsolved variable x with another type that contains x! Without this we would have infinite types. The result is the *most general* solution. The solution commits to as little as possible while still maintaining the ability to specialize to more specific types when needed.

3.  **Application & generalization.**
The solution is applied to the inferred type from STEP 1, to produce the final answer.
For `let` bindings, the bound expression is solved and its type is generalized against the current environment into a type scheme; at each use, the scheme is instantiated with fresh variables.
The reason for this is best explained with an example
```ocaml
let x = fun y -> y
```
`x` here is the ID function. Takes in something, gives you back that very same something.
However depending on usage, what should x really have as a type?
Without generalization and instantiation if `x 5` was the first time we applied x. We end up
LOCKING the type of `x` to be int -> int. However, this is wrong, as x by definition should be able
to take in any type. I should be allowed to call `x "Mirror"` as well and get back "Mirror".
In OCaml, `x` would have the type 'a -> 'a where 'a represents a type variable that can be filled in with a more specific type by the compiler based on what `x` is applied to. Note however that the type of `x` itself is still 'a -> 'a. OCaml achieves this through the concept of Type Schemes. This project uses the concepts of Generalization and Instantiation to provide `x` in simPL with a type scheme.

---

## Project Structure

```text
├── dune-project          # Dune project configuration
├── Makefile              # Launches the REPL via Dune
├── .ocamlinit            # Auto-loads the interp library in utop
├── .gitignore            # Git exclusion rules
└── src/
    ├── ast.ml            # Type definitions for the AST and types
    ├── lexer.mll         # OCamllex definition for lexical analysis
    ├── parser.mly        # Menhir parser definition
    ├── type_inference.ml # HM type inference (constraint gen, unification)
    ├── main.ml           # Interpreter logic and evaluation environment
    └── test.ml           # OUnit2 test suite
```

---

## Installation & Setup

### Prerequisites
Ensure you have [OPAM](https://opam.ocaml.org/) installed, then install the dependencies:

```bash
opam install dune menhir ounit2 qcheck utop
```

### Compiling the Project
All commands below assume you are in the **project root** (the directory containing `dune-project`). To compile the library and tests without starting the interactive environment:

```bash
dune build
```

---

## Running the Interpreter

Start the interactive REPL (`utop`) with the interpreter pre-loaded from the project root:

```bash
make
```

The `make` target launches `utop` with the `Interp`, `Main`, and `Ast` modules already opened (via `.ocamlinit`), so you can evaluate expressions immediately:

```ocaml
(* 1. Inferred type of a polymorphic function — note the type variable *)
interp_w_typ "fun x -> x";;
(* Output: -: 'a -> 'a = <func> *)

(* 2. Inference pins a variable via a constraint *)
interp_w_typ "let x = fun y -> y + 1 in x";;
(* Output: -: int -> int = <func> *)

(* 3. Currying — a function returning a function *)
interp_w_typ "let f = fun x -> fun y -> x + y in f";;
(* Output: -: int -> int -> int = <func> *)

(* 4. Higher-order function application *)
interp_w_typ "let twice = fun f -> fun x -> f (f x) in let addone = fun a -> a + 1 in twice addone 5";;
(* Output: -: int = 7 *)

(* 5. Lexical scope: the closure captures y = 5 at definition time,
   so the later rebinding to 6 does not affect it *)
interp_w_typ "let y = 5 in let x = fun a -> a + y in let y = 6 in x 4";;
(* Output: -: int = 9 *)
```

---

## Test Suite

The suite in `src/test.ml` uses `OUnit2` and verifies the interpreter through execution — a program that produces the correct value has, by construction, passed inference first.

*   **Arithmetic & Precedence:** Verifies precedence parsing for expressions like `2+2*10` (resolves to `22`) and nested operations.
*   **Lexical Scoping & Shadowing:** Validates that environments are correctly captured inside closures and that nested bindings do not corrupt outer scopes.
*   **Higher-Order Functions:** Covers currying, function composition, and passing closures to other functions.
*   **Static Type Checking (Error Cases):** Asserts that type-incorrect programs are rejected before execution — e.g. `1 + true` or applying a non-function value `5 4`.

To run the suite (from the project root):

```bash
dune build
dune exec src/test.exe
```

---

## Design Decisions & Tradeoffs

A few choices in this implementation favour clarity over performance or completeness. They are deliberate, and the alternatives are noted so the scope is explicit.

*   **Unification is not optimised.** Substitutions are applied eagerly across the constraint list and the accumulated solution at each elimination step, and the solution is represented as an ordered association list. It is assumed that for the small constraint sets that human-written programs produce, this is fast enough and keeps the algorithm easy to follow.

*   **Error reporting is intentionally coarse.** Every type mismatch surfaces as a single unification failure rather than a specific, located message. This decision was made to keep the scope manageable and also out of respect for the fact that providing accurate error messages is a very interesting research area with a lot of effort being put into it.

*   **Inferred types are displayed with raw internal variable names.** Fresh variables are minted from a global counter, so a printed type may read `'a2 -> 'a2` rather than the normalised `'a -> 'a`. The two are equivalent, what matters is that both sides share the *same* variable.

---

## Acknowledgements

The language and the overall interpreter structure are inspired by Chapter 10 ("Interpreters") of [*OCaml Programming: Correct + Efficient + Beautiful*](https://cs3110.github.io/textbook/chapters/interp/intro.html) (Clarkson et al.), which develops SimPL — the same AST and the lexer/parser/evaluator pipeline used here.

The type system departs from the textbook. The chapter implements type checking as a direct `typeof` function and notes that for SimPL, due to its narrow feature set, type inference is not necessarily needed. Both full constraint-based inference, and features such as closures and applications of closures are left as a CS3110 course assignment to students.

Implementing that assignment as a self studying student is the main source of inspiration for this project.