# COL226 — Programming Languages

Six OCaml projects that build, step by step, toward a working programming language and the theory
underneath it: abstract data types → a typed expression language → a lexer → a grammar → a full
interpreter → abstract machines for the λ-calculus → unification.

By the end, this repository contains a **complete language implementation** (lexer, parser, static
type checker, evaluator) plus the two machines — **Krivine** and **SECD** — that explain how
call-by-name and call-by-value actually work, and a **first-order unification** engine of the kind
that sits at the heart of Prolog and of Hindley–Milner type inference.

> **COL226 – Programming Languages**, IIT Delhi. Every assignment ships with its original
> `Problem_Statement.txt`.

---

## Table of Contents

- [Contents at a Glance](#contents-at-a-glance)
- [Environment Setup](#environment-setup)
- [Running the Code](#running-the-code)
- [Assignment 1 — Vectors and Matrices](#assignment-1--vectors-and-matrices)
- [Assignment 2 — Type Checker and Interpreter](#assignment-2--type-checker-and-interpreter)
- [Assignment 3(a) — Lexical Analysis](#assignment-3a--lexical-analysis)
- [Assignment 3(b) — Grammar and Parser](#assignment-3b--grammar-and-parser)
- [Assignment 3(c) — The Interpreter](#assignment-3c--the-interpreter)
- [Assignment 4 — Krivine and SECD Machines](#assignment-4--krivine-and-secd-machines)
- [Assignment 5 — Terms, Substitution, Unification](#assignment-5--terms-substitution-unification)
- [Repository Layout](#repository-layout)
- [Contributing](#contributing)

---

## Contents at a Glance

| # | Project | Core idea | Tooling |
|---|---|---|---|
| 1 | Vectors and Matrices | ADTs, tail recursion, algebraic laws | OCaml stdlib |
| 2 | Type Checker and Interpreter | A three-sorted DSL with `type_of` and `eval` | OCaml stdlib |
| 3(a) | Lexical Analysis | Regular expressions → token stream | `ocamllex` |
| 3(b) | Grammar and Parser | Unambiguous grammar → AST | `ocamlyacc` |
| 3(c) | Interpreter | Full pipeline with dimension-aware types | `ocamllex` + `ocamlyacc` |
| 4 | Functional Language Interpreters | Krivine (CBN) and SECD (CBV) machines | OCaml stdlib |
| 5 | Terms, Substitution, Unification | Most general unifiers over first-order terms | OCaml stdlib |

---

## Environment Setup

Everything here is plain OCaml — no opam packages required.

| Requirement | Used by |
|---|---|
| OCaml ≥ 4.08 (`ocaml`, `ocamlc`) | All assignments |
| `ocamllex` | 3(a), 3(b), 3(c) |
| `ocamlyacc` | 3(b), 3(c) |
| GNU Make | 3(b), 3(c) |

<details open>
<summary><b>Install via opam (recommended)</b></summary>

```bash
bash -c "sh <(curl -fsSL https://opam.ocaml.org/install.sh)"
opam init -y
opam switch create 4.14.1
eval $(opam env)
```
</details>

<details>
<summary><b>Linux (Debian / Ubuntu)</b></summary>

```bash
sudo apt update && sudo apt install -y ocaml ocaml-nox make
```
</details>

<details>
<summary><b>macOS</b></summary>

```bash
brew install ocaml opam && opam init -y && eval $(opam env)
```
</details>

<details>
<summary><b>Windows</b></summary>

Use **WSL2** with Ubuntu and follow the Linux instructions.
</details>

Verify:

```bash
ocaml -version && ocamllex -version && ocamlyacc -version
```

---

## Running the Code

Three patterns cover everything in this repository.

**Single-file modules (1, 2, 4, 5)** — load into the REPL, or compile:

```bash
ocaml Vectors_and_Matrices.ml            # run directly
# or
ocamlfind ocamlopt -package str file.ml  # not needed here — stdlib only
ocamlc -o prog file.ml && ./prog
```

Interactive exploration is usually the fastest way in:

```bash
ocaml
# #use "Vectors_and_Matrices.ml";;
# Vector.dot_prod [1.;2.;3.] [4.;5.;6.];;
```

> Directory names contain spaces — quote them: `cd "Assignment 1 - Vectors and Matrices"`.

**Lexer-only project (3a)**:

```bash
cd "Assignment 3(a) - Designing a small programming language -part (a) Lexical Analysis/Lexical Analysis"
ocamllex lexer.mll
ocamlc -o lexdemo lexer.ml main.ml
./lexdemo < program.txt
```

**Make-driven projects (3b, 3c)**:

```bash
make            # generates lexer.ml / parser.ml, then links
./mydsl < inputs/matrix_test.txt
make clean
```

---

## Assignment 1 — Vectors and Matrices

An abstract data type for vectors (`float list`) and matrices (`float list list`), implemented from
scratch — **no `List` module shortcuts**, everything hand-rolled and tail-recursive where it
matters.

`module Vector` provides `create`, `dim`, `is_zero`, `unit`, `scale`, `addv`, `dot_prod`, `inv`,
`length`, `angle`, and raises `DimensionError` on malformed input. `module Matrix` builds on it with
transpose, multiplication, determinant and inverse.

The accompanying proofs (in the problem statement) establish that the operations satisfy the vector
space axioms — commutativity, associativity, identity, inverse, distributivity.

```bash
cd "Assignment 1 - Vectors and Matrices"
ocaml Vectors_and_Matrices.ml
```

---

## Assignment 2 — Type Checker and Interpreter

A **three-sorted DSL** — booleans, scalars, vectors — with a definitional interpreter and a static
type checker over the same AST.

```ocaml
type expr = ...              (* Add, ScalProd, DotProd, Mag, Angle, IsZero, ... *)
exception Wrong of expr
let rec type_of : expr -> exptype   (* static typing; raises Wrong on ill-typed terms *)
let rec eval    : expr -> values    (* evaluation *)
```

The point of the pairing: `type_of e` succeeding should guarantee `eval e` does not get stuck — the
soundness property, made concrete. `testcases.ml` exercises both well-typed and deliberately
ill-typed programs.

```bash
cd "Assignment 2 - A Type Checker and Interpreter/A Type Checker and Interpreter"
ocaml a2.ml
ocaml testcases.ml
```

---

## Assignment 3(a) — Lexical Analysis

An `ocamllex` tokeniser for the matrix/vector language: keywords, identifiers, integer and float
literals, vector and matrix literals, comparison and logical operators, delimiters, comments, and
`Input`/`Print`.

`main.ml` is a token dumper — feed it a program and it prints the token stream, which is the fastest
way to debug a regular expression that is matching more (or less) than you intended.

```bash
cd ".../Lexical Analysis"
ocamllex lexer.mll && ocamlc -o lexdemo lexer.ml main.ml
./lexdemo < myprogram.txt
```

---

## Assignment 3(b) — Grammar and Parser

The grammar, written for `ocamlyacc` with **no reduce–reduce conflicts** and all shift–reduce
conflicts resolved through explicit precedence declarations. Output is an AST defined in `ast.ml`.

Includes `test1.txt` … `test5.txt` and a `makefile`; `ass3_2/` holds the revised submission with the
type checker attached.

```bash
cd ".../Designing a DSL for Matrix and Vector operations part (b) Grammars"
make
./mydsl < test1.txt
```

---

## Assignment 3(c) — The Interpreter

The full language: **lexer → parser → AST → type checker → interpreter**, ~1,300 lines.

Types carry dimensions (`Matrix_Float(2,3)` is distinct from `Matrix_Float(3,2)`), so shape errors
are caught statically. Twenty sample programs in `inputs/` include Gaussian elimination,
eigenvalues, binary search and prime testing written *in the DSL itself*.

```bash
cd "Assignment 3(c) - building an interpreter/Designing-a-programming-Language_in_OCaml-main"
make
./mydsl < inputs/gaussian_elimination.txt
```

Each run prints the AST, the type environment, the program's output, and the final variable
bindings. See that directory's own `README.md` for the complete language reference.

---

## Assignment 4 — Krivine and SECD Machines

Two abstract machines for the same λ-calculus, run side by side on the same expressions so the
difference in evaluation strategy is visible in the output.

| Machine | Strategy | Representation |
|---|---|---|
| **Krivine** | Call-by-name | Closures `{exp; env}` and a stack of closures |
| **SECD** | Call-by-value | Stack, Environment, Control, Dump — driven by compiled opcodes |

The language extends the pure λ-calculus with integers, booleans, `Plus`, `Times`, `And`, `Or`,
`Not`, `Gt`, `Lt`. Church encodings (`church_of_int`, `church_true`, `church_to_int`,
`church_to_bool`) let you check that arithmetic done *inside* the calculus agrees with arithmetic
done natively. `compile : lamexp -> opcode list` targets the SECD machine, and
`string_of_opcode_list` prints the generated code.

```bash
cd "Assignment 4 - Functional Language interpreters"
ocaml machine.ml
```

Output, per test expression:

```
Expression: <the term>
  Krivine: <call-by-name result>
  SECD:    <call-by-value result>
```

---

## Assignment 5 — Terms, Substitution, Unification

First-order terms over a signature, with substitutions and **most general unifiers**.

```ocaml
type variable = string
type symbol   = string * int          (* name, arity *)
type term     = V of variable | Node of symbol * term array
exception WRONG_SIGNATURE of string
exception NOT_UNIFIABLE
```

Implemented operations:

| Function | Meaning |
|---|---|
| `check_sig` | Signature validity — no duplicate symbols, no negative arities |
| `wfterm` | Well-formedness — every node has exactly its declared arity |
| `ht`, `size`, `vars` | Height, node count, and the variable set of a term |
| `subst` | Apply a substitution — implemented as a *composable* function, not a naive traversal |
| `compose_subst` | Composition of substitutions |
| `mgu` | Most general unifier, raising `NOT_UNIFIABLE` on failure |

`Node` uses `term array` rather than `term list`, so traversals go through `Array.map` /
`Array.fold_left`; variable sets use `Set.Make(String)`.

```bash
cd "Assignment 5 Terms, Substitution, Unification"
ocaml Terms_Substitution_Unification.ml
```

---

## Repository Layout

```
.
├── Assignment 1 - Vectors and Matrices/
│   ├── Vectors_and_Matrices.ml
│   └── Problem_Statement.txt
├── Assignment 2 - A Type Checker and Interpreter/
│   ├── A Type Checker and Interpreter/{a2.ml, testcases.ml}
│   └── Problem_Statement.txt
├── Assignment 3(a) - ... Lexical Analysis/
│   ├── Lexical Analysis/{lexer.mll, main.ml}
│   └── Problem_Statement.txt
├── Assignment 3(b) - ... Grammars/
│   ├── .../{ast.ml, lexer.mll, parser.mly, makefile, test1-5.txt}
│   ├── .../ass3_2/          # revised submission, with type checker
│   └── Problem_Statement.txt
├── Assignment 3(c) - building an interpreter/
│   ├── Designing-a-programming-Language_in_OCaml-main/   # full implementation + inputs/
│   └── Problem_Statement.txt
├── Assignment 4 - Functional Language interpreters/
│   ├── machine.ml           # Krivine + SECD
│   └── Problem_Statement.txt
└── Assignment 5 Terms, Substitution, Unification/
    ├── Terms_Substitution_Unification.ml
    └── Problem_Statement.txt
```

---

## Contributing

This is coursework, but the code is readable and the ideas are reusable. Useful improvements:

- Add `dune` project files so each assignment builds with `dune build`
- Convert the ad-hoc test drivers to `alcotest` or `ounit2`
- Replace `ocamlyacc` with **Menhir** in 3(b)/3(c) for far better error messages
- Add property-based tests (QCheck) for the vector-space laws in Assignment 1
- Extend Assignment 5's `mgu` with an occurs-check benchmark against a union-find implementation

```bash
git checkout -b feat/your-change
git commit -m "feat: describe your change"
```

---

## License

Released for educational use. Course material and problem statements belong to their original
authors.
