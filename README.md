# Lux: The Best Parts of Every Language

Lux is a statically-typed, compiled language that cherry-picks the most loved features from Python, Rust, Haskell, and Go — while deliberately leaving behind their worst pain points.

## What Lux Took (and from where)

### From Python: Clean, readable syntax
- **Indentation-based blocks** — no braces, no semicolons, no visual noise
- **`-- comments`** instead of `//` or `#` — visually clean
- **List/map comprehensions**: `[x * x for x in 0..10 if x % 2 == 0]`
- **Implicit line continuation** inside brackets — no backslashes needed
- **String interpolation**: `"Hello {name}, you are {age} years old"`

### From Rust: Safety and expressiveness
- **Immutable by default** — `let` binds are immutable, `var` opts into mutation
- **Algebraic data types** (enums with data):
  ```
  type Shape
      Circle(radius: Float)
      Rectangle(width: Float, height: Float)
  ```
- **Exhaustive pattern matching** with guards, destructuring, or-patterns, and rest patterns
- **Expression-oriented** — `if`, `match`, `for`, `loop` all return values
- **Pipe operator**: `data |> filter(fn(x) x > 0) |> map(fn(x) x * 2)`
- **Traits** for polymorphism with static dispatch
- **Generics** with trait bounds: `fn max[T: Ord](a: T, b: T) -> T`

### From Haskell/ML: Elegant type system
- **Bidirectional type inference** — you rarely need to write type annotations
- **Monomorphization** — generics compile to specialized native code, zero runtime cost
- **ADTs + pattern matching** as the core modeling primitive
- **First-class functions and lambdas**: `fn(x) x * 2`

### From Go: Practical concurrency
- **`spawn`/`await`** for cooperative coroutines — lightweight concurrent tasks
  ```
  let t1 = spawn compute(7)
  let t2 = spawn compute(8)
  let r1 = await t1
  let r2 = await t2
  ```
- Simple mental model: spawn starts work, await gets the result

### From C: Systems-level power
- **Raw pointers** (`*T`) when you need them
- **FFI via `extern` blocks** — call any C function directly:
  ```
  extern "C"
      fn printf(fmt: *Char, ...) -> Int
  ```
- **Compiles to native code** via LLVM — no VM, no garbage collector, no runtime overhead

## What Lux Left Behind

| Left out | From | Why |
|---|---|---|
| Braces and semicolons | C, Rust, Java | Visual noise that adds nothing |
| Garbage collector | Go, Java, Python | Unpredictable pauses, runtime overhead |
| Null pointers | C, Java, Go | The "billion dollar mistake" — use `Option[T]` instead |
| Header files | C, C++ | Redundant declarations — one source of truth |
| Complex lifetime annotations | Rust | Steep learning curve for questionable ergonomic benefit |
| `self` as a string | Python | `self` is a real keyword, not a convention you can misspell |
| Exceptions | Python, Java | Use pattern matching on result types instead |
| Dynamic typing | Python, JS | Bugs caught at compile time, not at 3am in production |
| Class hierarchies / inheritance | Java, C++ | Traits + composition instead of fragile inheritance trees |
| Significant `()` vs `(x,)` tuple confusion | Python | Clean tuple syntax without trailing comma hacks |
| Manual memory boilerplate | C | `defer` for cleanup, heap collections managed automatically |
| Colored function problem | JS/Rust async | `spawn`/`await` is cooperative and simple — no async infection |

## The Sweet Spot

Lux sits in a unique design space:

- **As readable as Python** — indentation, comprehensions, clean syntax
- **As safe as Rust** — immutability, ADTs, exhaustive matching, no null
- **As fast as C** — native LLVM compilation, monomorphized generics, no GC
- **As concurrent as Go** — lightweight coroutines with a simple spawn/await model
- **As expressive as Haskell** — first-class functions, pattern matching, type inference

All in a language where a complete program looks like:

```
fn factorial(n: Int) -> Int
    match n
        0 -> 1
        n -> n * factorial(n - 1)

fn main()
    let result = factorial(10)
    print("10! = {result}")
```

No imports, no boilerplate, no ceremony. Just the code that matters.

## Build

```bash
make            # Build the lux compiler (requires LLVM 21)
make clean      # Remove compiled binary and artifacts
```

## Usage

```bash
./lux <file.lux>              # Compile to executable (./output)
./lux <file.lux> -o name      # Compile to named executable
./lux <file.lux> --tokens     # Print token stream
./lux <file.lux> --ast        # Print AST
./lux <file.lux> --check      # Typecheck only
./lux <file.lux> --emit-llvm  # Print LLVM IR
```
