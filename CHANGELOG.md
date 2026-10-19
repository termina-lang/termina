# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html)
with pre-1.0 semantics: while in `0.x`, minor bumps may include breaking changes.

The transpiler version is kept in lockstep at the `MAJOR.MINOR` level with
[`termina-osal`](https://github.com/termina-lang/termina-osal): the same
`v0.X.Y` major and minor on both repositories guarantees compatibility.
Patch versions on either side are interchangeable.

## [Unreleased]

### Added

- Deques, which let a task, a handler or a resource keep boxes from one
  activation to the next. A deque is declared in `app.fin` as
  `resource name : Deque<T; N>;`, a queue of up to `N` boxes, and is accessed
  through a port `access DequeAccess<T>` of the one element connected to it
  (AE-012, AE-013). `take(&mut opt)` moves the box at the front out into an
  option, `put(b)` and `put_front(b)` move a box in at the back or the front,
  `try_put(b, &mut rejected)` gives the box back if the deque is full, and
  `replace(b, &mut evicted)` takes the box at the front out to make room. The
  transpiler checks that `put` and `put_front` only happen where the deque has
  room on every path, also across calls to methods of the same class (DQE-001,
  DQE-002), that every deque is both filled and taken (DQE-003, DQE-004), and
  that a deque gets boxes from somewhere other than itself (AE-014).
  A deque holds a type a box can hold (SE-227), takes no initial value (SE-230)
  and a capacity of at least one (CFE-017). The architecture check follows a
  box through the deques it goes through, so a box allocated from one pool and
  freed to another after being parked is AE-003. The procedures of a deque are
  in the OSAL, which checks again at run time that a `put` finds room, so this
  release needs a matching OSAL.

### Changed

- A procedure parameter of type `&mut Option<box T>` is filled by the
  procedure, as the option of an `alloc` is. The caller hands it over empty
  and moves what it gets back (BE-009, BE-010), and the procedure fills it
  exactly once on every path, with `alloc`, `take`, `try_put`, `replace`,
  `*result = Some(b)`, `*result = None` or a call to another procedure, and
  never reads it (BE-013). Before, the procedure got the box in the option on
  loan and could only read and write its value, and `*result = None` raised
  an internal error. The architecture check follows a box written into the
  option back to wherever the procedure got it, so a box from one pool freed
  by the caller to another is AE-003.

### Fixed

- A procedure with a parameter of type `Option<box T>`, which moves the
  option of the caller into the procedure, crashed the transpiler in the
  architecture check. The check now follows the box through the call.
- A box allocated from one pool and freed to another inside a `for` loop
  compiled without AE-003 and raised a `pool_free` runtime failure.

## [0.6.0] - 2026-10-15

### Added

- `--version` option, which prints the version of the transpiler.
- A `var` may be declared without an initializer (`var x : T;`). The
  transpiler checks that the object is assigned as a whole on every path before
  it is read (VUE-007) and before a field or an element of it is written
  (VUE-008). `let` still requires a value.
- Value analysis over the body of every function and member: it follows the
  values a local variable, a field of a struct, a loop iterator or the result of
  a call may hold, including the variants of enums, `Option`, `Status` and
  `Result`, and the ranges that arithmetic operations and casts produce, inside
  loops as well. It rejects a condition whose value is always the same
  (VAE-001), and says which assignments make it so, a `match` case that is
  never taken (VAE-002), an operation whose run-time check fails every time
  it runs, such as a shift by an amount that never falls below the width of
  the value (VAE-003), a loop whose range is empty (VAE-004), and a comparison
  outside a condition, or inside one with other operands, whose value is
  always the same (VAE-005). An object the analysis knows nothing of may hold
  any value of its type, so a comparison whose result the range of the type
  fixes, such as an unsigned value compared with `< 0`, is a VAE-001 or a
  VAE-005.
- Side-effect checks (SEF-001 to SEF-007): two mutable references to the same
  object in one expression, unordered side effects, and side effects in an
  initializer list, in the right operand of `&&` and `||`, or in the guard of a
  loop. An access to a `loc` field, an operation whose check the generated code
  keeps (an array index, a slice, a shift amount, a divisor or a signed
  operation) and a call to a `&mut self` method count as side effects, also
  through the functions a call reaches.
- Declarations whose uses all fall inside one inner block are rejected
  (VSE-001).
- Values that no path reads are rejected: a dead assignment (VUE-006) and a dead
  initializer (VUE-009).
- Actions that do not use `self` (VUE-003), methods, viewers and procedures that
  do not use `self` (VUE-004), and methods and viewers that no member of their
  class calls (VUE-005) are rejected.
- Run-time checks of integer arithmetic. A signed addition, subtraction,
  multiplication, division or remainder whose result falls outside the range of
  its type raises `EArithmeticOverflow`, and a division or a remainder by zero
  raises `EDivisionByZero`, instead of the undefined behavior of C. Requires
  `termina-osal v0.6.x`.
- An array access, a slice, a shift or an arithmetic operation is emitted
  without its run-time check when the transpiler shows that the check cannot
  fail, because its operands are constant or because the value analysis keeps
  them within the bounds the check tests, as an index that a loop or a guard
  keeps inside the array.
- Each build writes `output/checks.json`, which lists for every module the
  obligations of the run-time checks: the position of the operation, the
  property (index or slice in bounds, shift amount below the width, no
  overflow, non-zero divisor) and whether the transpiler discharged it, with
  the reason, or the generated code checks it.
- **BREAKING**: the priority of a task goes from 1 to 254 (CFE-014), no two
  tasks share one (AE-009), and a task without `#[priority]` is rejected
  (SE-222). The operating systems reserve 0 and 255. The argument may be a
  constant expression. A modifier repeated on a declaration is rejected (SE-223).
- The `ERuntimeFailure` variant of the built-in `Exception` enum, which the OSAL
  raises when an operation of the runtime fails.
- The `microseconds-per-tick` entry of `termina.yaml` sets the length of the
  tick of the system clock, 10000 microseconds when absent. It has to divide
  1000000 exactly. The generated `config.h` defines it as
  `TERMINA__TIME__MICROSECONDS_PER_TICK`, and `termina sched` takes it as the
  interval of the system timer of the scheduling model.
- **BREAKING**: the period of a periodic timer has to be a positive multiple of
  the tick (CFE-015). A period that is not advances by whole ticks on the
  targets, with a jitter of up to one tick, or on FreeRTOS at the wrong rate.
  On the RTEMS and FreeRTOS platforms it can be at most
  2^32 - 1 ticks (CFE-016), the width of the interval the runtime arms.
- Constant shift amounts outside the width of the type are rejected
  (CFE-013). A division or
  a remainder by a constant zero is rejected whatever the dividend (CFE-006),
  where only one of two constants was before.
- **BREAKING**: identifiers that C reserves are rejected: keywords, identifiers
  of the standard library, names that start with an underscore and an uppercase
  letter, and the names the target platform declares in the headers of the OSAL.
- **BREAKING**: identifiers and module names with two consecutive underscores,
  modules under the `termina` namespace and the names of the Termina types that
  take type arguments (`Option`, `Status`, `Result`...) are rejected.
- **BREAKING**: references to members of a packed struct are rejected on targets
  with strict alignment.
- **BREAKING**: shifts and bitwise operations (`<<`, `>>`, `&`, `|`, `^`) take
  unsigned operands only (SE-220, SE-221). The right operand of a shift was
  already unsigned; the left one and the operands of `&`, `|` and `^` could be
  signed, and their value in C then depends on the representation of negative
  numbers or is undefined.
- A global `const` may hold a struct, an enum, an `Option`, a `Status` or a
  `Result`, and an array of them, built from numbers, booleans and characters.
  The transpiler works out every integer of the initializer, and a reference to
  another constant is replaced by its value; the object is emitted with a
  designated initializer list. A field of a constant, an element read with a
  constant index and the test of its variant are constant expressions, so they
  can size an array, bound a loop or initialize another constant.
- The connection of a sink port checks the kind of entity that attends the
  emitter.
- The generated identifiers are checked against a per-platform maximum length.
  No supported platform sets a limit today.
- The generated `config.h` defines the size of the interrupt table and, under
  the `debug` profile, `TERMINA__PROFILE__DEBUG`; the generated Makefile passes the
  profile to the platform as `TERMINA_PROFILE`.
- Language server: go to definition, find references, document highlight,
  workspace symbols and hover. Diagnostics carry their code, explanation and
  related positions, and unused code is greyed out.
- The `rtems6-zynq7000-pynqz2` platform: RTEMS 6 on the Zynq-7000 of the
  PYNQ-Z2 board. Its interrupts are the 60 shared peripheral interrupts of the
  Zynq-7000, named after the vectors of the RTEMS board support package
  (`uart_0_irq`, `can_0_irq`, `fpga_0_irq`...), each one enabled with its flag
  in `termina.yaml` (`enable-uart-0-irq`...). Requires `termina-osal v0.6.x`.
- `termina new` writes a `.gitignore` that leaves out `output/bin/` and
  initializes a git repository, unless the directory is already inside one or
  `--vcs none` is given. `--vscode` also writes the `.vscode` and
  `.devcontainer` configuration for the platform. The name of the project may
  contain `-` besides letters, digits and `_`.
- Two variants of `Exception`: `EProcessorFault(u32, u32, usize, usize)`, a
  fault of the processor with its exception number, fault status, the address
  of the instruction and the faulting address, and `EKernelFailure(u32,
  usize)`, a fatal error raised by the operating system with its source and
  code. The values are those of the platform. Requires the matching
  `termina-osal`.
- The `EStackOverflow(usize)` variant of `Exception`: a stack overflow
  detected by the operating system, with the identifier of the task.
- Calls to C: `extern function` and `extern noreturn function` declare a
  function written in C, with the signature rules of a `function`. The
  prototype goes to the header of the module, with `_Noreturn` for a
  `noreturn` one, which cannot declare a return type (SE-225). A call to a
  `noreturn` function is a statement of its own (SE-224), placed as a `reboot`
  (EE-014, EE-015), and the generated C calls `termina__exec__handover()` before
  it. The `extern` section of `termina.yaml` lists the C sources of the
  `extern` folder and the static libraries of each platform. `termina build`
  checks the list against the folder and copies the files to `output/extern`,
  and the generated Makefile compiles them with the flags of the project and
  links them into `<project>.ext.o`. Requires `termina-osal v0.6.x`.
- The return type of an action is optional: an action that returns nothing
  omits `->`.
- The `system_except` action rejects a `reboot` or a call to a `noreturn`
  function (AE-010) and a send (AE-011). The runtime reboots once the action
  has run.

### Changed

- Error codes are numbered without gaps in every family: SE-180 to SE-224
  become SE-179 to SE-223, and the constant folding codes after the ones
  retired in this version move down to CFE-007 to CFE-016.
- A loop whose range is empty is reported by the value analysis as VAE-004,
  which replaces CFE-008 and CFE-009.
- Where the program reads an integer or boolean constant, by its name or as a
  field or an element of an aggregate constant, the generated code writes its
  value, so the read does not go to memory, not even from another module.
- The worst-case paths (`.twcep`) write every loop bound as its value, also
  those that name a constant or read a field or an element of one.
- **BREAKING**: the generated C moves to the `termina` namespace. OSAL types and
  functions are named `termina__<module>__<name>`, the glue `termina__app__*`,
  the macros `TERMINA__<MODULE>__<NAME>` and `TERMINA__APP_CONFIG__*`, the
  run-time checks `termina__check__*`, the local variables and parameters added
  by the generator `termina__this`, `termina__ev`..., and ignored parameters
  `termina__ignored__<name>`. `Option`, `Status` and `Result` become
  `Option__<T>`, `Status__<T>` and `Result__<Ok>__<Err>`. The fields that the
  generator adds to a struct start with a single underscore, and no other
  generated identifier starts with one. Requires `termina-osal v0.6.x`.
- **BREAKING**: the generated Makefile includes `platform/common.mk` of the
  OSAL, which compiles in C11 with `-pedantic-errors` and warnings beyond
  `-Wall -Wextra` (`-Wconversion`, `-Wshadow`, `-Wcast-qual`...) and provides
  the `cppcheck` and `cppcheck-misra` targets.
- **BREAKING**: mutexes are initialized with a `MutexProtocol` value, and the
  initial event is dispatched by the runtime through
  `termina__system_init__dispatch`. Requires `termina-osal v0.6.x`.
- **BREAKING**: error codes renumbered. The variable usage errors take the
  `VUE-` prefix, the box linearity errors `BE-`, the value analysis `VAE-`
  (which replaces CFE-007, now retired), and the worst-case time errors
  `WTE-003` to `WTE-008`.
- The generated Makefile passes the `include` directory of the project with
  `-iquote` instead of `-I`, in the new variable `PROJECT_INCLUDE_DIRS`, so the
  header of a module whose path repeats one of the system, such as `sys.types`,
  no longer shadows `<sys/types.h>` inside the system headers.
- Methods and viewers are emitted `static` in the source file of their class and
  leave its header.
- The function pointers of an access port name their parameters, and the
  generated code takes the address of a task entry or a procedure with `&` when
  it stores it.
- Boolean constants are emitted as `true` and `false` from `<stdbool.h>`.
- Parameters passed by value are declared `const`, functions without parameters
  are printed as `(void)`, and unused event and ignored parameters are cast to
  `void`.
- The width of `usize` and of `int` are platform parameters.
- The initialization glue in `main.c` sets the status once and checks it only
  between steps that can fail; empty init functions are no longer emitted.
- The generated code keeps the status that the functions of the runtime return
  in a `termina__error_code_t`. Requires `termina-osal v0.6.x`.
- `TERMINA__TIME__MICROSECONDS_PER_TICK` is an unsigned constant.
- **BREAKING**: the `freertos10-stm32l432xx` platform is renamed
  `freertos10-stm32l432-nucleol432kc`, and so is its key in the
  `platform-flags` of `termina.yaml`.
- **BREAKING**: the `rtems5-leon3-nexysa7` platform is renamed
  `rtems5-leon3sf-nexysa7`, and so is its key in the `platform-flags` of
  `termina.yaml`. The platform compiles for soft-float, and in Gaisler's naming
  `leon3` is the board support package with a floating-point unit.
- **BREAKING**: the `SystemAPI` of `rtems5-leon3sf-nexysa7` extends `SysTime`
  only, as that of the FreeRTOS platform does, so a call to a print procedure of
  `system_entry` is rejected. The RTEMS back-end of the OSAL no longer provides
  a console.
- A `let` and the variables that a `match` case binds are declared `const` in
  the generated C. A box bound by a case is a const pointer to writable data.
- The names that the platforms reserve are generated again. `linux`, `unix` and
  `sparc` are no longer reserved on `posix-gcc` and `rtems5-leon3sf-nexysa7`:
  the compiler predefines them in its GNU modes only, and the platforms build
  with `-std=c11`.
- The loop of a task raises `ERuntimeFailure` with operation `task_dispatch`
  when an event names a port the task does not have, instead of rebooting
  without calling the `system_except` handler.

### Removed

- **BREAKING**: the undocumented `aligned` struct modifier.
- **BREAKING**: the parameter list after the path name of a `.wcet` entry,
  which named the constant parameters of the member function. Functions have
  no constant parameters, and the `.twcep` files already name the path without
  one, so an entry is written as `Class::member::path0 = <time>`.
- The `app-folder` and `app-file` entries of `termina.yaml`. The application
  is always `app/app.fin`; a `termina.yaml` that still carries them is
  accepted and the entries are ignored.

### Fixed

- `config.h` defines `TERMINA__APP_CONFIG__MESSAGE_BUFFER_MEMORY` as 0 when
  the application has no message queues. The RTEMS back end of the OSAL sets
  `CONFIGURE_MESSAGE_BUFFER_MEMORY` from it, so an application without queues,
  such as the one `termina new` creates, did not compile on RTEMS.
- A procedure that takes `&self` can be called through an access port: the
  function pointer of the port takes the instance as `const void *`, as the
  procedure does, where it took `void *` and the generated C did not compile.
  Requires `termina-osal v0.6.x`.
- The index of `load_index` and `store_index` on an atomic array goes through
  the bounds check at run time, as that of any other array access, unless the
  transpiler shows that it falls inside the array; it was not checked.
- A constant subtraction whose result falls outside the range of its type is
  rejected (CFE-004, CFE-005); a signed one was folded into a value its type
  cannot hold.
- The size of an array behind a reference parameter is written as a literal
  when it is a constant; it was written as the name of the constant, which C
  takes as a variable-length array.
- `termina try` folds the constants, analyses the values and elaborates the
  run-time checks of the module before generating its code, as `termina build`
  does.
- The cast of a constant is a constant expression where one is required, such
  as the size of an array or the bound of a loop; it was rejected because the
  expected type was compared with the type written after `as`.
- Operations on unsigned types narrower than `int` are masked to the width of
  their type before they are cast back, so the bits above it are dropped.
- The right operand of `<<` and `>>` is bounded to the width of the type at run
  time.
- A multiplication of an unsigned type whose product can leave the range of the
  `int` of the platform, `u16` on the current ones, is carried out in the
  unsigned type as wide as `int`. C promotes both operands to a signed `int`, and
  `65535 * 65535` overflowed it, which is undefined behavior.
- A cast applied to a binary operation converts the result of the operation in
  its own type, and a constant left operand of a shift is cast to its type.
- An additive operand of `&`, `^` or `|` is parenthesised.
- `char` constants are emitted with correct C escapes, and non-ASCII character
  literals are rejected.
- `65536` is no longer accepted as a `u16` value.
- The constant folding, and with it the value analysis, divides and takes the
  remainder of signed values truncating towards zero, as C does. It rounded
  towards minus infinity, which folded `-7 / 2` to `-4` and could leave out the
  check of a divisor that is zero at run time. A constant quotient of the
  minimum of a type by `-1` is rejected (CFE-004).
- The value analysis converts with `as` the way C does: a value that an
  unsigned target type cannot hold is taken modulo its range, and one that a
  signed type cannot hold gives any value of the type. It kept the value
  unconverted, which could leave out the check of a divisor or an index that
  fails at run time. Such a conversion of a constant is rejected (CFE-004), as
  it already was outside the analysis.
- VAE-003 no longer rejects the remainder of the minimum of a type by `-1`,
  which the runtime defines as 0.
- The initializer of a global constant is worked out down to each integer it
  holds, also in the elements of a constant array, so the generated C
  initializes it with literals. An operation with a run-time check used to be
  emitted as a call, which C does not admit in the initializer of a static
  object, and an overflow or a division by zero in it is now rejected (CFE-004,
  CFE-006).
- The variants of the built-in `Exception` enum match the ones the OSAL raises:
  `EMsgQueueFull` becomes `EMsgQueueSendError` and `EMsgQueueRecvError`, and
  `EShiftAmountOutOfBounds` is added.
- The inner dimensions of multidimensional array references are checked.
- The memory area of a pool is declared as an array of `max_align_t` sized with
  `termina__pool__area_length`, so its blocks are aligned for any type. Requires
  `termina-osal v0.6.x`.
- The size of an array passed by reference to a procedure called through an
  access port, the system port included, is checked against the parameter
  (CFE-012). A `println` of a `[char; 32]` with the default output buffer size
  of 256 used to be accepted.
- A match case parameter is read from its own position when an earlier one is
  ignored.
- `&(*p)` is emitted as `p`.
- The initial event of a task passes the event, the entity and the time to the
  action; a failed initial event action is reported as an exception; task
  dispatchers report their failures with the `Task` exception source.
- A local name is checked against the declarations of every module of the
  project, not only those typed before it.
- An error in a parameter points at the parameter instead of at the function or
  the member that declares it.
- The language server clears a diagnostic once it is fixed and sends the range
  of a name instead of the range of the whole element.
- `termina sched` runs the side-effect check.
- `termina sched` evaluates the global constants that a `.twcep` or a `.wcet`
  file names, such as the bound of a loop, where it stopped with an internal
  error.
- When receiving the next event of a task fails, the loop of the task raises
  `EMsgQueueRecvError`; it left the loop and the function of the task returned.
- `termina new --vscode` runs the debugger from the root of the project, where
  the `#line` directives of the generated C find the `.fin` modules.
- The action that a `continue` reaches has to return the same type as the
  action that continues to it (SE-226). The two types were not compared.
- A `reboot` or a `continue` in the branch of an `if` without `else` that
  precedes the return of an action is accepted, as in an `if` with `else`. It
  raised EE-013 or EE-005.
- A call or a cast of a non-constant value where a constant is expected, such
  as a field of the initializer of a constant, is rejected (SE-003).
- **BREAKING**: a key of the `platform-flags` of `termina.yaml` that names no
  platform is rejected. It was ignored, and its flags were lost.
- **BREAKING**: `rtems5-leon3sf-nexysa7` has no `irq_0`. The interrupt
  controller of the LEON3 has no line 0, and `irq_0` installed its handler on
  line 1.
- With `enable-system-except`, `init_emitters` registers the handler of
  `system_except`.

## [0.5.0] - 2026-06-19

### Added

- `sys-print-output-buffer-size` and `sys-read-input-buffer-size` options in
  `termina.yaml` that set the fixed size of the system port's output (`print`,
  `println`) and input (`read`) buffers. Both default to 256. The values are
  emitted as `#define`s in the generated `config.h`, so the OSAL uses the same
  sizes.

### Changed

- **BREAKING**: the system port's `print`, `println` and `read` procedures no
  longer take an explicit size argument. `print`/`println` now take a reference
  to a `[char; sys-print-output-buffer-size]` array, and `read` fills a
  `[char; sys-read-input-buffer-size]` array. A call such as
  `print(15, &msg)` must be rewritten as `print(&msg)`, with the buffer declared
  at the configured size. Requires `termina-osal v0.5.x`.


## [0.4.0] - 2026-06-18

### Added

- `f32` and `f64` floating-point types: decimal and scientific-notation
  literals, arithmetic (`+ - * /`) and ordering (`< <= > >=`) operators mixing
  integers and floats, `int`/`float` and `f32`/`f64` casts, and the Prelude bit
  reinterpretation functions `f32_to_bits`/`f32_from_bits` (and the `f64`
  variants). The integer-only operators (`%`, bitwise, shifts) and equality
  (`==`, `!=`) are rejected on floating-point operands. Floats are printed with
  `print_f32`/`println_f32`/`print_f64`/`println_f64` (requires
  `termina-osal v0.4.x`).
- Aggregate initializers for arrays, structs, `Option`/`Status`/`Result` and
  enum variants are lowered to C initializer lists, and `const` globals may now
  hold arrays of constants (emitted as `static const T x[N] = { ... }`).

### Removed

- **BREAKING**: variable-length arrays and `const` parameters (the
  `TConstSubtype` feature). Array sizes must now be compile-time constants
  (MISRA C Rule 18.8); constant sizes are folded per module, in dependency
  order, before the architecture stage.


## [0.3.4] - 2026-05-27

### Fixed

- Report a proper error code when an action declares a mutable self reference in
  a task or handler class. This previously surfaced as an internal error; it is
  now reported as SE-212 (task) or SE-213 (handler).


## [0.3.3] - 2026-05-26

### Fixed

- Avoid an internal error (`findDisjointPaths`) that aborted the build when a
  resource was accessed only through an `#[unprotected]` resource. Unprotected
  resources are now inlined in the dependency graph of tasks and handlers, as
  was already done for other resources.


## [0.3.2] - 2026-05-25

### Changed

- Stack snapshot bumped from LTS-22.43 (GHC 9.6.6) to LTS-22.44 (GHC 9.6.7),
  the latest in the LTS-22 series. Package versions are essentially the
  same; only the patch level of GHC moves forward.

## [0.3.1] - 2026-05-25

### Fixed

- `package.yaml` now declares `license: MIT` to match the actual contents
  of `LICENSE`. Previously declared `BSD3`, an inconsistency dating from
  the initial Hpack scaffolding.
- `stack.yaml` is now tracked in the repository, pinned to Stackage
  snapshot `LTS-22.43` (GHC 9.6.6). Previously gitignored, which forced
  every fresh clone to run `stack init` and pick whichever snapshot was
  current at the time, breaking reproducibility. `stack.yaml.lock`
  remains gitignored; `termina.cabal` (regenerated by hpack from
  `package.yaml`) likewise.

## [0.3.0] - 2026-05-25

### Added

- Initial release under the formal versioning policy. Earlier history is
  preserved in `git log` but not catalogued here.
