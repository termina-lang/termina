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

- `termina generate`, which writes the C and the Makefile of the project
  without compiling them, so it does not require to have installed any
  of the toolchains.
- AE-023: an instance of a task, a handler, a resource, an emitter, a
  channel or a test class declared outside the main module of its
  deployment, the `app.fin` of the application or the `test.fin` of a test.
- Tests in the manifest, under `tests:`, one entry per folder of `tests/`,
  each with a `test.fin`, an optional `src/` and an optional `extern/`. An
  entry has the list of `platforms` the test runs on, a `timeout`, an optional
  `test-log-size`, an optional `harness-stack-size` (by default the stack of a
  task of the platform), an optional `microseconds-per-tick` (by default the
  one of `app`) and an optional `extern` section with the C of the test. A test is
  compiled from `src/` and its own `src/`, and its C from the `extern/` of the
  project and its own. A module of a test with the name of a module of `src/`,
  and a file in both `extern/` folders, are errors.
- `termina generate` and `termina build` take `--test <name>`, `--tests` and
  `--all`, which select one test, every test, or the application and every
  test, each test on every platform of its list. `--platform` keeps the tests
  of one platform, and `--profile` chooses the profile of the tests, by
  default the one of `app` and `debug` in a library.
- `termina new <name> --lib` creates a library, which has no `app/` folder;
  `--app` is the default. A library takes no `--platform`. `--exports
  <prefix>` writes the prefix in the manifest and creates its folder under
  `src/`; without it the library starts with `exports: []`, and the build
  asks for a prefix. The manifest has the name of the project and the prefix
  in quotes, so YAML reads a name such as `on` or `123` as text.
- `termina new --test <name>`, run inside a project, creates
  `tests/<name>/test.fin` and adds the entry of the test at the end of the
  `tests:` section of the manifest, leaving the rest of the file as it was.
  The test runs on `posix-gcc` and, in an application on a board, also on the
  platform of `app`, with a timeout of 10 seconds.
- `termina build` writes `provenance.json` in the folder of each deployment,
  with the SHA-256 of the binary and of the object of every module without
  their debugging information (`objcopy --strip-debug`), and the source of
  every module, so the objects of a test can be compared with those of the
  application. The binary of a test is hashed without its descriptor, the
  `.termina_test` section, which the copy of `termina deliver` does not
  have. The hashes are left out when the toolchain has no `objcopy`.
- The C of every deployment is generated twice. The C with the `#line`
  pragmas that point to the `.fin` goes to `output/app/` and
  `output/tests/<name>/<platform>/`, and is built, debugged and traced to
  Termina. The plain C, without them and broken to the usual width, goes to
  `output/plain/app/` and `output/plain/tests/<name>/<platform>/`; it is not
  built, and cppcheck and the MISRA checks report their findings at its lines
  instead of those of the `.fin`. Before, the C carried the pragmas only in
  the `debug` profile.
- The compiler runs from the folder of the project, and the debugging
  information names its files relative to it (`-ffile-prefix-map`), as the
  `#line` pragmas do, so a debugger run from the folder of the project finds
  the C and the `.fin` wherever the project is, and the same project gives
  the same binary in any folder.
- The modules of a test itself are built with `-fno-optimize-sibling-calls`,
  which the Makefile of the platform in the OSAL sets, so the return address
  of a check is the one of its call.
- A unit test writes its descriptor, `test.json`, in the folder of the
  deployment: its cases with their numbers, every check with its line and
  its text, the tasks and the emitters by identifier, and a stamp, the first
  32 bits of the SHA-256 of the rest, which the C of the test also carries.
  The build adds it to the binary in a section that is not loaded,
  `.termina_test`, when the toolchain has `objcopy`.
- `termina test [<name>] [--platform <p>] [--profile <p>]` builds the
  selected tests and runs them on posix: the record of each one goes to
  `record.bin` and its output to `test.log`, in the folder of the
  deployment. The report gives each case as passed, failed, with an
  exception, timed out or not run, each failed check with its file, its
  line, its text and its values, and warns about a case that ran no checks.
  It exits with 0 when every case passes, 1 when one does not and 2 when a
  test cannot be built or read.
- `termina test` runs the tests on the boards through the GDB server of each
  one (`ST-LINK_gdbserver` on the NUCLEO-L432KC, GRMON on the Nexys A7,
  `hw_server` on the PYNQ-Z2), with `gdb-multiarch` and the `addr2line` and
  `nm` of the toolchain: it loads the binary, selects the case, stops at the end of the
  run or at an exception, with its backtrace, and dumps the record from the
  RAM of the board. `--gdb <host>[:<port>]` gives the server of one board.
  `results.json` gives the version of the GDB that ran the test.
  Before each run on the PYNQ-Z2, an XSDB server, given by
  `--xsdb <host>[:<port>]`, resets the board and loads its bitstream with
  `assets/xsdb/init.tcl` of the project. On `posix-gcc`, `--with-gdb` runs
  the binary under the native `gdb`. The timeout counts from
  `termina__test__init`, so the load does not count.
- Concurrent tests: a `test.fin` without `test` instances runs its tasks, its
  timers and its interrupts as the application does, and ends when the
  system comes to rest after `finish()` of `test_scenario`. The test is built
  with a harness task at priority 254 and the hook of `system_except` unless
  the test wires one, and is reported as one case named after the test. A
  timeout after `finish()` says that the system did not come to rest, and on
  posix `check_elapsed` is reported as a measurement without a verdict.
- `termina test <name> --record <folder>` decodes the record that a debugging
  session dumped into `header.bin` and `area.bin` and writes the report,
  `results.json` and `report.xml`. The case still open when the record ends
  is reported as interrupted, with its last step.
- `termina test` writes, in the folder of the deployment, `results.json`,
  with the verdict of each case, the values of each failed check, the
  SHA-256 of the binary, the hashes of `provenance.json` and the commit of
  the project, and `report.xml`, in JUnit XML. `--case <instance>.<case>`
  runs one case of a unit test, and `--isolated` runs each case in a process
  of its own. The cases beyond an overflow of the record are reported as
  without a record.
- `termina deliver` writes in `output/deliverable/app/` a copy of the
  application that builds with the toolchain of the platform alone: the plain
  C, the files of the OSAL that the build opens, the license of the OSAL and
  of each project of its `vendor/` with a file there, and a flat Makefile
  with the sources listed one by one and the flags resolved for the platform
  in release, whose folder of the toolchain is still a variable
  (`TERMINA_RCC_DIR`, `TERMINA_RTEMS6_DIR`, `TERMINA_ARM_NONE_EABI_DIR`). It
  builds the copy and fails when its binary differs without debugging
  information from the one built from the C with the `#line` pragmas. Both
  are built in release whatever the profile of the manifest, the second one
  in `.termina/deliver/`, so `output/app/` and `output/plain/` keep the last
  build of the project.
  `provenance.json` of the copy holds the SHA-256 of every file, the versions
  of the transpiler, of the OSAL and of the compiler, and the stripped hashes
  of the binary and of the module objects. `--tests` delivers every test as
  well, into `output/deliverable/tests/<name>/<platform>/`; `--archive` packs
  each copy with its binary and link map into a `.tar.gz` that the same files
  always give byte for byte, and `--with-trace` adds the binary built from the
  C with the `#line` pragmas, under `trace/`.
- The generated Makefile has a `build-info` target, which prints the
  values of the build that `termina deliver` reads.
- `termina doctor` reports the OSAL that the build takes, with its folder
  and its version, the versions of `make`, `git` and `gdb-multiarch`, and for
  each platform the compiler and the `objcopy` that its `platform.mk` names,
  with the variable to set when the compiler is not found. Inside a project
  it marks the platforms of the application and of the tests, and compares
  the image of `.devcontainer/devcontainer.json` with the version of
  `termina`. It exits with 1 when the OSAL, `make` or the compiler of a
  platform of the project is missing.
- `termina clean` removes `output/` and `.termina/` of the project.
- `app.linker-script` in the manifest names a linker script of the project
  for the application, and `tests.<name>.linker-script` one for each
  platform of a test. The generated Makefile passes it to the OSAL as
  `TERMINA_LINKER_SCRIPT`, and `termina deliver` copies it to `ld/` in the
  deliverable. `posix-gcc` takes none, and the file must exist. Requires the
  matching OSAL.
- The `rtems6-rv32im-spike` platform, RTEMS 6 on the RV32IM of the Spike
  simulator, and the `rtems6-noel32im-artya7` platform, RTEMS 6 on a NOEL-V
  RV32IM on the Arty A7 board, both with the toolchain of
  `TERMINA_RTEMS6_DIR` (`/opt/rtems/6`) and with the interrupts `irq_1` to
  `irq_31` of the PLIC. `termina test` does not run the tests on them yet.
  Requires the matching OSAL.

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
  A deque holds a type a box can hold (SE-221), takes no initial value (SE-224)
  and a capacity of at least one (CFE-017). The architecture check follows a
  box through the deques it goes through, so a box allocated from one pool and
  freed to another after being parked is AE-003. The procedures of a deque are
  in the OSAL, which checks again at run time that a `put` finds room, so this
  release needs a matching OSAL.

- An enum variant or struct initializer may be passed as the argument of a
  call to a function, a method, a viewer or a procedure, as in
  `self->gpio.write_pin(pin, GPIOPinState::High)`. It is typed against the
  type of the parameter, as the initializer of a declaration is, and reaches
  C as a compound literal (`(GPIOPinState){ ... }`). The value of `send` and
  of `fire` may be an initializer too, which declares the temporary passed by
  address. SE-087 now only rejects an enum variant initializer outside a
  declaration, an assignment or an argument.
- `test class`, a class with access and out ports, private methods and
  `case`s, and `test` instances of it, wired like any other instance. A case
  takes a self reference and nothing else and returns nothing, and only the
  harness runs it, so a call to a case is SE-038. A test class has no fields,
  no in or sink ports, no actions, procedures or viewers, and provides no
  interface (SE-225 to SE-229); a case outside a test class is SE-230, one
  with parameters SE-231, and a `test` instance of another class SE-232. A
  test class is generated as a structure with its ports only, and each case
  as an action without parameter, declared in the header for the harness:
  the event, the test instance as `void * const termina__this` (`const` with
  `&self`) and the cast to `self`, without a lock. A `test` instance is a
  global of the C of its module, and `termina__app__init_globals` wires its
  ports, as it does those of a task.
- The components of a unit test, which take the place of production ones at
  the other end of the ports of the unit: a `TestEmitter<T>`, declared with
  `emitter`, at a sink port; a `TestChannel<T; N>`, declared with `channel`,
  at an out port; and a `TestPool<T; N>`, declared with `resource`, at an
  `Allocator<T>` port. A case reaches them through `TestEmitterAccess<T>`
  (`fire`), `TestChannelAccess<T>` (`pending` and `take`) and the
  `TestPoolAccess` interface (`in_use`, `max_in_use` and `fail_allocs`).
  None of them takes an initial value (SE-233), and a port only connects to
  its kind of component (SE-234) with the same type (SE-235).
- The C of a unit test. The components of a unit test are types of the
  runtime of the OSAL, the same for every type of message:
  `termina__test__emitter_t`, a `termina__msg_queue_t` and
  `termina__test__pool_t`, and their ports point to the functions of the
  runtime. `fire` passes the address of the value, through a temporary when
  it is not an object, and `take` passes the tag and the payload of the
  option apart. In `config.h` of a unit test, the harness takes the emitter
  identifier after `system_except` and the task identifier after the last
  task, and the test emitters follow it. Each test channel has a queue of
  messages and a queue of events, `<channel>__event_msg_queue_id`, which
  `main.c` creates. The test pools count as pools. `main.c` points the
  fields of a test channel to the harness and to its queue of events,
  initializes a test pool with `termina__test__pool__init` and protects it
  through the pool inside it, and fills the connection of a test emitter in
  its object; a test emitter at the sink of a handler reaches its action
  through a function `termina__test_fire__<emitter>` of `main.c`, which
  takes the value with its type. The `main.c` of a unit test defines the
  record of the test, with stamp zero until the descriptor exists, the
  reset before each case and the task of the harness, at priority 254 with
  `harness-stack-size`, which runs each case selected by the runner in the
  order of `termina test --list`, and registers the hook of the runtime as
  the handler of `system_except`. The Makefile of a test sets
  `TERMINA_TEST=1`, and the one of a unit test `-DTERMINA__TEST__CLOCK`.
- `test_entry`, which only a test has, provides the `TestAPI` interface: a
  `check`, a `check_eq_<type>` for each integer type, `bool` and `char` (the
  value obtained first and the one expected second), `check_elapsed`, `fail`,
  `step`, `advance` for the test clock and `finish`.
- Test classes, test instances and the components of a unit test are only
  accepted when a test is built (SE-236), and `TestAPI`, `TestPoolAccess`
  and `test_entry` only exist then.
- The response to an event ends: from the action that each emitter
  triggers, following the messages that each action sends and the actions it
  continues with, no chain reaches an action that is already on it (AE-015).
  Two tasks that kept sending messages to each other, or a task that sent
  messages to itself, compiled before and never came back to rest. In a unit
  test, a message that a case sends starts a chain too.
- `termina test --list` lists the tests of the manifest, each with its level
  (unit or concurrent), its platforms and its cases, named
  `<instance>.<case>`; with `--json`, each case also gives the file and the
  line where it is declared. It only parses the modules of each test, so it
  builds nothing and lists a test that does not type check.
- A channel is connected to exactly one out port (AE-016). AE-002 only
  checked the in port of a channel, so a second task or handler sending to
  the same channel compiled.
- The architecture checks the deployment of a test. A deployment with test
  instances is a unit test: it has no emitter other than its test emitters,
  and `system_except`, which the harness attends, is not connected (AE-019).
  A test emitter is connected to one sink port (AE-004, AE-001), a test
  channel to one out port (AE-005, AE-016) and a test pool to at
  least one allocator port of the unit (AE-018), and each of them to an
  access port of at least one test instance (AE-017). A deployment without
  test instances is a concurrent test and has no components of a unit test
  (AE-020). A call to `advance` in a concurrent test is AE-021, and one to
  `finish` in a unit test AE-022; both checks read the code, so a call in a
  branch that never runs counts. The test instances are not users of a
  resource when its protection is decided, and a box that a case takes from
  a test channel is followed back to the pool it came from (AE-003).
- Dependencies on libraries, each one a git repository and a tag under
  `dependencies:` of the project or of a test. `termina add <url> <tag>`
  writes the dependency in `termina.yaml`, resolves the dependencies of the
  dependencies, and writes the whole graph in `termina.lock`, each package
  with its tag, its commit and the SHA-256 of its files, and a copy of each
  package in `vendor/`, which is committed with the project. The copy holds
  the manifest of the package, its exported modules and the files of `extern/`
  that they list. A package is brought once for the application and all its
  tests, and an override of the root forces its tag. Only `https://` URLs are
  accepted; for an SSH dependency, `termina add` gives the `insteadOf` rule of
  git that keeps SSH on the machine of the user.
- Before it goes to `vendor/`, a package is checked: its manifest has a known
  version and `exports`, its `extern` section is valid and lists files of the
  package, it holds no symbolic link or submodule where files are copied, no
  two of its paths differ only in case, and it has no path dependency. Its
  type errors are left to the build of the consumer.
- Every build checks each copy in `vendor/` against the SHA-256 of the lock and
  stops on a copy missing or modified, and follows the dependencies added,
  changed or removed in `termina.yaml` since the lock was written.
  `termina restore` brings back from upstream the copies that do not match the
  lock. `termina update [<dependency>]` resolves the tags of the manifest
  again, and with `--check` reports what it would change and fails if
  anything would. `termina verify` checks without network and without writing
  that `vendor/` and the lock match the manifest. `build` and `generate` take
  `--offline`, and `deliver` runs `verify` first.
- A path dependency, `- path: <folder>`, takes a library from a local folder,
  which is checked as a fetched package on every build. A build in `debug`
  warns once for each folder that it cannot be reproduced from the lock, and
  one in `release` stops before building anything.
- The modules of a build only import modules of the folders they see: the
  project sees `src/` and the exported modules of its dependencies, a test
  also its own `src/` and dependencies, and a package its own modules and the
  dependencies it declares. A module found in two folders of the build is an
  error, and so is an exported module that imports a module outside its
  package and the dependencies it declares, or two packages whose prefixes
  overlap.
- `build`, `generate` and `deliver` check a library on its own: its `exports`
  (not empty, no prefix twice or inside another, a module under each one), its
  exported modules (they parse, import only exported modules and declare no
  test class and no instance), and the types of the exported modules together with those of
  every package that its dependencies reach, on each platform of its tests. A
  global of the library with the name of a global of a dependency is reported
  in the library (SE-131 to SE-133), and so is a global with the name of a
  local object of a dependency (SE-239).
- The commands that work on a project run from any folder inside it: they
  look for the nearest `termina.yaml` upwards, as git does, and from a copy in
  `vendor/` they take the project that holds it.
- SE-238: a `return` with a value of another type than the one of the
  function. It was an internal error.
- SE-240 and SE-241: each declaration only accepts its own modifiers.
  `priority` and `stack_size` go on a task, with a value, `unprotected` on a
  resource of a resource class and `packed` on a struct, without one. A task
  with `#[stack(2048)]` was created with the default stack, and
  `#[priority]` without a value stopped the transpiler with an internal error.
- SE-242: a reference to a fixed-location field, or to a field or element
  inside one. The field is a pointer to volatile memory in C, and the
  reference reached the callee without the `volatile`, which the C compiler
  rejects with the flags of the OSAL.
- SE-213 also rejects a top-level name that collides with a global symbol of
  what a deployment of the platform links.
- A boolean constant may be the result of a comparison or of `&&` and `||`.
  Those of integers and booleans are worked out by the transpiler and reach C
  as `true` or `false`; those of reals or characters are left to the C
  compiler, as the real constants are.

### Changed

- `config.h` reserves emitter identifier 0 for `system_except` in every
  build, whether the deployment connects it or not, and the other emitters
  take 1 onwards, so an event with identifier 0 always comes from
  `system_except`. It also reserves handler identifier 0,
  `system_except__handler_id`, for the handler of `system_except` of the
  runtime of a test, and the handlers of the application take 1 onwards.
- `ERuntimeFailure` and `EMsgQueueSendError` take the address of the call to
  the runtime as their first field, `ERuntimeFailure(usize, u32, i32)` and
  `EMsgQueueSendError(usize, usize, i32)`, 0 when the runtime runs the
  operation alone. The dispatch of a task passes 0 to
  `termina__except__runtime_failure`. This release needs a matching OSAL.
- A test reaches three resources in place of `test_entry` and `TestAPI`:
  `test_checks` (`TestChecks`, the checks, `fail` and `step`, which takes a
  `usize`), `test_clock` (`TestClock`, `advance`) and `test_scenario`
  (`TestScenario`, `finish`). Only a test class may declare an access port of
  `TestClock`, `TestPoolAccess`, `TestEmitterAccess<T>` or
  `TestChannelAccess<T>` (SE-237). A connection to `test_scenario` in a unit
  test is AE-020, which replaces AE-021 and AE-022. The three resources enter
  the architecture as `system_entry` does, and are dropped when nothing
  connects them, so they get the protection of their users, without counting
  the test instances, and their access ports point to the functions of the
  runtime named after the class, such as `TestChecksEntry__check`.
- An access port to `system_entry` carries `_that`, as any other access port,
  and a call through it passes the object after the event. This release needs
  a matching OSAL.
- `test` is a reserved word, so it can no longer name a variable, a field or
  any other object.

- `termina.yaml` has a new format, the one of the package manager, and the
  flat manifest of earlier versions is no longer read: a manifest without
  `project.manifest-version` is an error. The manifest has a `project` section
  (`name`, `version`, `manifest-version` and an optional `assurance`), either
  `exports` (a library) or `app` (an application, with `platform`, `profile`
  and an optional `microseconds-per-tick`), optional `dependencies`,
  `overrides`, `extern` and `tests`, and the top-level keys
  `sys-print-output-buffer-size` and `sys-read-input-buffer-size`. The layout
  of the project is fixed (`src/`, `app/app.fin`, `extern/`, `tests/`,
  `output/`, `efp/`) and it is always built with make, so `source-modules`,
  `app-folder`, `app-file`, `output-folder`, `efp-folder` and `builder` are
  gone. An unknown field is
  an error that names the closest known one, and the folders of the project
  are checked against the manifest, with every mismatch reported at once.
- `termina build` runs `make` after generating, and the application is
  generated into `output/app/` instead of `output/`. A test goes into
  `output/tests/<name>/<platform>/`, and its binary takes the name of the test.
  The VS Code configuration of `termina new` runs `termina build` and debugs
  `output/app/bin/<project>`, and its `.gitignore` leaves out the whole
  `output/` folder, which is generated again from the project.
- The emitters `system_init` and `system_except`, the resource `system_entry`
  and every interrupt of the platform exist in every program, and the ones the
  application does not connect are dropped before the architecture is
  checked, so an unconnected one is not an error and generates nothing. The
  flags `enable-system-init`, `enable-system-port`, `enable-system-except` and
  the `platform-flags` section, which only held the `enable-*-irq` flags, are
  gone. The names of every interrupt of the platform are reserved.
- Priority 254 is reserved for the test harness: tasks take priorities from 1
  to 253 (CFE-014).
- `config.h` only holds what belongs to the deployment, and only `main.c` and
  `init.c` include it. The port indices of a task class go into the header of
  the class, and the buffer sizes of `sys_print` and `sys_read` reach the OSAL
  as `-D` flags of the generated Makefile when they differ from 256. This
  release needs a matching OSAL.
- The generated Makefile sets `TERMINA_OSAL_DIR ?= /opt/termina/osal`, so the
  OSAL is looked up in `/opt/termina/osal` unless the environment of `make`
  sets the variable. It was `/opt/termina-osal`.
- `make clean` also removes the link map, `$(TARGET).map`.

- Actions return nothing. The parser no longer accepts a return type on an
  action, the task loop calls the action without checking a status, and
  `EActionFailure` and `ExceptSource` leave `Exception`. An action that used
  to end in `Failure` rebooted the system through `system_except`; an action
  now reports an error with a message, for instance to an FDIR task.
  `Status<i32>` is no longer a type of the OSAL prelude and is generated like
  any other status type where the application uses it. The return types that
  each kind of action had to have (SE-193 to SE-197) and the check on the
  type of a continued action (SE-226) are gone, and the semantic error codes
  are renumbered without gaps: SE-198 to SE-225 become SE-193 to SE-220, and
  SE-227 to SE-230 become SE-221 to SE-224.
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
- A task without `stack_size` gets the default stack size of its platform:
  2048 bytes on the Nucleo and 4096 on the other platforms, as before. On the
  RTEMS platforms it is the minimum of the architecture.
- The `extern` section lists the C of the project under the module whose
  `extern` functions it defines, with its `sources` in `extern/src/`, its
  `headers` in `extern/include/` and its `libraries` in
  `extern/lib/<platform>/`; sources and headers take patterns with `*` in the
  file name. A section of the old form is an error that gives the new one.
  Every file of `extern/` is listed under some module, each entry names a
  module of its source folder that declares an `extern` function, and each
  module with an `extern` function that a build loads has its entry;
  `app.fin` and `test.fin` declare none. A build compiles only the C of the
  modules it loads, with the headers of `extern/include/` before the
  generated ones, and a header there with the path of a generated one is an
  error. The C of each dependency is compiled with its own `-iquote` and
  linked into its own object, and `vendor/` gets only the C of the exported
  modules of a package. The C of a dependency is checked when the package is
  admitted, and that of a library in its own build.
- The language server checks the application, or the exported modules of a
  library, as the build does, through every stage after the type checker,
  with the text of the files open in the editor, and with the dependencies of
  the project. It finds the project from any folder inside it.
- The errors of parsing, type checking and the later passes go to the error
  output, as those of a C compiler do, and the standard output is line
  buffered for every command.
- The objects, the flags file and the `extern` object go to `obj/`, and `bin/`
  keeps the binary, its map and the hashes. A test named `src` failed to link
  because `bin/src` was the folder of the module objects.
- The errors of the manifest start with the path of the field, as in
  `tests.hk.timeout: must be positive`, and those of the folders of a project
  and of the `extern` sections with the file they refer to.
- PE-003 is now an imported module not found, and names the folders where the
  importer looks for it, in the order in which they are searched.
- `termina build` and `termina generate` in a library, and `--tests` in a
  project without tests, exit with 0. `termina test --list` exits with 2,
  not 1, when it cannot read the tests.
- `config.h` defines `TERMINA__APP_CONFIG__IRQ_LOCKS`, the number of
  resources and pools protected by an interrupt lock, and one
  `<resource>__irq_lock_id` for each, and `termina__app__enable_protection`
  writes it into the `irq.lock_id` of their lock type. A resource procedure
  calls `termina__resource__lock` and `termina__resource__unlock` without a
  lock variable. This release needs a matching OSAL.
- The handler of `system_except` no longer counts as a user when the lock of
  a resource is chosen, so a resource that one task shares with it takes no
  lock, and one that tasks and handlers share takes the lock that they need.
  `config.h` defines `TERMINA__APP_CONFIG__EXCEPT_PROTECTED`, the number of
  resources and pools protected by a lock that the handler reaches, nested
  ones included, and `termina__app__disable_protection` withdraws their
  protection; the runtime calls it before the handler. This release needs a
  matching OSAL.

### Removed

- `delay_in` leaves `SysTime`, and a program that calls it is rejected.

### Fixed

- The incremental build compared modification times, so a change of
  `--profile` kept the C of the previous profile, and the C of a deleted
  module stayed in the output and was compiled and linked. The
  transpiler now writes each generated file only when its content changes,
  so make compiles only what changed, and removes the C of the modules that
  are gone. A recipe of the generated Makefile that fails no longer leaves a
  target that make takes as up to date.
- A procedure with a parameter of type `Option<box T>`, which moves the
  option of the caller into the procedure, crashed the transpiler in the
  architecture check. The check now follows the box through the call.
- The `init.c` assigned `termina__pool__alloc` and `termina__pool__free` to
  the access ports of a pool without `&`, against Rule 17.12 of MISRA
  C:2023, which the rest of the wiring already followed.
- A box allocated from one pool and freed to another inside a `for` loop
  compiled without AE-003 and raised a `pool_free` runtime failure.
- A variable never read went without VUE-002 when another member function of
  the same class read a variable of the same name. Each body now has its own
  set of variables read; the fields and member functions used are still
  counted across the class.
- A `match` with a single case and no default, whose case binds no variable,
  left the matched object unused in the generated C, which the C compiler
  reported with `-Wunused-variable`. The object is now discarded with a cast
  to `void`.
- An `Option` whose only use was as the element of an array, such as a field
  `[Option<Pin>; 4]`, did not get its C type (`Option__Pin`), and the
  generated C did not compile.
- The connection of a periodic timer or an interrupt that targets a task left
  `task_id` unset, and the event that the runtime sends to the task carried
  whatever the stack held as the task that owns it.
- An addition, a subtraction, a multiplication or a left shift of a `u8` or a
  `u16` is carried out in the unsigned type as wide as `int`, masked to the
  width of its type and cast back to it, also when it is assigned, returned
  or passed as an argument. It was carried out in the signed `int` of the
  promotion, and an assignment such as `crc = crc << 1` was left without the
  mask and the cast, which clang reported with `-Wsign-conversion` and
  `-Wimplicit-int-conversion`.
- A change of profile rewrote the Makefile and left the objects built with
  the flags of the other profile, since their rules depended only on their
  sources. Every object, the binary and the `extern` object now depend on a
  file with the compiler, the assembler and the flags, which also holds the
  path and the `VERSION` of the OSAL, and the binary depends on the linker
  scripts of `LDFLAGS`. Another OSAL behind the same path, or a changed linker
  script, now rebuilds what it affects.
- A constant cast from a real, such as `const N : u32 = F as u32`, stopped
  the transpiler with an internal error. The cast is left to the C compiler.
- The language server reads a file again from disk when the editor saves or
  closes it, gives the type of a field access on a hover, and keeps the index
  of a module while it has a type error. Without a `termina.yaml`, it tells
  the editor with a message and checks nothing; `termina lsp` stopped the
  process, and the editor started it again each time.
- A hidden file such as `.DS_Store` in `extern/` stopped the build, and one in
  a package of `vendor/` changed its content hash; hidden files are left out
  of both, and of a deliverable. A hidden file listed in an `extern` section
  is an error.
- An exported module that does not parse is reported with the column and the
  source line of the error, and a file that is not UTF-8 with its path.

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
