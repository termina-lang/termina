# Termina

Termina is a domain-specific language for real-time embedded systems aimed at simplifying their implementation and reducing validation and verification costs.

This repository contains the implementation of a transpiler from Termina to C. The generated code is supported on an [operating system abstraction layer](https://github.com/termina-lang/termina-osal). This layer is responsible for adapting the generated code for different real-time operating systems.

## Main features

Termina adheres to the following core principles:

- **Ease of learning and use**. Termina follows the imperative paradigm with a syntax similar to C to facilitate adoption by current developers.
- **Domain-specific abstractions**. The language includes abstractions pertinent to reactive critical real-time systems, such as events, tasks, and asynchronous handlers, aligning design models with final application code.
- **Reactive programming model**. Tasks and handlers in Termina will be reactive components, incorporating various response actions. The language will enable global event definition and association with component actions, enhancing application analysis and verification.
- **_Run-to-completion_ semantics**. Actions in the system always complete once started, without blocking their executing tasks. This approach requires **all functions to be terminating**, with bounded loops and no recursion, aiding verification procedures.
- **Design patterns for safety and robustness**. Termina supports only two interaction mechanisms between reactive elements: shared resources and asynchronous message passing. A monitor-based approach manages shared resources.
- **Deterministic dynamic memory management**. Termina only allows the use of memory pools as a dynamic memory management mechanism.
- **Provision of memory-safety properties**. Termina implements a type system that guarantees that no error arises from memory management. In particular, the absence of access errors, null-pointer de-references and memory leaks.

## Documentation

The main source of documentation of this project is the [Termina book](https://termina-lang.github.io/termina-book/). It contains installation instructions and documentation on the syntax and use of the language.

## Status

The current status of the project is as follows:

- The project is still in a preliminary phase. The transpiler is able to generate C code for different platforms. This code is supported by the Termina OSAL that can be found in its [repository](https://github.com/termina-lang/termina-osal).
- The transpiler incorporates a type checker and a fully functional semantic parser capable of detecting errors and generating meaningful error messages.
- The transpiler implements a simple Language Server Protocol (LSP) server that can be used by different integrated development environments.
- The transpiler allows detecting a number of errors at compile time thanks to its type system and the semantic analyzer, but there are other errors such as, for example, the indexing of arrays out of bounds, whose detection is performed at runtime.
- The transpiler incorporates a mechanism for the execution of system calls in the form of procedures that allow, among other things, the obtaining of different parameters such as, for example, the time elapsed since the system boot.

## Future features

The following features are under consideration and are not yet present or available to the application developer:

- Generic programming. The language implements a small number of parametric types for memory management and inter-task communication, but there is no flexible mechanism to allow generic programming.
- Implementation of support for other real-time operating systems. The Termina runtime, available in this repository, supports a small number of platforms. It is planned to incorporate new platforms in the near future.

## Contribute

If you are interested in collaborating with us and would like more information, please contact us at the following e-mail address: team@termina-lang.org.
