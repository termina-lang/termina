# Termina Grammar in Backus-Naur Form

```c

// Programs are a list of constituent elements
<program> ::= {<element>}*

// Elements can be:
// - Tasks
// - Procedures
// - Event handlers
// - Declarations
<element> ::= <task-definition>
            | <procedure-definition>
            | <handler-definition>
            | <global-declaration>
            | // TODO: <product-type-specifier> 
                         
// Declarations can be:
// - volatile
// - globally protected objects
// - unprotected global objects (used by a single task or handler)
<global-declaration> ::= <volatile-declaration>
                       | <protected-declaration>
                       | <unprotected-declaration>
    
// This element declares a volatile global variable.
// These are not "real" variables. They are used to
// represent hardware registers that are mapped into
// main memory. The "address" is the fixed memory
// address into which the register is mapped.
// NOTE: these "variables" ARE NOT ATOMIC
<volatile-declaration> ::= 'volatile' <identifier> ':' <basic-type> 'at' <address> ';'

// This element declares a global object that implements an implicit protection
// mechanism that allows a data-race free access to its members.
<protected-declaration> ::= 'protected' <identifier> ':' <basic-type> '=' <constant> ';'

// This element declares a local object that can only be used by a single task or
// event handler.
<unprotected-declaration> ::= 'unprotected' <identifier> ':' <basic-type> '=' <constant> ';'

// The list of basic types
<basic-type> ::= 'u8'
               | 'u16'
               | 'u32'
               | 'u64'
               | 'i8'
               | 'i16'
               | 'i32'
               | 'i64'
               | 'bool' // Alias of i8
               | 'char' // Alias of i8

// Definition of a concurrent task
// TODO: Tasks must return Result<u32>
<task-definition> ::= 'task' <identifier> '(' <input-parameter> ')' <compound-statement>

// Definition of a function
// TODO: Allow functions to return product types
<function-definition> ::= 'fn' <identifier> '(' { <input-parameter> {',' <input-parameter>}* }? ')' '->' <basic-type> <compound-statement>

// Definition of an event handler
// TODO: Handlers must return Result<u32>
<handler-definition> ::= 'handler' <identifier> '(' { <input-parameter> }? ')' <compound-statement>

// Compound statements
<compound-statement> ::= '{' {<local-declaration>}* {<statement>}* '}'
    
// Input parameters of tasks, procedures and handlers
<input-parameter> ::= <identifier> ':' <basic-type>
    
// Declaration of a local variable. A la Rust. e.g.:
// let foo : u32 = 0;
<local-declaration> ::= 'let' <identifier> : <basic-type> '=' <constant> ';'

<statement> ::= // TODO: assignments, conditionals, loops, etc.

```
