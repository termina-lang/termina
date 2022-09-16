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
// - Atomic global object
// - Volatile object
// - Message Queue
// - Data Pool
<global-declaration> ::= <atomic-declaration>
                       | <volatile-declaration>
                       | <msg-queue-declaration>
                       | <pool-declaration>

// This element declares an Atomic global variable.
// These variables can be of one specific type. Access
// to these variables are guaranteed to be atomic by the
// underlying architecture.
<atomic-declaration> ::= 'Atomic' '<' <basic-type> '>' <identifier> ';'
    
// This element declares a Volatile global variable.
// These are not "real" variables. They are used to
// represent hardware registers that are mapped into
// main memory. The "address" is the fixed memory
// address into which the register is mapped.
// NOTE: these "variables" ARE NOT ATOMIC
<volatile-declaration> ::= 'Volatile' '<' <basic-type> '>' <identifier> 'at' <address> ';'

// This element declares a message queue of objects of
// a given type. It must inlcude the type of objects
// that can be enqueued and the maximum number of
// elements that can be enqueued at a given time.
// TODO: Allow the definition of queues of product types
<msg-queue-declaration> ::= 'MsgQueue' '<' <basic-type> ',' <integer> '>' <identifier> ';'
    
// This element declares a pool of objects of
// a given type. It must inlcude the type of objects
// that can be enqueued and the size of the pool.
// TODO: Allow the definition of pools of product types
<msg-queue-declaration> ::= 'Pool' '<' <basic-type> ',' <integer> '>' <identifier> ';'

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

// Definition of a concurrent task
// TODO: Allow procedures to return product types
<procedure-definition> ::= 'procedure' <identifier> '(' { <input-parameter> {',' <input-parameter>}* }? ')' ':' <basic-type> <compound-statement>

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
