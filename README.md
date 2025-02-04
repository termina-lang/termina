* Termina Language/Transpiler Implementation
** Module System
Inside termina: each file is a module maybe referring to other modules.
Each module may refer to /internal modules/, following a tree-like structure
defined by the file system itself.

Names follows perioded-relative file paths.
#+begin_src sh :noeval
touch src.fin
mkdir -p A
touch A/src.fin
mkdir -p B
touch B/src.fin
mkdir -p C/S
touch C/S/src.fin
#+end_src

So we have:
...

In ~./src.fin~, we can import ~module A;~, ~module B;~, ~module C.S;~.

*** Although we support modules, we have only one namespace.

We have modules, but since we are transpiling to C, we have only one namespace.

There are several solutions:
+ Providing unique names, but we do not want to break names defined by the user.
+ Reject Termina programs with at least two shared globals at different modules, even when they do not see each other.
+ Do nothing.

* Example

#+begin_src bash
stack run -- --print --output=examples/res.c examples/datadefs.fin
#+end_src

* Type Rules
https://hackmd.io/@parraman/BkFRR09Y2

* Meta TODOs!
** TODO Haddock include semantics package?
** TODO Typing rules .tex

* List of TODOs!
** DONE Create Project
** DONE First AST (Annotated AST)
Termina expressions are a (proper) subset of C, but it has 4 different top level constructs.
- Global variables?
- Rutines
- Functions
- I forgot the last one.
** DONE Simple Parser
** DONE Simple Pretty Printer
** TODO Simple C99 Printer
** DONE Ask Pablo: ClassMethod Ty Nothing?? Translates to void?
** DONE Ask Pablo: Variables types and scope.
Do we allow local shadowing? Local variables can have the same name as global ones shadowing them?

We do not allow shadowing.
** DONE [Q0] Type of constants. Literal numbers?
#+begin_src rust
let i : i32 = (45325245453252454532524545325245 : i32);
#+end_src

#+begin_src c
int i;
i = 45325245u;
#+end_src

#+begin_src haskell
AssignmentStmt (Variable "i")
  (Constant (I 45.245)) [] :: Statement a
#+end_src

Sol: Type annotations over constants
** TODO Constants are in their type? Wait until we have something better to check overflows and stuff?
** DONE Types of operations, multiplication division, it seems they are overloaded.
Overloaded.
** TODO [Q2] Type bitwiseshifts. Type dependent?
Isn't a shift bigger than the size of a variables undefined?
~(>>) : iY -> iX -> iY with {Y >= X}~?

Check Misra C + C std, etc. SAT, abstract interpretation, etc

** DONE [Q1] Type aliases are equivalent?
#+begin_quote
type A int
type B int

var a : A = 1
var b : B = 2

if (a == b)
then print "Capaz que sí"
else print "Capaz que no"
#+end_quote
Name equality.

However, Arrays are structural equivalent, no?

#+begin_src termina
type A Array i32 5

let a : A = [1,2,3,4,5]
let b : Array i32 5 = [1,2,3,4,5]

if a == b
then print("Capaz que sí")
else print("Capaz que no")
#+end_src

No, Name Equality, two types with different /names/ are different!
** TODO Add new Unbox (internal) operator
#+begin_src haskell
data CoreExpression a =
  ...
  | Unbox Identifier a
  ...

#+end_src
** TODO Functions cannot return Box
Returning boxes is not a good idea.

#+begin_quote
{
f:Box t -> Box t
x:Box t
y = f(x)
Free(y)
}

f(x) + 4  -- > Free(x)
z = g(x,y); free(z)
g(x,y) + 4 ;

__z = g(x,y), __z_v = *__z, free(__z);
E[__z_v + 4]

Unbox (F(x))
#+end_quote

** DONE FunctionCall shouldn't it be ~FunctionCall Ident [Expression a]~?

#+begin_src haskell
return $ \case {
                Identifier fs -> FunctionCall fs arguments);
                _ -> fail;
}
#+end_src

small example

#+begin_src termina
fn f(a : i32){return g(32)}
#+end_src

#+begin_src haskell
strParse "fn f(a : i32){return g((32::i32) + (45::i32));}"
#+end_src
Res
#+begin_src haskell
Right [Function "f" [Parameter {paramIdentifier = "a", paramTerminaType = Int32}] Nothing (BlockRet {blockBody = [], blockRet = (Just (FunctionCall "g" [BinOp Addition (Constant (I Int32 32)) (Constant (I Int32 45))]),[Position (line 1, column 15)])}) [Position (line 1, column 1)]]
#+end_src

** DONE [Q3] ~FieldValuesAssignmentsExpression fs? : ??~
Added type Type name.
Like in Rust,C, etc
#+begin_src rust
let t : FieldNameTy = FieldNameTy {fn1 : bla, ...}
#+end_src

** IDEA [Q4] Arrays -> Compiling time known length.
Type ~Array T E~, ~T~ is the type of elements, but what is ~E~? length?

~E~ should be an integer known at compiling time.

The idea of being known at compiler time is implemented using abstract interpretation.
I'll leave it for later, constant folding too.

** DONE [Q5] Patter Matching
*** DONE Compiler types with pattern matching: option.
*** Matching Case semantics.
As it is we have arbitrary expressions.

Classic PM, not almighty racket.

** DONE [Q6] Missing None and Some constructors?
** TODO C compiler flags
** TODO [Q7] String type? there is no string type, check it
** TODO [Q8] Check: x::NumTY, check \(x \in NumTy\)? \(x\) is constant.
** TODO [Q9] What's the type of an empty return? Unit? Unit is not part of our types.
I think we should added or we wont have procedures.
In that case we also should add value ~()~.
** DONE [Q11] Only correct breaks. Break no more.
Implement stack another check.

** TODO [Q10] Assignment expressions lhs is an expression?

#+begin_src haskell
data Statement a =
  ...
  | AssignmentStmt (Expression a) (Expression a) [ a ]
  ...
#+end_src

I assume this is a mistake, I'll fix it and ask later.
** DONE [Q12] Do we accept procedures?
#+begin_src c
void function_name() { return;}
#+end_src

Reads as
#+begin_src haskell
Function "function_name" [] Nothing ([], Ret ()) [] :: AnnASTElement ()
#+end_src

Returns Void, (C void)
** DONE [Q13] Why Static, Shared and Const have expressions?
Ask Pablo because I think he wanted to have something more concrete.
Statics should be a memory address?
#+begin_src  haskell
data Global a
  = Volatile Identifier (TerminaType a) Address [ a ]
  | Static Identifier (TerminaType a) (Maybe (Expression a)) [ a ]
    -- ---------------------------------^^^^^^^^^^^^^^^^^^^^^
  | Shared Identifier (TerminaType a)  (Maybe (Expression a)) [ a ]
    -- -------------------------------------^^^^^^^^^^^^^^^^^^^^^
  | Const Identifier (TerminaType a)  (Expression a) [ a ]
    -- ---------------------------------^^^^^^^^^^^^^^-------
  deriving (Show, Functor)
#+end_src

Default values for structure initialization.
Ground types do not have default values.

** DONE What are protected variables?
Classes a la C, functions API.
** DONE Following-up question from Q13: What to do when there is no expression?
Default values but only structures.
** TODO Constant Environment.
Read Only Environment
** TODO Implement Error Pretty Printer
** TODO Q14 Constant Checker?
If there is no function call, the expression should be constant, shouldn't it?
** TODO Shared can only have classes

Nothing | Just ( ... : Ty  ) <- Ty \in Class

#+begin_src haskell
...
  | Class Identifier [ClassMember a] [ a ]
#+end_src
#+begin_quote
Another definition of monitor is a thread-safe class, object, or module that
wraps around a mutex in order to safely allow access to a method or variable by
more than one thread.
#+end_quote

Members of ^^ Classes are ~shared~ objects.

*** TODO [Martin] (check Ocaml modules)
** TODO Q15 What are referenced expressions?
#+begin_src haskell
data Expression a = ...
  | ReferenceExpression (Expression a)
...
#+end_src

I checked the reference slides but I am a little bit confused.
** TODO Parser
** TODO Q16 Type of length expressions.
** TODO Q17 Can we assign stuff to arguments? Or are they Read-Only?
#+begin_src termina
bool func(x : u16) {
  var y : u16 = 5 : u16;
  x = 1;
  return (x + y);
}
#+end_src

#+begin_src termina
match(expr){
  case Some(x) => {
    x = x + 1; return(x);
  };
  case None => {return(42);}
}
#+end_src

Same goes for pattern matching bindings.

** TODO Q18 Types accepting default values.
Ground types do not have default values
Maybe that's enough.

** DONE Q19 We can only change local variable values.
Can we assign something to a global variable? I don't think so. See next Question
** DONE Q20 What globals can be assigned? LHS
** TODO Q21 Avoiding recursion through weird class methods.
The documentation (See hackmd) says that we can access members of classes without restriction.
However, we should be careful no to introduce recursion

#+begin_src
class bottom {
 fn f1(self){
   self.f2()
 }
 fn f2(self){
   self.f1()
 }
}
#+end_src

To solve this, I only permit syntactic order of use. Meaning, methods can only
use methods previously defined.
** TODO Q22 Can (IdentifierExpression) box self expressions exist?
We computing dependency between class methods we can find an object computing a
reference to self.
I don't think it should be possible, but I am not sure.
#+begin_src
 [[ IdentifierExpression e ann ]] \leadsto Variable "self" ann
#+end_src
** TODO Q23 Can we have box of box of box ??
** TODO Parser Addresses! (aside from numbers, 0x..., other stuff)
** TODO modifiers type check
** DONE type specifiers check
** TODO Mucho IMportant: Data.Map and stuff have undetectable errors!!
#+begin_quote
The size of a Map must not exceed maxBound::Int. Violation of this condition is not detected and if the size limit is exceeded, its behaviour is undefined.

The Map type is shared between the lazy and strict modules, meaning that the same Map value can be passed to functions in both modules. This means that the Functor, Traversable and Data instances are the same as for the Data.Map.Lazy module, so if they are used the resulting maps may contain suspended values (thunks).
#+end_quote

Same goes for Data.Set.
