# Plinth

I want this language to be fun to use, fun to implement, and useful for
building real software. Toward that end, the language should be minimal, the
compiler design should be modular, and there should be native, JavaScript, and
WebAssembly backends. To keep the runtime easy to understand, there should be
no need for a GC.

## Level 1

The first level of the language provides everything needed to write working
programs. While it does not provide many facilities for abstraction or re-use,
it includes features that are simple to implement and make programming more
pleasant. Notably absent is a type system for structuring data; while functions
and data are distinguished, nothing higher-level is provided.

### Data

The main kind of data in the language is the bitvector. There is no dedicated
facility in the language for adding structure to these bits. It is up to the
programmer to remember when a value is meant to be a pointer, an index into
an array, or some calculation meaningful to the application.

### Expressions

Computation is described through expressions, which comprise variables,
function calls, functions expressions, and let-expressions (an expression bound
to a name paired with another expression in which that name is in scope).

### Functions

The second kind of data is the function. Functions receive zero to many
parameters, possibly perform side-effects, and return a single result.
Functions are themselves expressions, so they can be defined almost anywhere:
at the top level scope, bound to names inside other functions, and even passed
directly as parameters.

Although the language does not support function closures, functions defined
within other functions are still allowed to refer to variables in the outer
scope, since it is assumed that the inner function's stack frame is only active
as long as the outer function's stack frame. To uphold this assumption, the
language prohibits returning a function pointer from the function where it is
defined.

### Types

Every expression has a corresponding "type expression", which is either bits
(with a specific length) or a function type (whose parameter and return value
have their own type expressions). The compiler enforces that functions are
always called with the right number of parameters, and that each parameter is
the right kind of value.

### Control flow

A match expression scrutinizes a value and picks the appropriate execution
path out of a collection of options.

There are no built-in loop constructs are not built-in; iteration must be
accomplished through recursion.

### Namespacing

All values live in the same global namespace, and shadowing is disallowed.
Mutual recursion is legal, but since names must be declared before they are
used, it requires using forward declarations in order for later functions to
refer to earlier ones.
