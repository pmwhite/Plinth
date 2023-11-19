The "type_of" program reads a Plinth program from stdin and prints its type to
stdout. However, the tests in this file are aimed at demonstrating how Plinth
name scoping works.

To keep each test succinct, we make some helper functions.

  $ test() { echo $1 | $TEST_DIR/type_of; }
  $ test_file() { $TEST_DIR/type_of; }

CLOSURES AND NESTED FUNCTIONS

The main complication of Plinth's scoping rules is that nested functions are
allowed, but closures are not. The consequence of this is that local variables
defined outside a function are not accessible within a function, unless the
value bound to that variable can be hoisted to the global scope. Whether an
expression can be hoisted is determined by a simple syntactic check of whether
the expression is a function or an integer.

Local variables bound in the top scope may be accessed within functions.

  $ test_file <<EOF
  > let a : 1 = 0
  > let x : 8 = match a { 0 -> 5 }
  > fn() x
  > EOF
  fn() 8

But when those variables are themselves defined in a function, they are
inaccessible.

  $ test_file <<EOF
  > fn()
  >   let a : 1 = 0
  >   let x : 8 = match a { 0 -> 5 }
  >   fn() x
  > EOF
  'x' is bound outside the current function and cannot be hoisted to the top scope.

  $ test_file <<EOF
  > let f : fn() 8 = fn() 8
  > let x : 0 = 0
  > let g = match x { 0 -> f }
  > fn() g()
  > EOF
  fn() 8

As already mentioned, the exceptions to this rule are function and integer
expressions. What made the previous example illegal was that "x" was bound to a
match expression.

  $ test_file <<EOF
  > fn()
  >   let x : 8 = 5
  >   fn() x
  > EOF
  fn() fn() 8

  $ test_file <<EOF
  > fn()
  >   let f : fn() 8 = fn() 5
  >   fn() f
  > EOF
  fn() fn() fn() 8

Subject to the caveat about closures being disallowed, let-expressions behave
as you would expect: when you bind an expression to a name, that name is in
scope for the body of the expression.

  $ test_file <<EOF
  > let a : 0 = 0
  > let b = 0
  > let c = 0
  > let d = 0
  > let e = 0
  > let f = 0
  > a
  > EOF
  0

If the name being bound is already in scope, the new binding shadows the one
that already exists.

  $ test_file <<EOF
  > let a : 0 = 0
  > let a : 1 = 0
  > a
  > EOF
  1

Once a name that shadows another goes out of scope, the other being comes back
to the foreground again.

  $ test_file <<EOF
  > let a : 0 = 0
  > let b : 1 =
  >   let a : 1 = 0
  >   a
  > a
  > EOF
  0

Shadowing a name does not have any affect on the usages of the previous binding.

  $ test_file <<EOF
  > let a : 0 = 0
  > let b = a
  > let a : 1 = 0
  > b
  > EOF
  0

Function parameters are of course in scope for the body of the function.

  $ test "fn(a : 0) a"
  fn(0) 0
