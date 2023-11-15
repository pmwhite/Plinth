The "type_of" program reads a Plinth program from stdin and prints its type to
stdout. However, the tests in this file are aimed at demonstrating how Plinth
name scoping works.

To keep each test succinct, we make some helper functions.

  $ test() { echo $1 | $TEST_DIR/type_of; }
  $ test_file() { $TEST_DIR/type_of; }

CLOSURES AND NESTED FUNCTIONS

The main complication of Plinth's scoping rules is that nested functions are
allowed, but closures are not. The consequence of this is that local variables
defined outside a function are not accessible within a function, unless that
local variable is bound immediately to a function.

Referring to local integer variable is not allowed.

  $ test "let x : 8 = 5 fn() x"
  Unbound name x

But referring to a local function variable is allowed.

  $ test_file <<EOF
  > let f : fn() 8 = fn() 8
  > fn() f()
  > EOF
  fn() 8

Why are local functions allowed? Because we can hoist them to global scope,
which means they can be accessed from anywhere - there is no need for a closure
environment.

A crucial point is that when some extra logic is used to compute which function
to use, then the compiler will complain. In the example below, the compiler
refuses to inspect the "match" expression in order to find out which function
"g" is bound to.

  $ test_file <<EOF
  > let f : fn() 8 = fn() 8
  > let x : 0 = 0
  > let g = match x { 0 -> f }
  > fn() g()
  > EOF
  Unbound name g

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

All names go in the main environment, but names bound to functions directly are
also put in a backup environment, so it is worth taking special care that
function bindings and data bindings interact correctly.

Data bindings can shadow function bindings.

  $ test_file <<EOF
  > let a : fn() 0 = fn() 0
  > let a : 1 = 0
  > a
  > EOF
  1

But inside a function, the shadow can be temporarily lifted.

  $ test_file <<EOF
  > let a : fn() 0 = fn() 0
  > let a : 1 = 0
  > fn() a
  > EOF
  fn() fn() 0
