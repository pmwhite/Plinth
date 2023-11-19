The "stack_instrs" program reads a Plinth program from stdin, compiles it to a
sequence of instructions for an abstract stack machine, which it prints to
stdout.

To keep each test succinct, we make some helper functions.

  $ test() { echo $1 | $TEST_DIR/stack_instrs; }
  $ test_file() { $TEST_DIR/stack_instrs; }

Some random examples.

A simple let expression.

  $ test "let x : 10 = 12 x"
  push 12:10

Comments work just like the expression they are attached to.

  $ test_file <<EOF
  > / this is a comment
  > let x : 2 = 12
  > x
  > EOF
  push 12:2

Functions get hoisted to the top-level, and then referred to there-after with
an integer.

  $ test "fn(x : 5) x"
  0 (arity 1)
    dup 0
  
  fn 0

To call a function, we push its arguments onto the stack in reverse, order,
followed by the function being called, and then invoke the function.

  $ test_file <<EOF
  > let f = fn(x:1) x
  > let g = fn(x:1) x
  > let h = fn(x:1, y:1) x
  > f(g(h(1,h(f(1), f(1)))))
  > EOF
  0 (arity 1)
    dup 0
  
  1 (arity 2)
    dup 0
  
  2 (arity 1)
    dup 0
  
  push 1:1
  fn 0
  call
  push 1:1
  fn 0
  call
  fn 1
  call
  push 1:1
  fn 1
  call
  fn 2
  call
  fn 0
  call

If we use a variable more than once, then it is saved on the stack so that it
doesn't need to be recomputed.

  $ test_file <<EOF
  > let f = fn(x:1) x
  > let g = fn(x:1, y:1) x
  > let x = f(f(1))
  > g(x, x)
  > EOF
  0 (arity 1)
    dup 0
  
  1 (arity 2)
    dup 0
  
  push 1:1
  fn 0
  call
  fn 0
  call
  save 0
  dup 0
  fn 1
  call
