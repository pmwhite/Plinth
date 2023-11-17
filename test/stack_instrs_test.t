The "stack_instrs" program reads a Plinth program from stdin, compiles it to a
sequence of instructions for an abstract stack machine, which it prints to
stdout.

To keep each test succinct, we make some helper functions.

  $ test() { echo $1 | $TEST_DIR/stack_instrs; }
  $ test_file() { $TEST_DIR/stack_instrs; }

Some random examples.

A simple let expression.

  $ test "let x : 10 = 12 x"
  push_fn 12:10

Comments work just like the expression they are attached to.

  $ test_file <<EOF
  > / this is a comment
  > let x : 2 = 12
  > x
  > EOF
  push_fn 12:2

Functions get hoisted to the top-level, and then referred to there-after with
an integer.

  $ test "fn(x : 5) x"
  0 (arity 1)
    dup 0
  
  push_fn 0

