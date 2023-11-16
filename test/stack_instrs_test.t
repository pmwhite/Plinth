The "stack_instrs" program reads a Plinth program from stdin, compiles it to a
sequence of instructions for an abstract stack machine, which it prints to
stdout.

To keep each test succinct, we make some helper functions.

  $ test() { echo $1 | $TEST_DIR/stack_instrs; }
  $ test_file() { $TEST_DIR/stack_instrs; }

Some random examples.

A simple let expression.

  $ test "let x : 10 = 12 x"
  load 12:10 into 0

Comments work just like the expression they are attached to.

  $ test_file <<EOF
  > / this is a comment
  > let x : 2 = 12
  > x
  > EOF
  load 12:2 into 0

Functions get hoisted to the top-level, and then referred to there-after with
an integer.

  $ test "fn(x : 5) x"
  0 (arity 1)
    copy 0 to 0
  
  load_fn 0

