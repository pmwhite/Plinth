The "stack_instrs" program reads a Plinth program from stdin, compiles it to a
sequence of instructions for an abstract stack machine, which it prints to
stdout.

To keep each test succinct, we make some helper functions.

  $ test() { echo $1 | $TEST_DIR/stack_instrs; }
  $ test_file() { $TEST_DIR/stack_instrs; }

Some random examples.

  $ test "let x : 10 = 12 x"
  load 12:10 into 0
