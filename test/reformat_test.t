Tests that reformatting code works properly

  $ echo "let x = let z = 4 z let y = 5 x" | $TEST_DIR/reformat
  let x =
    let z = 4
    z
  let y = 5
  x
