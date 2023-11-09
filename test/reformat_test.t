The "reformat" program reads a Plinth program from stdin and prints a formatted
version to stdout. It also checks that that formatting the same input twice
yields the same text as formatting it only once; this ensures a couple properties:

1. The parser and formatter roundtrip.
2. The formatter is stable.

To keep each test succinct, we make some helper functions.

  $ test() { echo $1 | $TEST_DIR/reformat; }
  $ test_file() { $TEST_DIR/reformat; }

SYMBOLS

Symbols can contain lowercase letters.

  $ test "hello"
  hello

Symbols can contain underscores.

  $ test "abc_def"
  abc_def

Symbols can contain digits.

  $ test "a100"
  a100

INTEGERS

A series of digits constitutes and integer literal.

  $ test "100"
  100

An integer literal may also begin with a sign character.

  $ test "+12345"
  +12345
  $ test "-12345"
  -12345

COMMENTS

A comment expression begins with a single forward slash, ends with a newline,
and must be followed immediately by another expression.

  $ test_file <<EOF
  > / This is a comment
  > let a = 123 c
  > EOF
  / This is a comment
  let a = 123
  c

A consequence of comment syntax works is that many comment expressions can be
sequenced, yielding what looks like a single comment block.

  $ test_file <<EOF
  > / Comment 1
  > / Comment 2
  > 1
  > EOF
  / Comment 1
  / Comment 2
  1

Comments can be used almost anywhere inside an expression.

  $ test_file <<EOF
  > / Comment 1
  > let a = let 
  > b = 
  > let 
  > c 
  > = 
  > d 
  > / Comment 2
  > e let g = h i j
  > EOF
  / Comment 1
  let a =
    let b =
      let c = d
      / Comment 2
      e
    let g = h
    i
  j

  $ test_file <<EOF
  > let a = 123 
  > let b = 
  > / Comment 1
  >   / Comment 2
  > x let e = y c
  > EOF
  let a = 123
  let b =
    / Comment 1
    / Comment 2
    x
  let e = y
  c

LET EXPRESSIONS

A let expression binds an expression to a name, which is then in scope for
another expression that must follow the binding.

  $ test "let a = 123 c"
  let a = 123
  c

Let expressions can be sequenced.

  $ test "let a = 123 let b = x let e = y c"
  let a = 123
  let b = x
  let e = y
  c

The expression being bound can also be a let-expression.

  $ test "let a = let b = let c = d e let g = h i j"
  let a =
    let b =
      let c = d
      e
    let g = h
    i
  j

The name in a let expression may be given a type annotation.

  $ test "let a : 123 = 5 a"
  let a : 123 = 5
  a

REC EXPRESSIONS

A rec expression is a lot like let expression.

It can bind expressions to names.

  $ test "rec a = 123 a"
  rec a = 123
  a

It can be nested and sequenced arbitrarily.

  $ test "rec a = 123 rec b = x rec e = y c"
  rec a = 123
  rec b = x
  rec e = y
  c

  $ test "rec a = rec b = rec c = 1 c b a"
  rec a =
    rec b =
      rec c = 1
      c
    b
  a

But it can also bind more than one name at once. This is different from using
multiple let bindings because the the expressions being bound can refer to the
names to which they are being bound, thus enabling ordinary recursion and
mutual recursion.

  $ test "rec a = b & b = a a"
  rec a = b
  & b = a
  a

Nesting and type annotation can happen anywhere in the rec expression.

  $ test "rec a = rec b:10 = rec c = d e & g = h i & x:10 = 10 j"
  rec a =
    rec b : 10 =
      rec c = d
      e
    & g = h
    i
  & x : 10 = 10
  j
