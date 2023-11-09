The "reformat" program reads a Plinth program from stdin and prints a formatted
version to stdout. It also checks that that formatting the same input twice
yields the same text as formatting it only once; this ensures a couple properties:

1. The parser and formatter roundtrip.
2. The formatter is stable.

This file specifies the Plinth syntax, but it does not specify the set of legal
Plinth programs. One useful difference between the two sets is that one can
reformat source code even if it contains, for example, type errors.

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

MATCH EXPRESSIONS

Match expressions enable conditional control flow. They consist of an
expression to be inspected and a list of cases. Each case is a pair of an
integer and an expression to be run if that matched value equals the integer.

  $ test "match x { 1 -> y 2 -> z }"
  match x {
    1 -> y
    2 -> z
  }

A match expression is allowed to not have any cases.

  $ test "match a {}"
  match a {}

The matched expression can be anything.

  $ test "match let x = y z { 1 -> x }"
  match
    let x = y
    z
  {
    1 -> x
  }

The body of each case can be a let expression.

  $ test "match x { 1 -> let x = y z }"
  match x {
    1 ->
      let x = y
      z
  }

The body of a match expression can be another match expression.

  $ test_file <<EOF
  > match a
  > { 1 ->
  >   match b {  2 -> c }
  > 3 -> d }
  > EOF
  match a {
    1 ->
      match b {
        2 -> c
      }
    3 -> d
  }

The expression being matched can be another match expression. It is not
formatted on its own lines unless it is itself a multiline expression.

  $ test "match match match a {} {} {}"
  match match match a {} {} {}

  $ test "match match match a { 0 -> 1 } {} {}"
  match
    match
      match a {
        0 -> 1
      }
    {}
  {}

  $ test "match match match a {} {} { 0 -> 1 }"
  match match match a {} {} {
    0 -> 1
  }

FUNCTIONS

Functions accept a bunch of argument identifiers and return a result
expression.

  $ test "fn(a,b,c)a"
  fn(a, b, c) a

  $ test "fn(a ) x"
  fn(a) x

If the body of a function is single line, then the whole expression is
a single line.

  $ test "fn ( a, b , c) match x { }"
  fn(a, b, c) match x {}

Functions can have no arguments.

  $ test "fn () c"
  fn() c

Functions can return other functions.

  $ test "fn() fn() fn() c"
  fn() fn() fn() c

Using multiline expressions inside a function body makes it multiline.

  $ test "fn() let a = fn() x fn() c"
  fn()
    let a = fn() x
    fn() c

  $ test "fn() match x { 1 -> 2 }"
  fn()
    match x {
      1 -> 2
    }

CALLS

Identifiers can be called with argument expressions. It is expected, of course,
that the identifier is bound to a function.

  $ test "f(a, b, c)"
  f(a, b, c)

Function calls can be nested.

  $ test "x(g(a,b,c), d, e(f, g, h))"
  x(g(a, b, c), d, e(f, g, h))

Arguments can be arbitrary expressions. If they are multiline expressions, then
the whole call gets made multiline.

  $ test "function(let a = junction(let a = a a, gumption(a, b, c)) a)"
  function(
    let a =
      junction(
        let a = a
        a,
        gumption(a, b, c))
    a)

But if the argument is a complex expression that is on a single line, then the
call will remain single line.

  $ test "a(match x {})"
  a(match x {})

Calls can be bound to names.

  $ test "let a = f(a, b, c) x"
  let a = f(a, b, c)
  x

  $ test "let a = f(let a = a a, b, c) x"
  let a =
    f(
      let a = a
      a,
      b,
      c)
  x

Calls can be matched on.

  $ test "match f(a, b, c) {}"
  match f(a, b, c) {}

A big nested call expression, with a multiline expression on the inside.

  $ test "a(b(c(d(f(g(h(i(let a = a a))))))))"
  a(
    b(
      c(
        d(
          f(
            g(
              h(
                i(
                  let a = a
                  a))))))))
