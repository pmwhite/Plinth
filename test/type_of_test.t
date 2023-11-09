The "type_of" program reads a Plinth program from stdin and prints its type to
stdout. Although the tests are mainly aimed at computing the correct type, they
do test a few pre-requisite program properties. For example, the source text
must be syntactically correct, and the names must be in scope in order to be
used.

To keep each test succinct, we make some helper functions.

  $ test() { echo $1 | $TEST_DIR/type_of; }
  $ test_file() { $TEST_DIR/type_of; }

INTEGERS

By itself, it is impossible to tell the type of an integer literal because it
could stand for a value of any number of bits.

  $ test "1"
  Could not infer type of '1' due to lack of information

But if enough information flows in from a type annotation, then literals type
can be inferred.

  $ test "let x : 0 = 0 x"
  0

LET EXPRESSIONS

Let expressions bind a name to an expression. If there is a type annotation,
the type is used to infer the type of the expression immediately.

  $ test "let x : 0 = 0 x"
  0

However, if there is no type annotation, then analyzing expression is deferred
until the name is actually used. This has a couple interesting consequences:

First, if a name is never used, then it will never be analyzed. In the example,
type of "x" is ambiguous because it could be any number of bits, but the
compiler doesnot complain.

  $ test "let x = 0 let y : 0 = 0 y"
  0

Second, if a name initially is ambiguous but later there is enough information
to infer the type, the compiler will happily oblige. The example below is
especially interesting; the compiler finds the type of "a" from the annotation
on "d". Even though "d" is not used, it gets analyzed because it has a type
annotation, and the fact that "d" is analyzed is enough to inform the compiler
about the type of "a".

  $ test "let a = 0 let b = a let c = b let d : 10 = c a"
  10

We've just seen that type information flows backwards along let-sequences. It
also goes forward.

  $ test "let a : 314 = 0 let b = a let c = b let d = c d"
  314

Type information also flows backward and forward through nested along the bound
expression axis.

  $ test "let a = let b = let c = let d : 1 = 0 d c b a"
  1

  $ test "let a : 2 = let b = let c = let d = 0 d c b a"
  2

COMMENTS

Comment type checking is not very interesting at all. The whole comment
expression always behaves the same as the expression immediately following the
comment.

  $ test_file <<EOF
  > / Comment
  > let x : 0 = 0 x
  > EOF
  0

FUNCTIONS AND CALLS

Type information both directions between arguments and results of function
expressions.

  $ test "fn(a:1) a"
  fn(1) 1

  $ test "fn(a) let x : 111 = a x"
  fn(111) 111

Type information can also come from outside the function itself, in a couple
ways. It's possible that the whole function type will flow into the
expression from, for instance, an annotation on a name to which the function is
bound.

  $ test "let f : fn(1) 1 = fn(x) x f"
  fn(1) 1

It's also possible that the return type of a function can flow in from one of
its usages. The "1" passed to "f" is ambiguous by itself, but because "a" was
given a type, that information is able to flow through "f" into its arguments.

  $ test "let f = fn(x) x let a : 1 = f(1) f"
  fn(1) 1

We can this even further. If a function returns another function, and we learn
about the return type of that function, then we also learn something about the
return type of the original function.

  $ test_file <<EOF
  > let id = fn() fn(x) x
  > let id2 = id()
  > let y : 1 = id2(10)
  > y
  > EOF
  1

How information flows through function calls is tricky. Both the arguments and
the function itself may have useful information for the other, but the
compiler doesn't know which direction is more useful up front. Rather than
trying to figure out the more useful direction, the compiler always chooses to
analyze the function first. Thus, information flows from the function
definition into the arguments at each of the use-sites; functions cannot learn
anything from the arguments they are called. However, as was demonstrated
above, they can still learn about their return type at a call site, which may
indirectly help determine their argument types.

Higher-order functions are allowed, and type information flows through just as
with any other function. Since ordinary functions are treat as expressions that
can be bound to names, the fact that a function comes in as an argument is not
too difficult of a leap.

  $ test_file <<EOF
  > let id = fn(x : 1) x 
  > let apply = fn(f:fn(1)1, x:1) f(x) 
  > let result = apply(id, 10) 
  > result
  > EOF
  1

MATCH EXPRESSIONS

Match expressions do not have a mechanism type annotation, so type information
must flow from the outside. The expression being matched must be some bits type
(that is, its type has to be a number, rather than a pointer or function type)

Information can flow from the outside of the expression.

  $ test_file <<EOF
  > let x : 1 = 1
  > let result : 1 =
  >   match x {
  >     0 -> 1
  >   }
  > result
  > EOF
  1

Or it can come from inside the expression.

  $ test_file <<EOF
  > let f = fn(a) a
  > let x : 1 = 1
  > let result : 1 =
  >   match f(x) {
  >     0 -> f(x)
  >   }
  > result
  > EOF
  1

REC EXPRESSIONS

Rec expressions type-check similarly to let expressions, but care has to be
taken such that the lazy analysis of expressions does not lead to an infinite
loop. That is, in the following expression,

  $ test_file <<EOF
  > rec x = y
  > & y = x
  > let a : 10 = x
  > a
  > EOF
  10

the analysis begins at "a"'s type annotation and flows backward into "x" and
then into "y". The compiler is careful to not flow from "y" into "x" again,
since "x" has already been visited.

Rec expressions can get information both from outside and from inside their
expression.

  $ test_file <<EOF
  > rec x = y
  > & y : 4 = x
  > x
  > EOF
  4

  $ test_file <<EOF
  > rec x : fn(11) 11 = fn(a : 11) y(a)
  > & y = fn(b : 11) x(b)
  > y(11)
  > EOF
  11

  $ test_file <<EOF
  > rec x = fn(a : 11) y(a)
  > & y : fn(11) 11 = fn(b : 11) x(b)
  > y(11)
  > EOF
  11
