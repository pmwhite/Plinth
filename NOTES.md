I'm deferring the work on instructions while I work on weakening what functions can actually do. Here's some code that I might want again:

```
[@@@warning "-37-32-69-34"]

(* The stack machine maintains a "frame pointer" that all locations are
   interpreted relative to. Calling a function causes the stack pointer to jump
   forward by the frame size of that function. At any point in time, data
   before the frame pointer comprises function arguments and local variables.
   Arguments are passed to a function by copying them in order to locations
   after the frame pointer. The return instruction causes the frame pointer to
   return to its location before the current function was called, and also
   writes the specified data into the location specified by the callsite.

   The "call" instruction specifies the place on the stack where the function's
   address is stored; all calls are indirect (at least at this level of
   compilation).
*)
type case =
  { data : string
  ; code : int
  }

(* The instruction set for a stack machine. The goal of this instruction set is
   to be easily translatable into both high-level JavaScript and a low-level
   assembly language. It is obvious that this representation brings us closer
   to generating assembly, but it is worth justifying why it is also a good
   representation for generating JavaScript. Each item on the stack represents
   a program variable; that is, we treat the stack as an array of items, not
   just an array of raw bits. Thus, even though we present this instruction set
   in low-level terms, it should still be easy enough to re-construct a
   high-level program.

   One important aspect of the instruction set is that we should be able to
   tell the type of every position on the stack.
*)
type instr =
  | Load of
      { size : int
      ; data : string
      ; dest : int
      }
  | Copy of
      { src : int
      ; dest : int
      }
  | Call of
      { code : int
      ; dest : int
      }
  | Return of
      { src : int
      ; size : int
      }
  | Jump of
      { src : int
      ; dest : int
      ; cases : case list
      }
  | Debug of
      { src : int
      ; size : int
      }

type instrs = { mutable instrs : instr list }

let instrs_add instrs instr = instrs.instrs <- instr :: instrs.instrs

type dest =
  | Return
  | Stack of int
```
