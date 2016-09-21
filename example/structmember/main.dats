#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"

staload "example.sats"

// Unsafe!
extern praxi __consume_view {from:view} (pf: from):<prf> void

extern castfn charptr2string: {l1:agz} (!ptr_v_1(char, l1) | ptr l1) -> string

fun print_foos {l1:agz} (pffoos: !ptr_v_1(char, l1) | foos: ptr l1): void = {
  val s = charptr2string (pffoos | foos)
  val () = println! ("foo->s = ", s)
  val () = assertloc (strcmp ("FooBar", s) = 0)
}

fun print_foo {l1:agz} (pffoo: !ptr_v_1(struct_c2ats_foo, l1) | foo: ptr l1): void = {
  val () = println! ("foo->i = ", foo->i)
  val () = assertloc (foo->i = 10)
  val (pffoos, fpffoos | foos) = take_struct_c2ats_foo_s (pffoo | foo)
  val () = if foos > 0 then print_foos (pffoos | foos) else undefined ()
  prval () = fpffoos pffoos
}

fun print_bar {l1:agz} (pfbar: !ptr_v_1(struct_c2ats_bar, l1) | bar: ptr l1): void = {
  val (pffoo, fpffoo | foo) = take_struct_c2ats_bar_f (pfbar | bar)
  val () = if foo > 0 then print_foo (pffoo | foo) else undefined ()
  prval () = fpffoo pffoo
}

implement main0 () = {
  val (pfbar | bar) = fun_c2ats_alloc_bar ()
  val () = if bar > 0 then print_bar (pfbar | bar) else undefined ()
  val () = fun_c2ats_free_bar (pfbar | bar)
  prval () = __consume_view pfbar
}
