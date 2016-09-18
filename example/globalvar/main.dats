#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"

staload "example.sats"

implement main0 () = {
  // g_foo
  val (pfg_foo | g_foo) = takeout_c2ats_g_foo
  val i = g_foo->i
  val s = $UN.cast{string}(g_foo->s)
  val () = println! ("g_foo = {", i, ", \"", s, "\"}")
  val () = assertloc (i = 1234)
  val () = assertloc (strcmp ("Hello, world!", s) = 0)
  prval () = addback_c2ats_g_foo (pfg_foo | g_foo)

  // g_int
  val (pfg_int | g_int) = takeout_c2ats_g_int
  val () = println! ("g_int = ", !g_int)
  val () = assertloc (!g_int = 9191)
  prval () = addback_c2ats_g_int (pfg_int | g_int)

  // g_str
  val (pfg_str | g_str) = takeout_c2ats_g_str
  prval ptr_v_2_cons (pfat1, pfat2) = pfg_str
  val str = $UN.cast{string}(!g_str)
  prval () = pfg_str := ptr_v_2_cons (pfat1, pfat2)
  val () = println! ("g_str = \"", str, "\"")
  val () = assertloc (strcmp ("Foo Bar", str) = 0)
  prval () = addback_c2ats_g_str (pfg_str | g_str)
}
