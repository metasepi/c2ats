#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"

staload "example.sats"

// Unsafe!
extern praxi __consume_view {from:view} (pf: from):<prf> void

implement main0 () = {
  val (pffoo | foo) = fun_c2ats_get_foo ()
  val () = fun_c2ats_free_foo (pffoo | foo)
  prval () = __consume_view pffoo
}
