#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"

staload "example.sats"

// Unsafe!
extern praxi __consume_view {from:view} (pf: from):<prf> void

implement main0 () = {
  val (pfbar | bar) = fun_c2ats_alloc_bar ()
  val () = fun_c2ats_free_bar (pfbar | bar)
  prval () = __consume_view pfbar
}
