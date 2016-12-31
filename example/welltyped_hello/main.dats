#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

staload "example_welltyped.sats"

implement main0 () = {
  val s = "Hello, world!\n"
  val _ = fun_c2ats_printf(s)
}
