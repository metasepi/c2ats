#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"

staload "out/c2ats_prelude.sats"

fun my_printf (s: string): void = {
  val p = string2ptr s
  val (pfat, fpfat | p) = $UN.ptr_vtake p
  val ret = fun_c2ats_printf (pfat | p)
  prval () = fpfat pfat
}

implement main0 () = {
  val s = "Hello, world!\n"
  val () = my_printf s
}
