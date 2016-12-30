#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"

staload "example_welltyped.sats"

fun my_fread {l:addr}{n:int | n > 0}
  (pffp: !type_c2ats_FILE @ l | fp: ptr l, len: size_t (n)): (size_t, strptr) = ret where {
  implement{} string_tabulate$fopr (s) = '_'
  val buf = strnptr2strptr (string_tabulate len)
  val pbuf = strptr2ptr buf

  val r = fun_c2ats_fread (pffp | pbuf, 1UL, $UN.cast2ulint len, fp)
  val pbufr = add_ptr_bsz (pbuf, r)
  val () = if r < len then $UN.ptr0_set<char>(pbufr, '\0')

  val ret = (r, buf)
}

fun readshow {l:addr} (pffp: !type_c2ats_FILE @ l | fp: ptr l): void = {
  val (r, str) = my_fread (pffp | fp, i2sz(128))
  val () = print str
  val () = free str
  val () = if r > 0 then readshow (pffp | fp)
}

implement main0 () = {
  val (pffp | fp) = fun_c2ats_fopen ("main.dats", "r")
  val () = readshow (pffp | fp)
  val r = fun_c2ats_fclose (pffp | fp)
}
