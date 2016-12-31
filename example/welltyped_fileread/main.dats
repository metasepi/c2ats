#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"
staload STRING = "libats/libc/SATS/string.sats"

staload "example_welltyped.sats"

fun my_fread {l:addr}{n:nat}
  (fp: !FILEref2, len: size_t (n)):
  [m:nat | m <= n] (size_t (m), strnptr (m)) = ret where {
  implement{} string_tabulate$fopr (s) = '_'
  val buf_strnptr = string_tabulate len
  val buf_ptr     = strnptr2ptr buf_strnptr
  val _ = $STRING.memset_unsafe (buf_ptr, 0, len)

  val r = fun_c2ats_fread (buf_strnptr, i2sz(1), len, fp)
  val ret = (r, buf_strnptr)
}

fun readshow {l:addr} (fp: !FILEref2): void = {
  val (r, str) = my_fread (fp, i2sz(128))
  val str = strnptr2strptr str
  val () = print str
  val () = free str
  val () = if r > 0 then readshow fp
}

implement main0 () = {
  val fp = fun_c2ats_fopen ("main.dats", "r")
  val () = readshow fp
  val r = fun_c2ats_fclose fp
}
