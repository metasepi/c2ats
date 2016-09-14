#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"

staload "example.sats"

fun my_fopen (file: string, mode: string):
  [l:addr] (ptr_v_1(type_c2ats_FILE, l) | ptr l) = ret where {
  val pn = string2ptr file
  val (pfnat, fpfnat | pn) = $UN.ptr_vtake pn
  val pm = string2ptr mode
  val (pfmat, fpfmat | pm) = $UN.ptr_vtake pm

  val (pffp | fp) = fun_c2ats_fopen (pfnat, pfmat | pn, pm)

  prval () = fpfnat pfnat
  prval () = fpfmat pfmat

  val ret = (pffp | fp)
}

fun my_fread {l:addr}{n:int | n > 0}
  (pffp: !type_c2ats_FILE @ l | fp: ptr l, len: size_t (n)): (size_t, strptr) = ret where {
  implement{} string_tabulate$fopr (s) = '_'
  val buf = strnptr2strptr (string_tabulate len)
  val pbuf = strptr2ptr buf

  val r = fun_c2ats_fread (pffp | pbuf, 1UL, $UN.cast2ulint len, fp)
  val r = $UN.cast2size r

  val ret = (r, buf)
}

fun my_fclose {l:addr} (pffp: type_c2ats_FILE @ l | fp: ptr l): int = ret where {
  val ret = fun_c2ats_fclose (pffp | fp)

  val fp = __cast (pffp | fp) where {
    extern castfn __cast: {l1:addr} (ptr_v_1(type_c2ats_FILE, l1) | ptr l1) -> ()
  }
}

fun readshow {l:addr} (pffp: !type_c2ats_FILE @ l | fp: ptr l): void = {
  val (r, str) = my_fread (pffp | fp, i2sz(128))
  val () = print str
  val () = free str
  val () = if r > 0 then readshow (pffp | fp)
}

implement main0 () = {
  val (pffp | fp) = my_fopen ("main.dats", "r")
  val () = readshow (pffp | fp)
  val r = my_fclose (pffp | fp)
}
