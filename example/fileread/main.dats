#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats" // Unsafe!
staload STRING = "libats/libc/SATS/string.sats"

staload "example.sats"

extern praxi __create_view {to:view} ():<prf> to // Unsafe!
extern praxi __consume_view {from:view} (pf: from):<prf> void // Unsafe!

fun my_fopen (file: string, mode: string):
    [l:addr] (type_c2ats_FILE@l | ptr(l)) = ret where {
  val pn = string2ptr(file)
  val (pfnat, fpfnat | pn) = $UN.ptr_vtake(pn)
  val pm = string2ptr(mode)
  val (pfmat, fpfmat | pm) = $UN.ptr_vtake(pm)

  val (pffp | fp) = fun_c2ats_fopen(pfnat, pfmat | pn, pm)

  prval () = fpfnat(pfnat)
  prval () = fpfmat(pfmat)
  val ret = (pffp | fp)
}

fun my_fread {l:addr}{n:nat}
    (pffp: !type_c2ats_FILE@l | fp: ptr(l), len: size_t(n)):
    [m:int] (size_t(m), strnptr(m)) = ret where {
  implement{} string_tabulate$fopr(s) = '_'
  val buf_strptr = strnptr2strptr(string_tabulate(len))
  val buf_ptr = strptr2ptr(buf_strptr)
  val _ = $STRING.memset_unsafe(buf_ptr, 0, len)

  val r = fun_c2ats_fread(pffp | buf_ptr, 1UL, $UN.cast2ulint(len), fp)
  val r = $UN.cast(r)
  val buf_strnptr = strptr2strnptr(buf_strptr)
  val ret = (r, buf_strnptr)
}

fun my_fclose {l:addr} (pffp: type_c2ats_FILE@l | fp: ptr(l)): int
    = ret where {
  val ret = fun_c2ats_fclose(pffp | fp)
  prval () = __consume_view(pffp)
}

fun readshow {l:addr} (pffp: !type_c2ats_FILE@l | fp: ptr(l)): void = {
  val (r, str) = my_fread(pffp | fp, i2sz(128))
  val str = strnptr2strptr(str)
  val () = print(str)
  val () = free(str)
  val () = if r > 0 then readshow(pffp | fp)
}

implement main0 () = {
  val (pffp | fp) = my_fopen("main.dats", "r")
  val () = readshow(pffp | fp)
  val r = my_fclose(pffp | fp)
}
