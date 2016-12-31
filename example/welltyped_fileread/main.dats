#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

staload STRING = "libats/libc/SATS/string.sats"

staload "example_welltyped.sats"

fun my_fopen (file: string, mode: string):
    [l:agz] (type_c2ats_FILE@l | ptr(l)) = ret where {
  val (pffp | fp) = fun_c2ats_fopen(file, mode)
  val () = assertloc(fp > 0)
  val ret = (pffp | fp)
}

fun my_fread {l:agz}{n:nat}
    (pffp: !type_c2ats_FILE@l | fp: ptr(l), len: size_t(n)):
    [m:nat | m <= n] (size_t(m), strnptr(m)) = ret where {
  implement{} string_tabulate$fopr(s) = '_'
  val buf_strnptr = string_tabulate(len)
  val buf_ptr = strnptr2ptr(buf_strnptr)
  val _ = $STRING.memset_unsafe(buf_ptr, 0, len)

  val r = fun_c2ats_fread(pffp | buf_strnptr, i2sz(1), len, fp)
  val ret = (r, buf_strnptr)
}

fun readshow {l:agz} (pffp: !type_c2ats_FILE@l | fp: ptr(l)): void = {
  val (r, str) = my_fread(pffp | fp, i2sz(128))
  val str = strnptr2strptr(str)
  val () = print(str)
  val () = free(str)
  val () = if r > 0 then readshow(pffp | fp)
}

implement main0 () = {
  val (pffp | fp) = my_fopen("main.dats", "r")
  val () = readshow(pffp | fp)
  val r = fun_c2ats_fclose(pffp | fp)
}
