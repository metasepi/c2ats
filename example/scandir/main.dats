#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"

staload "example.sats"

fun my_scandir (dirp: string): void = {
  val pdp = string2ptr dirp
  val (pfdat, fpfdat | pdp) = $UN.ptr_vtake pdp
  var pde: ptr
  val (pfeat, fpfeat | ppde) = __take addr@pde where {
    extern castfn __take: {l1:addr} (ptr l1) -> [l2,l3:addr] (ptr_v_3(struct_c2ats_dirent, l1, l2, l3), ptr_v_3(struct_c2ats_dirent, l1, l2, l3) -<lin,prf> void | ptr l1)
  }

  val n = fun_c2ats_scandir (pfdat, pfeat | pdp, ppde, $UN.cast the_null_ptr, fun_c2ats_alphasort)

  prval () = fpfeat pfeat
  prval () = fpfdat pfdat

  // xxx TODO
}

implement main0 () = {
  val () = my_scandir "/var"
}
