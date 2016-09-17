(* https://developer.gnome.org/gtk3/stable/gtk-getting-started.html *)
#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"

staload "example.sats"

// Unsafe!
extern praxi __create_view {to:view} ():<prf> to
extern praxi __keep_castview {to:view}{from:view} (pf: !from):<prf> to
extern praxi __consume_view {from:view} (pf: from):<prf> void

fun{} take_gcharp2tr (s: string):
  [l:addr] (type_c2ats_gchar@l, type_c2ats_gchar@l -<lin,prf> void | ptr l) = ret where {
  val p = string2ptr s
  val ret = $UN.ptr_vtake p
}

fun activate {l:addr} (pfapp: !type_c2ats_GtkApplication@l | app: ptr l,
  user_data: type_c2ats_gpointer): void = {
  // window = gtk_application_window_new (app);
  val (pfwindow | window) = fun_c2ats_gtk_application_window_new (pfapp | app)

  // gtk_window_set_title (GTK_WINDOW (window), "Window");
  prval pfgtkwin = __keep_castview pfwindow
  val (pfgchar, fpfgchar | pgchar) = take_gcharp2tr "Window"
  val () = fun_c2ats_gtk_window_set_title (pfgtkwin, pfgchar | $UN.cast window, pgchar)
  prval () = fpfgchar pfgchar

  // gtk_window_set_default_size (GTK_WINDOW (window), 200, 200);
  val () = fun_c2ats_gtk_window_set_default_size (pfgtkwin | $UN.cast window, 200, 200)
  prval () = __consume_view pfgtkwin

  // gtk_widget_show_all (window);
  val () = fun_c2ats_gtk_widget_show_all (pfwindow | window)
  prval () = __consume_view pfwindow
}

implement main0 () = {
  // app = gtk_application_new ("org.gtk.example", G_APPLICATION_FLAGS_NONE);
  val (pfgchar, fpfgchar | pgchar) = take_gcharp2tr "org.gtk.example"
  val (pfapp | app) =
    fun_c2ats_gtk_application_new (pfgchar | pgchar, enum_c2ats_G_APPLICATION_FLAGS_NONE)
  prval () = fpfgchar pfgchar

  // g_signal_connect (app, "activate", G_CALLBACK (activate), NULL);
  val (pfgchar, fpfgchar | pgchar) = take_gcharp2tr "activate"
  val _ = fun_c2ats_g_signal_connect_data (pfgchar |
    app, pgchar, $UN.cast activate, the_null_ptr, $UN.cast the_null_ptr, 0)
  prval () = fpfgchar pfgchar

  // status = g_application_run (G_APPLICATION (app), argc, argv);
  prval pfgapp = __keep_castview pfapp
  prval pfargv = __create_view ()
  val argv = $UN.castvwtp0 the_null_ptr
  val status = fun_c2ats_g_application_run (pfgapp, pfargv | app, 0, argv)
  prval () = __consume_view pfgapp
  prval () = __consume_view pfargv

  // g_object_unref (app);
  val () = fun_c2ats_g_object_unref ($UN.cast app)
  prval () = __consume_view pfapp
}
