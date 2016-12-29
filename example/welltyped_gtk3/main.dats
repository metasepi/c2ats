(* https://developer.gnome.org/gtk3/stable/gtk-getting-started.html *)
#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"

staload "example_welltyped.sats"

// Unsafe!
extern praxi __create_view {to:view} ():<prf> to
extern praxi __keep_castview {to:view}{from:view} (pf: !from):<prf> to
extern praxi __consume_view {from:view} (pf: from):<prf> void

fun{} take_gcharp2tr (s: string):
  [l:addr] (type_c2ats_gchar@l, type_c2ats_gchar@l -<lin,prf> void | ptr l) = ret where {
  val p = string2ptr s
  val ret = $UN.ptr_vtake p
}

fun print_hello {l:addr} (pfgtkw: !type_c2ats_GtkWidget@l, p: type_c2ats_gpointer): void =
  println! "Hello World"

fun activate {l:addr} (pfapp: !type_c2ats_GtkApplication@l | app: ptr l,
  user_data: type_c2ats_gpointer): void = {
  // window = gtk_application_window_new (app);
  val (pfwindow | window) = fun_c2ats_gtk_application_window_new (pfapp | app)

  // gtk_window_set_title (GTK_WINDOW (window), "Window");
  prval pfgtkwin = __keep_castview pfwindow
  val (pfgchar, fpfgchar | pgchar) = take_gcharp2tr "Window"
  val () = fun_c2ats_gtk_window_set_title (pfgtkwin, pfgchar | window, pgchar)
  prval () = fpfgchar pfgchar

  // gtk_window_set_default_size (GTK_WINDOW (window), 200, 200);
  val () = fun_c2ats_gtk_window_set_default_size (pfgtkwin | window, 200, 200)
  prval () = __consume_view pfgtkwin

  // button_box = gtk_button_box_new (GTK_ORIENTATION_HORIZONTAL);
  val (pfbox | box) = fun_c2ats_gtk_button_box_new enum_c2ats_GTK_ORIENTATION_HORIZONTAL

  // gtk_container_add (GTK_CONTAINER (window), button_box);
  prval pfcontainer = __keep_castview pfwindow
  val () = fun_c2ats_gtk_container_add (pfcontainer, pfbox | window, box)
  prval () = __consume_view pfcontainer

  // button = gtk_button_new_with_label ("Hello World");
  val (pfgchar, fpfgchar | pgchar) = take_gcharp2tr "Hello World"
  val (pfbutton | button) = fun_c2ats_gtk_button_new_with_label (pfgchar | pgchar)
  prval () = fpfgchar pfgchar

  // g_signal_connect (button, "clicked", G_CALLBACK (print_hello), NULL);
  val (pfgchar, fpfgchar | pgchar) = take_gcharp2tr "clicked"
  val _ = fun_c2ats_g_signal_connect_data (pfgchar |
    button, pgchar, $UN.cast print_hello, the_null_ptr, $UN.cast the_null_ptr, 0)

  // g_signal_connect_swapped (button, "clicked", G_CALLBACK (gtk_widget_destroy), window);
  val _ = fun_c2ats_g_signal_connect_data (pfgchar | button, pgchar,
    $UN.cast fun_c2ats_gtk_widget_destroy, window, $UN.cast the_null_ptr,
    enum_c2ats_G_CONNECT_SWAPPED)
  prval () = fpfgchar pfgchar

  // gtk_container_add (GTK_CONTAINER (button_box), button);
  prval pfcontainer = __keep_castview pfbox
  val () = fun_c2ats_gtk_container_add (pfcontainer, pfbutton | box, button)
  prval () = __consume_view pfcontainer
  prval () = __consume_view pfbutton
  prval () = __consume_view pfbox

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
  val () = fun_c2ats_g_object_unref app
  prval () = __consume_view pfapp
}
