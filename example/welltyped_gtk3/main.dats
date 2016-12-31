(* https://developer.gnome.org/gtk3/stable/gtk-getting-started.html *)
#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"

staload "example_welltyped.sats"

// Unsafe!
extern praxi __create_view {to:view} ():<prf> to
extern praxi __consume_view {from:view} (pf: from):<prf> void

fun print_hello {l:addr} (pfgtkw: !type_c2ats_GtkWidget@l, p: type_c2ats_gpointer): void =
  println! "Hello World"

fun activate {l:addr} (pfapp: !type_c2ats_GtkApplication@l | app: ptr l,
  user_data: type_c2ats_gpointer): void = {
  // window = gtk_application_window_new (app);
  val (pfwindow | window) = fun_c2ats_gtk_application_window_new (pfapp | app)

  // gtk_window_set_title (GTK_WINDOW (window), "Window");
  prval pfgtkwin = __create_view ()
  val () = fun_c2ats_gtk_window_set_title (pfgtkwin | window, "Window")

  // gtk_window_set_default_size (GTK_WINDOW (window), 200, 200);
  val () = fun_c2ats_gtk_window_set_default_size (pfgtkwin | window, 200, 200)
  prval () = __consume_view pfgtkwin

  // button_box = gtk_button_box_new (GTK_ORIENTATION_HORIZONTAL);
  val (pfbox | box) = fun_c2ats_gtk_button_box_new enum_c2ats_GTK_ORIENTATION_HORIZONTAL

  // gtk_container_add (GTK_CONTAINER (window), button_box);
  prval pfcontainer = __create_view ()
  val () = fun_c2ats_gtk_container_add (pfcontainer, pfbox | window, box)
  prval () = __consume_view pfcontainer

  // button = gtk_button_new_with_label ("Hello World");
  val (pfbutton | button) = fun_c2ats_gtk_button_new_with_label ("Hello World")

  // g_signal_connect (button, "clicked", G_CALLBACK (print_hello), NULL);
  val _ = fun_c2ats_g_signal_connect_data (
    button, "clicked", $UN.cast print_hello, the_null_ptr, $UN.cast the_null_ptr, 0)

  // g_signal_connect_swapped (button, "clicked", G_CALLBACK (gtk_widget_destroy), window);
  val _ = fun_c2ats_g_signal_connect_data (button, "clicked",
    $UN.cast fun_c2ats_gtk_widget_destroy, window, $UN.cast the_null_ptr,
    enum_c2ats_G_CONNECT_SWAPPED)

  // gtk_container_add (GTK_CONTAINER (button_box), button);
  prval pfcontainer = __create_view ()
  val () = fun_c2ats_gtk_container_add (pfcontainer, pfbutton | box, button)
  prval () = __consume_view pfcontainer
  prval () = __consume_view pfbutton
  prval () = __consume_view pfbox

  // gtk_widget_show_all (window);
  val () = fun_c2ats_gtk_widget_show_all (pfwindow | window)
  prval () = __consume_view pfwindow
}

implement main0 (argc, argv) = {
  // app = gtk_application_new ("org.gtk.example", G_APPLICATION_FLAGS_NONE);
  val (pfapp | app) =
    fun_c2ats_gtk_application_new ("org.gtk.example", enum_c2ats_G_APPLICATION_FLAGS_NONE)

  // g_signal_connect (app, "activate", G_CALLBACK (activate), NULL);
  val _ = fun_c2ats_g_signal_connect_data (
    app, "activate", $UN.cast activate, the_null_ptr, $UN.cast the_null_ptr, 0)

  // status = g_application_run (G_APPLICATION (app), argc, argv);
  prval pfgapp = __create_view ()
  val status = fun_c2ats_g_application_run (pfgapp | app, argc, argv)
  prval () = __consume_view pfgapp

  // g_object_unref (app);
  val () = fun_c2ats_g_object_unref app
  prval () = __consume_view pfapp
}
