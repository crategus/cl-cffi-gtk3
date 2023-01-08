(in-package :gtk-test)

(def-suite gtk-window-group :in gtk-suite)
(in-suite gtk-window-group)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkWindowGroup

(test window-group-class
  ;; Type check
  (is (g:type-is-object "GtkWindowGroup"))
  ;; Check the registered name
  (is (eq 'gtk:window-group
          (gobject:symbol-for-gtype "GtkWindowGroup")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkWindowGroup")
          (g:gtype (cffi:foreign-funcall "gtk_window_group_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkWindowGroup")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkWindowGroup")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkWindowGroup")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GtkWindowGroup")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkWindowGroup")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkWindowGroup" GTK-WINDOW-GROUP
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_window_group_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkWindowGroup"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_window_group_new
;;;     gtk_window_group_add_window
;;;     gtk_window_group_remove_window
;;;     gtk_window_group_list_windows
;;;     gtk_window_group_get_current_grab
;;;     gtk_window_group_get_current_device_grab

;;; --- 2022-12-26 -------------------------------------------------------------
