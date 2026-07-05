(in-package :gtk-test)

(def-suite gtk-window-group :in gtk-suite)
(in-suite gtk-window-group)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkWindowGroup

(test gtk-window-group-class
  ;; Check type
  (is (g:type-is-object "GtkWindowGroup"))
  ;; Check registered name
  (is (eq 'gtk:window-group
          (glib:symbol-for-gtype "GtkWindowGroup")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkWindowGroup")
          (g:gtype (cffi:foreign-funcall "gtk_window_group_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkWindowGroup")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkWindowGroup")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkWindowGroup")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkWindowGroup")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkWindowGroup")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkWindowGroup" GTK:WINDOW-GROUP
                      (:SUPERCLASS G:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_window_group_get_type")
                      NIL)
             (gobject:get-gtype-definition "GtkWindowGroup"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_window_group_new

(test gtk-window-group-new
  (glib-test:with-check-memory (group)
    (is (typep (setf group (gtk:window-group-new)) 'gtk:window-group))))

;;;     gtk_window_group_add_window
;;;     gtk_window_group_remove_window

(test gtk-window-group-add/remove-window
  (glib-test:with-check-memory (group window1 window2)
    (setf group (gtk:window-group-new))
    (setf window1 (gtk:window-new :toplevel))
    (setf window2 (gtk:window-new :toplevel))
    (is-false (gtk:window-group-add-window group window1))
    (is-false (gtk:window-group-add-window group window2))
    (is (= 2 (length (gtk:window-group-list-windows group))))
    (is-false (gtk:window-group-remove-window group window1))
    (is (= 1 (length (gtk:window-group-list-windows group))))
    (is (eq window2
            (first (gtk:window-group-list-windows group))))
    (is-false (gtk:window-group-remove-window group window2))
    (is (= 0 (length (gtk:window-group-list-windows group))))
    ;; Destroy windows
    (is-false (gtk:widget-destroy window1))
    (is-false (gtk:widget-destroy window2))))

;;;     gtk_window_group_list_windows

(test gtk-window-group-list-windows
  (glib-test:with-check-memory (group window)
    (setf group (gtk:window-group-new))
    (setf window (gtk:window-new :toplevel))
    (is-false (gtk:window-group-list-windows group))
    (is-false (gtk:window-group-add-window group window))
    (is (every (lambda (x) (typep x 'gtk:window))
               (gtk:window-group-list-windows group)))
    ;; Destroy window
    (is-false (gtk:widget-destroy window))))

;;;     gtk_window_group_get_current_grab
;;;     gtk_window_group_get_current_device_grab

;;; 2026-06-10
