(in-package :gtk-test)

(def-suite gtk-invisible :in gtk-suite)
(in-suite gtk-invisible)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkInvisible

(test gtk-invisible-class
  ;; Type check
  (is (g:type-is-object "GtkInvisible"))
  ;; Check the registered name
  (is (eq 'gtk:invisible
          (glib:symbol-for-gtype "GtkInvisible")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkInvisible")
          (g:gtype (cffi:foreign-funcall "gtk_invisible_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkInvisible")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkInvisible")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (list-interfaces "GtkInvisible")))
  ;; Check the class properties
  (is (equal '("screen")
             (list-properties "GtkInvisible")))
  ;; Check the style properties
  (is (equal '()
             (list-style-properties "GtkInvisible")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkInvisible")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkInvisible" GTK-INVISIBLE
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_invisible_get_type")
                       ((SCREEN GTK-INVISIBLE-SCREEN "screen" "GdkScreen" T T)))
             (gobject:get-g-type-definition "GtkInvisible"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-invisible-properties
  (let ((invisible (make-instance 'gtk:invisible)))
    (is (typep (gtk:invisible-screen invisible) 'gdk:screen))
    (is (eq (gdk:screen-default) (gtk:invisible-screen invisible)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_invisible_new

(test gtk-invisible-new
  (let ((invisible (gtk:invisible-new)))
    (is (typep invisible 'gtk:invisible))
    (is (eq (gdk:screen-default) (gtk:invisible-screen invisible)))))

;;;     gtk_invisible_new_for_screen

(test gtk-invisible-new-for-screen
  (let ((invisible (gtk:invisible-new-for-screen (gdk:screen-default))))
    (is (typep invisible 'gtk:invisible))
    (is (eq (gdk:screen-default) (gtk:invisible-screen invisible)))))

;;; 2024-3-17
