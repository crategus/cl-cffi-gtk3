(in-package :gtk-test)

(def-suite gtk-invisible :in gtk-suite)
(in-suite gtk-invisible)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkInvisible

(test gtk-invisible-class
  ;; Check type
  (is (g:type-is-object "GtkInvisible"))
  ;; Check registered name
  (is (eq 'gtk:invisible
          (glib:symbol-for-gtype "GtkInvisible")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkInvisible")
          (g:gtype (cffi:foreign-funcall "gtk_invisible_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkInvisible")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkInvisible")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkInvisible")))
  ;; Check class properties
  (is (equal '("screen")
             (glib-test:list-properties "GtkInvisible")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkInvisible")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkInvisible")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkInvisible" GTK:INVISIBLE
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                       :TYPE-INITIALIZER "gtk_invisible_get_type")
                      ((SCREEN INVISIBLE-SCREEN "screen" "GdkScreen" T T)))
             (gobject:get-gtype-definition "GtkInvisible"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-invisible-properties
  (glib-test:with-check-memory (invisible)
    (is (typep (setf invisible (make-instance 'gtk:invisible)) 'gtk:invisible))
    (is (typep (gtk:invisible-screen invisible) 'gdk:screen))
    (is (eq (gdk:screen-default) (gtk:invisible-screen invisible)))
    (is-false (gtk:widget-destroy invisible))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_invisible_new

(test gtk-invisible-new
  (glib-test:with-check-memory (invisible)
    (is (typep (setf invisible (gtk:invisible-new)) 'gtk:invisible))
    (is (typep invisible 'gtk:invisible))
    (is (eq (gdk:screen-default) (gtk:invisible-screen invisible)))
    (is-false (gtk:widget-destroy invisible))))

;;;     gtk_invisible_new_for_screen

(test gtk-invisible-new-for-screen
  (glib-test:with-check-memory (invisible)
    (is (typep (setf invisible
                     (gtk:invisible-new-for-screen (gdk:screen-default)))
               'gtk:invisible))
    (is (typep invisible 'gtk:invisible))
    (is (eq (gdk:screen-default) (gtk:invisible-screen invisible)))
    (is-false (gtk:widget-destroy invisible))))

;;; 2025-06-06
