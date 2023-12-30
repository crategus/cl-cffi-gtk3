(in-package :gtk-test)

(def-suite gtk-arrow :in gtk-suite)
(in-suite gtk-arrow)

;;; Types and Values

;;;     GtkArrow

(test gtk-arrow-class
  ;; Type check
  (is (g:type-is-object "GtkArrow"))
  ;; Check the registered name
  (is (eq 'gtk:arrow
          (glib:symbol-for-gtype "GtkArrow")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkArrow")
          (g:gtype (cffi:foreign-funcall "gtk_arrow_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkMisc")
          (g:type-parent "GtkArrow")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkArrow")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (list-interfaces "GtkArrow")))
  ;; Check the class properties
  (is (equal '("arrow-type" "shadow-type")
             (list-properties "GtkArrow")))
  ;; Check the style properties
  (is (equal '("arrow-scaling")
             (list-style-properties "GtkArrow")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkArrow")))
  ;; CSS information
  (is (string= "widget"
               (gtk:widget-class-css-name "GtkArrow")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkArrow" GTK-ARROW
                               (:SUPERCLASS GTK-MISC :EXPORT T :INTERFACES
                                ("AtkImplementorIface" "GtkBuildable")
                                :TYPE-INITIALIZER "gtk_arrow_get_type")
                               ((ARROW-TYPE GTK-ARROW-ARROW-TYPE "arrow-type"
                                 "GtkArrowType" T T)
                                (SHADOW-TYPE GTK-ARROW-SHADOW-TYPE
                                 "shadow-type" "GtkShadowType" T T)))
             (gobject:get-g-type-definition "GtkArrow"))))

;;; --- Properties -------------------------------------------------------------

;;;     arrow-type
;;;     shadow-type

(test gtk-arrow-properties
  (let ((arrow (make-instance 'gtk:arrow)))
    (is-false (gtk:arrow-arrow-type arrow))
    (is-false (gtk:arrow-shadow-type arrow))))

;;; --- Style Properties -------------------------------------------------------

;;;     arrow-scaling

(test gtk-arrow-properties
  (let ((arrow (make-instance 'gtk:arrow)))
    (is (= 0.7 (gtk:widget-style-property arrow "arrow-scaling")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_arrow_new

(test gtk-arrow-new
  (let (arrow)
    (is (typep (setf arrow (gtk:arrow-new :up :in)) 'gtk:arrow))
    (is (eq :up (gtk:arrow-arrow-type arrow)))
    (is (eq :in (gtk:arrow-shadow-type arrow)))))

;;;     gtk_arrow_set

(test gtk-arrow-new
  (let ((arrow (make-instance 'gtk:arrow)))
    (is-false (gtk:arrow-set arrow :up :in))
    (is (eq :up (gtk:arrow-arrow-type arrow)))
    (is (eq :in (gtk:arrow-shadow-type arrow)))))

;;; 2023-12-30