(in-package :gtk-test)

(def-suite gtk-arrow :in gtk-suite)
(in-suite gtk-arrow)

;;; Types and Values

;;;     GtkArrow

(test gtk-arrow-class
  ;; Check type
  (is (g:type-is-object "GtkArrow"))
  ;; Check registered name
  (is (eq 'gtk:arrow
          (glib:symbol-for-gtype "GtkArrow")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkArrow")
          (g:gtype (cffi:foreign-funcall "gtk_arrow_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkMisc")
          (g:type-parent "GtkArrow")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkArrow")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkArrow")))
  ;; Check class properties
  (is (equal '("arrow-type" "shadow-type")
             (glib-test:list-properties "GtkArrow")))
  ;; Check style properties
  (is (equal '("arrow-scaling")
             (gtk-test:list-style-properties "GtkArrow")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkArrow")))
  ;; CSS information
  (is (string= "widget"
               (gtk:widget-class-css-name "GtkArrow")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkArrow" GTK:ARROW
                       (:SUPERCLASS GTK:MISC
                        :EXPORT T
                        :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_arrow_get_type")
                       ((ARROW-TYPE ARROW-ARROW-TYPE
                         "arrow-type" "GtkArrowType" T T)
                        (SHADOW-TYPE ARROW-SHADOW-TYPE
                         "shadow-type" "GtkShadowType" T T)))
             (gobject:get-gtype-definition "GtkArrow"))))

;;; --- Properties -------------------------------------------------------------

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

;;; 2024-9-23
