(in-package :gtk-test)

(def-suite gtk-button-box :in gtk-suite)
(in-suite gtk-button-box)

;;; Types and Values

;;;     GtkButtonBoxStyle

(test gtk-button-box-style
  ;; Check type
  (is (g:type-is-enum "GtkButtonBoxStyle"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkButtonBoxStyle")
          (g:gtype (cffi:foreign-funcall "gtk_button_box_style_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:button-box-style
          (glib:symbol-for-gtype "GtkButtonBoxStyle")))
  ;; Check names
  (is (equal '("GTK_BUTTONBOX_SPREAD" "GTK_BUTTONBOX_EDGE" "GTK_BUTTONBOX_START"
               "GTK_BUTTONBOX_END" "GTK_BUTTONBOX_CENTER" "GTK_BUTTONBOX_EXPAND")
             (glib-test:list-enum-item-names "GtkButtonBoxStyle")))
  ;; Check values
  (is (equal '(1 2 3 4 5 6)
             (glib-test:list-enum-item-values "GtkButtonBoxStyle")))
  ;; Check nick names
  (is (equal '("spread" "edge" "start" "end" "center" "expand")
             (glib-test:list-enum-item-nicks "GtkButtonBoxStyle")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkButtonBoxStyle" GTK:BUTTON-BOX-STYLE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_button_box_style_get_type")
                       (:SPREAD 1)
                       (:EDGE 2)
                       (:START 3)
                       (:END 4)
                       (:CENTER 5)
                       (:EXPAND 6))
             (gobject:get-gtype-definition "GtkButtonBoxStyle"))))

;;;     GtkButtonBox

(cffi:foreign-funcall "gtk_hbutton_box_get_type" :size)
(cffi:foreign-funcall "gtk_vbutton_box_get_type" :size)

(test gtk-button-box-class
  ;; Check type
  (is (g:type-is-object "GtkButtonBox"))
  ;; Check registered name
  (is (eq 'gtk:button-box
          (glib:symbol-for-gtype "GtkButtonBox")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkButtonBox")
          (g:gtype (cffi:foreign-funcall "gtk_button_box_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkBox") (g:type-parent "GtkButtonBox")))
  ;; Check children
  (is (equal '("GtkHButtonBox" "GtkVButtonBox")
             (glib-test:list-children "GtkButtonBox")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (glib-test:list-interfaces "GtkButtonBox")))
  ;; Check class properties
  (is (equal '("layout-style")
             (glib-test:list-properties "GtkButtonBox")))
  ;; Check style properties
  (is (equal '("child-internal-pad-x" "child-internal-pad-y" "child-min-height"
               "child-min-width")
             (gtk-test:list-style-properties "GtkButtonBox")))
  ;; Check child properties
  (is (equal '("expand" "fill" "non-homogeneous" "pack-type" "padding"
               "position" "secondary")
             (gtk-test:list-child-properties "GtkButtonBox")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkButtonBox")))
  ;; CSS information
  (is (string= "buttonbox"
               (gtk:widget-class-css-name "GtkButtonBox")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkButtonBox" GTK:BUTTON-BOX
                       (:SUPERCLASS GTK:BOX
                        :EXPORT T
                        :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_button_box_get_type")
                       ((LAYOUT-STYLE BUTTON-BOX-LAYOUT-STYLE
                         "layout-style" "GtkButtonBoxStyle" T T)))
             (gobject:get-gtype-definition "GtkButtonBox"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-button-box-properties
  (let ((box (make-instance 'gtk:button-box)))
    (is (eq :edge (gtk:button-box-layout-style box)))))

;;; --- Child Properties -------------------------------------------------------

(test gtk-button-box-child-properties
  (let ((box (make-instance 'gtk:button-box))
        (button (make-instance 'gtk:button)))
    (is-false (gtk:box-pack-start box button))
    (is-false (gtk:button-box-child-non-homogeneous box button))
    (is-false (gtk:button-box-child-secondary box button))))

;;; --- Style Properties -------------------------------------------------------

(test gtk-button-box-style-properties
  (let ((box (make-instance 'gtk:button-box)))
    (is (= 4 (gtk:widget-style-property box "child-internal-pad-x")))
    (is (= 0 (gtk:widget-style-property box "child-internal-pad-y")))
    (is (= 27 (gtk:widget-style-property box "child-min-height")))
    (is (= 85 (gtk:widget-style-property box "child-min-width")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_button_box_new

(test gtk-button-box-new
  (is (typep (gtk:button-box-new :horizontal) 'gtk:button-box))
  (is (typep (gtk:button-box-new :vertical) 'gtk:button-box)))

;;;     gtk_button_box_get_layout
;;;     gtk_button_box_set_layout

(test gtk-button-box-layout
  (let ((box (make-instance 'gtk:button-box)))
    (is (eq :start (setf (gtk:button-box-layout box) :start)))
    (is (eq :start (gtk:button-box-layout box)))))

;;; 2024-9-21
