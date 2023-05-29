(in-package :gtk-test)

(def-suite gtk-button-box :in gtk-suite)
(in-suite gtk-button-box)

;;; Types and Values

;;;     GtkButtonBoxStyle

(test button-box-style
  ;; Check the type
  (is (g:type-is-enum "GtkButtonBoxStyle"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkButtonBoxStyle")
          (g:gtype (cffi:foreign-funcall "gtk_button_box_style_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:button-box-style
          (glib:symbol-for-gtype "GtkButtonBoxStyle")))
  ;; Check the names
  (is (equal '("GTK_BUTTONBOX_SPREAD" "GTK_BUTTONBOX_EDGE" "GTK_BUTTONBOX_START"
               "GTK_BUTTONBOX_END" "GTK_BUTTONBOX_CENTER" "GTK_BUTTONBOX_EXPAND")
             (list-enum-item-name "GtkButtonBoxStyle")))
  ;; Check the values
  (is (equal '(1 2 3 4 5 6)
             (list-enum-item-value "GtkButtonBoxStyle")))
  ;; Check the nick names
  (is (equal '("spread" "edge" "start" "end" "center" "expand")
             (list-enum-item-nick "GtkButtonBoxStyle")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GtkButtonBoxStyle"
                             GTK-BUTTON-BOX-STYLE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_button_box_style_get_type")
                             (:SPREAD 1)
                             (:EDGE 2)
                             (:START 3)
                             (:END 4)
                             (:CENTER 5)
                             (:EXPAND 6))
             (gobject:get-g-type-definition "GtkButtonBoxStyle"))))

;;;     GtkButtonBox

(cffi:foreign-funcall "gtk_hbutton_box_get_type" :size)
(cffi:foreign-funcall "gtk_vbutton_box_get_type" :size)

(test button-box-class
  ;; Type check
  (is (g:type-is-object "GtkButtonBox"))
  ;; Check the registered name
  (is (eq 'gtk:button-box
          (glib:symbol-for-gtype "GtkButtonBox")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkButtonBox")
          (g:gtype (cffi:foreign-funcall "gtk_button_box_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkBox") (g:type-parent "GtkButtonBox")))
  ;; Check the children
  (is (equal '("GtkHButtonBox" "GtkVButtonBox")
             (list-children "GtkButtonBox")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (list-interfaces "GtkButtonBox")))
  ;; Check the class properties
  (is (equal '("layout-style")
             (list-properties "GtkButtonBox")))
  ;; Get the names of the style properties.
  (is (equal '("child-internal-pad-x" "child-internal-pad-y" "child-min-height"
               "child-min-width")
             (list-style-properties "GtkButtonBox")))
  ;; Get the names of the child properties
  (is (equal '("expand" "fill" "non-homogeneous" "pack-type" "padding"
               "position" "secondary")
             (list-child-properties "GtkButtonBox")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkButtonBox")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkButtonBox" GTK-BUTTON-BOX
                       (:SUPERCLASS GTK-BOX
                        :EXPORT T
                        :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_button_box_get_type")
                       ((LAYOUT-STYLE GTK-BUTTON-BOX-LAYOUT-STYLE
                         "layout-style" "GtkButtonBoxStyle" T T)))
             (gobject:get-g-type-definition "GtkButtonBox"))))

;;; --- Properties -------------------------------------------------------------

;;;     GtkButtonBoxStyle  layout-style          Read / Write

;;; --- Child Properties -------------------------------------------------------

;;;              gboolean  non-homogeneous       Read / Write
;;;              gboolean  secondary             Read / Write

;;; --- Style Properties -------------------------------------------------------

;;;                  gint  child-internal-pad-x  Read
;;;                  gint  child-internal-pad-y  Read
;;;                  gint  child-min-height      Read
;;;                  gint  child-min-width       Read

;;; --- Functions --------------------------------------------------------------

;;;     gtk_button_box_new
;;;     gtk_button_box_get_layout
;;;     gtk_button_box_get_child_secondary
;;;     gtk_button_box_get_child_non_homogeneous
;;;     gtk_button_box_set_layout
;;;     gtk_button_box_set_child_secondary
;;;     gtk_button_box_set_child_non_homogeneous

;;; --- 2023-5-29 --------------------------------------------------------------
