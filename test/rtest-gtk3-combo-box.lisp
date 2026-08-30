(in-package :gtk-test)

(def-suite gtk-combo-box :in gtk-suite)
(in-suite gtk-combo-box)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkComboBox

(test gtk-combo-box-class
  ;; Check type
  (is (g:type-is-object "GtkComboBox"))
  ;; Check registered name
  (is (eq 'gtk:combo-box
          (glib:symbol-for-gtype "GtkComboBox")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkComboBox")
          (g:gtype (cffi:foreign-funcall "gtk_combo_box_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkBin")
          (g:type-parent "GtkComboBox")))
  ;; Check children
  (is (equal '("GtkAppChooserButton" "GtkComboBoxText")
             (glib-test:list-children "GtkComboBox")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkCellLayout"
               "GtkCellEditable")
             (glib-test:list-interfaces "GtkComboBox")))
  ;; Check class properties
  (is (equal '("active" "active-id" "add-tearoffs" "button-sensitivity"
               "cell-area" "column-span-column" "editing-canceled"
               "entry-text-column" "has-entry" "has-frame" "id-column" "model"
               "popup-fixed-width" "popup-shown" "row-span-column"
               "tearoff-title" "wrap-width")
             (glib-test:list-properties "GtkComboBox")))
  ;; Check style properties
  (is (equal '("appears-as-list" "arrow-scaling" "arrow-size" "shadow-type")
             (gtk-test:list-style-properties "GtkComboBox")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkComboBox")))
  ;; Check signals
  (is (equal '("changed" "format-entry-text" "move-active" "popdown" "popup")
             (glib-test:list-signals "GtkComboBox")))
  ;; Check CSS information
  (is (string= "combobox"
               (gtk:widget-class-css-name "GtkComboBox")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkComboBox" GTK:COMBO-BOX
                      (:SUPERCLASS GTK:BIN
                       :EXPORT T
                       :INTERFACES
                       ("AtkImplementorIface" "GtkBuildable"
                        "GtkCellEditable" "GtkCellLayout")
                       :TYPE-INITIALIZER "gtk_combo_box_get_type")
                      ((ACTIVE COMBO-BOX-ACTIVE "active" "gint" T T)
                       (ACTIVE-ID COMBO-BOX-ACTIVE-ID "active-id"
                        "gchararray" T T)
                       (ADD-TEAROFFS COMBO-BOX-ADD-TEAROFFS "add-tearoffs"
                        "gboolean" T T)
                       (BUTTON-SENSITIVITY COMBO-BOX-BUTTON-SENSITIVITY
                        "button-sensitivity" "GtkSensitivityType" T T)
                       (CELL-AREA COMBO-BOX-CELL-AREA "cell-area"
                        "GtkCellArea" T NIL)
                       (COLUMN-SPAN-COLUMN COMBO-BOX-COLUMN-SPAN-COLUMN
                        "column-span-column" "gint" T T)
                       (ENTRY-TEXT-COLUMN COMBO-BOX-ENTRY-TEXT-COLUMN
                        "entry-text-column" "gint" T T)
                       (HAS-ENTRY COMBO-BOX-HAS-ENTRY "has-entry" "gboolean"
                        T NIL)
                       (HAS-FRAME COMBO-BOX-HAS-FRAME "has-frame" "gboolean"
                        T T)
                       (ID-COLUMN COMBO-BOX-ID-COLUMN "id-column" "gint" T T)
                       (MODEL COMBO-BOX-MODEL "model" "GtkTreeModel" T T)
                       (POPUP-FIXED-WIDTH COMBO-BOX-POPUP-FIXED-WIDTH
                        "popup-fixed-width" "gboolean" T T)
                       (POPUP-SHOWN COMBO-BOX-POPUP-SHOWN "popup-shown"
                        "gboolean" T NIL)
                       (ROW-SPAN-COLUMN COMBO-BOX-ROW-SPAN-COLUMN
                        "row-span-column" "gint" T T)
                       (TEAROFF-TITLE COMBO-BOX-TEAROFF-TITLE "tearoff-title"
                        "gchararray" T T)
                       (WRAP-WIDTH COMBO-BOX-WRAP-WIDTH "wrap-width" "gint" T T)))
             (gobject:get-gtype-definition "GtkComboBox"))))

;;; --- Signals ----------------------------------------------------------------

;;;     changed
;;;     format-entry-text
;;;     move-active
;;;     popdown
;;;     popup

;;; --- Properties -------------------------------------------------------------

;;;     active
;;;     active-id
;;;     add-tearoffs
;;;     button-sensitivity
;;;     cell-area
;;;     column-span-column
;;;     entry-text-column
;;;     focus-on-click
;;;     has-entry
;;;     has-frame
;;;     id-column
;;;     model
;;;     popup-fixed-width
;;;     popup-shown
;;;     row-span-column
;;;     tearoff-title
;;;     wrap-width

(test gtk-combo-box-properties
  (glib-test:with-check-memory (box :strong 1)
    (is (typep (setf box (gtk:combo-box-new)) 'gtk:combo-box))
    (is (= -1 (gtk:combo-box-active box)))
    (is-false (gtk:combo-box-active-id box))
    (is-false (gtk:combo-box-add-tearoffs box))
    (is (eq :auto (gtk:combo-box-button-sensitivity box)))
    (is (typep (gtk:combo-box-cell-area box) 'gtk:cell-area-box))
    (is (= -1 (gtk:combo-box-column-span-column box)))
    (is (= -1 (gtk:combo-box-entry-text-column box)))
    (is-true (gtk:combo-box-focus-on-click box))
    (is-false (gtk:combo-box-has-entry box))
    (is-true (gtk:combo-box-has-frame box))
    (is (= -1 (gtk:combo-box-id-column box)))
    (is-false (gtk:combo-box-model box))
    (is-true (gtk:combo-box-popup-fixed-width box))
    (is-false (gtk:combo-box-popup-shown box))
    (is (= -1 (gtk:combo-box-row-span-column box)))
    (is-false (gtk:combo-box-tearoff-title box))
    (is (= 0 (gtk:combo-box-wrap-width box)))))

;;; --- Style Properties -------------------------------------------------------

;;;     appears-as-list
;;;     arrow-scaling
;;;     arrow-size
;;;     shadow-type

(test gtk-combo-box-style-properties
  (glib-test:with-check-memory (box)
    (is (typep (setf box (gtk:combo-box-new)) 'gtk:combo-box))
    (is-false (gtk:widget-style-property box "appears-as-list"))
    (is (= 1.0 (gtk:widget-style-property box "arrow-scaling")))
    (is (= 15 (gtk:widget-style-property box "arrow-size")))
    (is (eq :none (gtk:widget-style-property box "shadow-type")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_combo_box_new

(test gtk-combo-box-new
  (glib-test:with-check-memory (box)
    (is (typep (setf box (gtk:combo-box-new)) 'gtk:combo-box))))

;;;     gtk_combo_box_new_with_entry

(test gtk-combo-box-new-with-entry
  (glib-test:with-check-memory (box)
    (is (typep (setf box (gtk:combo-box-new-with-entry)) 'gtk:combo-box))))

;;;     gtk_combo_box_new_with_model

(test gtk-combo-box-new-with-model
  (glib-test:with-check-memory (box (model 2) :strong 1)
    (is (typep (setf model (gtk:list-store-new)) 'gtk:tree-model))
    (is (typep (setf box (gtk:combo-box-new-with-model model)) 'gtk:combo-box))
    ;; Remove references
    (is-false (setf (gtk:combo-box-model box) nil))))

;;;     gtk_combo_box_new_with_model_and_entry

(test gtk-combo-box-new-with-model-and-entry
  (glib-test:with-check-memory (box (model 2) :strong 1)
    (is (typep (setf model (gtk:list-store-new)) 'gtk:tree-model))
    (is (typep (setf box (gtk:combo-box-new-with-model-and-entry model)) 'gtk:combo-box))
    ;; Remove references
    (is-false (setf (gtk:combo-box-model box) nil))))

;;;     gtk_combo_box_new_with_area

(test gtk-combo-box-new-with-area
  (glib-test:with-check-memory (box area)
    (is (typep (setf area (gtk:cell-area-box-new)) 'gtk:cell-area-box))
    (is (typep (setf box (gtk:combo-box-new-with-area box)) 'gtk:combo-box))))

;;;     gtk_combo_box_new_with_area_and_entry

(test gtk-combo-box-new-with-area-and-entry
  (glib-test:with-check-memory (box area)
    (is (typep (setf area (gtk:cell-area-box-new)) 'gtk:cell-area-box))
    (is (typep (setf box (gtk:combo-box-new-with-area-and-entry box)) 'gtk:combo-box))))

;;;     gtk_combo_box_get_active_iter
;;;     gtk_combo_box_set_active_iter
;;;     gtk_combo_box_popup_for_device
;;;     gtk_combo_box_popup
;;;     gtk_combo_box_popdown
;;;     gtk_combo_box_get_popup_accessible
;;;     gtk_combo_box_get_row_separator_func
;;;     gtk_combo_box_set_row_separator_func
;;;     gtk_combo_box_set_title                             not exported
;;;     gtk_combo_box_get_title                             not exported

;;; 2026-07-10
