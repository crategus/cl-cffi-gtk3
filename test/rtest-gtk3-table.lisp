(in-package :gtk-test)

(def-suite gtk-table :in gtk-suite)
(in-suite gtk-table)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAttachOptions

(test gtk-attach-options
  ;; Check type
  (is (g:type-is-flags "GtkAttachOptions"))
  ;; Check registered name
  (is (eq 'gtk:attach-options
          (glib:symbol-for-gtype "GtkAttachOptions")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAttachOptions")
          (g:gtype (cffi:foreign-funcall "gtk_attach_options_get_type" :size))))
  ;; Check names
  (is (equal '("GTK_EXPAND" "GTK_SHRINK" "GTK_FILL")
             (gtk-test:list-flags-item-name "GtkAttachOptions")))
  ;; Check values
  (is (equal '(1 2 4)
             (gtk-test:list-flags-item-value "GtkAttachOptions")))
  ;; Check nick names
  (is (equal '("expand" "shrink" "fill")
             (gtk-test:list-flags-item-nick "GtkAttachOptions")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-G-FLAGS "GtkAttachOptions" GTK-ATTACH-OPTIONS
                                      (:EXPORT T
                                       :TYPE-INITIALIZER
                                       "gtk_attach_options_get_type")
                                      (:EXPAND 1)
                                      (:SHRINK 2)
                                      (:FILL 4))
             (gobject:get-g-type-definition "GtkAttachOptions"))))

;;;     GtkTable

(test gtk-table-class
  ;; Check type
  (is (g:type-is-object "GtkTable"))
  ;; Check registered name
  (is (eq 'gtk:table
          (glib:symbol-for-gtype "GtkTable")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTable")
          (g:gtype (cffi:foreign-funcall "gtk_table_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkContainer")
          (g:type-parent "GtkTable")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkTable")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (gtk-test:list-interfaces "GtkTable")))
  ;; Check class properties
  (is (equal '("column-spacing" "homogeneous" "n-columns" "n-rows"
               "row-spacing")
             (gtk-test:list-properties "GtkTable")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkTable")))
  ;; Check child properties
  (is (equal '("bottom-attach" "left-attach" "right-attach" "top-attach"
               "x-options" "x-padding" "y-options" "y-padding")
             (gtk-test:list-child-properties "GtkTable")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkTable")))
  ;; Check CSS information
  (is (string= "widget"
               (gtk:widget-class-css-name "GtkTable")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkTable" GTK-TABLE
                               (:SUPERCLASS GTK-CONTAINER :EXPORT T :INTERFACES
                                ("AtkImplementorIface" "GtkBuildable")
                                :TYPE-INITIALIZER "gtk_table_get_type")
                               ((COLUMN-SPACING GTK-TABLE-COLUMN-SPACING
                                 "column-spacing" "guint" T T)
                                (HOMOGENEOUS GTK-TABLE-HOMOGENEOUS
                                 "homogeneous" "gboolean" T T)
                                (N-COLUMNS GTK-TABLE-N-COLUMNS "n-columns"
                                 "guint" T T)
                                (N-ROWS GTK-TABLE-N-ROWS "n-rows" "guint" T T)
                                (ROW-SPACING GTK-TABLE-ROW-SPACING
                                 "row-spacing" "guint" T T)))
             (gobject:get-g-type-definition "GtkTable"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-table-properties
  (let ((table (make-instance 'gtk:table)))
    (is (= 0 (gtk:table-column-spacing table)))
    (is-false (gtk:table-homogeneous table))
    (is (= 1 (gtk:table-n-columns table)))
    (is (= 1 (gtk:table-n-rows table)))
    (is (= 0 (gtk:table-row-spacing table)))))

;;; --- Child Properties -------------------------------------------------------

(test gtk-table-child-properties
  (let ((table (make-instance 'gtk:table))
        (button (make-instance 'gtk:button)))
    (is-false (gtk:container-add table button))
    (is (= 1 (gtk:table-child-bottom-attach table button)))
    (is (= 0 (gtk:table-child-left-attach table button)))
    (is (= 1 (gtk:table-child-right-attach table button)))
    (is (= 0 (gtk:table-child-top-attach table button)))
    (is (equal '(:expand :fill) (gtk:table-child-x-options table button)))
    (is (= 0 (gtk:table-child-x-padding table button)))
    (is (equal '(:expand :fill) (gtk:table-child-y-options table button)))
    (is (= 0 (gtk:table-child-y-padding table button)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_table_new

(test gtk-table-new
  (is (typep (gtk:table-new 2 3) 'gtk:table))
  (is (typep (gtk:table-new 2 3 nil) 'gtk:table))
  (is (typep (gtk:table-new 2 3 t) 'gtk:table)))

;;;     gtk_table_resize
;;;     gtk_table_get_size

(test gtk-table-size/resize
  (let ((table (gtk:table-new 4 5)))
    (is (equal '(4 5) (multiple-value-list (gtk:table-size table))))
    (is-false (gtk:table-resize table 6 7))
    (is (equal '(6 7) (multiple-value-list (gtk:table-size table))))))

;;;     gtk_table_attach

(test gtk-table-attach
  (let ((table (gtk:table-new 2 2 t))
        (button1 (gtk:button-new-with-label "Button 1"))
        (button2 (gtk:button-new-with-label "Button 2"))
        (quit (gtk:button-new-with-label "Quit")))
    (is-false (gtk:table-attach table button1 0 1 0 1))
    (is-false (gtk:table-attach table button2 1 2 0 1))
    (is-false (gtk:table-attach table quit    0 2 1 2))))

;;;     gtk_table_set_row_spacing
;;;     gtk_table_set_col_spacing
;;;     gtk_table_get_row_spacing
;;;     gtk_table_get_col_spacing

(test gtk-table-row/col-spacing
  (let ((table (gtk:table-new 4 5)))
    (is (= 0 (gtk:table-get-col-spacing table 2)))
    (is-false (gtk:table-set-col-spacing table 2 12))
    (is (= 12 (gtk:table-get-col-spacing table 2)))
    (is (= 0 (gtk:table-get-row-spacing table 2)))
    (is-false (gtk:table-set-row-spacing table 2 12))
    (is (= 12 (gtk:table-get-row-spacing table 2)))))

;;; 2024-6-27
