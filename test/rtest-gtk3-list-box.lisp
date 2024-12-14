(in-package :gtk-test)

(def-suite gtk-list-box :in gtk-suite)
(in-suite gtk-list-box)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkListBox

(test gtk-list-box-class
  ;; Check type
  (is (g:type-is-object "GtkListBox"))
  ;; Check registered name
  (is (eq 'gtk:list-box
          (glib:symbol-for-gtype "GtkListBox")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkListBox")
          (g:gtype (cffi:foreign-funcall "gtk_list_box_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkContainer") (g:type-parent "GtkListBox")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkListBox")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkListBox")))
  ;; Check class properties
  (is (equal '("activate-on-single-click" "selection-mode")
             (glib-test:list-properties "GtkListBox")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkListBox")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkListBox")))
  ;; Check signals
  (is (equal '("activate-cursor-row" "move-cursor" "row-activated"
               "row-selected" "select-all" "selected-rows-changed"
               "toggle-cursor-row" "unselect-all")
             (glib-test:list-signals "GtkListBox")))
  ;; CSS information
  (is (string= "list"
               (gtk:widget-class-css-name "GtkListBox")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkListBox" GTK:LIST-BOX
                       (:SUPERCLASS GTK:CONTAINER
                        :EXPORT T
                        :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_list_box_get_type")
                       ((ACTIVATE-ON-SINGLE-CLICK
                         LIST-BOX-ACTIVATE-ON-SINGLE-CLICK
                         "activate-on-single-click" "gboolean" T T)
                        (SELECTION-MODE LIST-BOX-SELECTION-MODE
                         "selection-mode" "GtkSelectionMode" T T)))
             (gobject:get-gtype-definition "GtkListBox"))))

;;;     GtkListBoxRow

(test gtk-list-box-row-class
  ;; Check type
  (is (g:type-is-object "GtkListBoxRow"))
  ;; Check registered name
  (is (eq 'gtk:list-box-row
          (glib:symbol-for-gtype "GtkListBoxRow")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkListBoxRow")
          (g:gtype (cffi:foreign-funcall "gtk_list_box_row_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkBin") (g:type-parent "GtkListBoxRow")))
  ;; Check children
  (is (or (equal '()
                 (glib-test:list-children "GtkListBoxRow"))
          (equal '("GtkPlacesViewRow" "GtkSidebarRow")
                 (glib-test:list-children "GtkListBoxRow"))))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActionable")
             (glib-test:list-interfaces "GtkListBoxRow")))
  ;; Check class properties
  (is (equal '("action-name" "action-target" "activatable" "selectable")
             (glib-test:list-properties "GtkListBoxRow")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkListBoxRow")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkListBoxRow")))
  ;; Check signals
  (is (equal '("activate")
             (glib-test:list-signals "GtkListBoxRow")))
  ;; CSS information
  (is (string= "row"
               (gtk:widget-class-css-name "GtkListBoxRow")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkListBoxRow" GTK:LIST-BOX-ROW
                       (:SUPERCLASS GTK:BIN
                        :EXPORT T
                        :INTERFACES
                        ("AtkImplementorIface" "GtkActionable" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_list_box_row_get_type")
                       ((ACTIVATABLE LIST-BOX-ROW-ACTIVATABLE
                         "activatable" "gboolean" T T)
                        (SELECTABLE LIST-BOX-ROW-SELECTABLE
                         "selectable" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkListBoxRow"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-list-box-properties
  (let ((listbox (make-instance 'gtk:list-box)))
    (is-true (gtk:list-box-activate-on-single-click listbox))
    (is (eq :single (gtk:list-box-selection-mode listbox)))
    ;;Check memory management
    (is (= 1 (g:object-ref-count listbox)))))

(test gtk-list-box-row-properties
  (let ((listboxrow (make-instance 'gtk:list-box-row)))
    (is-true (gtk:list-box-row-activatable listboxrow))
    (is-true (gtk:list-box-row-selectable listboxrow))
    ;;Check memory management
    (is (= 1 (g:object-ref-count listboxrow)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_list_box_new

(test gtk-list-box-new
  (is (eq (g:gtype "GtkListBox")
          (g:type-from-instance (gtk:list-box-new)))))

;;;     gtk_list_box_prepend

(test gtk-list-box-prepend
  (let ((listbox (make-instance 'gtk:list-box))
        (row1 (gtk:list-box-row-new))
        (row2 (gtk:list-box-row-new))
        (row3 (gtk:list-box-row-new)))
    (is-false (gtk:list-box-prepend listbox row1))
    (is-false (gtk:list-box-prepend listbox row2))
    (is-false (gtk:list-box-prepend listbox row3))

    (is (= 3 (length (gtk:container-children listbox))))
    (is (eq (g:gtype "GtkListBoxRow")
            (g:type-from-instance (first (gtk:container-children listbox)))))
    ;; Check memory management
    (is-false (map nil (lambda (child)
                         (gtk:container-remove listbox child))
                       (gtk:container-children listbox)))
    (is (= 1 (g:object-ref-count row1)))
    (is (= 1 (g:object-ref-count row2)))
    (is (= 1 (g:object-ref-count row3)))
    (is (= 1 (g:object-ref-count listbox)))))

;;;     gtk_list_box_insert

(test gtk-list-box-insert
  (let ((listbox (make-instance 'gtk:list-box))
        (row1 (gtk:list-box-row-new))
        (row2 (gtk:list-box-row-new))
        (row3 (gtk:list-box-row-new)))
    (is-false (gtk:list-box-insert listbox row1 -1))
    (is-false (gtk:list-box-insert listbox row2  0))
    (is-false (gtk:list-box-insert listbox row3  1))

    (is (= 3 (length (gtk:container-children listbox))))
    (is (eq (g:gtype "GtkListBoxRow")
            (g:type-from-instance (first (gtk:container-children listbox)))))
    ;; Check memory management
    (is-false (map nil (lambda (child)
                         (gtk:container-remove listbox child))
                       (gtk:container-children listbox)))
    (is (= 1 (g:object-ref-count row1)))
    (is (= 1 (g:object-ref-count row2)))
    (is (= 1 (g:object-ref-count row3)))
    (is (= 1 (g:object-ref-count listbox)))))

;;;     gtk_list_box_select_row
;;;     gtk_list_box_unselect_row
;;;     gtk_list_box_get_selected_row

(test gtk-list-box-select-row
  (let ((listbox (make-instance 'gtk:list-box))
        (row1 (make-instance 'gtk:list-box-row))
        (row2 (make-instance 'gtk:list-box-row))
        (row3 (make-instance 'gtk:list-box-row)))

    (is-false (gtk:list-box-prepend listbox row1))
    (is-false (gtk:list-box-prepend listbox row2))
    (is-false (gtk:list-box-prepend listbox row3))

    (is-false (gtk:list-box-select-row listbox row2))
    (is (eq row2 (gtk:list-box-selected-row listbox)))

    (is-false (gtk:list-box-unselect-row listbox row2))
    (is-false (gtk:list-box-selected-row listbox))
    ;; Check memory management
    (is-false (map nil (lambda (child)
                         (gtk:container-remove listbox child))
                       (gtk:container-children listbox)))
    (is (= 1 (g:object-ref-count row1)))
    (is (= 1 (g:object-ref-count row2)))
    (is (= 1 (g:object-ref-count row3)))
    (is (= 1 (g:object-ref-count listbox)))))

;;;     gtk_list_box_select_all
;;;     gtk_list_box_unselect_all

(test gtk-list-box-select-all
  (let ((listbox (make-instance 'gtk:list-box :selection-mode :multiple))
        (row1 (make-instance 'gtk:list-box-row :visible t))
        (row2 (make-instance 'gtk:list-box-row :visible t))
        (row3 (make-instance 'gtk:list-box-row :visible t))
        (row4 (make-instance 'gtk:list-box-row :visible t)))

    (is-false (gtk:list-box-prepend listbox row1))
    (is-false (gtk:list-box-prepend listbox row2))
    (is-false (gtk:list-box-prepend listbox row3))
    (is-false (gtk:list-box-prepend listbox row4))

    (is (= 4 (length (gtk:container-children listbox))))
    (is (= 0 (length (gtk:list-box-selected-rows listbox))))

    (is (eq :multiple (gtk:list-box-selection-mode listbox)))
    (is-false (gtk:list-box-select-all listbox))
    (is (= 4 (length (gtk:list-box-selected-rows listbox))))
    (is-false (gtk:list-box-unselect-all listbox))
    (is (= 0 (length (gtk:list-box-selected-rows listbox))))
    ;; Check memory management
    (is-false (map nil (lambda (child)
                         (gtk:container-remove listbox child))
                       (gtk:container-children listbox)))
    (is (= 1 (g:object-ref-count row1)))
    (is (= 1 (g:object-ref-count row2)))
    (is (= 1 (g:object-ref-count row3)))
    (is (= 1 (g:object-ref-count row4)))
    (is (= 1 (g:object-ref-count listbox)))))

;;;     GtkListBoxForeachFunc
;;;     gtk_list_box_selected_foreach

(test gtk-list-box-selected-foreach
  (let ((listbox (make-instance 'gtk:list-box :selection-mode :multiple))
        (row1 (make-instance 'gtk:list-box-row :visible t))
        (row2 (make-instance 'gtk:list-box-row :visible t))
        (row3 (make-instance 'gtk:list-box-row :visible t))
        (row4 (make-instance 'gtk:list-box-row :visible t))
        (row5 (make-instance 'gtk:list-box-row :visible t)))

    (is-false (gtk:list-box-prepend listbox row1))
    (is-false (gtk:list-box-prepend listbox row2))
    (is-false (gtk:list-box-prepend listbox row3))
    (is-false (gtk:list-box-prepend listbox row4))
    (is-false (gtk:list-box-prepend listbox row5))

    (is-false (gtk:list-box-select-row listbox row2))
    (is (eq row2 (gtk:list-box-selected-row listbox)))
    (is (= 5 (length (gtk:container-children listbox))))
    (is-false (gtk:list-box-select-all listbox))
    (is (= 5 (length (gtk:list-box-selected-rows listbox))))
    (let ((count 0))
      (is-false (gtk:list-box-selected-foreach listbox
                                               (lambda (box row)
                                                 (declare (ignore box row))
                                                 (setf count (1+ count)))))
      (is (= 5 count)))
    ;; Check memory management
    (is-false (map nil (lambda (child)
                         (gtk:container-remove listbox child))
                       (gtk:container-children listbox)))
    (is (= 1 (g:object-ref-count row1)))
    (is (= 1 (g:object-ref-count row2)))
    (is (= 1 (g:object-ref-count row3)))
    (is (= 1 (g:object-ref-count row4)))
    (is (= 1 (g:object-ref-count row5)))
    (is (= 1 (g:object-ref-count listbox)))))

;;;     gtk_list_box_get_selected_rows

(test gtk-list-box-selected-rows
  (let ((listbox (make-instance 'gtk:list-box :selection-mode :multiple))
        (row1 (make-instance 'gtk:list-box-row :visible t))
        (row2 (make-instance 'gtk:list-box-row :visible t))
        (row3 (make-instance 'gtk:list-box-row :visible t))
        (row4 (make-instance 'gtk:list-box-row :visible t)))
    (is-false (gtk:list-box-prepend listbox row1))
    (is-false (gtk:list-box-prepend listbox row2))
    (is-false (gtk:list-box-prepend listbox row3))
    (is-false (gtk:list-box-prepend listbox row4))

    (is-false (gtk:list-box-select-row listbox row4))
    (is (= 1 (length (gtk:list-box-selected-rows listbox))))
    (is-false (gtk:list-box-select-all listbox))
    (is (= 4 (length (gtk:list-box-selected-rows listbox))))
    ;; Check memory management
    (is-false (map nil (lambda (child)
                         (gtk:container-remove listbox child))
                       (gtk:container-children listbox)))
    (is (= 1 (g:object-ref-count row1)))
    (is (= 1 (g:object-ref-count row2)))
    (is (= 1 (g:object-ref-count row3)))
    (is (= 1 (g:object-ref-count row4)))
    (is (= 1 (g:object-ref-count listbox)))))

;;;     gtk_list_box_get_adjustment
;;;     gtk_list_box_set_adjustment

(test gtk-list-box-adjustment
  (let ((listbox (make-instance 'gtk:list-box))
        (adjustment (make-instance 'gtk:adjustment)))

    (is-false (gtk:list-box-adjustment listbox))
    (is (eq adjustment (setf (gtk:list-box-adjustment listbox) adjustment)))
    (is (eq adjustment (gtk:list-box-adjustment listbox)))
    ;; Check memory management
    (is-false (setf (gtk:list-box-adjustment listbox) nil))
    (is (= 1 (g:object-ref-count adjustment)))
    (is (= 1 (g:object-ref-count listbox)))))

;;;     gtk_list_box_set_placeholder

;;;     gtk_list_box_get_row_at_index

(test gtk-list-box-row-at-index
  (let ((listbox (make-instance 'gtk:list-box))
        (row1 (make-instance 'gtk:list-box-row))
        (row2 (make-instance 'gtk:list-box-row))
        (row3 (make-instance 'gtk:list-box-row))
        (row4 (make-instance 'gtk:list-box-row)))

    (is-false (gtk:list-box-prepend listbox row1))
    (is-false (gtk:list-box-prepend listbox row2))
    (is-false (gtk:list-box-prepend listbox row3))
    (is-false (gtk:list-box-prepend listbox row4))

    (is-false (gtk:list-box-row-at-index listbox -1))
    (is (eq row4 (gtk:list-box-row-at-index listbox 0)))
    (is (eq row3 (gtk:list-box-row-at-index listbox 1)))
    (is (eq row2 (gtk:list-box-row-at-index listbox 2)))
    (is (eq row1 (gtk:list-box-row-at-index listbox 3)))
    ;; Check memory management
    (is-false (map nil (lambda (child)
                         (gtk:container-remove listbox child))
                       (gtk:container-children listbox)))
    (is (= 1 (g:object-ref-count row1)))
    (is (= 1 (g:object-ref-count row2)))
    (is (= 1 (g:object-ref-count row3)))
    (is (= 1 (g:object-ref-count row4)))
    (is (= 1 (g:object-ref-count listbox)))))

;;;     gtk_list_box_get_row_at_y
;;;     gtk_list_box_invalidate_filter
;;;     gtk_list_box_invalidate_headers
;;;     gtk_list_box_invalidate_sort

;;;     gtk_list_box_set_filter_func

;; TODO: This test is not finished

(test gtk-list-box-set-filter-func
  (let ((listbox (make-instance 'gtk:list-box))
        (row1 (make-instance 'gtk:list-box-row))
        (row2 (make-instance 'gtk:list-box-row))
        (row3 (make-instance 'gtk:list-box-row))
        (label1 (make-instance 'gtk:label))
        (label2 (make-instance 'gtk:label))
        (button (make-instance 'gtk:button)))

    (is-false (gtk:list-box-prepend listbox row1))
    (is-false (gtk:list-box-prepend listbox row2))
    (is-false (gtk:list-box-prepend listbox row3))

    (is-false (gtk:container-add (gtk:list-box-row-at-index listbox 0) label1))
    (is-false (gtk:container-add (gtk:list-box-row-at-index listbox 1) button))
    (is-false (gtk:container-add (gtk:list-box-row-at-index listbox 2) label2))

    (is-false (gtk:list-box-set-filter-func listbox
                  (lambda (row)
                    (let ((child (first (gtk:container-children row))))
                      (eq 'gtk:button (type-of child))))))

    (is-false (gtk:list-box-invalidate-filter listbox))
    ;; Check memory management
    (is-false (gtk:container-remove (gtk:list-box-row-at-index listbox 2) label2))
    (is-false (gtk:container-remove (gtk:list-box-row-at-index listbox 1) button))
    (is-false (gtk:container-remove (gtk:list-box-row-at-index listbox 0) label1))
    (is-false (map nil (lambda (child)
                         (gtk:container-remove listbox child))
                       (gtk:container-children listbox)))
    (is (= 1 (g:object-ref-count label1)))
    (is (= 1 (g:object-ref-count label2)))
    (is (= 1 (g:object-ref-count button)))
    (is (= 1 (g:object-ref-count row1)))
    (is (= 1 (g:object-ref-count row2)))
    (is (= 1 (g:object-ref-count row3)))
    (is (= 1 (g:object-ref-count listbox)))))

;;;     gtk_list_box_set_header_func
;;;     gtk_list_box_set_sort_func
;;;     gtk_list_box_drag_highlight_row
;;;     gtk_list_box_drag_unhighlight_row

;;;     gtk_list_box_bind_model
;;;     gtk_list_box_row_new
;;;     gtk_list_box_row_changed
;;;     gtk_list_box_row_is_selected
;;;     gtk_list_box_row_get_header
;;;     gtk_list_box_row_set_header
;;;     gtk_list_box_row_get_index

;;; 2024-12-14
