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
    (is (eq :single (gtk:list-box-selection-mode listbox)))))

(test gtk-list-box-row-properties
  (let ((listboxrow (make-instance 'gtk:list-box-row)))
    (is-true (gtk:list-box-row-activatable listboxrow))
    (is-true (gtk:list-box-row-selectable listboxrow))))

;;; --- Functions --------------------------------------------------------------

;;;     (*GtkListBoxFilterFunc)
;;;     (*GtkListBoxSortFunc)
;;;     (*GtkListBoxUpdateHeaderFunc)

;;;     gtk_list_box_new

(test gtk-list-box-new
  (is (string= "GtkListBox" (g:object-type-name (gtk:list-box-new)))))

;;;     gtk_list_box_prepend

(test gtk-list-box-prepend
  (let ((listbox (make-instance 'gtk:list-box)))

    (is-false (gtk:list-box-prepend listbox (make-instance 'gtk:list-box-row)))
    (is-false (gtk:list-box-prepend listbox (make-instance 'gtk:list-box-row)))
    (is-false (gtk:list-box-prepend listbox (make-instance 'gtk:list-box-row)))

    (is (= 3 (length (gtk:container-children listbox))))
    (is (string= "GtkListBoxRow"
                 (g:object-type-name (first (gtk:container-children listbox)))))))

;;;     gtk_list_box_insert

(test gtk-list-box-insert
  (let ((listbox (make-instance 'gtk:list-box)))

    (is-false (gtk:list-box-insert listbox (make-instance 'gtk:list-box-row) -1))
    (is-false (gtk:list-box-insert listbox (make-instance 'gtk:list-box-row)  0))
    (is-false (gtk:list-box-insert listbox (make-instance 'gtk:list-box-row)  1))

    (is (= 3 (length (gtk:container-children listbox))))
    (is (string= "GtkListBoxRow"
                 (g:object-type-name (first (gtk:container-children listbox)))))))

;;;     gtk_list_box_select_row
;;;     gtk_list_box_unselect_row
;;;     gtk_list_box_get_selected_row

(test gtk-list-box-select-row
  (let ((listbox (make-instance 'gtk:list-box))
        (listboxrow (make-instance 'gtk:list-box-row)))

    (is-false (gtk:list-box-prepend listbox (make-instance 'gtk:list-box-row)))
    (is-false (gtk:list-box-prepend listbox listboxrow))
    (is-false (gtk:list-box-prepend listbox (make-instance 'gtk:list-box-row)))

    (is-false (gtk:list-box-select-row listbox listboxrow))
    (is (equal listboxrow (gtk:list-box-selected-row listbox)))

    (is-false (gtk:list-box-unselect-row listbox listboxrow))
    (is (equal nil (gtk:list-box-selected-row listbox)))))

;;;     gtk_list_box_select_all
;;;     gtk_list_box_unselect_all

(test gtk-list-box-select-all
  (let ((listbox (make-instance 'gtk:list-box :selection-mode :multiple)))

    (is-false (gtk:list-box-prepend listbox (make-instance 'gtk:list-box-row :visible t)))
    (is-false (gtk:list-box-prepend listbox (make-instance 'gtk:list-box-row :visible t)))
    (is-false (gtk:list-box-prepend listbox (make-instance 'gtk:list-box-row :visible t)))
    (is-false (gtk:list-box-prepend listbox (make-instance 'gtk:list-box-row :visible t)))

    (is (= 4 (length (gtk:container-children listbox))))
    (is (= 0 (length (gtk:list-box-selected-rows listbox))))

    (is (eq :multiple (gtk:list-box-selection-mode listbox)))
    (is-false (gtk:list-box-select-all listbox))
    (is (= 4 (length (gtk:list-box-selected-rows listbox))))
    (is-false (gtk:list-box-unselect-all listbox))
    (is (= 0 (length (gtk:list-box-selected-rows listbox))))))

;;;     GtkListBoxForeachFunc
;;;     gtk_list_box_selected_foreach

(test gtk-list-box-selected-foreach
  (let ((listbox (make-instance 'gtk:list-box :selection-mode :multiple))
        (listboxrow (make-instance 'gtk:list-box-row :visible t)))
    (is-false (gtk:list-box-prepend listbox
                                    (make-instance 'gtk:list-box-row
                                                   :visible t)))
    (is-false (gtk:list-box-prepend listbox listboxrow))
    (is-false (gtk:list-box-prepend listbox
                                    (make-instance 'gtk:list-box-row
                                                   :visible t)))
    (is-false (gtk:list-box-prepend listbox
                                    (make-instance 'gtk:list-box-row
                                                   :visible t)))
    (is-false (gtk:list-box-prepend listbox
                                    (make-instance 'gtk:list-box-row
                                                   :visible t)))
    (is-false (gtk:list-box-select-row listbox listboxrow))
    (is (eq listboxrow (gtk:list-box-selected-row listbox)))
    (is (= 5 (length (gtk:container-children listbox))))

;; This is wrong. We cannot modify the listbox within foreach callback
;    (is-false (gtk:list-box-selected-foreach listbox
;                                             (lambda (box row)
;                                               (gtk:container-remove box row))))
;    (is (equal nil (gtk:list-box-selected-row listbox)))

    (is-false (gtk:list-box-select-all listbox))
    (is (= 5 (length (gtk:list-box-selected-rows listbox))))
    (let ((count 0))
      (is-false (gtk:list-box-selected-foreach listbox
                                               (lambda (box row)
                                                 (declare (ignore box row))
                                                 (setf count (1+ count)))))
      (is (= 5 count)))))

;;;     gtk_list_box_get_selected_rows

(test gtk-list-box-selected-rows
  (let ((listbox (make-instance 'gtk:list-box :selection-mode :multiple))
        (row (make-instance 'gtk:list-box-row :visible t)))

    (is-false (gtk:list-box-prepend listbox
                                    (make-instance 'gtk:list-box-row
                                                   :visible t)))
    (is-false (gtk:list-box-prepend listbox
                                    (make-instance 'gtk:list-box-row
                                                   :visible t)))
    (is-false (gtk:list-box-prepend listbox
                                    (make-instance 'gtk:list-box-row
                                                   :visible t)))
    (is-false (gtk:list-box-prepend listbox row))

    (is-false (gtk:list-box-select-row listbox row))
    (is (= 1 (length (gtk:list-box-selected-rows listbox))))
    (is-false (gtk:list-box-select-all listbox))
    (is (= 4 (length (gtk:list-box-selected-rows listbox))))))

;;;     gtk_list_box_get_adjustment
;;;     gtk_list_box_set_adjustment

(test gtk-list-box-adjustment
  (let ((listbox (make-instance 'gtk:list-box)))

    (is-false (gtk:list-box-adjustment listbox))
    (is (eq 'gtk:adjustment (type-of (setf (gtk:list-box-adjustment listbox)
                                           (make-instance 'gtk:adjustment)))))
    (is (eq 'gtk:adjustment (type-of (gtk:list-box-adjustment listbox))))))

;;;     gtk_list_box_set_placeholder

;;;     gtk_list_box_get_row_at_index

(test gtk-list-box-row-at-index
  (let ((listbox (make-instance 'gtk:list-box))
        (listboxrow (make-instance 'gtk:list-box-row)))

    (is-false (gtk:list-box-prepend listbox (make-instance 'gtk:list-box-row)))
    (is-false (gtk:list-box-prepend listbox (make-instance 'gtk:list-box-row)))
    (is-false (gtk:list-box-prepend listbox listboxrow))
    (is-false (gtk:list-box-prepend listbox (make-instance 'gtk:list-box-row)))

    (is-false (gtk:list-box-row-at-index listbox -1))
    (is (equal listboxrow (gtk:list-box-row-at-index listbox 1)))
    (is (eq 'gtk:list-box-row (type-of (gtk:list-box-row-at-index listbox 0))))
    (is (eq 'gtk:list-box-row (type-of (gtk:list-box-row-at-index listbox 2))))
    (is (eq 'gtk:list-box-row (type-of (gtk:list-box-row-at-index listbox 3))))
))

;;;     gtk_list_box_get_row_at_y
;;;     gtk_list_box_invalidate_filter
;;;     gtk_list_box_invalidate_headers
;;;     gtk_list_box_invalidate_sort

;;;     gtk_list_box_set_filter_func

(test gtk-list-box-set-filter-func
  (let ((listbox (make-instance 'gtk:list-box)))

    (is-false (gtk:list-box-prepend listbox (make-instance 'gtk:list-box-row)))
    (is-false (gtk:list-box-prepend listbox (make-instance 'gtk:list-box-row)))
    (is-false (gtk:list-box-prepend listbox (make-instance 'gtk:list-box-row)))

    (is-false (gtk:container-add (gtk:list-box-row-at-index listbox 0)
                                 (make-instance 'gtk:label)))
    (is-false (gtk:container-add (gtk:list-box-row-at-index listbox 1)
                                 (make-instance 'gtk:button)))
    (is-false (gtk:container-add (gtk:list-box-row-at-index listbox 2)
                                 (make-instance 'gtk:label)))

    (is-false (gtk:list-box-set-filter-func listbox
                  (lambda (row)
                    (let ((child (first (gtk:container-children row))))
                      (eq 'gtk:button (type-of child))))))

    (is-false (gtk:list-box-invalidate-filter listbox))))

;;;     gtk_list_box_set_header_func
;;;     gtk_list_box_set_sort_func
;;;     gtk_list_box_drag_highlight_row
;;;     gtk_list_box_drag_unhighlight_row
;;;
;;;     (*GtkListBoxCreateWidgetFunc)
;;;
;;;     gtk_list_box_bind_model
;;;     gtk_list_box_row_new
;;;     gtk_list_box_row_changed
;;;     gtk_list_box_row_is_selected
;;;     gtk_list_box_row_get_header
;;;     gtk_list_box_row_set_header
;;;     gtk_list_box_row_get_index

;;; 2024-9-21
