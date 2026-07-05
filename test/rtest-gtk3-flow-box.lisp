(in-package :gtk-test)

(def-suite gtk-flow-box :in gtk-suite)
(in-suite gtk-flow-box)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFlowBoxChild

(test gtk-flow-box-child-class
  ;; Check type
  (is (g:type-is-object "GtkFlowBoxChild"))
  ;; Check registered name
  (is (eq 'gtk:flow-box-child
          (glib:symbol-for-gtype "GtkFlowBoxChild")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFlowBoxChild")
          (g:gtype (cffi:foreign-funcall "gtk_flow_box_child_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkBin") (g:type-parent "GtkFlowBoxChild")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkFlowBoxChild")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkFlowBoxChild")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkFlowBoxChild")))
  ;; Check style properties.
  (is (equal '()
             (gtk-test:list-style-properties "GtkFlowBoxChild")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkFlowBoxChild")))
  ;; Check signals
  (is (equal '("activate")
             (glib-test:list-signals "GtkFlowBoxChild")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkFlowBoxChild" GTK:FLOW-BOX-CHILD
                      (:SUPERCLASS GTK:BIN
                       :EXPORT T
                       :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                       :TYPE-INITIALIZER "gtk_flow_box_child_get_type")
                      NIL)
             (gobject:get-gtype-definition "GtkFlowBoxChild"))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate

(test gtk-flow-box-child-activate-signal
  (let* ((name "activate")
         (gtype (g:gtype "GtkFlowBoxChild"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_flow_box_child_new
;;;     gtk_flow_box_child_get_index
;;;     gtk_flow_box_child_is_selected
;;;     gtk_flow_box_child_changed
;;;     gtk_flow_box_child_changed

(test gtk-flow-box-child-new
  (glib-test:with-check-memory (child)
    (is (typep (setf child (gtk:flow-box-child-new)) 'gtk:flow-box-child))
    (is (= -1 (gtk:flow-box-child-index child)))
    (is-false (gtk:flow-box-child-is-selected child))
    (is-false (gtk:flow-box-child-changed child))))

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFlowBox

(test gtk-flow-box-class
  ;; Check type
  (is (g:type-is-object "GtkFlowBox"))
  ;; Check registered name
  (is (eq 'gtk:flow-box
          (glib:symbol-for-gtype "GtkFlowBox")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFlowBox")
          (g:gtype (cffi:foreign-funcall "gtk_flow_box_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkContainer") (g:type-parent "GtkFlowBox")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkFlowBox")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (glib-test:list-interfaces "GtkFlowBox")))
  ;; Check class properties
  (is (equal '("activate-on-single-click" "column-spacing" "homogeneous"
               "max-children-per-line" "min-children-per-line" "orientation"
               "row-spacing" "selection-mode")
             (glib-test:list-properties "GtkFlowBox")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkFlowBox")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkFlowBox")))
  ;; Check signals
  (is (equal '("activate-cursor-child" "child-activated" "move-cursor"
               "select-all" "selected-children-changed" "toggle-cursor-child"
               "unselect-all")
             (glib-test:list-signals "GtkFlowBox")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkFlowBox" GTK:FLOW-BOX
                      (:SUPERCLASS GTK:CONTAINER
                       :EXPORT T
                       :INTERFACES
                       ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                       :TYPE-INITIALIZER "gtk_flow_box_get_type")
                      ((ACTIVATE-ON-SINGLE-CLICK
                        FLOW-BOX-ACTIVATE-ON-SINGLE-CLICK
                        "activate-on-single-click" "gboolean" T T)
                       (COLUMN-SPACING FLOW-BOX-COLUMN-SPACING
                        "column-spacing" "guint" T T)
                       (HOMOGENEOUS FLOW-BOX-HOMOGENEOUS
                        "homogeneous" "gboolean" T T)
                       (MAX-CHILDREN-PER-LINE FLOW-BOX-MAX-CHILDREN-PER-LINE
                        "max-children-per-line" "guint" T T)
                       (MIN-CHILDREN-PER-LINE FLOW-BOX-MIN-CHILDREN-PER-LINE
                        "min-children-per-line" "guint" T T)
                       (ROW-SPACING FLOW-BOX-ROW-SPACING
                        "row-spacing" "guint" T T)
                       (SELECTION-MODE FLOW-BOX-SELECTION-MODE
                        "selection-mode" "GtkSelectionMode" T T)))
             (gobject:get-gtype-definition "GtkFlowBox"))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate-cursor-child

(test gtk-flow-box-activate-cursor-child-signal
  (let* ((name "activate-cursor-child")
         (gtype (g:gtype "GtkFlowBox"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     child-activated

(test gtk-flow-box-child-activated-signal
  (let* ((name "child-activated")
         (gtype (g:gtype "GtkFlowBox"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkFlowBoxChild")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     move-cursor

(test gtk-flow-box-move-cursor-signal
  (let* ((name "move-cursor")
         (gtype (g:gtype "GtkFlowBox"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "gboolean") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkMovementStep" "gint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     select-all

(test gtk-flow-box-select-all-signal
  (let* ((name "select-all")
         (gtype (g:gtype "GtkFlowBox"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     selected-children-changed

(test gtk-flow-box-selected-children-changed-signal
  (let* ((name "selected-children-changed")
         (gtype (g:gtype "GtkFlowBox"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     toggle-cursor-child

(test gtk-flow-box-toggle-cursor-child-signal
  (let* ((name "toggle-cursor-child")
         (gtype (g:gtype "GtkFlowBox"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     unselect-all

(test gtk-flow-box-unselect-all-signal
  (let* ((name "unselect-all")
         (gtype (g:gtype "GtkFlowBox"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

;;;     activate-on-single-click
;;;     column-spacing
;;;     homogeneous
;;;     max-children-per-line
;;;     min-children-per-line
;;;     row-spacing
;;;     selection-mode

(test gtk-flow-box-properties
  (glib-test:with-check-memory (flowbox)
    (is (typep (setf flowbox (gtk:flow-box-new)) 'gtk:flow-box))
    ;; Property ACTIVATE-ON-SINGLE-CLICK
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkFlowBox" "activate-on-single-click")))
    (is-true (gtk:flow-box-activate-on-single-click flowbox))
    (is-false (setf (gtk:flow-box-activate-on-single-click flowbox) nil))
    ;; Property COLUMN-SPACING
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkFlowBox" "column-spacing")))
    (is (= 0 (gtk:flow-box-column-spacing flowbox)))
    (is (= 6 (setf (gtk:flow-box-column-spacing flowbox) 6)))
    ;; Property HOMOGENEOUS
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkFlowBox" "homogeneous")))
    (is-false (gtk:flow-box-homogeneous flowbox))
    (is-true (setf (gtk:flow-box-homogeneous flowbox) t))
    ;; Property MAX-CHILDREN-PER-LINE
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkFlowBox" "max-children-per-line")))
    (is (= 7 (gtk:flow-box-max-children-per-line flowbox)))
    (is (= 9 (setf (gtk:flow-box-max-children-per-line flowbox) 9)))
    ;; Property MIN-CHILDREN-PER-LINE
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkFlowBox" "min-children-per-line")))
    (is (= 0 (gtk:flow-box-min-children-per-line flowbox)))
    (is (= 2 (setf (gtk:flow-box-min-children-per-line flowbox) 2)))
    ;; Property ROW-SPACING
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkFlowBox" "row-spacing")))
    (is (= 0 (gtk:flow-box-row-spacing flowbox)))
    (is (= 6 (setf (gtk:flow-box-row-spacing flowbox) 6)))
    ;; Property SELECTION-MODE
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkFlowBox" "selection-mode")))
    (is (eq :single (gtk:flow-box-selection-mode flowbox)))
    (is (eq :browse (setf (gtk:flow-box-selection-mode flowbox) :browse)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_flow_box_new

(test gtk-flow-box-new
  (glib-test:with-check-memory (flowbox)
    (is (typep (setf flowbox (gtk:flow-box-new)) 'gtk:flow-box))))

;;;     gtk_flow_box_insert
;;;     gtk_flow_box_get_child_at_index

(test gtk-flow-box-insert
  (glib-test:with-check-memory (flowbox (child1 2) (child2 2) (child3 2) :strong 3)
    (setf flowbox (gtk:flow-box-new))
    (setf child1 (gtk:button-new))
    (setf child2 (gtk:button-new))
    (setf child3 (gtk:button-new))

    (is-false (gtk:flow-box-insert flowbox child1 -1))
    (is-false (gtk:flow-box-insert flowbox child2 0))
    (is-false (gtk:flow-box-insert flowbox child3 2))

    (is (eq child2 (gtk:bin-child (gtk:flow-box-child-at-index flowbox 0))))
    (is (eq child1 (gtk:bin-child (gtk:flow-box-child-at-index flowbox 1))))
    (is (eq child3 (gtk:bin-child (gtk:flow-box-child-at-index flowbox 2))))

    (is-false (gtk:container-remove flowbox child1))
    (is-false (gtk:container-remove flowbox child2))
    (is-false (gtk:container-remove flowbox child3))))

;;;     gtk_flow_box_get_child_at_pos

;;;     gtk_flow_box_set_hadjustment
;;;     gtk_flow_box_set_vadjustment

(test gtk-flow-box-set-adjustment
  (glib-test:with-check-memory (flowbox (hadjustment 3) (vadjustment 3) :strong 2)
    (setf flowbox (gtk:flow-box-new))
    (setf hadjustment (make-instance 'gtk:adjustment))
    (setf vadjustment (make-instance 'gtk:adjustment))
    (is-false (gtk:flow-box-set-hadjustment flowbox hadjustment))
    (is-false (gtk:flow-box-set-vadjustment flowbox vadjustment))))

;;;     GtkFlowBoxForeachFunc
;;;     gtk_flow_box_selected_foreach

;;;     gtk_flow_box_get_selected_children
;;;     gtk_flow_box_select_child
;;;     gtk_flow_box_unselect_child
;;;     gtk_flow_box_select_all
;;;     gtk_flow_box_unselect_all

;;;     GtkFlowBoxFilterFunc
;;;     gtk_flow_box_set_filter_func
;;;     gtk_flow_box_invalidate_filter

;;;     GtkFlowBoxSortFunc
;;;     gtk_flow_box_set_sort_func
;;;     gtk_flow_box_invalidate_sort

;;;     GtkFlowBoxCreateWidgetFunc
;;;     gtk_flow_box_bind_model

;;; 2026-06-11
