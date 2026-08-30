(in-package :gtk-test)

(def-suite gtk-tool-item-group :in gtk-suite)
(in-suite gtk-tool-item-group)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkToolItemGroup

(test gtk-tool-item-group-class
  ;; Check type
  (is (g:type-is-object "GtkToolItemGroup"))
  ;; Check registered name
  (is (eq 'gtk:tool-item-group
          (glib:symbol-for-gtype "GtkToolItemGroup")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkToolItemGroup")
          (g:gtype (cffi:foreign-funcall "gtk_tool_item_group_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkContainer")
          (g:type-parent "GtkToolItemGroup")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkToolItemGroup")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkToolShell")
             (glib-test:list-interfaces "GtkToolItemGroup")))
  ;; Check class properties
  (is (equal '("collapsed" "ellipsize" "header-relief" "label" "label-widget")
             (glib-test:list-properties "GtkToolItemGroup")))
  ;; Check style properties
  (is (equal '("expander-size" "header-spacing")
             (gtk-test:list-style-properties "GtkToolItemGroup")))
  ;; Check child properties
  (is (equal '("expand" "fill" "homogeneous" "new-row" "position")
             (gtk-test:list-child-properties "GtkToolItemGroup")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkToolItemGroup")))
  ;; Check CSS information
  (is (string= "toolitemgroup"
               (gtk:widget-class-css-name "GtkToolItemGroup")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkToolItemGroup" GTK:TOOL-ITEM-GROUP
                      (:SUPERCLASS GTK:CONTAINER
                       :EXPORT T
                       :INTERFACES
                       ("AtkImplementorIface" "GtkBuildable" "GtkToolShell")
                       :TYPE-INITIALIZER "gtk_tool_item_group_get_type")
                      ((COLLAPSED TOOL-ITEM-GROUP-COLLAPSED "collapsed"
                        "gboolean" T T)
                       (ELLIPSIZE TOOL-ITEM-GROUP-ELLIPSIZE "ellipsize"
                        "PangoEllipsizeMode" T T)
                       (HEADER-RELIEF TOOL-ITEM-GROUP-HEADER-RELIEF
                        "header-relief" "GtkReliefStyle" T T)
                       (LABEL TOOL-ITEM-GROUP-LABEL "label" "gchararray" T T)
                       (LABEL-WIDGET TOOL-ITEM-GROUP-LABEL-WIDGET
                        "label-widget" "GtkWidget" T T)))
             (gobject:get-gtype-definition "GtkToolItemGroup"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-tool-item-group-properties
  (glib-test:with-check-memory (group)
    (setf group (gtk:tool-item-group-new "label"))
    ;; Property COLLAPSED
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkToolItemGroup" "collapsed")))
    (is-false (gtk:tool-item-group-collapsed group))
    (is-true (setf (gtk:tool-item-group-collapsed group) t))
    ;; Property ELLIPSIZE
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkToolItemGroup" "ellipsize")))
    (is (eq :none (gtk:tool-item-group-ellipsize group)))
    (is (eq :start (setf (gtk:tool-item-group-ellipsize group) :start)))
    ;; Property HEADER-RELIEF
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkToolItemGroup" "header-relief")))
    (is (eq :normal (gtk:tool-item-group-header-relief group)))
    (is (eq :half (setf (gtk:tool-item-group-header-relief group) :half)))
    ;; Property LABEL
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkToolItemGroup" "label")))
    (is (string= "label" (gtk:tool-item-group-label group)))
    (is (string= "text" (setf (gtk:tool-item-group-label group) "text")))
    ;; Property LABEL-WIDGET
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkToolItemGroup" "label-widget")))
    (is (typep (gtk:tool-item-group-label-widget group) 'gtk:label))
    (is-false (setf (gtk:tool-item-group-label-widget group) nil))))

;;; --- Child Properties -------------------------------------------------------

;;;     expand
;;;     fill
;;;     homogeneous
;;;     new-row
;;;     position

(test gtk-tool-item-group-child-properties
  (glib-test:with-check-memory (group (button 3) :strong 1)
    (setf group (gtk:tool-item-group-new "label"))
    (setf button (gtk:menu-tool-button-new-from-stock "gtk-ok"))
    (is-false (gtk:tool-item-group-insert group button -1))
    ;; Check child properties
    (is-false (gtk:tool-item-group-child-expand group button))
    (is-true (gtk:tool-item-group-child-fill group button))
    (is-true (gtk:tool-item-group-child-homogeneous group button))
    (is-false (gtk:tool-item-group-child-new-row group button))
    (is (= 0 (gtk:tool-item-group-child-position group button)))))

;;; --- Style Properties -------------------------------------------------------

;;;     expander-size
;;;     header-spacing

(test gtk-tool-item-group-style-properties
  (glib-test:with-check-memory (group)
    (setf group (gtk:tool-item-group-new "label"))
    (is (= 11 (gtk:widget-style-property group "expander-size")))
    (is (= 2 (gtk:widget-style-property group "header-spacing")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_tool_item_group_new

(test gtk-tool-item-group-new
  (glib-test:with-check-memory (group)
    (is (typep (setf group (gtk:tool-item-group-new "label")) 'gtk:tool-item-group))))

;;;     gtk_tool_item_group_get_drop_item
;;;     gtk_tool_item_group_get_n_items
;;;     gtk_tool_item_group_get_nth_item
;;;     gtk_tool_item_group_insert

;;; 2025-05-29
