(in-package :gtk-test)

(def-suite gtk-tool-button :in gtk-suite)
(in-suite gtk-tool-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkToolButton

(test gtk-tool-button-class
  ;; Check type
  (is (g:type-is-object "GtkToolButton"))
  ;; Check registered name
  (is (eq 'gtk:tool-button
          (glib:symbol-for-gtype "GtkToolButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkToolButton")
          (g:gtype (cffi:foreign-funcall "gtk_tool_button_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkToolItem")
          (g:type-parent "GtkToolButton")))
  ;; Check children
  (is (equal '("GtkMenuToolButton" "GtkToggleToolButton")
             (glib-test:list-children "GtkToolButton")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActivatable"
               "GtkActionable")
             (glib-test:list-interfaces "GtkToolButton")))
  ;; Check class properties
  (is (equal '("action-name" "action-target" "icon-name" "icon-widget" "label"
               "label-widget" "stock-id" "use-underline")
             (glib-test:list-properties "GtkToolButton")))
  ;; Check style properties
  (is (equal '("icon-spacing")
             (gtk-test:list-style-properties "GtkToolButton")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkToolButton")))
  ;; Check signals
  (is (equal '("clicked")
             (glib-test:list-signals "GtkToolButton")))
  ;; Check CSS information
  (is (string= "toolbutton"
               (gtk:widget-class-css-name "GtkToolButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkToolButton" GTK:TOOL-BUTTON
                      (:SUPERCLASS GTK:TOOL-ITEM :EXPORT T :INTERFACES
                       ("AtkImplementorIface" "GtkActionable"
                        "GtkActivatable" "GtkBuildable")
                       :TYPE-INITIALIZER "gtk_tool_button_get_type")
                      ((ICON-NAME TOOL-BUTTON-ICON-NAME "icon-name"
                        "gchararray" T T)
                       (ICON-WIDGET TOOL-BUTTON-ICON-WIDGET "icon-widget"
                        "GtkWidget" T T)
                       (LABEL TOOL-BUTTON-LABEL "label" "gchararray" T T)
                       (LABEL-WIDGET TOOL-BUTTON-LABEL-WIDGET "label-widget"
                        "GtkWidget" T T)
                       (STOCK-ID TOOL-BUTTON-STOCK-ID "stock-id" "gchararray"
                        T T)
                       (USE-UNDERLINE TOOL-BUTTON-USE-UNDERLINE
                        "use-underline" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkToolButton"))))

;;; --- Signals ----------------------------------------------------------------

;;;     clicked

(test gtk-tool-button-clicked-signal
  (let* ((name "clicked")
         (gtype (g:gtype "GtkToolButton"))
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

;;; --- Properties -------------------------------------------------------------

;;;     icon-name
;;;     icon-widget
;;;     label
;;;     label-widget
;;;     stock-id
;;;     use-underline

(test gtk-tool-button-properties
  (glib-test:with-check-memory (button)
    (setf button (gtk:tool-button-new))
    ;; Property ICON-NAME
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkToolButton" "icon-name")))
    (is-false (gtk:tool-button-icon-name button))
    ;; Property ICON-WIDGET
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkToolButton" "icon-widget")))
    (is-false (gtk:tool-button-icon-widget button))
    ;; Property LABEL
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkToolButton" "label")))
    (is-false (gtk:tool-button-label button))
    ;; Property LABEL-WIDGET
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkToolButton" "label-widget")))
    (is-false (gtk:tool-button-label-widget button))
    ;; Property STOCK-ID
    (is (equal '(:DEPRECATED :READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkToolButton" "stock-id")))
    (is-false (gtk:tool-button-stock-id button))
    ;; Property USE-UNDERLINE
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkToolButton" "use-underline")))
    (is-false (gtk:tool-button-use-underline button))))

;;; --- Style Properties -------------------------------------------------------

;;;     icon-spacing

(test gtk-tool-button-properties
  (glib-test:with-check-memory (button)
    (setf button (gtk:tool-button-new))
    (is (= 4 (gtk:widget-style-property button "icon-spacing")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_tool_button_new

(test gtk-tool-button-new
  (glib-test:with-check-memory (button)
    (is (typep (setf button (gtk:tool-button-new)) 'gtk:tool-button))))

;;;     gtk_tool_button_new_from_stock

(test gtk-tool-button-new-from-stock
  (glib-test:with-check-memory (button)
    (is (typep (setf button
                     (gtk:tool-button-new-from-stock "gtk-ok")) 'gtk:tool-button))))

;;; 2026-05-29
