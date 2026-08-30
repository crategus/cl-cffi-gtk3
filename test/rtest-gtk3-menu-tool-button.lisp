(in-package :gtk-test)

(def-suite gtk-menu-tool-button :in gtk-suite)
(in-suite gtk-menu-tool-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkMenuToolButton

(test gtk-menu-tool-button-class
  ;; Check type
  (is (g:type-is-object "GtkMenuToolButton"))
  ;; Check registered name
  (is (eq 'gtk:menu-tool-button
          (glib:symbol-for-gtype "GtkMenuToolButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkMenuToolButton")
          (g:gtype (cffi:foreign-funcall "gtk_menu_tool_button_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkToolButton")
          (g:type-parent "GtkMenuToolButton")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkMenuToolButton")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActivatable"
               "GtkActionable")
             (glib-test:list-interfaces "GtkMenuToolButton")))
  ;; Check class properties
  (is (equal '("menu")
             (glib-test:list-properties "GtkMenuToolButton")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkMenuToolButton")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkMenuToolButton")))
  ;; Check signals
  (is (equal '("show-menu")
             (glib-test:list-signals "GtkMenuToolButton")))
  ;; Check CSS information
  (is (string= "toolbutton"
               (gtk:widget-class-css-name "GtkMenuToolButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkMenuToolButton" GTK:MENU-TOOL-BUTTON
                      (:SUPERCLASS GTK:TOOL-BUTTON
                       :EXPORT T
                       :INTERFACES
                       ("AtkImplementorIface" "GtkActionable" "GtkActivatable"
                        "GtkBuildable")
                       :TYPE-INITIALIZER "gtk_menu_tool_button_get_type")
                      ((MENU MENU-TOOL-BUTTON-MENU "menu" "GtkMenu" T T)))
             (gobject:get-gtype-definition "GtkMenuToolButton"))))

;;; --- Signals ----------------------------------------------------------------

;;;     show-menu

(test gtk-text-activate-signal
  (let* ((name "show-menu")
         (gtype (g:gtype "GtkMenuToolButton"))
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

;;; --- Properties -------------------------------------------------------------

;;;     menu

(test gtk-menu-tool-button-properties
  (glib-test:with-check-memory (button)
    (setf button (gtk:menu-tool-button-new))
    ;; Property MENU
    (is (equal '(:READABLE :WRITABLE)
               (glib-test:list-param-flags "GtkMenuToolButton" "menu")))
    (is-false (gtk:menu-tool-button-menu button))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_menu_tool_button_new

(test gtk-menu-tool-button-new
  (glib-test:with-check-memory (button)
    (is (typep (setf button (gtk:menu-tool-button-new)) 'gtk:menu-tool-button))))

;;;     gtk_menu_tool_button_new_from_stock

(test gtk-menu-tool-button-new-from-stock
  (glib-test:with-check-memory (button)
    (is (typep (setf button
                     (gtk:menu-tool-button-new-from-stock "gtk-ok"))
               'gtk:menu-tool-button))))

;;;     gtk_menu_tool_button_set_arrow_tooltip_text

(test gtk-menu-tool-button-set-arrow-tooltip-text
  (glib-test:with-check-memory (button)
    (setf button (gtk:menu-tool-button-new))
    (is-false (gtk:menu-tool-button-set-arrow-tooltip-text button "text"))))

;;;     gtk_menu_tool_button_set_arrow_tooltip_markup

(test gtk-menu-tool-button-set-arrow-tooltip-markup
  (glib-test:with-check-memory (button)
    (setf button (gtk:menu-tool-button-new))
    (is-false (gtk:menu-tool-button-set-arrow-tooltip-markup button "text"))))

;;; 2026-05-30