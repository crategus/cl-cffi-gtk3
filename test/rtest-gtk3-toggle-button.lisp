(in-package :gtk-test)

(def-suite gtk-toggle-button :in gtk-suite)
(in-suite gtk-toggle-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkToggleButton

(test gtk-toggle-button-class
  ;; Check type
  (is (g:type-is-object "GtkToggleButton"))
  ;; Check registered name
  (is (eq 'gtk:toggle-button
          (glib:symbol-for-gtype "GtkToggleButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkToggleButton")
          (g:gtype (cffi:foreign-funcall "gtk_toggle_button_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkButton")
          (g:type-parent "GtkToggleButton")))
  ;; Check children
  (is (equal '("GtkCheckButton" "GtkMenuButton")
             (glib-test:list-children "GtkToggleButton")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActionable"
               "GtkActivatable")
             (glib-test:list-interfaces "GtkToggleButton")))
  ;; Check class properties
  (is (equal '("active" "draw-indicator" "inconsistent")
             (glib-test:list-properties "GtkToggleButton")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkToggleButton")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkToggleButton")))
  ;; Check signals
  (is (equal '("toggled")
             (glib-test:list-signals "GtkToggleButton")))
  ;; CSS information
  (is (string= "button"
               (gtk:widget-class-css-name "GtkToggleButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkToggleButton" GTK:TOGGLE-BUTTON
                      (:SUPERCLASS GTK:BUTTON
                       :EXPORT T
                       :INTERFACES
                       ("AtkImplementorIface" "GtkActionable" "GtkActivatable"
                        "GtkBuildable")
                       :TYPE-INITIALIZER "gtk_toggle_button_get_type")
                      ((ACTIVE TOGGLE-BUTTON-ACTIVE "active" "gboolean" T T)
                       (DRAW-INDICATOR TOGGLE-BUTTON-DRAW-INDICATOR
                        "draw-indicator" "gboolean" T T)
                       (INCONSISTENT TOGGLE-BUTTON-INCONSISTENT
                        "inconsistent" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkToggleButton"))))

;;; --- Properties -------------------------------------------------------------

;;;     active
;;;     draw-indicator
;;;     inconsistent

(test gtk-toggle-button-properties
  (glib-test:with-check-memory (button)
    (is (typep (setf button (make-instance 'gtk:toggle-button)) 'gtk:toggle-button))
    (is-false (gtk:toggle-button-active button))
    (is-false (gtk:toggle-button-draw-indicator button))
    (is-false (gtk:toggle-button-inconsistent button))))

;;; --- Signals ----------------------------------------------------------------

;;;     toggled

(test gtk-toggle-button-toggled-signal
  (let* ((name "toggled")
         (gtype (g:gtype "GtkToggleButton"))
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

;;; --- Functions --------------------------------------------------------------

;;;     gtk_toggle_button_new

(test gtk-toggle-button-new
  (glib-test:with-check-memory (button)
    (is (typep (setf button (gtk:toggle-button-new)) 'gtk:toggle-button))))

;;;     gtk_toggle_button_new_with_label

(test gtk-toggle-button-new-with-label
  (glib-test:with-check-memory (button)
    (is (typep (setf button
                     (gtk:toggle-button-new-with-label "label")) 'gtk:toggle-button))))

;;;     gtk_toggle_button_new_with_mnemonic

(test gtk-toggle-button-new-with-mnemonic
  (glib-test:with-check-memory (button)
    (is (typep (setf button
                     (gtk:toggle-button-new-with-mnemonic "_label")) 'gtk:toggle-button))))

;;;     gtk_toggle_button_set_mode
;;;     gtk_toggle_button_get_mode
;;;     gtk_toggle_button_toggled

;;; 2026-06-26
