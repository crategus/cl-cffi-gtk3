(in-package :gtk-test)

(def-suite gtk-scale-button :in gtk-suite)
(in-suite gtk-scale-button)

;;; ---Types and Values --------------------------------------------------------

;;;     GtkScaleButton

(test gtk-scale-button-class
  ;; Check type
  (is (g:type-is-object "GtkScaleButton"))
  ;; Check registered name
  (is (eq 'gtk:scale-button
          (glib:symbol-for-gtype "GtkScaleButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkScaleButton")
          (g:gtype (cffi:foreign-funcall "gtk_scale_button_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkButton")
          (g:type-parent "GtkScaleButton")))
  ;; Check children
  (is (equal '("GtkVolumeButton")
             (glib-test:list-children "GtkScaleButton")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkActionable"
               "GtkActivatable" "GtkOrientable")
             (glib-test:list-interfaces "GtkScaleButton")))
  ;; Check class properties
  (is (equal '("adjustment" "icons" "orientation" "size" "value")
             (glib-test:list-properties "GtkScaleButton")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkScaleButton")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkScaleButton")))
  ;; Check signals
  (is (equal '("popdown" "popup" "value-changed")
             (glib-test:list-signals "GtkScaleButton")))
  ;; Check CSS information
  (is (string= "button"
               (gtk:widget-class-css-name "GtkScaleButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkScaleButton" GTK:SCALE-BUTTON
                      (:SUPERCLASS GTK:BUTTON
                       :EXPORT T
                       :INTERFACES
                       ("AtkImplementorIface" "GtkActionable"
                        "GtkActivatable" "GtkBuildable" "GtkOrientable")
                       :TYPE-INITIALIZER "gtk_scale_button_get_type")
                      ((ADJUSTMENT SCALE-BUTTON-ADJUSTMENT "adjustment"
                        "GtkAdjustment" T T)
                       (ICONS SCALE-BUTTON-ICONS "icons" "GStrv" T T)
                       (SIZE SCALE-BUTTON-SIZE "size" "GtkIconSize" T T)
                       (VALUE SCALE-BUTTON-VALUE "value" "gdouble" T T)))
             (gobject:get-gtype-definition "GtkScaleButton"))))

;;; --- Signals ----------------------------------------------------------------

;;;     popdown

(test gtk-scale-button-popdown-signal
  (let* ((name "popdown")
         (gtype (g:gtype "GtkScaleButton"))
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

;;;     popup

(test gtk-scale-button-popup-signal
  (let* ((name "popup")
         (gtype (g:gtype "GtkScaleButton"))
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

;;;     value-changed

(test gtk-scale-button-value-changed-signal
  (let* ((name "value-changed")
         (gtype (g:gtype "GtkScaleButton"))
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
    (is (equal '("gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

;;;     adjustment
;;;     icons
;;;     size
;;;     value

(test gtk-scale-button-properties
  (glib-test:with-check-memory (button :strong 1)
    (is (typep (setf button (make-instance 'gtk:scale-button)) 'gtk:scale-button))
    (is (typep (gtk:scale-button-adjustment button) 'gtk:adjustment))
    (is-false (gtk:scale-button-icons button))
    (is (eq :small-toolbar (gtk:scale-button-size button)))
    (is (= 0.0 (gtk:scale-button-value button)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_scale_button_new

(test gtk-scale-button-new
  (glib-test:with-check-memory (button :strong 1)
    (is (typep (setf button
                     (gtk:scale-button-new :dialog 10 100.0 7/2))
               'gtk:scale-button))))

;;;     gtk_scale_button_get_popup
;;;     gtk_scale_button_get_plus_button
;;;     gtk_scale_button_get_minus_button

(test gtk-scale-button-get
  (glib-test:with-check-memory (button :strong 4)
    (is (typep (setf button
                     (gtk:scale-button-new :dialog 10 100.0 7/2))
               'gtk:scale-button))
    (is (typep (gtk:scale-button-popup button) 'gtk:popover))
    (is (typep (gtk:scale-button-plus-button button) 'gtk:button))
    (is (typep (gtk:scale-button-minus-button button) 'gtk:button))))

;;; 2026-06-27
