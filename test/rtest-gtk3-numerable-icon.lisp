(in-package :gtk-test)

(def-suite gtk-numerable-icon :in gtk-suite)
(in-suite gtk-numerable-icon)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkNumerableIcon

(test gtk-numerable-icon-class
  ;; Check type
  (is (g:type-is-object "GtkNumerableIcon"))
  ;; Check registered name
  (is (eq 'gtk:numerable-icon
          (glib:symbol-for-gtype "GtkNumerableIcon")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkNumerableIcon")
          (g:gtype (cffi:foreign-funcall "gtk_numerable_icon_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GEmblemedIcon")
          (g:type-parent "GtkNumerableIcon")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkNumerableIcon")))
  ;; Check interfaces
  (is (equal '("GIcon")
             (glib-test:list-interfaces "GtkNumerableIcon")))
  ;; Check class properties
  (is (equal '("background-icon" "background-icon-name" "count" "label"
               "style-context")
             (glib-test:list-properties "GtkNumerableIcon")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkNumerableIcon")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkNumerableIcon" GTK:NUMERABLE-ICON
                      (:SUPERCLASS G:EMBLEMED-ICON
                       :EXPORT T
                       :INTERFACES ("GIcon")
                       :TYPE-INITIALIZER "gtk_numerable_icon_get_type")
                      ((BACKGROUND-ICON NUMERABLE-ICON-BACKGROUND-ICON
                        "background-icon" "GIcon" T T)
                       (BACKGROUND-ICON-NAME
                        NUMERABLE-ICON-BACKGROUND-ICON-NAME
                        "background-icon-name" "gchararray" T T)
                       (COUNT NUMERABLE-ICON-COUNT "count" "gint" T T)
                       (LABEL NUMERABLE-ICON-LABEL "label" "gchararray" T T)
                       (STYLE-CONTEXT NUMERABLE-ICON-STYLE-CONTEXT
                        "style-context" "GtkStyleContext" T T)))
             (gobject:get-gtype-definition "GtkNumerableIcon"))))

;;; --- Properties -------------------------------------------------------------

;;;     background-icon
;;;     background-icon-name
;;;     count
;;;     label
;;;     style-context

(test gtk-numerable-icon-properties
  (glib-test:with-check-memory (icon)
    (setf icon (make-instance 'gtk:numerable-icon
                              :count 1))
    (is-false (gtk:numerable-icon-background-icon icon))
    (is-false (gtk:numerable-icon-background-icon-name icon))
    (is (= 1 (gtk:numerable-icon-count icon)))
    (is-false (gtk:numerable-icon-label icon))
    (is-false (gtk:numerable-icon-style-context icon))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_numerable_icon_new

(test gtk-numerable-icon-new
  (when *first-run-testsuite*
    (glib-test:with-check-memory (icon :strong 1)
      (is (typep (setf icon
                       (gtk:numerable-icon-new (g:themed-icon-new "gtk-ok")))
                 'gtk:numerable-icon)))))

;;;     gtk_numerable_icon_new_with_style_context

;; TODO: Improve the test. We get a warning:
;; (sbcl:8869): Gtk-CRITICAL **: 23:43:53.125: gtk_style_context_set_path:
;; assertion 'GTK_IS_CSS_PATH_NODE (root)' failed

#+nil
(test gtk-numerable-icon-new-with-style-context
  (let* ((widget (make-instance 'gtk:button))
         (context (gtk:widget-style-context widget))
         (icon (g:themed-icon-new "gtk-ok")))
    (is (typep (gtk:numerable-icon-new-with-style-context icon context)
               'gtk:numerable-icon))))

;;; 2024-12-31
