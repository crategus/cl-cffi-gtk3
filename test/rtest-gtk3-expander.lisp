(in-package :gtk-test)

(def-suite gtk-expander :in gtk-suite)
(in-suite gtk-expander)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkExpander

(test gtk-expander-class
  ;; Check type
  (is (g:type-is-object "GtkExpander"))
  ;; Check registered name
  (is (eq 'gtk:expander
          (glib:symbol-for-gtype "GtkExpander")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkExpander")
          (g:gtype (cffi:foreign-funcall "gtk_expander_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkBin")
          (g:type-parent "GtkExpander")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkExpander")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkExpander")))
  ;; Check class properties
  (is (equal '("expanded" "label" "label-fill" "label-widget" "resize-toplevel"
               "spacing" "use-markup" "use-underline")
             (glib-test:list-properties "GtkExpander")))
  ;; Check style properties
  (is (equal '("expander-size" "expander-spacing")
             (gtk-test:list-style-properties "GtkExpander")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkExpander")))
  ;; Check signals
  (is (equal '("activate")
             (glib-test:list-signals "GtkExpander")))
  ;; Check CSS information
  (is (string= "expander"
               (gtk:widget-class-css-name "GtkExpander")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkExpander" GTK:EXPANDER
                      (:SUPERCLASS GTK:BIN
                       :EXPORT T
                       :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                       :TYPE-INITIALIZER "gtk_expander_get_type")
                      ((EXPANDED EXPANDER-EXPANDED "expanded" "gboolean" T T)
                       (LABEL EXPANDER-LABEL "label" "gchararray" T T)
                       (LABEL-FILL EXPANDER-LABEL-FILL "label-fill" "gboolean" T T)
                       (LABEL-WIDGET EXPANDER-LABEL-WIDGET "label-widget"
                        "GtkWidget" T T)
                       (RESIZE-TOPLEVEL EXPANDER-RESIZE-TOPLEVEL
                        "resize-toplevel" "gboolean" T T)
                       (SPACING EXPANDER-SPACING "spacing" "gint" T T)
                       (USE-MARKUP EXPANDER-USE-MARKUP "use-markup" "gboolean" T T)
                       (USE-UNDERLINE EXPANDER-USE-UNDERLINE "use-underline"
                        "gboolean" T T)))
             (gobject:get-gtype-definition "GtkExpander"))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate

(test gtk-expander-activate-signal
  (let* ((name "activate")
         (gtype (g:gtype "GtkExpander"))
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

;;;     expanded
;;;     label
;;;     label-fill
;;;     label-widget
;;;     resize-toplevel
;;;     spacing
;;;     use-markup
;;;     use-underline

(test gtk-expander-properties
  (glib-test:with-check-memory (expander label)
    (setf expander (gtk:expander-new "label"))
    (setf label (gtk:label-new "widget"))
    ;; Propertery EXPANDED
    (is-false (gtk:expander-expanded expander))
    (is-true (setf (gtk:expander-expanded expander) t))
    ;; Property LABEL
    (is (string= "label" (gtk:expander-label expander)))
    (is (string= "text" (setf (gtk:expander-label expander) "text")))
    ;; Property LABEL-FILL
    (is-false (gtk:expander-label-fill expander))
    (is-true (setf (gtk:expander-label-fill expander) t))
    ;; Property LABEL-WIDGET
    (is (eq label (setf (gtk:expander-label-widget expander) label)))
    (is (eq label (gtk:expander-label-widget expander)))
    ;; Property RESIZE-TOPLEVEL
    (is-false (gtk:expander-resize-toplevel expander))
    (is-true (setf (gtk:expander-resize-toplevel expander) t))
    ;; Property SPACING
    (is (= 0 (gtk:expander-spacing expander)))
    (is (= 6 (setf (gtk:expander-spacing expander) 6)))
    ;; Property USE-MARKUP
    (is-false (gtk:expander-use-markup expander))
    (is-true (setf (gtk:expander-use-markup expander) t))
    ;; Property USE-UNDERLINE
    (is-false (gtk:expander-use-underline expander))
    (is-true (setf (gtk:expander-use-underline expander) t))
    ;; Remove label widget from expander
    (setf (gtk:expander-label-widget expander) nil)))

;;; --- Style Properties -------------------------------------------------------

;;;     expander-size
;;;     expander-spacing

(test gtk-expander-style-properties
  (glib-test:with-check-memory (expander)
    (setf expander (gtk:expander-new "label"))
    (is (= 10 (gtk:widget-style-property expander "expander-size")))
    (is (=  2 (gtk:widget-style-property expander "expander-spacing")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_expander_new

(test gtk-expander-new
  (glib-test:with-check-memory (expander)
    (is (typep (setf expander
                     (gtk:expander-new "label")) 'gtk:expander))))

;;;     gtk_expander_new_with_mnemonic

(test gtk-expander-new-with-mnemonic
  (glib-test:with-check-memory (expander)
    (is (typep (setf expander
                     (gtk:expander-new-with-mnemonic "_label")) 'gtk:expander))))

;;; 2026-06-16
