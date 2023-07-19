(in-package :gtk-test)

(def-suite gtk-hsv :in gtk-suite)
(in-suite gtk-hsv)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkHSV

(test gtk-hsv-class
  ;; Type check
  (is (g:type-is-object "GtkHSV"))
  ;; Check the registered name
  (is (eq 'gtk:hsv
          (glib:symbol-for-gtype "GtkHSV")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkHSV")
          (g:gtype (cffi:foreign-funcall "gtk_hsv_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkHSV")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkHSV")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (list-interfaces "GtkHSV")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GtkHSV")))
  ;; Check the style properties
  (is (equal '()
             (list-style-properties "GtkHSV")))
  ;; Check the signals
  (is (equal '("changed" "move")
             (list-signals "GtkHSV")))
  ;; CSS information
  (is (string= "widget"
               (gtk:widget-class-css-name "GtkHSV")))
  #-windows
  (is (string=
"[widget:dir(ltr)]
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context
                       (make-instance 'gtk:hsv))
                   :recurse)))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkHSV" GTK-H-S-V
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_hsv_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkHSV"))))

;;; --- Signals ----------------------------------------------------------------

;;;     changed

(test gtk-hsv-changed-signal
  (let ((query (g:signal-query (g:signal-lookup "changed" "GtkHSV"))))
    (is (string= "changed" (g:signal-query-signal-name query)))
    (is (string= "GtkHSV" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

;;;     move

(test gtk-hsv-move-signal
  (let ((query (g:signal-query (g:signal-lookup "move" "GtkHSV"))))
    (is (string= "move" (g:signal-query-signal-name query)))
    (is (string= "GtkHSV" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("GtkDirectionType")
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_hsv_new

(test gtk-hsv-new
  (is (typep (gtk:hsv-new) 'gtk:hsv)))

;;;     gtk_hsv_set_color
;;;     gtk_hsv_get_color

(test gtk-hsv-color
  (let ((hsv (gtk:hsv-new)))
    (is-false (gtk:hsv-set-color hsv 0.5d0 0.6d0 0.7d0))
    (is (equal '(0.5d0 0.6d0 0.7d0) 
               (multiple-value-list (gtk:hsv-get-color hsv))))))

;;;     gtk_hsv_set_metrics
;;;     gtk_hsv_get_metrics

(test gtk-hsv-metrics
  (let ((hsv (gtk:hsv-new)))
    (is-false (gtk:hsv-set-metrics hsv 500 20))
    (is (equal '(500 20) (multiple-value-list (gtk:hsv-get-metrics hsv))))))

;;;     gtk_hsv_is_adjusting

;;;     gtk_hsv_to_rgb

(test gtk-rgb-to-hsv
  (is (equal '(0.0d0 0.0d0 0.0d0) 
             (multiple-value-list (gtk:hsv-to-rgb 0 0.0 0.0d0))))
  (is (equal '(0.0d0 0.0d0 0.0d0) 
             (multiple-value-list (gtk:hsv-to-rgb 0 0.0 1.0d0)))))

;;;     gtk_rgb_to_hsv

(test gtk-rgb-to-hsv
  (is (equal '(0.0d0 0.0d0 0.0d0) 
             (multiple-value-list (gtk:rgb-to-hsv 0 0.0 0.0d0))))
  (is (equal '(0.0d0 0.0d0 1.0d0) 
             (multiple-value-list (gtk:rgb-to-hsv 1 1.0 1.0d0)))))

;;; --- 2023-6-15 --------------------------------------------------------------
