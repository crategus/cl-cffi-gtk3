(in-package :gtk-test)

(def-suite gtk-overlay :in gtk-suite)
(in-suite gtk-overlay)

;;; ---Types and Values --------------------------------------------------------

;;;     GtkOverlay

(test gtk-overlay-class
  ;; Type check
  (is (g:type-is-object "GtkOverlay"))
  ;; Check the registered name
  (is (eq 'gtk:overlay
          (glib:symbol-for-gtype "GtkOverlay")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkOverlay")
          (g:gtype (cffi:foreign-funcall "gtk_overlay_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkBin")
          (g:type-parent "GtkOverlay")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkOverlay")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (list-interfaces "GtkOverlay")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GtkOverlay")))
  ;; Check the style properties
  (is (equal '()
             (list-style-properties "GtkOverlay")))
  ;; Check the child properties
  (is (equal '("index" "pass-through")
             (list-child-properties "GtkOverlay")))
  ;; Check the signals
  (is (equal '("get-child-position")
             (list-signals "GtkOverlay")))
  ;; CSS information
  (is (string= "overlay"
               (gtk:widget-class-css-name "GtkOverlay")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkOverlay" GTK-OVERLAY
                               (:SUPERCLASS GTK-BIN :EXPORT T :INTERFACES
                                ("AtkImplementorIface" "GtkBuildable")
                                :TYPE-INITIALIZER "gtk_overlay_get_type")
                               NIL)
             (gobject:get-g-type-definition "GtkOverlay"))))

;;; --- Child Properties -------------------------------------------------------

;;;     index
;;;     pass-through

(test gtk-overlay-child-properties
  (let ((overlay (make-instance 'gtk:overlay))
        (button (make-instance 'gtk:button)))
    (is-false (gtk:overlay-add-overlay overlay button))
    (is (= 0 (gtk:overlay-child-index overlay button)))
    (is-false (gtk:overlay-child-pass-through overlay button))))

;;; --- Signals ----------------------------------------------------------------

;;;     get-child-position

(test gtk-overlay-get-child-position-signal
  (let ((query (g:signal-query (g:signal-lookup "get-child-position"
                                                "GtkOverlay"))))
    (is (string= "get-child-position" (g:signal-query-signal-name query)))
    (is (string= "GtkOverlay" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "gboolean" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("GdkRectangle" "GtkWidget")
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_overlay_new

(test gtk-overlay-new
  (is (typep (gtk:overlay-new) 'gtk:overlay)))

;;;     gtk_overlay_add_overlay
;;;     gtk_overlay_reorder_overlay

(test gtk-overlay-add/reorder-overlay
  (let ((overlay (make-instance 'gtk:overlay))
        (button1 (make-instance 'gtk:button))
        (button2 (make-instance 'gtk:button)))
    (is-false (gtk:overlay-add-overlay overlay button1))
    (is-false (gtk:overlay-add-overlay overlay button2))
    (is (= 0 (gtk:overlay-child-index overlay button1)))
    (is (= 1 (gtk:overlay-child-index overlay button2)))
    (is-false (gtk:overlay-reorder-overlay overlay button2 0))
    (is (= 1 (gtk:overlay-child-index overlay button1)))
    (is (= 0 (gtk:overlay-child-index overlay button2)))))

;;; 2024-1-1