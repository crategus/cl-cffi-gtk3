(in-package :gtk-test)

(def-suite gtk-overlay :in gtk-suite)
(in-suite gtk-overlay)

;;; ---Types and Values --------------------------------------------------------

;;;     GtkOverlay

(test gtk-overlay-class
  ;; Check type
  (is (g:type-is-object "GtkOverlay"))
  ;; Check registered name
  (is (eq 'gtk:overlay
          (glib:symbol-for-gtype "GtkOverlay")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkOverlay")
          (g:gtype (cffi:foreign-funcall "gtk_overlay_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkBin")
          (g:type-parent "GtkOverlay")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkOverlay")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkOverlay")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkOverlay")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkOverlay")))
  ;; Check child properties
  (is (equal '("index" "pass-through")
             (gtk-test:list-child-properties "GtkOverlay")))
  ;; Check signals
  (is (equal '("get-child-position")
             (glib-test:list-signals "GtkOverlay")))
  ;; CSS information
  (is (string= "overlay"
               (gtk:widget-class-css-name "GtkOverlay")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkOverlay" GTK:OVERLAY
                      (:SUPERCLASS GTK:BIN
                       :EXPORT T
                       :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                       :TYPE-INITIALIZER "gtk_overlay_get_type")
                      NIL)
             (gobject:get-gtype-definition "GtkOverlay"))))

;;; --- Signals ----------------------------------------------------------------

;;;     get-child-position

(test gtk-overlay-get-child-position-signal
  (let* ((name "get-child-position")
         (gtype (g:gtype "GtkOverlay"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "gboolean") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GtkWidget" "GdkRectangle")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Child Properties -------------------------------------------------------

(test gtk-overlay-child-properties
  (glib-test:with-check-memory (overlay button)
    (setf overlay (make-instance 'gtk:overlay))
    (setf button (make-instance 'gtk:button))
    (is-false (gtk:overlay-add-overlay overlay button))
    (is (= 0 (gtk:overlay-child-index overlay button)))
    (is-false (gtk:overlay-child-pass-through overlay button))
    ;; Remove button from overlay
    (is-false (gtk:container-remove overlay button))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_overlay_new

(test gtk-overlay-new
  (glib-test:with-check-memory (overlay)
    (is (typep (setf overlay (gtk:overlay-new)) 'gtk:overlay))))

;;;     gtk_overlay_add_overlay
;;;     gtk_overlay_reorder_overlay

(test gtk-overlay-add/reorder-overlay
  (glib-test:with-check-memory (overlay button1 button2)
    (setf overlay (make-instance 'gtk:overlay))
    (setf button1 (make-instance 'gtk:button))
    (setf button2 (make-instance 'gtk:button))
    (is-false (gtk:overlay-add-overlay overlay button1))
    (is-false (gtk:overlay-add-overlay overlay button2))
    (is (= 0 (gtk:overlay-child-index overlay button1)))
    (is (= 1 (gtk:overlay-child-index overlay button2)))
    (is-false (gtk:overlay-reorder-overlay overlay button2 0))
    (is (= 1 (gtk:overlay-child-index overlay button1)))
    (is (= 0 (gtk:overlay-child-index overlay button2)))
    ;; Remove buttons from overlay
    (is-false (gtk:container-remove overlay button1))
    (is-false (gtk:container-remove overlay button2))))

;;; 2026-06-20
