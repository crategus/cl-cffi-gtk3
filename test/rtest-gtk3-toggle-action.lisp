(in-package :gtk-test)

(def-suite gtk-toggle-action :in gtk-suite)
(in-suite gtk-toggle-action)

(defvar *verbose-gtk-toggle-action* t)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkToggleAction

(test gtk-toggle-action-class
  ;; Check type
  (is (g:type-is-object "GtkToggleAction"))
  ;; Check registered name
  (is (eq 'gtk:toggle-action
          (glib:symbol-for-gtype "GtkToggleAction")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkToggleAction")
          (g:gtype (cffi:foreign-funcall "gtk_toggle_action_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkAction")
          (g:type-parent "GtkToggleAction")))
  ;; Check children
  (is (equal '("GtkRadioAction")
             (glib-test:list-children "GtkToggleAction")))
  ;; Check interfaces
  (is (equal '("GtkBuildable")
             (glib-test:list-interfaces "GtkToggleAction")))
  ;; Check class properties
  (is (equal '("active" "draw-as-radio")
             (glib-test:list-properties "GtkToggleAction")))
  ;; Check signals
  (is (equal '("toggled")
             (glib-test:list-signals "GtkToggleAction")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkToggleAction" GTK:TOGGLE-ACTION
                      (:SUPERCLASS GTK:ACTION
                       :EXPORT T
                       :INTERFACES ("GtkBuildable")
                       :TYPE-INITIALIZER "gtk_toggle_action_get_type")
                      ((ACTIVE TOGGLE-ACTION-ACTIVE "active" "gboolean" T T)
                       (DRAW-AS-RADIO TOGGLE-ACTION-DRAW-AS-RADIO
                        "draw-as-radio" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkToggleAction"))))

;;; --- Signals ----------------------------------------------------------------

(test gtk-toggle-action-toggled-signal
  (let* ((name "toggled")
         (gtype (g:gtype "GtkToggleAction"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

;;;     gtk:toggle-action-active

(test gtk-toggle-action-active
  (glib-test:with-check-memory (action)
    (is (typep (setf action (gtk:toggle-action-new "action")) 'gtk:action))
    (is-false (gtk:toggle-action-active action))
    (is-true (setf (gtk:toggle-action-active action) t))
    (is-true (gtk:toggle-action-active action))))

;;;     gtk:toggle-action-draw-as-radio

(test gtk-toggle-action-draw-as-radio
  (glib-test:with-check-memory (action)
    (is (typep (setf action (gtk:toggle-action-new "action")) 'gtk:action))
    (is-false (gtk:toggle-action-draw-as-radio action))
    (is-true (setf (gtk:toggle-action-draw-as-radio action) t))
    (is-true (gtk:toggle-action-draw-as-radio action))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_toggle_action_new

(test gtk-toggle-action-new
  (glib-test:with-check-memory (action)
    (is (typep (setf action
                     (gtk:toggle-action-new "action")) 'gtk:toggle-action))))

;;;     gtk_toggle_action_toggled

;; FIXME: This tests hangs the testsuite on Windows

#-windows
(test gtk-toggle-action-toggled
  (let ((message nil))
    (gtk:within-main-loop
      (let ((action (gtk:toggle-action-new "action")))
        (g:signal-connect action "toggled"
           (lambda (action)
             (setf message "TOGGLED CALLED")
             (when *verbose-gtk-toggle-action*
               (format t "~&Signal TOGGLED for ~a~%" (gtk:action-name action)))
             (gtk:leave-gtk-main)))
        (gtk:toggle-action-toggled action)))
    (gtk:join-gtk-main)
    (is (string= "TOGGLED CALLED" message))))

;;; 2025-06-05
