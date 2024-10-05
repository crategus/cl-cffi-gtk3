(in-package :gtk-test)

(def-suite gtk-radio-action :in gtk-suite)
(in-suite gtk-radio-action)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkRadioAction

(test gtk-radio-action-class
  ;; Check type
  (is (g:type-is-object "GtkRadioAction"))
  ;; Check registered name
  (is (eq 'gtk:radio-action
          (glib:symbol-for-gtype "GtkRadioAction")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkRadioAction")
          (g:gtype (cffi:foreign-funcall "gtk_radio_action_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkToggleAction")
          (g:type-parent "GtkRadioAction")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkRadioAction")))
  ;; Check interfaces
  (is (equal '("GtkBuildable")
             (glib-test:list-interfaces "GtkRadioAction")))
  ;; Check class properties
  (is (equal '("current-value" "group" "value")
             (glib-test:list-properties "GtkRadioAction")))
  ;; Check signals
  (is (equal '("changed")
             (glib-test:list-signals "GtkRadioAction")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkRadioAction" GTK:RADIO-ACTION
                       (:SUPERCLASS GTK:TOGGLE-ACTION
                        :EXPORT T
                        :INTERFACES ("GtkBuildable")
                        :TYPE-INITIALIZER "gtk_radio_action_get_type")
                       ((CURRENT-VALUE RADIO-ACTION-CURRENT-VALUE
                         "current-value" "gint" T T)
                        (GROUP RADIO-ACTION-GROUP "group" "GtkRadioAction" NIL T)
                        (VALUE RADIO-ACTION-VALUE "value" "gint" T T)))
             (gobject:get-gtype-definition "GtkRadioAction"))))

;;; --- Signals ----------------------------------------------------------------

;;;     changed

(test gtk-radio-action-changed-signal
  (let* ((name "changed")
         (gtype (g:gtype "GtkRadioAction"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:NO-RECURSE :RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("GtkRadioAction")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

;;;     gtk:radio-action-current-value

(test gtk-radio-action-current-value
  (let ((action (gtk:radio-action-new "action")))
    (is (= 0 (gtk:radio-action-current-value action)))
    (is (= 1 (setf (gtk:radio-action-value action) 1)))
    (is (= 1 (setf (gtk:radio-action-current-value action) 1)))))

;;;     gtk:radio-action-group

;; TODO: Does not work in the Lisp binding.

#+nil
(test gtk-radio-action-group
  (let ((action (make-instance 'gtk:radio-action))
        (group (gtk:action-group-new "Group")))
    ;; Property group is not readable but writable
    (is (eq group (setf (gtk:radio-action-group action) group)))))

;;;     gtk:radio-action-value

(test gtk-radio-action-value
  (let ((action (make-instance 'gtk:radio-action)))
    (is (= 0 (gtk:radio-action-value action)))
    (is (= 1 (setf (gtk:radio-action-value action) 1)))
    (is (= 1 (gtk:radio-action-value action)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_radio_action_new

(test gtk-radio-action-new
  (is (typep (gtk:radio-action-new "action") 'gtk:radio-action)))

;;;     gtk_radio_action_join_group

(test gtk-radio-acton-join-group
  (let ((actions '(("action0" "label" "tooltip" "stock-id" 0)
                   ("action1" "label" "tooltip" "stock-id" 1)
                   ("action1" "label" "tooltip" "stock-id" 2)))
;        (group (gtk:action-group-new "Group"))
        (lastaction nil))
     (dolist (action actions)
       (setf action (apply #'gtk:radio-action-new action))
       (gtk:radio-action-join-group action lastaction)
       (setf lastaction action)
       (is-false (gtk:action-action-group action)))))

;;; 2024-9-26
