(in-package :gtk-test)

(def-suite gtk-action-group :in gtk-suite)
(in-suite gtk-action-group)

(defvar *verbose-gtk-action-group* nil)

;;;     GtkActionGroup

(test gtk-action-group-class
  ;; Check type
  (is (g:type-is-object "GtkActionGroup"))
  ;; Check registered name
  (is (eq 'gtk:action-group
          (glib:symbol-for-gtype "GtkActionGroup")))
  ;; Check parent
  (is (eq (g:gtype "GObject") (g:type-parent "GtkActionGroup")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkActionGroup")))
  ;; Check interfaces
  (is (equal '("GtkBuildable")
             (glib-test:list-interfaces "GtkActionGroup")))
  ;; Check class properties
  (is (equal '("accel-group" "name" "sensitive" "visible")
             (glib-test:list-properties "GtkActionGroup")))
  ;; Check signals
  (is (equal '("connect-proxy" "disconnect-proxy" "post-activate"
               "pre-activate")
             (glib-test:list-signals "GtkActionGroup")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkActionGroup" GTK:ACTION-GROUP
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES ("GtkBuildable")
                        :TYPE-INITIALIZER "gtk_action_group_get_type")
                       ((ACCEL-GROUP ACTION-GROUP-ACCEL-GROUP
                         "accel-group" "GtkAccelGroup" T T)
                        (NAME ACTION-GROUP-NAME "name" "gchararray" T NIL)
                        (SENSITIVE ACTION-GROUP-SENSITIVE
                         "sensitive" "gboolean" T T)
                        (VISIBLE ACTION-GROUP-VISIBLE "visible" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkActionGroup"))))

;;; --- Signals ----------------------------------------------------------------

;;;     connect-proxy

(test gtk-action-connect-proxy-signal
  (let* ((name "connect-proxy")
         (gtype (g:gtype "GtkActionGroup"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '()
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("GtkAction" "GtkWidget")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     disconnect-proxy

(test gtk-action-disconnect-proxy-signal
  (let* ((name "disconnect-proxy")
         (gtype (g:gtype "GtkActionGroup"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '()
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("GtkAction" "GtkWidget")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     post-activate

(test gtk-action-post-activate-signal
  (let* ((name "post-activate")
         (gtype (g:gtype "GtkActionGroup"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '()
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("GtkAction")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     pre-activate

(test gtk-action-pre-activate-signal
  (let* ((name "pre-activate")
         (gtype (g:gtype "GtkActionGroup"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '()
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("GtkAction")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

;;;     gtk:action-group-accel-group

(test gtk-action-group-accel-group
  (let ((group (make-instance 'gtk:action-group :name "AppWindowActions")))
    ;; No accel group
    (is-false (gtk:action-group-accel-group group))
    ;; Set accel group
    (setf (gtk:action-group-accel-group group) (gtk:accel-group-new))
    (is (typep (gtk:action-group-accel-group group) 'gtk:accel-group))
    ;; Set nil
    (setf (gtk:action-group-accel-group group) nil)
    (is-false (gtk:action-group-accel-group group))))

;;;     gtk:action-group-name

(test gtk-action-group-name
  (let ((group (make-instance 'gtk:action-group :name "AppWindowActions")))
    (is (string= "AppWindowActions" (gtk:action-group-name group)))))

;;;     gtk:action-group-sensitive

(test gtk-action-group-sensitive
  (let ((group (make-instance 'gtk:action-group :name "AppWindowActions")))
    (is-true (gtk:action-group-sensitive group))
    (setf (gtk:action-group-sensitive group) nil)
    (is-false (gtk:action-group-sensitive group))))

;;;     gtk:action-group-visible

(test gtk-action-group-visible
  (let ((group (make-instance 'gtk:action-group :name "AppWindowActions")))
    (is-true (gtk:action-group-visible group))
    (setf (gtk:action-group-visible group) nil)
    (is-false (gtk:action-group-visible group))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_action_group_new

(test gtk-action-group-new
  (let (group)
    (is (typep (setf group
                     (gtk:action-group-new "ActionGroup"))
               'gtk:action-group))
  (is (string= "ActionGroup" (gtk:action-group-name group)))))

;;;     gtk_action_group_add_action
;;;     gtk_action_group_add_action_with_accel
;;;     gtk-action-group-list-actions
;;;     gtk_action_group_get_action
;;;     gtk-action-group-remove-action

(test gtk-action-group-add/remove/list/get
  (let ((group (gtk:action-group-new "ActionGroup"))
        (action1 (gtk:action-new "action1"))
        (action2 (gtk:action-new "action2"))
        (action3 (gtk:action-new "action3"))
        (action4 (gtk:action-new "action4")))
    ;; Add actions
    (is-false (gtk:action-group-add-action group action1))
    (is (eq action1 (gtk:action-group-action group "action1")))
    (is-false (gtk:action-group-add-action group action2 "<Control>a"))
    (is (eq action2 (gtk:action-group-action group "action2")))
    (is-false (gtk:action-group-add-action group action3 ""))
    (is (eq action3 (gtk:action-group-action group "action3")))
    (is-false (gtk:action-group-add-action group action4 nil))
    (is (eq action4 (gtk:action-group-action group "action4")))
    ;; List actions
    (is (equal '("action1" "action2" "action3" "action4")
               (sort (mapcar #'gtk:action-name
                             (gtk:action-group-list-actions group))
                     #'string<)))
    ;; Remove actions
    (is-false (gtk:action-group-remove-action group action1))
    (is-false (gtk:action-group-action group "action1"))
    (is (equal '("action2" "action3" "action4")
               (sort (mapcar #'gtk:action-name
                             (gtk:action-group-list-actions group))
                     #'string<)))))

;;;     gtk_action_group_add_actions
;;;     gtk_action_group_add_actions_full                   not implemented

(test gtk-action-group-add-actions
  (let ((group (gtk:action-group-new "AppWindowActions"))
        (actions '(("Open"            ; name
                    "gtk-stock-open"  ; stock-id
                    "_Open"           ; label
                    "<ctrl>o"         ; accelerator
                    "Open a file"     ; tooltip
                    nil)              ; callback function
                   ("Close"
                    "gtk-stock-close"
                    "_Close"
                    "<ctrl>c"
                    "Close a file"
                    nil))))
    (is-false (gtk:action-group-add-actions group actions))
    (is (equal '("Close" "Open")
               (sort (mapcar #'gtk:action-name
                             (gtk:action-group-list-actions group))
                     #'string<)))))

;;;      gtk_action_group_add_toggle_actions
;;;      gtk_action_group_add_toggle_actions_full           not implemented

(test gtk-action-group-add-toggle-actions
  (let ((group (gtk:action-group-new "AppWindowActions"))
        (actions '(("Open"            ; name
                    "gtk-stock-open"  ; stock-id
                    "_Open"           ; label
                    "<ctrl>o"         ; accelerator
                    "Open a file"     ; tooltip
                    nil)              ; callback function
                   ("Close"
                    "gtk-stock-close"
                    "_Close"
                    "<ctrl>c"
                    "Close a file"
                    nil))))
    (is-false (gtk:action-group-add-toggle-actions group actions))
    (is (equal '("Close" "Open")
               (sort (mapcar #'gtk:action-name
                             (gtk:action-group-list-actions group))
                     #'string<)))))

;;;      gtk_action_group_add_radio_actions
;;;      gtk_action_group_add_radio_actions-full            not implemented

(test gtk-action-group-add-radio-actions
  (let ((group (gtk:action-group-new "AppWindowActions"))
        (actions '(("Red" nil                      ; name, stock id
                    "_Red" "<control>R"            ; label, accelerator
                    "Blood" 0)                     ; tooltip, value
                   ("Green" nil                    ; name, stock id
                    "_Green" "<control>G"          ; label, accelerator
                    "Grass" 1)                     ; tooltip, value
                   ("Blue" nil                     ; name, stock id
                    "_Blue" "<control>B"           ; label, accelerator
                    "Sky" 2))))
    (is-false (gtk:action-group-add-radio-actions group actions 0 nil))
    (is (equal '("Blue" "Green" "Red")
               (sort (mapcar #'gtk:action-name
                             (gtk:action-group-list-actions group))
                     #'string<)))))

;;;      gtk_action_group_set_translate_func
;;;      gtk_action_group_set_translation_domain
;;;      gtk_action_group_translate_string

;;; 2024-9-24
