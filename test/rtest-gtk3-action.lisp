(in-package :gtk-test)

(def-suite gtk-action :in gtk-suite)
(in-suite gtk-action)

(defvar *verbose-gtk-action* nil)

;;;     GtkAction

(test gtk-action-class
  ;; Check type
  (is (g:type-is-object "GtkAction"))
  ;; Check registered name
  (is (eq 'gtk:action
          (glib:symbol-for-gtype "GtkAction")))
  ;; Check parent
  (is (eq (g:gtype "GObject") (g:type-parent "GtkAction")))
  ;; Check children
  (is (equal '("GtkRecentAction" "GtkToggleAction")
             (glib-test:list-children "GtkAction")))
  ;; Check interfaces
  (is (equal '("GtkBuildable")
             (glib-test:list-interfaces "GtkAction")))
  ;; Check class properties
  (is (equal '("action-group" "always-show-image" "gicon" "hide-if-empty"
               "icon-name" "is-important" "label" "name" "sensitive"
               "short-label" "stock-id" "tooltip" "visible" "visible-horizontal"
               "visible-overflown" "visible-vertical")
             (glib-test:list-properties "GtkAction")))
  ;; Check signals
  (is (equal '("activate")
             (glib-test:list-signals "GtkAction")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkAction" GTK:ACTION
                      (:SUPERCLASS G:OBJECT
                       :EXPORT T
                       :INTERFACES ("GtkBuildable")
                       :TYPE-INITIALIZER "gtk_action_get_type")
                      ((ACTION-GROUP ACTION-ACTION-GROUP
                        "action-group" "GtkActionGroup" T T)
                       (ALWAYS-SHOW-IMAGE ACTION-ALWAYS-SHOW-IMAGE
                        "always-show-image" "gboolean" T T)
                       (GICON ACTION-GICON "gicon" "GIcon" T T)
                       (HIDE-IF-EMPTY ACTION-HIDE-IF-EMPTY
                        "hide-if-empty" "gboolean" T T)
                       (ICON-NAME ACTION-ICON-NAME "icon-name" "gchararray" T T)
                       (IS-IMPORTANT ACTION-IS-IMPORTANT
                        "is-important" "gboolean" T T)
                       (LABEL ACTION-LABEL "label" "gchararray" T T)
                       (NAME ACTION-NAME "name" "gchararray" T NIL)
                       (SENSITIVE ACTION-SENSITIVE "sensitive" "gboolean" T T)
                       (SHORT-LABEL ACTION-SHORT-LABEL
                        "short-label" "gchararray" T T)
                       (STOCK-ID ACTION-STOCK-ID "stock-id" "gchararray" T T)
                       (TOOLTIP ACTION-TOOLTIP "tooltip" "gchararray" T T)
                       (VISIBLE ACTION-VISIBLE "visible" "gboolean" T T)
                       (VISIBLE-HORIZONTAL ACTION-VISIBLE-HORIZONTAL
                        "visible-horizontal" "gboolean" T T)
                       (VISIBLE-OVERFLOWN  ACTION-VISIBLE-OVERFLOWN
                        "visible-overflown" "gboolean" T T)
                       (VISIBLE-VERTICAL ACTION-VISIBLE-VERTICAL
                        "visible-vertical" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkAction"))))

;;; --- Signals ----------------------------------------------------------------

(test gtk-action-activate-signal
  (let* ((name "activate")
         (gtype (g:gtype "GtkAction"))
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
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

;;;     gtk:action-action-group

(test gtk-action-action-group
  (glib-test:with-check-memory (group action)
    (is (typep (setf group
                     (gtk:action-group-new "ActionGroup")) 'gtk:action-group))
    (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
    (is-false (gtk:action-action-group action))
    (is (eq group (setf (gtk:action-action-group action) group)))
    (is (eq group (gtk:action-action-group action)))))

;;;     gtk:action-always-show-image

(test gtk-action-always-show-image
  (glib-test:with-check-memory (action)
    (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
    (is-false (gtk:action-always-show-image action))
    (is-true (setf (gtk:action-always-show-image action) t))
    (is-true (gtk:action-always-show-image action))))

;;;     gtk:action-gicon

(test gtk-action-gicon
  (glib-test:with-check-memory (action gicon)
    (let ((icon "edit-find"))
      (is (typep (setf gicon (g:themed-icon-new icon)) 'g:themed-icon))
      (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
      (is-false (gtk:action-gicon action))
      (is (eq gicon (setf (gtk:action-gicon action) gicon)))
      (is (eq gicon (gtk:action-gicon action)))
      ;; Remove references
      (is-false (setf (gtk:action-gicon action) nil)))))

;;;     gtk:action-hide-if-empty

(test gtk-action-hide-if-empty
  (glib-test:with-check-memory (action)
    (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
    (is-true (gtk:action-hide-if-empty action))
    (is-false (setf (gtk:action-hide-if-empty action) nil))
    (is-false (gtk:action-hide-if-empty action))))

;;;     gtk:action-icon-name

(test gtk-action-icon-name
  (glib-test:with-check-memory (action)
    (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
    (is-false (gtk:action-icon-name action))
    (is (string= "edit-find" (setf (gtk:action-icon-name action) "edit-find")))
    (is (string= "edit-find" (gtk:action-icon-name action)))))

;;;     gtk:action-is-important

(test gtk-action-is-important
  (glib-test:with-check-memory (action)
    (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
    (is-false (gtk:action-is-important action))
    (is-true (setf (gtk:action-is-important action) t))
    (is-true (gtk:action-is-important action))))

;;;     gtk:action-label

(test gtk-action-label
  (glib-test:with-check-memory (action)
    (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
    (is-false (gtk:action-label action))
    (is (string= "label" (setf (gtk:action-label action) "label")))
    (is (string= "label" (gtk:action-label action)))))

;;;     gtk:action-name

(test gtk-action-name
  (glib-test:with-check-memory (action)
    (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
    (is (string= "action" (gtk:action-name action)))))

;;;     gtk:action-sensitive

(test gtk-action-sensitive
  (glib-test:with-check-memory (action)
    (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
    (is-true (gtk:action-sensitive action))
    (setf (gtk:action-sensitive action) nil)
    (is-false (gtk:action-sensitive action))))

;;;     gtk:action-short-label

(test gtk-action-short-label
  (glib-test:with-check-memory (action)
    (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
    (is-false (gtk:action-short-label action))
    (is (string= "label" (setf (gtk:action-short-label action) "label")))
    (is (string= "label" (gtk:action-short-label action)))))

;;;     gtk:action-stock-id

(test gtk-action-stock-id
  (glib-test:with-check-memory (action)
    (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
    (is-false (gtk:action-stock-id action))
    (is (string= "gtk-ok" (setf (gtk:action-stock-id action) "gtk-ok")))
    (is (string= "gtk-ok" (gtk:action-stock-id action)))))

;;;     tooltip

(test gtk-action-tooltip
  (glib-test:with-check-memory (action)
    (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
    (is-false (gtk:action-tooltip action))
    (is (string= "tooltip" (setf (gtk:action-tooltip action) "tooltip")))
    (is (string= "tooltip" (gtk:action-tooltip action)))))

;;;     gtk:action-visible

(test gtk-action-visible
  (glib-test:with-check-memory (action)
    (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
    (is-true (gtk:action-visible action))
    (setf (gtk:action-visible action) nil)
    (is-false (gtk:action-visible action))))

;;;     visible-horizontal

(test gtk-action-visible-horizontal
  (glib-test:with-check-memory (action)
    (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
    (is-true (gtk:action-visible-horizontal action))
    (setf (gtk:action-visible-horizontal action) nil)
    (is-false (gtk:action-visible-horizontal action))))

;;;     visible-overflown

(test gtk-action-visible-overflown
  (glib-test:with-check-memory (action)
    (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
    (is-true (gtk:action-visible-overflown action))
    (setf (gtk:action-visible-overflown action) nil)
    (is-false (gtk:action-visible-overflown action))))

;;;     visible-vertical

(test gtk-action-visible-vertical
  (glib-test:with-check-memory (action)
    (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
    (is-true (gtk:action-visible-vertical action))
    (setf (gtk:action-visible-vertical action) nil)
    (is-false (gtk:action-visible-vertical action))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_action_new

(test gtk-action-new.1
  (glib-test:with-check-memory (action)
    (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
    (is (string= "action" (gtk:action-name action)))
    (is-false (gtk:action-label action))
    (is-false (gtk:action-tooltip action))
    (is-false (gtk:action-stock-id action))))

(test gtk-action-new.2
  (glib-test:with-check-memory (action)
    (is (typep (setf action
                     (gtk:action-new "action" "label" "tooltip" "stock-id"))
               'gtk:action))
    (is (string= "action" (gtk:action-name action)))
    (is (string= "label" (gtk:action-label action)))
    (is (string= "tooltip" (gtk:action-tooltip action)))
    (is (string= "stock-id" (gtk:action-stock-id action)))))

;;;     gtk_action_is_sensitive

(test gtk-action-is-sensitive
  (glib-test:with-check-memory (action)
    (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
    (is-true (gtk:action-is-sensitive action))
    (setf (gtk:action-sensitive action) nil)
    (is-false (gtk:action-is-sensitive action))))

;;;     gtk_action_is_visible

(test gtk-action-is-visible
  (glib-test:with-check-memory (action)
    (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
    (is-true (gtk:action-is-visible action))
    (setf (gtk:action-visible action) nil)
    (is-false (gtk:action-is-visible action))))

;;;     gtk_action_activate

;; FIXME: This test hangs the testsuite on Windows

#-windows
(test gtk-action-activate
  (let ((message nil))
    (gtk:within-main-loop
      (let ((action (gtk:action-new "action")))
        (g:signal-connect action "activate"
           (lambda (action)
             (setf message "ACTIVATE CALLED")
             (when *verbose-gtk-action*
               (format t "~&Signal ACTIVATE for ~a~%" (gtk:action-name action)))
             (gtk:leave-gtk-main)))
        (gtk:action-activate action)))
    (gtk:join-gtk-main)
    (is (string= "ACTIVATE CALLED" message))))

;;;     gtk_action_create_icon

(test gtk-action-create-icon
  (glib-test:with-check-memory (action)
    (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
    ;; Check for a stock-id and for icon-name and gicon
    (setf (gtk:action-stock-id action) "gtk-ok")
    (is (typep (gtk:action-create-icon action :dialog) 'gtk:image))))

;;;     gtk_action_create_menu_item

#+nil
(test gtk-action-create-menu-item
  (glib-test:with-check-memory ((action 2) :strong 2)
    (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
    (is (typep (gtk:action-create-menu-item action) 'gtk:image-menu-item))))

;;;     gtk_action_create_tool_item

#+nil
(test gtk-action-create-tool-item
  (glib-test:with-check-memory ((action 2) :strong 2)
    (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
    (is (typep (gtk:action-create-tool-item action) 'gtk:tool-button))))

;;;     gtk_action_create_menu

;; TODO: Create a test for a result not nil

(test gtk-action-create-menu
  (glib-test:with-check-memory (action)
    (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
    (is-false (gtk:action-create-menu action))))

;;;     gtk_action_get_proxies

#+nil
(test gtk-action-proxies
  (glib-test:with-check-memory ((action 3) :strong 3)
    (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
    (is-false (gtk:action-proxies action))
    ;; Add a tool item to list of proxies
    (gtk:action-create-tool-item action)
    (is (typep (first (gtk:action-proxies action)) 'gtk:tool-button))
    ;; Add a menu item to list of proxies
    (gtk:action-create-menu-item action)
    (is (typep (first (gtk:action-proxies action)) 'gtk:image-menu-item))))

;;;     gtk_action_connect_accelerator
;;;     gtk_action_disconnect_accelerator

#+nil
(test gtk-action-connect-accelerator
  (glib-test:with-check-memory ((group 2) action :strong 1)
    (is (typep (setf group (gtk:accel-group-new)) 'gtk:accel-group))
    (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
    (is (string= "<test>/File/Exit"
                 (setf (gtk:action-accel-path action) "<test>/File/Exit")))
    (is-false (gtk:action-set-accel-group action group))
    (is-false (gtk:action-connect-accelerator action))
    (is-false (gtk:action-disconnect-accelerator action))))

;;;      gtk_action_block_activate
;;;      gtk_action_unblock_activate

;; TODO: This example does not work. The code hangs.

#+nil
(test gtk-action-block/unblock-activate
  (let ((message nil))
    (gtk:within-main-loop
      (let ((action (gtk:action-new "action")))
        (g:signal-connect action "activate"
           (lambda (action)
             (setf message "ACTIVATE CALLED")
             (when *verbose-gtk-action*
               (format t "~&Signal ACTIVATE for ~a~%" (gtk:action-name action)))
             (gtk:leave-gtk-main)))
        (gtk:action-block-activate action)
        (gtk:action-activate action)
        (gtk:action-unblock-activate action)))
    (gtk:join-gtk-main)
    (is (string= "ACTIVATE CALLED" message))))

;;;      gtk_action_get_accel_path
;;;      gtk_action_set_accel_path

(test gtk-action-accel-path
  (glib-test:with-check-memory (action)
    (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
    (is-false (gtk:action-accel-path action))
    (is (string= "<test>/File/Exit"
                 (setf (gtk:action-accel-path action) "<test>/File/Exit")))
    (is (string= "<test>/File/Exit" (gtk:action-accel-path action)))))

;;;      gtk_action_get_accel_closure                       not exported

;;;      gtk_action_set_accel_group

(test gtk-action-set-accel-group
  (glib-test:with-check-memory (action group)
    (is (typep (setf action (gtk:action-new "action")) 'gtk:action))
    (is (typep (setf group (gtk:accel-group-new)) 'gtk:accel-group))
    (is-false (gtk:action-set-accel-group action group))
    ;; Remove references
    (is-false (gtk:action-set-accel-group action nil))))

;;; 2025-06-05
