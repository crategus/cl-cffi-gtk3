(in-package :gtk-test)

(def-suite gtk-application :in gtk-suite)
(in-suite gtk-application)

(defvar *verbose-gtk-application* nil)

;;; --- GtkApplicationInhibitFlags ---------------------------------------------

(test gtk-application-inhibit-flags
  ;; Check type
  (is (g:type-is-flags "GtkApplicationInhibitFlags"))
  ;; Check registered name
  (is (eq 'gtk:application-inhibit-flags
          (glib:symbol-for-gtype "GtkApplicationInhibitFlags")))
  ;; Check names
  (is (equal '("GTK_APPLICATION_INHIBIT_LOGOUT" "GTK_APPLICATION_INHIBIT_SWITCH"
               "GTK_APPLICATION_INHIBIT_SUSPEND" "GTK_APPLICATION_INHIBIT_IDLE")
             (glib-test:list-flags-item-names "GtkApplicationInhibitFlags")))
  ;; Check values
  (is (equal '(1 2 4 8)
             (glib-test:list-flags-item-values "GtkApplicationInhibitFlags")))
  ;; Check nick names
  (is (equal '("logout" "switch" "suspend" "idle")
             (glib-test:list-flags-item-nicks "GtkApplicationInhibitFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GtkApplicationInhibitFlags"
                                     GTK:APPLICATION-INHIBIT-FLAGS
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_application_inhibit_flags_get_type")
                                     (:LOGOUT 1)
                                     (:SWITCH 2)
                                     (:SUSPEND 4)
                                     (:IDLE 8))
             (gobject:get-gtype-definition "GtkApplicationInhibitFlags"))))

;;; --- GtkApplication ---------------------------------------------------------

(test gtk-application-class
  ;; Check
  (is (g:type-is-object "GtkApplication"))
  ;; Check registered name
  (is (eq 'gtk:application
          (glib:symbol-for-gtype "GtkApplication")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkApplication")
          (g:gtype (cffi:foreign-funcall "gtk_application_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GApplication") (g:type-parent "GtkApplication")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkApplication")))
  ;; Check interfaces
  (is (equal '("GActionGroup" "GActionMap")
             (glib-test:list-interfaces "GtkApplication")))
  ;; Check class properties
  (is (equal '("active-window" "app-menu" "menubar" "register-session"
               "screensaver-active")
             (glib-test:list-properties "GtkApplication")))
  ;; Check signals
  (is (equal '("query-end" "window-added" "window-removed")
             (glib-test:list-signals "GtkApplication")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkApplication" GTK:APPLICATION
                      (:SUPERCLASS G:APPLICATION
                       :EXPORT T
                       :INTERFACES ("GActionGroup" "GActionMap")
                       :TYPE-INITIALIZER "gtk_application_get_type")
                      ((ACTIVE-WINDOW APPLICATION-ACTIVE-WINDOW
                        "active-window" "GtkWindow" T NIL)
                       (APP-MENU APPLICATION-APP-MENU
                        "app-menu" "GMenuModel" T T)
                       (MENUBAR APPLICATION-MENUBAR
                        "menubar" "GMenuModel" T T)
                       (REGISTER-SESSION APPLICATION-REGISTER-SESSION
                        "register-session" "gboolean" T T)
                       (SCREENSAVER-ACTIVE APPLICATION-SCREENSAVER-ACTIVE
                        "screensaver-active" "gboolean" T NIL)))
             (gobject:get-gtype-definition "GtkApplication"))))

;;; --- Properties and Accessors -----------------------------------------------

(test gtk-application-properties
  (let ((message nil)
        (application (make-instance 'gtk:application
                                    :application-id "com.crategus.test"
                                    :flags :non-unique
                                    :register-session nil
                                    :inactivity-timeout 1000)))
    ;; Connect signal "startup"
    (g:signal-connect application "startup"
                      (lambda (app)
                        (declare (ignore app))
                        (when *verbose-gtk-application*
                          (format t "~&Application is in startup.~%"))))
    ;; TODO: Move the code to "startup" signal handler
    ;; Connect signal "activate"
    (g:signal-connect application "activate"
                      (lambda (app)
                        (g:application-hold app)
                        (push "activate" message)
                        (when *verbose-gtk-application*
                          (format t "~&Application is in activate.~%"))
                        ;; gtk-application-active-window
                        (is-false (gtk:application-active-window app))
                        ;; gtk-application-app-menu
                        (is-false (gtk:application-app-menu app))
                        (setf (gtk:application-app-menu app)
                              (make-instance 'g:menu))
                        (is (typep (gtk:application-app-menu app) 'g:menu))
                        (is-false (setf (gtk:application-app-menu app) nil))
                        ;; gtk-application-menubar
                        (is-false (gtk:application-menubar app))
                        (setf (gtk:application-menubar app)
                              (make-instance 'g:menu))
                        (is (typep (gtk:application-menubar app) 'g:menu))
                        (is-false (setf (gtk:application-menubar app) nil))
                        ;; gtk-application-register-session
                        (is-false (gtk:application-register-session app))
                        (setf (gtk:application-register-session app) t)
                        (is-true (gtk:application-register-session app))
                        ;; gtk-application-screensaver-active
                        (is-false (gtk:application-screensaver-active app))
                        (g:application-release app)))
    ;; Connect signal "shutdown"
    (g:signal-connect application "shutdown"
                      (lambda (app)
                        (declare (ignore app))
                        (push "shutdown" message)
                        (when *verbose-gtk-application*
                          (format t "~&Application is in shutdown.~%"))))
    ;; Run the application
    (g:application-run application nil)
    ;; Check the collected messages from the signal handlers
    (is (equal '("shutdown" "activate") message))))

;;; --- Signals ----------------------------------------------------------------

(test gtk-application-signals
  (let ((message nil)
        (application (make-instance 'gtk:application
                                    :application-id "com.crategus.test"
                                    :flags :non-unique
                                    :register-session t
                                    :inactivity-timeout 1000)))
    ;; Connect signal "query-end", will not be executed
    (g:signal-connect application "query-end"
                      (lambda (app)
                        (declare (ignore app))
                        (push "query-end" message)
                        (when *verbose-gtk-application*
                          (format t "~&Application is in query-end.~%"))))
    ;; Connect signal "window-added"
    (g:signal-connect application "window-added"
                      (lambda (app window)
                        (declare (ignore app window))
                        (push "window-added" message)
                        (when *verbose-gtk-application*
                          (format t "~&Application is in window-added.~%"))))
    ;; Connect signal "window-removed"
    (g:signal-connect application "window-removed"
                      (lambda (app window)
                        (declare (ignore app window))
                        (push "window-removed" message)
                        (when *verbose-gtk-application*
                          (format t "~&Application is in window-removed.~%"))))
    ;; Connect signal "activate"
    (g:signal-connect application "activate"
                      (lambda (app)
                        (g:application-hold app)
                        (push "activate" message)
                        (when *verbose-gtk-application*
                          (format t "~&Application is in activate.~%"))
                        ;; Code for emitting window-added and window-removed
                        (let ((window (make-instance 'gtk:application-window)))
                          ;; Add window to the application
                          (gtk:application-add-window app window)
                          ;; Remove window from the application
                          (gtk:application-remove-window app window)
                          (gtk:widget-destroy window))
                        (g:application-release app)))
    ;; Connect signal "shutdown"
    (g:signal-connect application "shutdown"
                      (lambda (app)
                        (declare (ignore app))
                        (push "shutdown" message)
                        (when *verbose-gtk-application*
                          (format t "~&Application is in shutdown.~%"))))
    ;; Run the application
    (g:application-run application nil)
    ;; Check the collected messages from the signal handlers
    (is (equal '("shutdown" "window-removed" "window-added" "activate")
               message))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_application_new

(test gtk-application-new
  (let ((application (gtk:application-new "com.crategus.test" '(:handles-open))))
    (is-true (g:application-id-is-valid "com.crategus.test"))
    (is (string= "com.crategus.test"
                 (g:application-application-id application)))
    (is (equal '(:handles-open) (g:application-flags application))))
  ;; Create application without ID
  (is-false (g:application-application-id
                (gtk:application-new nil '(:handles-open)))))

;;;     gtk_application_add_window
;;;     gtk_application_remove_window
;;;     gtk_application_get_windows
;;;     gtk_application_get_window_by_id

(test gtk-application-add-window
  (let ((message nil)
        (application (make-instance 'gtk:application
                                    :application-id "com.crategus.test"
                                    :flags :non-unique
                                    :register-session nil
                                    :inactivity-timeout 1000)))
    ;; Connect signal "window-added"
    (g:signal-connect application "window-added"
                      (lambda (app window)
                        (declare (ignore app window))
                        (push "window-added" message)
                        (when *verbose-gtk-application*
                          (format t "~&Application is in window-added.~%"))))
    ;; Connect signal "window-removed"
    (g:signal-connect application "window-removed"
                      (lambda (app window)
                        (declare (ignore app window))
                        (push "window-removed" message)
                        (when *verbose-gtk-application*
                          (format t "~&Application is in window-removed.~%"))))
    ;; Connect signal "activate"
    (g:signal-connect application "activate"
                      (lambda (app)
                        (g:application-hold app)
                        (push "activate" message)
                        (when *verbose-gtk-application*
                          (format t "~&Application is in activate.~%"))
                        ;; Code for emitting window-added and window-removed
                        (let ((window-id 0)
                              (window (make-instance 'gtk:application-window)))
                          ;; Add window to the application
                          (gtk:application-add-window app window)
                          ;; Get the window ID
                          (is (= 1 (setf window-id
                                         (gtk:application-window-id window))))
                          ;; Get the window by ID
                          (is (equal window
                                     (gtk:application-window-by-id app
                                                                   window-id)))
                          ;; Check the list of windows
                          (is-true (member window
                                           (gtk:application-windows app)
                                           :test #'eq))
                          ;; Remove window from the application
                          (gtk:application-remove-window app window)
                          (gtk:widget-destroy window))
                        (g:application-release app)))
    ;; Connect signal "shutdown"
    (g:signal-connect application "shutdown"
                      (lambda (app)
                        (declare (ignore app))
                        (push "shutdown" message)
                        (when *verbose-gtk-application*
                          (format t "~&Application is in shutdown.~%"))))
    ;; Run the application
    (g:application-run application nil)
    ;; Check the collected messages from the signal handlers
    (is (equal '("shutdown" "window-removed" "window-added" "activate")
               message))))

;;;     gtk_application_inhibit
;;;     gtk_application_uninhibit
;;;     gtk_application_is_inhibited

;;;     gtk_application_prefers_app_menu
;;;     gtk_application_get_menu_by_id

;;;     TODO: Work out an example which uses gresources

;;;     gtk_application_add_accelerator
;;;     gtk_application_remove_accelerator

;;;     gtk_application_list_action_descriptions

(test gtk-application-list-action-descriptions
  (let ((application (make-instance 'gtk:application)))
    (is (equal '()
               (gtk:application-list-action-descriptions application)))
    (setf (gtk:application-accels-for-action application "win::close")
          '("<Control>q" "<Shift><Alt>F1" "<Release>z"))
    (is (equal '("win::close")
               (gtk:application-list-action-descriptions application)))))

;;;     gtk_application_get_accels_for_action
;;;     gtk_application_set_accels_for_action

(test gtk-application-accels-for-action
  (let ((application (make-instance 'gtk:application)))
    (is-false (gtk:application-accels-for-action application "win::close"))
    (setf (gtk:application-accels-for-action application "win::close")
          '("<Control>q" "<Shift><Alt>F1" "<Release>z"))
    (is (equal '("<Primary>q" "<Shift><Alt>F1" "<Release>z")
               (gtk:application-accels-for-action application "win::close")))))

;;;     gtk_application_get_actions_for_accel

(test gtk-application-actions-for-accel
  (let ((application (make-instance 'gtk:application)))
    (setf (gtk:application-accels-for-action application "win::close")
          '("<Control>q" "<Shift><Alt>F1" "<Release>z"))
    (is (equal '("win::close")
               (gtk:application-actions-for-accel application "<Control>q")))
    (setf (gtk:application-accels-for-action application "win::save")
          '("<Control>q"))
    (is (equal '("win::close" "win::save")
               (gtk:application-actions-for-accel application "<Control>q")))))

;;; 2025-06-02
