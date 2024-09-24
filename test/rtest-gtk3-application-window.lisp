(in-package :gtk-test)

(def-suite gtk-application-window :in gtk-suite)
(in-suite gtk-application-window)

;;; --- GtkApplicationWindow ---------------------------------------------------

(test gtk-application-window-class
  ;; Check type
  (is (g:type-is-object "GtkApplicationWindow"))
  ;; Check registered name
  (is (eq 'gtk:application-window
          (glib:symbol-for-gtype "GtkApplicationWindow")))
  ;; Check parent
  (is (eq (g:gtype "GtkWindow") (g:type-parent "GtkApplicationWindow")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkApplicationWindow")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GActionGroup" "GActionMap")
             (glib-test:list-interfaces "GtkApplicationWindow")))
  ;; Check class properties
  (is (equal '("show-menubar")
             (glib-test:list-properties "GtkApplicationWindow")))
  ;; Check style properties.
  (is (equal '()
             (gtk-test:list-style-properties "GtkApplicationWindow")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkApplicationWindow")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkApplicationWindow")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkApplicationWindow"
                                      GTK:APPLICATION-WINDOW
                       (:SUPERCLASS GTK:WINDOW
                        :EXPORT T
                        :INTERFACES
                        ("AtkImplementorIface" "GActionGroup" "GActionMap"
                         "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_application_window_get_type")
                       ((SHOW-MENUBAR APPLICATION-WINDOW-SHOW-MENUBAR
                         "show-menubar" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkApplicationWindow"))))

;;; --- Properties and Accessors -----------------------------------------------

;;; --- gtk-application-window-show-menubar ------------------------------------

(test gtk-application-window-show-menubar
  (let ((window (make-instance 'gtk:application-window)))
    ;; Default value is true
    (is-true  (gtk:application-window-show-menubar window))
    ;; Set show-menubar property to nil
    (setf (gtk:application-window-show-menubar window) nil)
    (is-false (gtk:application-window-show-menubar window))))

;;; --- Functions --------------------------------------------------------------

;;; --- gtk-application-window-new ---------------------------------------------

(test gtk-application-window-new
  (let ((application (make-instance 'gtk:application)))
    (is (typep application 'gtk:application))
    ;; Works only for a registered application
;    (is (typep (gtk:application-window-new application) 'gtk:application-window))
    ;; Create a window with make-instance
    (is (typep (make-instance 'gtk:application-window) 'gtk:application-window))))

;;; --- gtk-application-window-id ----------------------------------------------

(test gtk-application-window-id
  (let ((window (make-instance 'gtk:application-window)))
    ;; Zero if the window is not added to a GtkApplication
    (is (= 0 (gtk:application-window-id window)))))

;;; --- gtk_application_window_set_help_overlay --------------------------------
;;; --- gtk_application_window_get_help_overlay --------------------------------

(test gtk-application-window-help-overlay
  (let ((window (make-instance 'gtk:application-window))
        (help-overlay (make-instance 'gtk:shortcuts-window)))
    ;; Default value is nil
    (is-false (gtk:application-window-help-overlay window))
    ;; Set a GtkShortcutsWindow
    (setf (gtk:application-window-help-overlay window) help-overlay)
    ;; Retrieve the GtkShortcutsWindow
    (is (typep (gtk:application-window-help-overlay window)
               'gtk:shortcuts-window))))

;;; 2024-9-23
