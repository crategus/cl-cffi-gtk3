(in-package :gtk-test)

(def-suite gtk-tool-shell :in gtk-suite)
(in-suite gtk-tool-shell)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkToolShell

(test gtk-tool-shell-interface
  ;; Check type
  (is (g:type-is-interface "GtkToolShell"))
  ;; Check registered name
  (is (eq 'gtk:tool-shell
          (glib:symbol-for-gtype "GtkToolShell")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkToolShell")
          (g:gtype (cffi:foreign-funcall "gtk_tool_shell_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GtkWidget")
             (glib-test:list-interface-prerequisites "GtkToolShell")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GtkToolShell")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkToolShell")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkToolShell" GTK:TOOL-SHELL
                      (:EXPORT T
                       :TYPE-INITIALIZER "gtk_tool_shell_get_type"))
             (gobject:get-gtype-definition "GtkToolShell"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_tool_shell_get_ellipsize_mode
;;;     gtk_tool_shell_get_icon_size
;;;     gtk_tool_shell_get_orientation
;;;     gtk_tool_shell_get_relief_style
;;;     gtk_tool_shell_get_style
;;;     gtk_tool_shell_get_text_alignment
;;;     gtk_tool_shell_get_text_orientation
;;;     gtk_tool_shell_get_text_size_group

(test gtk-tool-shell-get
  (glib-test:with-check-memory (toolbar)
    (is (typep (setf toolbar (gtk:toolbar-new)) 'gtk:toolbar))
    (is (eq :none (gtk:tool-shell-ellipsize-mode toolbar)))
    (is (eq :large-toolbar (gtk:tool-shell-icon-size toolbar)))
    (is (eq :horizontal (gtk:tool-shell-orientation toolbar)))
    (is (eq :none (gtk:tool-shell-relief-style toolbar)))
    (is (eq :both-horiz (gtk:tool-shell-style toolbar)))
    (is (= 0.5 (gtk:tool-shell-text-alignment toolbar)))
    (is (eq :horizontal (gtk:tool-shell-text-orientation toolbar)))
    (is-false (gtk:tool-shell-text-size-group toolbar))))

;;;     gtk_tool_shell_rebuild_menu

(test gtk-tool-shell-rebuild-name
  (glib-test:with-check-memory (toolbar)
    (is (typep (setf toolbar (gtk:toolbar-new)) 'gtk:toolbar))
    (is-false (gtk:tool-shell-rebuild-menu toolbar))))

;;; 2026-05-29
