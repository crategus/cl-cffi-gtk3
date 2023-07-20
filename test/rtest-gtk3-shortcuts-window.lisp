(in-package :gtk-test)

(def-suite gtk-shortcuts-window :in gtk-suite)
(in-suite gtk-shortcuts-window)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkShortcutsWindow

(test gtk-shortcuts-window-class
  ;; Type check
  (is (g:type-is-object "GtkShortcutsWindow"))
  ;; Check the registered name
  (is (eq 'gtk:shortcuts-window
          (glib:symbol-for-gtype "GtkShortcutsWindow")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkShortcutsWindow")
          (g:gtype (cffi:foreign-funcall "gtk_shortcuts_window_get_type" 
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWindow")
          (g:type-parent "GtkShortcutsWindow")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkShortcutsWindow")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (list-interfaces "GtkShortcutsWindow")))
  ;; Check the class properties
  (is (equal '("section-name" "view-name")
             (list-properties "GtkShortcutsWindow")))
  ;; Check the style properties
  (is (equal '()
             (list-style-properties "GtkShortcutsWindow")))
  ;; Check the child properties
  (is (equal '()
             (list-child-properties "GtkShortcutsWindow")))
  ;; Check the signals
  (is (equal '("close" "search")
             (list-signals "GtkShortcutsWindow")))
  ;; CSS information
  (is (string= "window"
               (gtk:widget-class-css-name "GtkShortcutsWindow")))
  (is (string=
"[window.background.csd:dir(ltr)]
  decoration:dir(ltr)
  headerbar.titlebar:dir(ltr)
    stack:dir(ltr)
      label.title:dir(ltr)
      label.title:dir(ltr)
      button.flat.toggle.popup:dir(ltr)
        box.horizontal:dir(ltr)
          label:dir(ltr)
          widget:dir(ltr)
    button.image-button.toggle:dir(ltr)
      image:dir(ltr)
    box.right.horizontal:dir(ltr)
      [separator.vertical.titlebutton:dir(ltr)]
      button.close.titlebutton:dir(ltr)
        image:dir(ltr)
  box.vertical:dir(ltr)
    searchbar:dir(ltr)
      revealer:dir(ltr)
        box.horizontal:dir(ltr)
          box.vertical:dir(ltr)
          box.vertical:dir(ltr)
            entry.search:dir(ltr)
              image.left:disabled:dir(ltr)
              undershoot.left:dir(ltr)
              undershoot.right:dir(ltr)
          box.vertical:dir(ltr)
            [button.close.flat:dir(ltr)]
              image:dir(ltr)
    stack:dir(ltr)
      scrolledwindow:dir(ltr)
        overshoot.left:dir(ltr)
        undershoot.left:dir(ltr)
        overshoot.right:dir(ltr)
        undershoot.right:dir(ltr)
        overshoot.top:dir(ltr)
        undershoot.top:dir(ltr)
        overshoot.bottom:dir(ltr)
        undershoot.bottom:dir(ltr)
        scrollbar.bottom.horizontal:dir(ltr)
          contents
            trough:dir(ltr)
              slider:dir(ltr)
        scrollbar.vertical.right:dir(ltr)
          contents
            trough:dir(ltr)
              slider:dir(ltr)
        viewport.frame:dir(ltr)
          box.vertical:dir(ltr)
            box.vertical:dir(ltr)
            box.vertical:dir(ltr)
      grid.dim-label.horizontal:dir(ltr)
        image:dir(ltr)
        label:dir(ltr)
        label:dir(ltr)
  [popover.background:dir(ltr)]
    list:dir(ltr)
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context
                       (make-instance 'gtk:shortcuts-window))
                   :recurse)))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkShortcutsWindow" 
                                             GTK-SHORTCUTS-WINDOW
                       (:SUPERCLASS GTK-WINDOW :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_shortcuts_window_get_type")
                       ((SECTION-NAME GTK-SHORTCUTS-WINDOW-SECTION-NAME
                         "section-name" "gchararray" T T)
                        (VIEW-NAME GTK-SHORTCUTS-WINDOW-VIEW-NAME "view-name"
                         "gchararray" T T)))
             (gobject:get-g-type-definition "GtkShortcutsWindow"))))

;;; --- Properties -------------------------------------------------------------

;;;     section-name
;;;     view-name

;;; --- Signals ----------------------------------------------------------------

;;;     close
;;;     search

;;; --- 2023-7-19 --------------------------------------------------------------
