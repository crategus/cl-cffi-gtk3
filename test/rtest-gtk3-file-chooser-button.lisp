(in-package :gtk-test)

(def-suite gtk-file-chooser-button :in gtk-suite)
(in-suite gtk-file-chooser-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileChooserButton

(test gtk-file-chooser-button-class
  ;; Type check
  (is (g:type-is-object "GtkFileChooserButton"))
  ;; Check the registered symbol
  (is (eq 'gtk:file-chooser-button
          (glib:symbol-for-gtype "GtkFileChooserButton")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFileChooserButton")
          (g:gtype (cffi:foreign-funcall "gtk_file_chooser_button_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkBox")
          (g:type-parent "GtkFileChooserButton")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkFileChooserButton")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable"
               "GtkFileChooser")
             (list-interfaces "GtkFileChooserButton")))
  ;; Check the class properties
  (is (equal '("action" "create-folders" "dialog" "do-overwrite-confirmation"
               "extra-widget" "filter" "local-only" "preview-widget"
               "preview-widget-active" "select-multiple" "show-hidden" "title"
               "use-preview-label" "width-chars")
             (list-properties "GtkFileChooserButton")))
  ;; Check the style properties
  (is (equal '()
             (list-style-properties "GtkFileChooserButton")))
  ;; Check the child properties
  (is (equal '("expand" "fill" "pack-type" "padding" "position")
             (list-child-properties "GtkFileChooserButton")))
  ;; Check the list of signals
  (is (equal '("file-set")
             (list-signals "GtkFileChooserButton")))
  ;; CSS information
  (is (string= "filechooserbutton"
               (gtk:widget-class-css-name "GtkFileChooserButton")))
  #-windows
  (is (string=
"[filechooserbutton.horizontal:dir(ltr)]
  button.file:dir(ltr)
    box.horizontal:dir(ltr)
      image:dir(ltr)
      label:dir(ltr)
      separator.vertical:dir(ltr)
      image:dir(ltr)
  [combobox:dir(ltr)]
    box.linked.horizontal:dir(ltr)
      button.combo:dir(ltr)
        box.horizontal:dir(ltr)
          cellview:dir(ltr)
          arrow:dir(ltr)
    [window.background.popup:dir(ltr)]
      decoration:dir(ltr)
      [menu#gtk-combobox-popup-menu:dir(ltr)]
        [arrow.top:dir(ltr)]
        menuitem:dir(ltr)
          cellview:dir(ltr)
        menuitem:dir(ltr)
          cellview:dir(ltr)
        menuitem:dir(ltr)
          cellview:dir(ltr)
        menuitem:dir(ltr)
          cellview:dir(ltr)
        menuitem:dir(ltr)
          cellview:dir(ltr)
        separator:dir(ltr)
        separator:dir(ltr)
        menuitem:dir(ltr)
          cellview:dir(ltr)
        menuitem:dir(ltr)
          cellview:dir(ltr)
        [arrow.bottom:dir(ltr)]
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context
                       (make-instance 'gtk:file-chooser-button))
                   :recurse)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkFileChooserButton"
                                     GTK-FILE-CHOOSER-BUTTON
                       (:SUPERCLASS GTK-BOX :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkFileChooser"
                         "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_file_chooser_button_get_type")
                       ((DIALOG GTK-FILE-CHOOSER-BUTTON-DIALOG "dialog"
                         "GtkFileChooser" NIL NIL)
                        (TITLE GTK-FILE-CHOOSER-BUTTON-TITLE "title"
                         "gchararray" T T)
                        (WIDTH-CHARS GTK-FILE-CHOOSER-BUTTON-WIDTH-CHARS
                         "width-chars" "gint" T T)))
             (gobject:get-g-type-definition "GtkFileChooserButton"))))

;;; --- Properties -------------------------------------------------------------

;;;     dialog
;;;     focus-on-click
;;;     title
;;;     width-chars

(test gtk-file-chooser-button-properties
  (let ((button (make-instance 'gtk:file-chooser-button)))
;    (signals(error) (gtk:file-chooser-button-dialog button))
    (is-true (gtk:file-chooser-button-focus-on-click button))
    (is (string= "Datei auswählen" (gtk:file-chooser-button-title button)))
    (is (= -1 (gtk:file-chooser-button-width-chars button)))))

;;; --- Signals ----------------------------------------------------------------

;;;     file-set

(test gtk-file-chooser-button-file-set-signal
  (let ((query (g:signal-query (g:signal-lookup "file-set"
                                                "GtkFileChooserButton"))))
    (is (string= "file-set" (g:signal-query-signal-name query)))
    (is (string= "GtkFileChooserButton"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_file_chooser_button_new

(test gtk-file-chooser-button-new.1
  (let ((button nil))
    (is (typep (setf button
                     (gtk:file-chooser-button-new "title" :open))
               'gtk:file-chooser-button))
    (is (string= "title" (gtk:file-chooser-button-title button)))
    (is (eq :open (gtk:file-chooser-action button)))))

(test gtk-file-chooser-button-new.2
  (let ((button nil))
    (is (typep (setf button
                     (gtk:file-chooser-button-new "title" :select-folder))
               'gtk:file-chooser-button))
    (is (string= "title" (gtk:file-chooser-button-title button)))
    (is (eq :select-folder (gtk:file-chooser-action button)))))

;;;     gtk_file_chooser_button_new_with_dialog

(test gtk-file-chooser-button-new-with-dialog
  (let ((button nil)
        (dialog (make-instance 'gtk:file-chooser-dialog)))
    (is (typep (setf button
                     (gtk:file-chooser-button-new-with-dialog dialog))
               'gtk:file-chooser-button))
    (is (string= "Datei auswählen" (gtk:file-chooser-button-title button)))
    (is (eq :open (gtk:file-chooser-action button)))))

;;; --- 2023-6-11 --------------------------------------------------------------
