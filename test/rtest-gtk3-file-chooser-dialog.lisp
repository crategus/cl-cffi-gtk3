(in-package :gtk-test)

(def-suite gtk-file-chooser-dialog :in gtk-suite)
(in-suite gtk-file-chooser-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileChooserDialog

(test gtk-file-chooser-dialog-class
  ;; Type check
  (is (g:type-is-object "GtkFileChooserDialog"))
  ;; Check the registered name
  (is (eq 'gtk:file-chooser-dialog
          (glib:symbol-for-gtype "GtkFileChooserDialog")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFileChooserDialog")
          (g:gtype (cffi:foreign-funcall "gtk_file_chooser_dialog_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkDialog")
          (g:type-parent "GtkFileChooserDialog")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkFileChooserDialog")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkFileChooser")
             (list-interfaces "GtkFileChooserDialog")))
  ;; Check the class properties
  (is (equal '("action" "create-folders" "do-overwrite-confirmation"
               "extra-widget" "filter" "local-only" "preview-widget"
               "preview-widget-active" "select-multiple" "show-hidden"
               "use-preview-label")
             (list-properties "GtkFileChooserDialog")))
  ;; Check the style properties
  (is (equal '()
             (list-style-properties "GtkFileChooserDialog")))
  ;; Check the child properties
  (is (equal '()
             (list-child-properties "GtkFileChooserDialog")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkFileChooserDialog")))
  ;; CSS information
  (is (string= "dialog"
               (gtk:widget-class-css-name "GtkFileChooserDialog")))
  #-windows
  (is (string=
"[dialog.background.csd:dir(ltr)]
  decoration:dir(ltr)
  headerbar.titlebar:dir(ltr)
    box.vertical:dir(ltr)
      label.title:dir(ltr)
      [label.subtitle:dir(ltr)]
    box.right.horizontal:dir(ltr)
      [separator.vertical.titlebutton:dir(ltr)]
      button.close.titlebutton:dir(ltr)
        image:dir(ltr)
  box.vertical.dialog-vbox:dir(ltr)
    filechooser.vertical:dir(ltr)
      box.vertical:dir(ltr)
        paned.horizontal:dir(ltr)
          placessidebar.sidebar.frame:dir(ltr)
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
              list:dir(ltr)
                row.activatable.sidebar-row:dir(ltr)
                  revealer.sidebar-revealer:dir(ltr)
                    widget:dir(ltr)
                      box.horizontal:dir(ltr)
                        image.sidebar-icon:dir(ltr)
                        label.sidebar-label:dir(ltr)
                        [image.sidebar-icon:dir(ltr)]
                        [button.sidebar-button.image-button:dir(ltr)]
                          image:dir(ltr)
                        [spinner:dir(ltr):checked]
                row.activatable.sidebar-row:dir(ltr)
                  revealer.sidebar-revealer:dir(ltr)
                    widget:dir(ltr)
                      box.horizontal:dir(ltr)
                        image.sidebar-icon:dir(ltr)
                        label.sidebar-label:dir(ltr)
                        [image.sidebar-icon:dir(ltr)]
                        [button.sidebar-button.image-button:dir(ltr)]
                          image:dir(ltr)
                        [spinner:dir(ltr):checked]
                row.activatable.sidebar-row:dir(ltr)
                  revealer.sidebar-revealer:dir(ltr)
                    widget:dir(ltr)
                      box.horizontal:dir(ltr)
                        image.sidebar-icon:dir(ltr)
                        label.sidebar-label:dir(ltr)
                        [image.sidebar-icon:dir(ltr)]
                        [button.sidebar-button.image-button:dir(ltr)]
                          image:dir(ltr)
                        [spinner:dir(ltr):checked]
                row.activatable.sidebar-row:dir(ltr)
                  revealer.sidebar-revealer:dir(ltr)
                    widget:dir(ltr)
                      box.horizontal:dir(ltr)
                        image.sidebar-icon:dir(ltr)
                        label.sidebar-label:dir(ltr)
                        [image.sidebar-icon:dir(ltr)]
                        [button.sidebar-button.image-button:dir(ltr)]
                          image:dir(ltr)
                        [spinner:dir(ltr):checked]
                row.activatable.sidebar-row:dir(ltr)
                  revealer.sidebar-revealer:dir(ltr)
                    widget:dir(ltr)
                      box.horizontal:dir(ltr)
                        image.sidebar-icon:dir(ltr)
                        label.sidebar-label:dir(ltr)
                        [image.sidebar-icon:dir(ltr)]
                        [button.sidebar-button.image-button:dir(ltr)]
                          image:dir(ltr)
                        [spinner:dir(ltr):checked]
                row.activatable.sidebar-row:dir(ltr)
                  revealer.sidebar-revealer:dir(ltr)
                    widget:dir(ltr)
                      box.horizontal:dir(ltr)
                        image.sidebar-icon:dir(ltr)
                        label.sidebar-label:dir(ltr)
                        [image.sidebar-icon:dir(ltr)]
                        [button.sidebar-button.image-button:dir(ltr)]
                          image:dir(ltr)
                        [spinner:dir(ltr):checked]
                row.activatable.sidebar-row:dir(ltr)
                  revealer.sidebar-revealer:dir(ltr)
                    widget:dir(ltr)
                      box.horizontal:dir(ltr)
                        image.sidebar-icon:dir(ltr)
                        label.sidebar-label:dir(ltr)
                        [image.sidebar-icon:dir(ltr)]
                        [button.sidebar-button.image-button:dir(ltr)]
                          image:dir(ltr)
                        [spinner:dir(ltr):checked]
                row.activatable.sidebar-row:dir(ltr)
                  revealer.sidebar-revealer:dir(ltr)
                    widget:dir(ltr)
                      box.horizontal:dir(ltr)
                        image.sidebar-icon:dir(ltr)
                        label.sidebar-label:dir(ltr)
                        [image.sidebar-icon:dir(ltr)]
                        button.sidebar-button.image-button:dir(ltr)
                          image:dir(ltr)
                        [spinner:dir(ltr):checked]
                row.activatable.sidebar-row:dir(ltr)
                  revealer.sidebar-revealer:dir(ltr)
                    widget:dir(ltr)
                      box.horizontal:dir(ltr)
                        image.sidebar-icon:dir(ltr)
                        label.sidebar-label:dir(ltr)
                        [image.sidebar-icon:dir(ltr)]
                        button.sidebar-button.image-button:dir(ltr)
                          image:dir(ltr)
                        [spinner:dir(ltr):checked]
                [row.activatable.sidebar-new-bookmark-row.sidebar-row:dir(ltr)]
                  revealer.sidebar-revealer:dir(ltr)
                    widget:dir(ltr)
                      box.horizontal:dir(ltr)
                        image.sidebar-icon:dir(ltr)
                        label.sidebar-label:dir(ltr)
                        [image.sidebar-icon:dir(ltr)]
                        [button.sidebar-button.image-button:dir(ltr)]
                          image:dir(ltr)
                        [spinner:dir(ltr):checked]
                row.activatable.sidebar-row:dir(ltr)
                  revealer.sidebar-revealer:dir(ltr)
                    widget:dir(ltr)
                      box.horizontal:dir(ltr)
                        image.sidebar-icon:dir(ltr)
                        label.sidebar-label:dir(ltr)
                        [image.sidebar-icon:dir(ltr)]
                        [button.sidebar-button.image-button:dir(ltr)]
                          image:dir(ltr)
                        [spinner:dir(ltr):checked]
                separator.horizontal:dir(ltr)
                separator.horizontal:dir(ltr)
          separator:dir(ltr)
          box.vertical:dir(ltr)
            revealer:dir(ltr)
              box#pathbarbox.view.vertical:dir(ltr)
                stack:dir(ltr)
                  box.horizontal:dir(ltr)
                    widget.linked.path-bar:dir(ltr)
                      button.slider-button:dir(ltr)
                        image:dir(ltr)
                      button.slider-button:dir(ltr)
                        image:dir(ltr)
                    [button.toggle.popup:dir(ltr)]
                      image:dir(ltr)
                  box.horizontal:dir(ltr)
                  box.horizontal:dir(ltr)
                    entry.search:dir(ltr)
                      image.left:disabled:dir(ltr)
                      undershoot.left:dir(ltr)
                      undershoot.right:dir(ltr)
                    [spinner:dir(ltr):checked]
            box.horizontal:dir(ltr)
              stack.view:dir(ltr)
                box.vertical:dir(ltr)
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
                    treeview.view:dir(ltr)
                      header:dir(ltr)
                        button:dir(ltr)
                          box.horizontal:dir(ltr)
                            widget:dir(ltr)
                              label:dir(ltr)
                            [image:dir(ltr)]
                        [button:dir(ltr)]
                          box.horizontal:dir(ltr)
                            widget:dir(ltr)
                              label:dir(ltr)
                            [image:dir(ltr)]
                        button:dir(ltr)
                          box.horizontal:dir(ltr)
                            widget:dir(ltr)
                              label:dir(ltr)
                            [image:dir(ltr)]
                        button:dir(ltr)
                          box.horizontal:dir(ltr)
                            widget:dir(ltr)
                              label:dir(ltr)
                            [image:dir(ltr)]
                        button:dir(ltr)
                          box.horizontal:dir(ltr)
                            widget:dir(ltr)
                              label:dir(ltr)
                            [image:dir(ltr)]
                  [actionbar:dir(ltr)]
                    revealer:dir(ltr)
                      box.horizontal:dir(ltr)
                        label:dir(ltr)
                placesview.vertical:dir(ltr)
                  stack:dir(ltr)
                    frame:dir(ltr)
                      border.flat:dir(ltr)
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
                        viewport:dir(ltr)
                          list:dir(ltr)
                            row.activatable:dir(ltr)
                              widget:dir(ltr)
                                box.horizontal:dir(ltr)
                                  image:dir(ltr)
                                  label:dir(ltr)
                                  label.dim-label:dir(ltr)
                                  label.dim-label:dir(ltr)
                                  stack:dir(ltr)
                                    button.sidebar-button.image-button:dir(ltr)
                                      image:dir(ltr)
                                    spinner:dir(ltr):checked
                            row.activatable:disabled:dir(ltr)
                              label:disabled:dir(ltr)
                            box.vertical:dir(ltr)
                              label:dir(ltr)
                              separator.horizontal:dir(ltr)
                    box.vertical:dir(ltr)
                      image.dim-label:dir(ltr)
                      label:dir(ltr)
                      label.dim-label:dir(ltr)
                  [actionbar.background:dir(ltr)]
                    revealer:dir(ltr)
                      box.horizontal:dir(ltr)
                        label:dir(ltr)
                        box.linked.horizontal:dir(ltr)
                          entry:dir(ltr)
                            undershoot.left:dir(ltr)
                            undershoot.right:dir(ltr)
                            image.right:dir(ltr)
                            [window.background:dir(ltr)]
                              decoration:dir(ltr)
                              frame:dir(ltr)
                                border:dir(ltr)
                                [box.vertical:dir(ltr)]
                                  [scrolledwindow:dir(ltr)]
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
                                    [treeview.view:dir(ltr)]
                                      header:dir(ltr)
                                        button:dir(ltr)
                                          box.horizontal:dir(ltr)
                                            widget:dir(ltr)
                                              label:dir(ltr)
                                            [image:dir(ltr)]
                          button.toggle.popup.server-list-button:dir(ltr)
                            image:dir(ltr)
                        button.text-button:disabled:dir(ltr)
                          label:disabled:dir(ltr)
                grid.dim-label.horizontal:dir(ltr)
                  image.dim-label:dir(ltr)
                  label:dir(ltr)
                  label.dim-label:dir(ltr)
              [box.vertical:dir(ltr)]
      [actionbar:dir(ltr)]
        revealer:dir(ltr)
          box.horizontal:dir(ltr)
            box.horizontal:dir(ltr)
            box.horizontal:dir(ltr)
              combobox:dir(ltr)
                box.linked.horizontal:dir(ltr)
                  button.combo:disabled:dir(ltr)
                    box.horizontal:disabled:dir(ltr)
                      cellview:disabled:dir(ltr)
                      arrow:disabled:dir(ltr)
                [window.background.popup:dir(ltr)]
                  decoration:dir(ltr)
                  [menu#gtk-combobox-popup-menu:dir(ltr)]
                    [arrow.top:dir(ltr)]
                    [arrow.bottom:dir(ltr)]
    [box.horizontal.dialog-action-box:dir(ltr)]
      buttonbox.horizontal.dialog-action-area:dir(ltr)
  [popover.background:dir(ltr)]
    grid.horizontal:dir(ltr)
      label:dir(ltr)
      entry:dir(ltr)
        undershoot.left:dir(ltr)
        undershoot.right:dir(ltr)
      button.text-button.suggested-action:disabled:dir(ltr)
        label:disabled:dir(ltr)
      label:dir(ltr)
  [popover.background:dir(ltr)]
    grid.horizontal:dir(ltr)
      label:dir(ltr)
      entry:dir(ltr)
        undershoot.left:dir(ltr)
        undershoot.right:dir(ltr)
      button.text-button.suggested-action:disabled:dir(ltr)
        label:disabled:dir(ltr)
      label:dir(ltr)
  [popover.background:dir(ltr)]
    box.vertical:dir(ltr)
      label.dim-label:dir(ltr)
      label:dir(ltr)
      label:dir(ltr)
      grid.horizontal:dir(ltr)
        label:dir(ltr)
        label:dir(ltr)
        label:dir(ltr)
        label:dir(ltr)
        label:dir(ltr)
        label:dir(ltr)
        label:dir(ltr)
        label:dir(ltr)
        label:dir(ltr)
        label:dir(ltr)
        label:dir(ltr)
        label:dir(ltr)
        label:dir(ltr)
        label:dir(ltr)
  [popover.background:dir(ltr)]
    stack:dir(ltr)
      box.vertical:dir(ltr)
        image.dim-label:dir(ltr)
        label.dim-label:dir(ltr)
      box.vertical:dir(ltr)
        label:dir(ltr)
        scrolledwindow.frame:dir(ltr)
          overshoot.left:dir(ltr)
          undershoot.left:dir(ltr)
          overshoot.right:dir(ltr)
          undershoot.right:dir(ltr)
          overshoot.top:dir(ltr)
          undershoot.top:dir(ltr)
          overshoot.bottom:dir(ltr)
          undershoot.bottom:dir(ltr)
          scrollbar.bottom.horizontal:dir(ltr)
            contents:dir(ltr)
              trough:dir(ltr)
                slider:dir(ltr)
          scrollbar.vertical.right:dir(ltr)
            contents:dir(ltr)
              trough:dir(ltr)
                slider:dir(ltr)
          viewport:dir(ltr)
            list:dir(ltr)
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context
                       (make-instance 'gtk:file-chooser-dialog))
                   :recurse)))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFileChooserDialog"
                                     GTK-FILE-CHOOSER-DIALOG
                       (:SUPERCLASS GTK-DIALOG :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkFileChooser")
                        :TYPE-INITIALIZER "gtk_file_chooser_dialog_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkFileChooserDialog"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_file_chooser_dialog_new

(test gtk-file-chooser-dialog-new
  (let ((dialog nil))
    (is (typep (setf dialog
                     (gtk:file-chooser-dialog-new "title"
                                                  nil
                                                  :save
                                                  "_OK"
                                                  :accept
                                                  "_Canel"
                                                  :reject))
               'gtk:file-chooser-dialog))))

;;; --- 2023-6-11 --------------------------------------------------------------
