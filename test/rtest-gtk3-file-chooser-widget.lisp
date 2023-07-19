(in-package :gtk-test)

(def-suite gtk-file-chooser-widget :in gtk-suite)
(in-suite gtk-file-chooser-widget)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileChooserWidget

(test gtk-file-chooser-widget-class
  ;; Type check
  (is (g:type-is-object "GtkFileChooserWidget"))
  ;; Check the registered name
  (is (eq 'gtk:file-chooser-widget
          (glib:symbol-for-gtype "GtkFileChooserWidget")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFileChooserWidget")
          (g:gtype (cffi:foreign-funcall "gtk_file_chooser_widget_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkBox")
          (g:type-parent "GtkFileChooserWidget")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkFileChooserWidget")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable"
               "GtkFileChooser" "GtkFileChooserEmbed")
             (list-interfaces "GtkFileChooserWidget")))
  ;; Check the class properties
  (is (equal '("action" "create-folders" "do-overwrite-confirmation"
               "extra-widget" "filter" "local-only" "preview-widget"
               "preview-widget-active" "search-mode" "select-multiple"
               "show-hidden" "subtitle" "use-preview-label")
             (list-properties "GtkFileChooserWidget")))
  ;; Check the style properties
  (is (equal '()
             (list-style-properties "GtkFileChooserWidget")))
  ;; Check the child properties
  (is (equal '("expand" "fill" "pack-type" "padding" "position")
             (list-child-properties "GtkFileChooserWidget")))
  ;; Check the signals
  (is (equal '("desktop-folder" "down-folder" "home-folder" "location-popup"
               "location-popup-on-paste" "location-toggle-popup"
               "places-shortcut" "quick-bookmark" "recent-shortcut"
               "search-shortcut" "show-hidden" "up-folder")
             (list-signals "GtkFileChooserWidget")))
  ;; CSS information
  (is (string= "filechooser"
               (gtk:widget-class-css-name "GtkFileChooserWidget")))
  #-windows
  (is (string=
"[filechooser.vertical:dir(ltr)]
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
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context
                       (make-instance 'gtk:file-chooser-widget))
                   :recurse)))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFileChooserWidget"
                                     GTK-FILE-CHOOSER-WIDGET
                       (:SUPERCLASS GTK-BOX :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkFileChooser"
                         "GtkFileChooserEmbed" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_file_chooser_widget_get_type")
                       ((SEARCH-MODE GTK-FILE-CHOOSER-WIDGET-SEARCH-MODE
                         "search-mode" "gboolean" T T)
                        (SUBTITLE GTK-FILE-CHOOSER-WIDGET-SUBTITLE "subtitle"
                         "gchararray" T NIL)))
             (gobject:get-g-type-definition "GtkFileChooserWidget"))))

;;; --- Properties -------------------------------------------------------------

;;;     search-mode
;;;     subtitle

(test gtk-file-chooser-widget-properties.1
  (let ((chooser (make-instance 'gtk:file-chooser-widget)))
    (is-false (gtk:file-chooser-widget-search-mode chooser))
    (is-false (gtk:file-chooser-widget-subtitle chooser))))

(test gtk-file-chooser-widget-properties.2
  (let ((chooser (make-instance 'gtk:file-chooser-widget
                                :search-mode t)))
    (is-true (gtk:file-chooser-widget-search-mode chooser))
    (is (string= "Suchen" (gtk:file-chooser-widget-subtitle chooser)))))

;;; --- Signals ----------------------------------------------------------------

;;;     desktop-folder

(test gtk-file-chooser-widget-desktop-folder-signal
  ;; Query info for the signal
  (let ((query (g:signal-query (g:signal-lookup "desktop-folder"
                                                "GtkFileChooserWidget"))))
    (is (string= "desktop-folder" (g:signal-query-signal-name query)))
    (is (string= "GtkFileChooserWidget"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:action :RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

;;;     down-folder

(test gtk-file-chooser-widget-down-folder-signal
  ;; Query info for the signal
  (let ((query (g:signal-query (g:signal-lookup "down-folder"
                                                "GtkFileChooserWidget"))))
    (is (string= "down-folder" (g:signal-query-signal-name query)))
    (is (string= "GtkFileChooserWidget"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:action :RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

;;;     home-folder

(test gtk-file-chooser-widget-home-folder-signal
  ;; Query info for the signal
  (let ((query (g:signal-query (g:signal-lookup "home-folder"
                                                "GtkFileChooserWidget"))))
    (is (string= "home-folder" (g:signal-query-signal-name query)))
    (is (string= "GtkFileChooserWidget"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:action :RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

;;;     location-popup

(test gtk-file-chooser-widget-location-popup-signal
  ;; Query info for the signal
  (let ((query (g:signal-query (g:signal-lookup "location-popup"
                                                "GtkFileChooserWidget"))))
    (is (string= "location-popup" (g:signal-query-signal-name query)))
    (is (string= "GtkFileChooserWidget"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:action :RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("gchararray")
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

;;;     location-popup-on-paste

(test gtk-file-chooser-widget-location-popup-on-paste-signal
  ;; Query info for the signal
  (let ((query (g:signal-query (g:signal-lookup "location-popup-on-paste"
                                                "GtkFileChooserWidget"))))
    (is (string= "location-popup-on-paste" (g:signal-query-signal-name query)))
    (is (string= "GtkFileChooserWidget"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:action :RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

;;;     location-toggle-popup

(test gtk-file-chooser-widget-location-toggle-popup-signal
  ;; Query info for the signal
  (let ((query (g:signal-query (g:signal-lookup "location-toggle-popup"
                                                "GtkFileChooserWidget"))))
    (is (string= "location-toggle-popup" (g:signal-query-signal-name query)))
    (is (string= "GtkFileChooserWidget"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:action :RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

;;;     places-shortcut

(test gtk-file-chooser-widget-places-shortcut-signal
  ;; Query info for the signal
  (let ((query (g:signal-query (g:signal-lookup "places-shortcut"
                                                "GtkFileChooserWidget"))))
    (is (string= "places-shortcut" (g:signal-query-signal-name query)))
    (is (string= "GtkFileChooserWidget"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:action :RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

;;;     quick-bookmark

(test gtk-file-chooser-widget-quick-bookmark-signal
  ;; Query info for the signal
  (let ((query (g:signal-query (g:signal-lookup "quick-bookmark"
                                                "GtkFileChooserWidget"))))
    (is (string= "quick-bookmark" (g:signal-query-signal-name query)))
    (is (string= "GtkFileChooserWidget"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:action :RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("gint")
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

;;;     recent-shortcut

(test gtk-file-chooser-widget-recent-shortcut-signal
  ;; Query info for the signal
  (let ((query (g:signal-query (g:signal-lookup "recent-shortcut"
                                                "GtkFileChooserWidget"))))
    (is (string= "recent-shortcut" (g:signal-query-signal-name query)))
    (is (string= "GtkFileChooserWidget"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:action :RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

;;;     search-shortcut

(test gtk-file-chooser-widget-search-shortcut-signal
  ;; Query info for the signal
  (let ((query (g:signal-query (g:signal-lookup "search-shortcut"
                                                "GtkFileChooserWidget"))))
    (is (string= "search-shortcut" (g:signal-query-signal-name query)))
    (is (string= "GtkFileChooserWidget"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:action :RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

;;;     show-hidden

(test gtk-file-chooser-widget-show-hidden-signal
  ;; Query info for the signal
  (let ((query (g:signal-query (g:signal-lookup "show-hidden"
                                                "GtkFileChooserWidget"))))
    (is (string= "show-hidden" (g:signal-query-signal-name query)))
    (is (string= "GtkFileChooserWidget"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:action :RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

;;;     up-folder

(test gtk-file-chooser-widget-up-folder-signal
  ;; Query info for the signal
  (let ((query (g:signal-query (g:signal-lookup "up-folder"
                                                "GtkFileChooserWidget"))))
    (is (string= "up-folder" (g:signal-query-signal-name query)))
    (is (string= "GtkFileChooserWidget"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:action :RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_file_chooser_widget_new

(test gtk-file-chooser-widget-new
  (is (typep (gtk:file-chooser-widget-new :open) 'gtk:file-chooser-widget))
  (is (typep (gtk:file-chooser-widget-new :save) 'gtk:file-chooser-widget)))

;;; --- 2023-6-25 --------------------------------------------------------------
