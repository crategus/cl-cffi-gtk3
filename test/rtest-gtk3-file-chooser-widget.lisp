(in-package :gtk-test)

(def-suite gtk-file-chooser-widget :in gtk-suite)
(in-suite gtk-file-chooser-widget)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileChooserWidget

(test gtk-file-chooser-widget-class
  ;; Check type
  (is (g:type-is-object "GtkFileChooserWidget"))
  ;; Check registered name
  (is (eq 'gtk:file-chooser-widget
          (glib:symbol-for-gtype "GtkFileChooserWidget")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFileChooserWidget")
          (g:gtype (cffi:foreign-funcall "gtk_file_chooser_widget_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkBox")
          (g:type-parent "GtkFileChooserWidget")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkFileChooserWidget")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable"
               "GtkFileChooser" "GtkFileChooserEmbed")
             (glib-test:list-interfaces "GtkFileChooserWidget")))
  ;; Check class properties
  (is (equal '("action" "create-folders" "do-overwrite-confirmation"
               "extra-widget" "filter" "local-only" "preview-widget"
               "preview-widget-active" "search-mode" "select-multiple"
               "show-hidden" "subtitle" "use-preview-label")
             (glib-test:list-properties "GtkFileChooserWidget")))
  ;; Check style properties
  (is (equal '()
             (gtk-test:list-style-properties "GtkFileChooserWidget")))
  ;; Check child properties
  (is (equal '("expand" "fill" "pack-type" "padding" "position")
             (gtk-test:list-child-properties "GtkFileChooserWidget")))
  ;; Check signals
  (is (equal '("desktop-folder" "down-folder" "home-folder" "location-popup"
               "location-popup-on-paste" "location-toggle-popup"
               "places-shortcut" "quick-bookmark" "recent-shortcut"
               "search-shortcut" "show-hidden" "up-folder")
             (glib-test:list-signals "GtkFileChooserWidget")))
  ;; CSS information
  (is (string= "filechooser"
               (gtk:widget-class-css-name "GtkFileChooserWidget")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkFileChooserWidget"
                                      GTK:FILE-CHOOSER-WIDGET
                       (:SUPERCLASS GTK:BOX
                        :EXPORT T
                        :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkFileChooser"
                         "GtkFileChooserEmbed" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_file_chooser_widget_get_type")
                       ((SEARCH-MODE FILE-CHOOSER-WIDGET-SEARCH-MODE
                         "search-mode" "gboolean" T T)
                        (SUBTITLE FILE-CHOOSER-WIDGET-SUBTITLE
                         "subtitle" "gchararray" T NIL)))
             (gobject:get-gtype-definition "GtkFileChooserWidget"))))

;;; --- Properties -------------------------------------------------------------

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

;;; 2024-9-23
