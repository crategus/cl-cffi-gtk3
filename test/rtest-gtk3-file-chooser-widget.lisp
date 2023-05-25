(in-package :gtk-test)

(def-suite gtk-file-chooser-widget :in gtk-suite)
(in-suite gtk-file-chooser-widget)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileChooserWidget

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
;;;     down-folder
;;;     home-folder
;;;     location-popup
;;;     location-popup-on-paste
;;;     location-toggle-popup
;;;     places-shortcut
;;;     quick-bookmark
;;;     recent-shortcut
;;;     search-shortcut
;;;     show-hidden
;;;     up-folder

;;; --- Functions --------------------------------------------------------------

;;;     gtk_file_chooser_widget_new

(test gtk-file-chooser-widget-new
  (is (typep (gtk:file-chooser-widget-new :open) 'gtk:file-chooser-widget))
  (is (typep (gtk:file-chooser-widget-new :save) 'gtk:file-chooser-widget)))

;;; --- 2023-5-25 --------------------------------------------------------------
