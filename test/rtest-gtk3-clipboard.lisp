(in-package :gtk-test)

(def-suite gtk-clipboard :in gtk-suite)
(in-suite gtk-clipboard)

(defparameter *verbose-gtk-clipboard* nil)

;;; Types and Values

;;; --- GtkClipboard -----------------------------------------------------------

(test gtk-clipboard-class
  ;; Check type
  (is (g:type-is-object "GtkClipboard"))
  ;; Check registered name
  (is (eq 'gtk:clipboard
          (glib:symbol-for-gtype "GtkClipboard")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkClipboard")
          (g:gtype (cffi:foreign-funcall "gtk_clipboard_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkClipboard")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkClipboard")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkClipboard")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkClipboard")))
  ;; Check signals
  (is (equal '("owner-change")
             (glib-test:list-signals "GtkClipboard")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkClipboard" GTK:CLIPBOARD
                      (:SUPERCLASS G:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_clipboard_get_type")
                      NIL)
             (gobject:get-gtype-definition "GtkClipboard"))))

;;; --- Signals ----------------------------------------------------------------

(test gtk-clipboard-owner-change-signal
  (let* ((name "owner-change")
         (gtype (g:gtype "GtkClipboard"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("GdkEvent")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     GtkClipboardReceivedFunc
;;;     GtkClipboardTextReceivedFunc
;;;     GtkClipboardImageReceivedFunc
;;;     GtkClipboardTargetsReceivedFunc
;;;     GtkClipboardRichTextReceivedFunc
;;;     GtkClipboardURIReceivedFunc
;;;     GtkClipboardGetFunc
;;;     GtkClipboardClearFunc

;;;     gtk_clipboard_get

(test gtk-clipboard-get.1
  (is (typep (gtk:clipboard-get "CLIPBOARD") 'gtk:clipboard))
  (is (typep (gtk:clipboard-get "PRIMARY") 'gtk:clipboard))
  (is (typep (gtk:clipboard-get "SECONDARY") 'gtk:clipboard)))

(test gtk-clipboard-get.2
  (let ((clipboard (gtk:clipboard-get "myclipboard")))
    (is (typep clipboard 'gtk:clipboard))
    (is (eq clipboard (gtk:clipboard-get "myclipboard")))
    (is (not (eq clipboard (gtk:clipboard-get "PRIMARY"))))))

;;;     gtk_clipboard_for_display

(test gtk-clipboard-for-display.1
  (let ((display (gdk:display-default)))
    (is (typep (gtk:clipboard-for-display display "CLIPBOARD") 'gtk:clipboard))
    (is (typep (gtk:clipboard-for-display display "PRIMARY") 'gtk:clipboard))
    (is (typep (gtk:clipboard-for-display display "SECONDARY") 'gtk:clipboard))))

(test gtk-clipboard-for-display.2
  (let* ((display (gdk:display-default))
         (clipboard (gtk:clipboard-for-display display "myclipboard")))
    (is (typep clipboard 'gtk:clipboard))
    (is (eq clipboard (gtk:clipboard-for-display display "myclipboard")))
    (is (not (eq clipboard (gtk:clipboard-for-display display "PRIMARY"))))))

;;;     gtk_clipboard_display

(test gtk-clipboard-display
  (let ((clipboard (gtk:clipboard-get "CLIPBOARD")))
    (is (typep (gtk:clipboard-display clipboard) 'gdk:display))
    (is (eq (gdk:display-default) (gtk:clipboard-display clipboard)))))

;;;     gtk_clipboard_default

(test gtk-clipboard-default
  (is (typep (gtk:clipboard-default (gdk:display-default)) 'gtk:clipboard)))

;;;     gtk_clipboard_set_with_data

(test gtk-clipboard-set-with-data
  (flet ((get-func (clipboard selection info)
           (is (string= "Text"
                        (setf (gtk:selection-data-text selection) "Text")))
           (when *verbose-gtk-clipboard*
             (format t "~&in GTK-CLIPBOARD-GET-FUNC callback~%")
             (format t " clipboard : ~a~%" clipboard)
             (format t " selection : ~a~%" selection)
             (format t "      info : ~a~%" info)
             (format t "    length : ~a~%" (gtk:selection-data-length selection))
             (format t "      text : ~a~%" (gtk:selection-data-text selection)))
           (is (typep clipboard 'gtk:clipboard))
           (is (typep selection 'gtk:selection-data))
           (is (= 4 (gtk:selection-data-length selection)))
           (is (string= "Text" (gtk:selection-data-text selection))))
         (received-func (clipboard selection)
           (when *verbose-gtk-clipboard*
             (format t "~&in GKT-CLIPBOARD-RECEIVED-FUNC callback~%")
             (format t " clipboard : ~a~%" clipboard)
             (format t " selection : ~a~%" selection)
             (format t "    length : ~a~%" (gtk:selection-data-length selection))
             (format t "      text : ~a~%" (gtk:selection-data-text selection)))
           (is (typep clipboard 'gtk:clipboard))
           (is (typep selection 'gtk:selection-data))
           (is (= 4 (gtk:selection-data-length selection)))
           (is (string= "Text" (gtk:selection-data-text selection)))))
  (let ((targets '(("text/html" 0 0)
                   ("STRING" 0 1)
                   ("number" 0 2)
                   ("image/jpeg" 0 3)
                   ("text/uri-list" 0 4)))
       (clipboard (gtk:clipboard-get "PRIMARY")))
    (is (string= "PRIMARY" (gtk:clipboard-selection clipboard)))
    (is-true (gtk:clipboard-set-with-data clipboard targets #'get-func))
    (when (gtk:clipboard-wait-is-text-available clipboard)
      (is-false (gtk:clipboard-request-contents clipboard
                                                "STRING"
                                                #'received-func))))))

;;;     gtk_clipboard_set_with_owner
;;;     gtk_clipboard_get_owner

;;  not implemented

;;;     gtk_clipboard_clear

(test gtk-clipboard-clear
  (flet ((get-func (clipboard selection info)
           (is (string= "Text"
                        (setf (gtk:selection-data-text selection) "Text")))
           (when *verbose-gtk-clipboard*
             (format t "~&in GTK-CLIPBOARD-GET-FUNC callback~%")
             (format t " clipboard : ~a~%" clipboard)
             (format t " selection : ~a~%" selection)
             (format t "      info : ~a~%" info)
             (format t "    length : ~a~%" (gtk:selection-data-length selection))
             (format t "      text : ~a~%" (gtk:selection-data-text selection)))
           (is (typep clipboard 'gtk:clipboard))
           (is (typep selection 'gtk:selection-data))
           (is (= 4 (gtk:selection-data-length selection)))
           (is (string= "Text" (gtk:selection-data-text selection))))
         (received-func (clipboard selection)
           (when *verbose-gtk-clipboard*
             (format t "~&in GKT-CLIPBOARD-RECEIVED-FUNC callback~%")
             (format t " clipboard : ~a~%" clipboard)
             (format t " selection : ~a~%" selection)
             (format t "    length : ~a~%" (gtk:selection-data-length selection))
             (format t "      text : ~a~%" (gtk:selection-data-text selection)))
           (is (typep clipboard 'gtk:clipboard))
           (is (typep selection 'gtk:selection-data))
           (is (= 4 (gtk:selection-data-length selection)))
           (is (string= "Text" (gtk:selection-data-text selection)))))
  (let ((targets '(("text/html" 0 0)
                   ("STRING" 0 1)
                   ("number" 0 2)
                   ("image/jpeg" 0 3)
                   ("text/uri-list" 0 4)))
       (clipboard (gtk:clipboard-get "PRIMARY")))
    (is (string= "PRIMARY" (gtk:clipboard-selection clipboard)))
    (is-true (gtk:clipboard-set-with-data clipboard targets #'get-func))
    (is-false (gtk:clipboard-clear clipboard)))))

;;;     gtk_clipboard_set_text
;;;     gtk_clipboard_request_text

;; TODO: After executing this test, the clipboard on Windows no longer work
;; Check this for Linux too. Can we avoid this?

#+nil
(test gtk-clipboard-set-text
  (flet ((request-text (clipboard text)
           (is (typep clipboard 'gtk:clipboard))
           (is (string= text "This is text."))))
  (let ((clipboard (gtk:clipboard-get "CLIPBOARD")))
    (is-false (gtk:clipboard-set-text clipboard "This is text."))
    (is-false (gtk:clipboard-request-text clipboard #'request-text)))))

;;;     gtk_clipboard_set_image
;;;     gtk_clipboard_request_image

;; TODO: After executing this test, the clipboard on Windows no longer work
;; Check this for Linux too. Can we avoid this?

#+nil
(test gtk-clipboard-set-image
  (flet ((request-image (clipboard  pixbuf)
           (is (typep clipboard 'gtk:clipboard))
           (is (typep pixbuf 'gdk:pixbuf))
           (when *verbose-gtk-clipboard*
             (format t "~%REQUEST-IMAGE:~%")
             (format t " clipboard : ~a~%" clipboard)
             (format t "    pixbuf : ~a~%" pixbuf))))
  (let ((clipboard (gtk:clipboard-get "CLIPBOARD"))
        (pixbuf (gtk:icon-theme-load-icon (gtk:icon-theme-default)
                                          "gtk-ok"
                                          48
                                          0)))
    (is (typep pixbuf 'gdk:pixbuf))
    (gtk:clipboard-set-image clipboard pixbuf)
    (gtk:clipboard-request-image clipboard #'request-image))))

;;;     gtk_clipboard_request_contents

#+nil
(test gtk-clipboard-request-contents.1
  (flet ((request-contents (clipboard selection)
           (is (typep clipboard 'gtk:clipboard))
           (is (typep selection 'gtk:selection-data))
           (is (string= "This is text."
                        (gtk:selection-data-text selection)))
           (when *verbose-gtk-clipboard*
             (format t "~%REQUEST-CONTENS~%")
             (format t " clipboard : ~a~%" clipboard)
             (format t " selection : ~a~%" selection))))
  (let ((clipboard (gtk:clipboard-get "CLIPBOARD")))
    (is-false (gtk:clipboard-set-text clipboard "This is text."))
    (is-false (gtk:clipboard-request-contents clipboard
                                              "STRING"
                                              #'request-contents)))))

#+nil
(test gtk-clipboard-request-contents.2
  (flet ((request-contents (clipboard selection)
           (is (typep clipboard 'gtk:clipboard))
           (is (typep selection 'gtk:selection-data))
           ;; FIXME: This should be true. It works for text, not for a pixbuf.
           (is-false (typep (gtk:selection-data-pixbuf selection) 'gdk:pixbuf))
           (when *verbose-gtk-clipboard*
             (format t "~%REQUEST-CONTENS~%")
             (format t " clipboard : ~a~%" clipboard)
             (format t " selection : ~a~%" selection))))
  (let ((clipboard (gtk:clipboard-get "CLIPBOARD"))
        (pixbuf (gtk:icon-theme-load-icon (gtk:icon-theme-default)
                                          "gtk-ok"
                                          48
                                          0)))
    (is-false (gtk:clipboard-set-image clipboard pixbuf))
    (is-false (gtk:clipboard-request-contents clipboard
                                              "PIXMAP"
                                              #'request-contents)))))

;;;     gtk_clipboard_request_targets

(test gtk-clipboard-request-targets
  (flet ((request-targets (clipboard atoms n-atoms)
           (when *verbose-gtk-clipboard*
             (format t "~%REQUEST-TARGETS~%")
             (format t " clipboard : ~a~%" clipboard)
             (format t "     atoms : ~a~%" atoms)
             (format t "   n-atoms : ~a~%" n-atoms)
             (cffi:with-foreign-object (targets-ar 'gdk:atom-as-string n-atoms)
               (loop for i from 0 below n-atoms
                     do (format t "    target : ~a~%"
                                  (cffi:mem-aref targets-ar
                                            'gdk:atom-as-string i)))))))
  (let ((clipboard (gtk:clipboard-get "CLIPBOARD")))
    (is-false (gtk:clipboard-request-targets clipboard #'request-targets))
)))

;;;     gtk_clipboard_request_rich_text
;;;     gtk_clipboard_request_uris
;;;     gtk_clipboard_wait_for_contents
;;;     gtk_clipboard_wait_for_text
;;;     gtk_clipboard_wait_for_image
;;;     gtk_clipboard_wait_for_rich_text
;;;     gtk_clipboard_wait_for_uris
;;;     gtk_clipboard_wait_is_text_available
;;;     gtk_clipboard_wait_is_image_available
;;;     gtk_clipboard_wait_is_rich_text_available
;;;     gtk_clipboard_wait_is_uris_available
;;;     gtk_clipboard_wait_for_targets
;;;     gtk_clipboard_wait_is_target_available
;;;     gtk_clipboard_set_can_store
;;;     gtk_clipboard_store

;;;     gtk_clipboard_get_selection

(test gtk-clipboard-selection
  (let ((clipboard (gtk:clipboard-default (gdk:display-default))))
    (is (string= "CLIPBOARD" (gtk:clipboard-selection clipboard)))))

;;; 2025-07-03
