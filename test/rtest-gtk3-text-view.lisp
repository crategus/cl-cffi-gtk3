(in-package :gtk-test)

(def-suite gtk-text-view :in gtk-suite)
(in-suite gtk-text-view)

;; TODO: Work on this tests is neeeded

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTextViewLayer                                    not exported

;;;     GtkTextWindowType

(test gtk-text-window-type
  ;; Check type
  (is (g:type-is-enum "GtkTextWindowType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTextWindowType")
          (g:gtype (cffi:foreign-funcall "gtk_text_window_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:text-window-type
          (glib:symbol-for-gtype "GtkTextWindowType")))
  ;; Check names
  (is (equal '("GTK_TEXT_WINDOW_PRIVATE" "GTK_TEXT_WINDOW_WIDGET"
               "GTK_TEXT_WINDOW_TEXT" "GTK_TEXT_WINDOW_LEFT"
               "GTK_TEXT_WINDOW_RIGHT" "GTK_TEXT_WINDOW_TOP"
               "GTK_TEXT_WINDOW_BOTTOM")
             (glib-test:list-enum-item-names "GtkTextWindowType")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6)
             (glib-test:list-enum-item-values "GtkTextWindowType")))
  ;; Check nick names
  (is (equal '("private" "widget" "text" "left" "right" "top" "bottom")
             (glib-test:list-enum-item-nicks "GtkTextWindowType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkTextWindowType" GTK:TEXT-WINDOW-TYPE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER "gtk_text_window_type_get_type")
                                    (:PRIVATE 0)
                                    (:WIDGET 1)
                                    (:TEXT 2)
                                    (:LEFT 3)
                                    (:RIGHT 4)
                                    (:TOP 5)
                                    (:BOTTOM 6))
             (gobject:get-gtype-definition "GtkTextWindowType"))))

;;;     GtkTextExtendSelection

(test gtk-text-extend-selection
  ;; Check type
  (is (g:type-is-enum "GtkTextExtendSelection"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTextExtendSelection")
          (g:gtype (cffi:foreign-funcall "gtk_text_extend_selection_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:text-extend-selection
          (glib:symbol-for-gtype "GtkTextExtendSelection")))
  ;; Check names
  (is (equal '("GTK_TEXT_EXTEND_SELECTION_WORD" "GTK_TEXT_EXTEND_SELECTION_LINE")
             (glib-test:list-enum-item-names "GtkTextExtendSelection")))
  ;; Check values
  (is (equal '(0 1)
             (glib-test:list-enum-item-values "GtkTextExtendSelection")))
  ;; Check nick names
  (is (equal '("word" "line")
             (glib-test:list-enum-item-nicks "GtkTextExtendSelection")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkTextExtendSelection"
                                    GTK:TEXT-EXTEND-SELECTION
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gtk_text_extend_selection_get_type")
                                    (:WORD 0)
                                    (:LINE 1))
             (gobject:get-gtype-definition "GtkTextExtendSelection"))))

;;;     GtkWrapMode                                         -> gtk.text-tag.lisp

;;;     GtkTextChildAnchor

(test gtk-text-child-anchor-class
  ;; Check type
  (is (g:type-is-object "GtkTextChildAnchor"))
  ;; Check registered name
  (is (eq 'gtk:text-child-anchor
          (glib:symbol-for-gtype "GtkTextChildAnchor")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTextChildAnchor")
          (g:gtype (cffi:foreign-funcall "gtk_text_child_anchor_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkTextChildAnchor")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkTextChildAnchor")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkTextChildAnchor")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "GtkTextChildAnchor")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkTextChildAnchor")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkTextChildAnchor" GTK:TEXT-CHILD-ANCHOR
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_text_child_anchor_get_type")
                      NIL)
             (gobject:get-gtype-definition "GtkTextChildAnchor"))))

;;;     gtk_text_child_anchor_new

(test gtk-text-child-anchor-new
  (glib-test:with-check-memory (anchor)
    (is (typep (setf anchor (gtk:text-child-anchor-new)) 'gtk:text-child-anchor))))

;;;     gtk_text_child_anchor_get_widgets
;;;     gtk_text_child_anchor_get_deleted

(test gtk-text-child-anchor-widgets
  (glib-test:with-check-memory (buffer anchor)
    (is (typep (setf buffer
                     (make-instance 'gtk:text-buffer
                                    :text
                                    "Some sample text for the text buffer."))
               'gtk:text-buffer))
    (let ((iter (gtk:text-buffer-iter-at-offset buffer 12)))
      (is (typep (setf anchor
                       (gtk:text-buffer-create-child-anchor buffer iter))
                 'gtk:text-child-anchor))
      (is (= 2 (g:object-ref-count anchor)))
      ;; Move iter one char backwards
      (is-true (gtk:text-iter-move iter :count -1))
      (is (typep (gtk:text-iter-child-anchor iter) 'gtk:text-child-anchor))

      (is-false (gtk:text-child-anchor-widgets anchor))
      (is-false (gtk:text-child-anchor-deleted anchor)))
    ;; Clear text buffer
    (is (string= "" (setf (gtk:text-buffer-text buffer) "")))))

;;; ----------------------------------------------------------------------------

;;;     GtkTextView

(test gtk-text-view-class
  ;; Check type
  (is (g:type-is-object "GtkTextView"))
  ;; Check registered name
  (is (eq 'gtk:text-view
          (glib:symbol-for-gtype "GtkTextView")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTextView")
          (g:gtype (cffi:foreign-funcall "gtk_text_view_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkContainer")
          (g:type-parent "GtkTextView")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkTextView")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkScrollable")
             (glib-test:list-interfaces "GtkTextView")))
  ;; Check class properties
  (is (equal '("accepts-tab" "bottom-margin" "buffer" "cursor-visible"
               "editable" "hadjustment" "hscroll-policy" "im-module" "indent"
               "input-hints" "input-purpose" "justification" "left-margin"
               "monospace" "overwrite" "pixels-above-lines" "pixels-below-lines"
               "pixels-inside-wrap" "populate-all" "right-margin" "tabs"
               "top-margin" "vadjustment" "vscroll-policy" "wrap-mode")
             (glib-test:list-properties "GtkTextView")))
  ;; Check style properties
  (is (equal '("error-underline-color")
             (gtk-test:list-style-properties "GtkTextView")))
  ;; Check child properties
  (is (equal '()
             (gtk-test:list-child-properties "GtkTextView")))
  ;; Check signals
  (is (equal '("backspace" "copy-clipboard" "cut-clipboard" "delete-from-cursor"
               "extend-selection" "insert-at-cursor" "insert-emoji"
               "move-cursor" "move-viewport" "paste-clipboard" "populate-popup"
               "preedit-changed" "select-all" "set-anchor"
               "toggle-cursor-visible" "toggle-overwrite")
             (glib-test:list-signals "GtkTextView")))
  ;; Check CSS information
  (is (string= "textview"
               (gtk:widget-class-css-name "GtkTextView")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkTextView" GTK:TEXT-VIEW
                      (:SUPERCLASS GTK:CONTAINER
                       :EXPORT T
                       :INTERFACES
                       ("AtkImplementorIface" "GtkBuildable" "GtkScrollable")
                       :TYPE-INITIALIZER "gtk_text_view_get_type")
                      ((ACCEPTS-TAB TEXT-VIEW-ACCEPTS-TAB "accepts-tab"
                        "gboolean" T T)
                       (BOTTOM-MARGIN TEXT-VIEW-BOTTOM-MARGIN "bottom-margin"
                        "gint" T T)
                       (BUFFER TEXT-VIEW-BUFFER "buffer" "GtkTextBuffer" T T)
                       (CURSOR-VISIBLE TEXT-VIEW-CURSOR-VISIBLE
                        "cursor-visible" "gboolean" T T)
                       (EDITABLE TEXT-VIEW-EDITABLE "editable" "gboolean" T T)
                       (IM-MODULE TEXT-VIEW-IM-MODULE "im-module"
                        "gchararray" T T)
                       (INDENT TEXT-VIEW-INDENT "indent" "gint" T T)
                       (INPUT-HINTS TEXT-VIEW-INPUT-HINTS "input-hints"
                        "GtkInputHints" T T)
                       (INPUT-PURPOSE TEXT-VIEW-INPUT-PURPOSE "input-purpose"
                        "GtkInputPurpose" T T)
                       (JUSTIFICATION TEXT-VIEW-JUSTIFICATION "justification"
                        "GtkJustification" T T)
                       (LEFT-MARGIN TEXT-VIEW-LEFT-MARGIN "left-margin"
                        "gint" T T)
                       (MONOSPACE TEXT-VIEW-MONOSPACE "monospace" "gboolean" T T)
                       (OVERWRITE TEXT-VIEW-OVERWRITE "overwrite" "gboolean" T T)
                       (PIXELS-ABOVE-LINES TEXT-VIEW-PIXELS-ABOVE-LINES
                        "pixels-above-lines" "gint" T T)
                       (PIXELS-BELOW-LINES TEXT-VIEW-PIXELS-BELOW-LINES
                        "pixels-below-lines" "gint" T T)
                       (PIXELS-INSIDE-WRAP TEXT-VIEW-PIXELS-INSIDE-WRAP
                        "pixels-inside-wrap" "gint" T T)
                       (POPULATE-ALL TEXT-VIEW-POPULATE-ALL "populate-all"
                        "gboolean" T T)
                       (RIGHT-MARGIN TEXT-VIEW-RIGHT-MARGIN "right-margin"
                        "gint" T T)
                       (TABS TEXT-VIEW-TABS "tabs" "PangoTabArray" T T)
                       (TOP-MARGIN TEXT-VIEW-TOP-MARGIN "top-margin" "gint" T T)
                       (WRAP-MODE TEXT-VIEW-WRAP-MODE "wrap-mode"
                        "GtkWrapMode" T T)))
             (gobject:get-gtype-definition "GtkTextView"))))

;;; --- Signals ----------------------------------------------------------------

;;;     backspace
;;;     copy-clipboard
;;;     cut-clipboard
;;;     delete-from-cursor
;;;     extend-selection
;;;     insert-at-cursor
;;;     insert-emoji
;;;     move-cursor
;;;     move-viewport
;;;     paste-clipboard
;;;     populate-popup
;;;     preedit-changed
;;;     select-all
;;;     set-anchor
;;;     toggle-cursor-visible
;;;     toggle-overwrite

;;; --- Properties -------------------------------------------------------------

#+nil
(test gtk-text-view-properties
  (glib-test:with-check-memory (view)
    (is (typep (setf view (gtk:text-view-new)) 'gtk:text-view))
    (is-true (gtk:text-view-accepts-tab view))
    (is (= 0 (gtk:text-view-bottom-margin view)))
    (is (typep (gtk:text-view-buffer view) 'gtk:text-buffer))
    (is-false (setf (gtk:text-view-buffer view) nil))
    (is-true (gtk:text-view-cursor-visible view))
    (is-true (gtk:text-view-editable view))
    (is-false (gtk:text-view-im-module view))
    (is-true (gtk:text-view-indent view))
    (is-false (gtk:text-view-input-hints view))
    (is (eq :free-form (gtk:text-view-input-purpose view)))
    (is (eq :left (gtk:text-view-justification view)))
    (is (= 0 (gtk:text-view-left-margin view)))
    (is-false (gtk:text-view-monospace view))
    (is-false (gtk:text-view-overwrite view))
    (is (= 0 (gtk:text-view-pixels-above-lines view)))
    (is (= 0 (gtk:text-view-pixels-below-lines view)))
    (is (= 0 (gtk:text-view-pixels-inside-wrap view)))
    (is-false (gtk:text-view-populate-all view))
    (is (= 0 (gtk:text-view-right-margin view)))
    (is-false (gtk:text-view-tabs view))
    (is (= 0 (gtk:text-view-top-margin view)))
    (is (eq :none (gtk:text-view-wrap-mode view)))))

;;; --- Style Properties -------------------------------------------------------

;;;     error-underline-color

;; This test causes memory errors. Check the GdkColor implementation!

#+nil
(test gtk-text-view-child-properties
  (glib-test:with-check-memory (view)
    (is (typep (setf view (gtk:text-view-new)) 'gtk:text-view))
    (is (gdk:color-equal (gdk:color-new :red 51143 :green 5654 :blue 11051)
                         (gtk:widget-style-property view
                                                    "error-underline-color")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_text_view_new


(test gtk-text-view-new
  (glib-test:with-check-memory (view)
    (is (typep (setf view (gtk:text-view-new)) 'gtk:text-view))))

;;;     gtk_text_view_new_with_buffer

#+nil
(test gtk-text-view-new-with-buffer
  (glib-test:with-check-memory ((view 2) buffer :strong 1)
    (is (typep (setf buffer (gtk:text-buffer-new)) 'gtk:text-buffer))
    (is (typep (setf view (gtk:text-view-new-with-buffer buffer)) 'gtk:text-view))
    ;; Remove reference
    (is-false (setf (gtk:text-view-buffer view) nil))))

;;;     gtk_text_view_get_hadjustment                       Deprecated 3.0
;;;     gtk_text_view_get_vadjustment                       Deprecated 3.0

;;;     gtk_text_view_scroll_to_mark
;;;     gtk_text_view_scroll_to_iter
;;;     gtk_text_view_scroll_mark_onscreen
;;;     gtk_text_view_move_mark_onscreen
;;;     gtk_text_view_place_cursor_onscreen
;;;
;;;     gtk_text_view_get_visible_rect
;;;     gtk_text_view_get_iter_location
;;;     gtk_text_view_get_cursor_locations
;;;     gtk_text_view_get_line_at_y
;;;     gtk_text_view_get_line_yrange
;;;     gtk_text_view_get_iter_at_location
;;;     gtk_text_view_get_iter_at_position
;;
;;;     gtk_text_view_buffer_to_window_coords
;;;     gtk_text_view_window_to_buffer_coords
;;;
;;;     gtk_text_view_get_window
;;;     gtk_text_view_get_window_type
;;;     gtk_text_view_set_border_window_size
;;;     gtk_text_view_get_border_window_size
;;;
;;;     gtk_text_view_forward_display_line
;;;     gtk_text_view_backward_display_line
;;;     gtk_text_view_forward_display_line_end
;;;     gtk_text_view_backward_display_line_start
;;;     gtk_text_view_starts_display_line
;;;     gtk_text_view_move_visually
;;;     gtk_text_view_add_child_at_anchor

;;;     gtk_text_view_add_child_in_window
;;;     gtk_text_view_move_child

;;;     gtk_text_view_reset_cursor_blink

;;;     gtk_text_view_default_attributes                    not implemented

;;;     gtk_text_view_im_context_filter_keypress
;;;     gtk_text_view_reset_im_context

;;; 2026-08-30
