(in-package :gtk-test)

(def-suite gtk-color-selection :in gtk-suite)
(in-suite gtk-color-selection)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkColorSelection

(test container-class
  ;; Type check
  (is (g:type-is-object "GtkColorSelection"))
  ;; Check the registered name
  (is (eq 'gtk:color-selection
          (glib:symbol-for-gtype "GtkColorSelection")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkColorSelection")
          (g:gtype (cffi:foreign-funcall "gtk_color_selection_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkBox")
          (g:type-parent "GtkColorSelection")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkColorSelection")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
             (list-interfaces "GtkColorSelection")))
  ;; Check the class properties
  (is (equal '("current-alpha" "current-color" "current-rgba"
               "has-opacity-control" "has-palette")
             (list-properties "GtkColorSelection")))
  ;; Check the style properties
  (is (equal '()
             (list-style-properties "GtkColorSelection")))
  ;; Check the child properties
  (is (equal '("expand" "fill" "pack-type" "padding" "position")
             (list-child-properties "GtkColorSelection")))
  ;; Check the signals
  (is (equal '("color-changed")
             (list-signals "GtkColorSelection")))
  ;; CSS information
  (is (string= "box"
               (gtk:widget-class-css-name "GtkColorSelection")))
  (is (string=
"[box.vertical:dir(ltr)]
  box.horizontal:dir(ltr)
    box.vertical:dir(ltr)
      widget:dir(ltr)
      box.horizontal:dir(ltr)
        frame:dir(ltr)
          border:dir(ltr)
          box.horizontal:dir(ltr)
            widget:dir(ltr)
            widget:dir(ltr)
        button:dir(ltr)
          image:dir(ltr)
    box.vertical:dir(ltr)
      grid.horizontal:dir(ltr)
        label:dir(ltr)
        spinbutton.horizontal:dir(ltr)
          undershoot.left:dir(ltr)
          undershoot.right:dir(ltr)
          entry:dir(ltr)
          button.down:dir(ltr)
          button.up:dir(ltr)
        label:dir(ltr)
        spinbutton.horizontal:dir(ltr)
          undershoot.left:dir(ltr)
          undershoot.right:dir(ltr)
          entry:dir(ltr)
          button.down:disabled:dir(ltr)
          button.up:dir(ltr)
        label:dir(ltr)
        spinbutton.horizontal:dir(ltr)
          undershoot.left:dir(ltr)
          undershoot.right:dir(ltr)
          entry:dir(ltr)
          button.down:disabled:dir(ltr)
          button.up:dir(ltr)
        label:dir(ltr)
        spinbutton.horizontal:dir(ltr)
          undershoot.left:dir(ltr)
          undershoot.right:dir(ltr)
          entry:dir(ltr)
          button.down:disabled:dir(ltr)
          button.up:dir(ltr)
        label:dir(ltr)
        spinbutton.horizontal:dir(ltr)
          undershoot.left:dir(ltr)
          undershoot.right:dir(ltr)
          entry:dir(ltr)
          button.down:disabled:dir(ltr)
          button.up:dir(ltr)
        label:dir(ltr)
        spinbutton.horizontal:dir(ltr)
          undershoot.left:dir(ltr)
          undershoot.right:dir(ltr)
          entry:dir(ltr)
          button.down:disabled:dir(ltr)
          button.up:dir(ltr)
        separator.horizontal:dir(ltr)
        [label:dir(ltr)]
        [scale.horizontal:dir(ltr)]
          contents
            trough:dir(ltr)
              slider:dir(ltr)
              highlight.top:dir(ltr)
        [entry:dir(ltr)]
          undershoot.left:dir(ltr)
          undershoot.right:dir(ltr)
        label:dir(ltr)
        entry:dir(ltr)
          undershoot.left:dir(ltr)
          undershoot.right:dir(ltr)
      [box.vertical:dir(ltr)]
        label:dir(ltr)
        grid.horizontal:dir(ltr)
          frame:dir(ltr)
            border:dir(ltr)
            widget:dir(ltr)
          frame:dir(ltr)
            border:dir(ltr)
            widget:dir(ltr)
          frame:dir(ltr)
            border:dir(ltr)
            widget:dir(ltr)
          frame:dir(ltr)
            border:dir(ltr)
            widget:dir(ltr)
          frame:dir(ltr)
            border:dir(ltr)
            widget:dir(ltr)
          frame:dir(ltr)
            border:dir(ltr)
            widget:dir(ltr)
          frame:dir(ltr)
            border:dir(ltr)
            widget:dir(ltr)
          frame:dir(ltr)
            border:dir(ltr)
            widget:dir(ltr)
          frame:dir(ltr)
            border:dir(ltr)
            widget:dir(ltr)
          frame:dir(ltr)
            border:dir(ltr)
            widget:dir(ltr)
          frame:dir(ltr)
            border:dir(ltr)
            widget:dir(ltr)
          frame:dir(ltr)
            border:dir(ltr)
            widget:dir(ltr)
          frame:dir(ltr)
            border:dir(ltr)
            widget:dir(ltr)
          frame:dir(ltr)
            border:dir(ltr)
            widget:dir(ltr)
          frame:dir(ltr)
            border:dir(ltr)
            widget:dir(ltr)
          frame:dir(ltr)
            border:dir(ltr)
            widget:dir(ltr)
          frame:dir(ltr)
            border:dir(ltr)
            widget:dir(ltr)
          frame:dir(ltr)
            border:dir(ltr)
            widget:dir(ltr)
          frame:dir(ltr)
            border:dir(ltr)
            widget:dir(ltr)
          frame:dir(ltr)
            border:dir(ltr)
            widget:dir(ltr)
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context
                       (make-instance 'gtk:color-selection))
                   :recurse)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkColorSelection" GTK-COLOR-SELECTION
                       (:SUPERCLASS GTK-BOX :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_color_selection_get_type")
                       ((CURRENT-ALPHA GTK-COLOR-SELECTION-CURRENT-ALPHA
                         "current-alpha" "guint" T T)
                        (CURRENT-COLOR GTK-COLOR-SELECTION-CURRENT-COLOR
                         "current-color" "GdkColor" T T)
                        (CURRENT-RGBA GTK-COLOR-SELECTION-CURRENT-RGBA
                         "current-rgba" "GdkRGBA" T T)
                        (HAS-OPACITY-CONTROL
                         GTK-COLOR-SELECTION-HAS-OPACITY-CONTROL
                         "has-opacity-control" "gboolean" T T)
                        (HAS-PALETTE GTK-COLOR-SELECTION-HAS-PALETTE
                         "has-palette" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkColorSelection"))))

;;; --- Properties -------------------------------------------------------------

;;;     current-alpha
;;;     current-color
;;;     current-rgba
;;;     has-opacity-control
;;;     has-palette

(test gtk-color-selection-properties
  (let ((selection (make-instance 'gtk:color-selection)))
    (is (= 65535 (gtk:color-selection-current-alpha selection)))
    (is (typep (gtk:color-selection-current-color selection) 'gdk:color))
    (is (typep (gtk:color-selection-current-rgba selection) 'gdk:rgba))
    (is-false (gtk:color-selection-has-opacity-control selection))
    (is-false (gtk:color-selection-has-palette selection))))

;;; --- Signals ----------------------------------------------------------------

;;;     color-changed

(test gtk-color-selection-color-changed-signal
  (let ((query (g:signal-query (g:signal-lookup "color-changed"
                                                "GtkColorSelection"))))
    (is (string= "color-changed" (g:signal-query-signal-name query)))
    (is (string= "GtkColorSelection"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (sort (mapcar #'g:type-name (g:signal-query-param-types query))
                     #'string<)))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_color_selection_new

(test gtk-color-selection-new
  (is (typep (gtk:color-selection-new) 'gtk:color-selection)))

;;;     gtk_color_selection_get_previous_alpha
;;;     gtk_color_selection_set_previous_alpha

(test gtk-color-selection-previous-alpha
  (let ((selection (gtk:color-selection-new)))
    (is (= 255 (setf (gtk:color-selection-previous-alpha selection) 255)))
    ;; TODO: The value is not set!? Why?
    (is (= 65535 (gtk:color-selection-previous-alpha selection)))))

;;;     gtk_color_selection_get_previous_color
;;;     gtk_color_selection_set_previous_color

(test gtk-color-selection-previous-color
  (let ((selection (gtk:color-selection-new)))
    (is (typep (setf (gtk:color-selection-previous-color selection)
                     (gdk:color-new :red 65535)) 'gdk:color))
    (is (gdk:color-equal (gdk:color-new :red 65535)
                         (gtk:color-selection-previous-color selection)))))

;;;     gtk_color_selection_get_previous_rgba
;;;     gtk_color_selection_set_previous_rgba

(test gtk-color-selection-previous-rgba
  (let ((selection (gtk:color-selection-new)))
    (is (typep (setf (gtk:color-selection-previous-rgba selection)
                     (gdk:rgba-new :red 1.0)) 'gdk:rgba))
    (is (gdk:rgba-equal (gdk:rgba-new :red 1.0 :alpha 1.0)
                        (gtk:color-selection-previous-rgba selection)))))

;;;     gtk_color_selection_is_adjusting
;;;     gtk_color_selection_palette_from_string
;;;     gtk_color_selection_palette_to_string
;;;     gtk_color_selection_set_change_palette_with_screen_hook

;;; --- 2023-6-14 --------------------------------------------------------------
