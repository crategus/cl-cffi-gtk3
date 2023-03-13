;;; ----------------------------------------------------------------------------
;;; gdk3.window.lisp
;;;
;;; The documentation of this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; Windows
;;;
;;;    Onscreen display areas in the target window system
;;;
;;; Types and Values
;;;
;;;     GdkWindow
;;;     GdkWindowType
;;;     GdkWindowWindowClass
;;;     GdkWindowHints
;;;     GdkGravity
;;;     GdkGeometry
;;;     GdkAnchorHints
;;;     GdkWindowEdge
;;;     GdkWindowTypeHint
;;;     GdkWindowAttr
;;;     GdkWindowAttributesType
;;;     GdkFullscreenMode
;;;     GdkFilterReturn
;;;     GdkModifierType        --> gdk.event-structures.lisp
;;;     GdkModifierIntent
;;;     GdkWMDecoration
;;;     GdkWMFunction
;;;
;;; Functions
;;;
;;;     gdk_window_new
;;;     gdk_window_destroy
;;;     gdk_window_get_window_type
;;;     gdk_window_get_display
;;;     gdk_window_get_screen
;;;     gdk_window_get_visual
;;;     gdk_window_at_pointer                              deprecated
;;;     gdk_window_show
;;;     gdk_window_show_unraised
;;;     gdk_window_hide
;;;     gdk_window_is_destroyed
;;;     gdk_window_is_visible
;;;     gdk_window_is_viewable
;;;     gdk_window_is_input_only
;;;     gdk_window_is_shaped
;;;     gdk_window_get_state
;;;     gdk_window_withdraw
;;;     gdk_window_iconify
;;;     gdk_window_deiconify
;;;     gdk_window_stick
;;;     gdk_window_unstick
;;;     gdk_window_maximize
;;;     gdk_window_unmaximize
;;;     gdk_window_fullscreen
;;;     gdk_window_fullscreen_on_monitor
;;;     gdk_window_unfullscreen
;;;     gdk_window_get_fullscreen_mode
;;;     gdk_window_set_fullscreen_mode
;;;     gdk_window_set_keep_above
;;;     gdk_window_set_keep_below
;;;     gdk_window_set_opacity
;;;     gdk_window_set_composited                          deprecated
;;;     gdk_window_get_composited                          deprecated
;;;     gdk_window_set_pass_through
;;;     gdk_window_get_pass_through
;;;     gdk_window_move
;;;     gdk_window_resize
;;;     gdk_window_move_resize
;;;     gdk_window_scroll
;;;     gdk_window_move_to_rect
;;;     gdk_window_move_region
;;;     gdk_window_flush                                   deprecated
;;;     gdk_window_has_native
;;;     gdk_window_ensure_native
;;;     gdk_window_reparent
;;;     gdk_window_raise
;;;     gdk_window_lower
;;;     gdk_window_restack
;;;     gdk_window_focus
;;;     gdk_window_register_dnd
;;;     gdk_window_begin_resize_drag
;;;     gdk_window_begin_resize_drag_for_device
;;;     gdk_window_begin_move_drag
;;;     gdk_window_begin_move_drag_for_device
;;;     gdk_window_show_window_menu
;;;     gdk_window_constrain_size
;;;     gdk_window_beep
;;;     gdk_window_get_scale_factor
;;;     gdk_window_set_opaque_region
;;;     gdk_window_create_gl_context
;;;     gdk_window_mark_paint_from_clip
;;;     gdk_window_get_clip_region
;;;     gdk_window_begin_paint_rect                        deprecated
;;;     gdk_window_begin_paint_region                      deprecated
;;;     gdk_window_end_paint                               deprecated
;;;     gdk_window_begin_draw_frame
;;;     gdk_window_end_draw_frame
;;;     gdk_window_get_visible_region
;;;     GdkWindowInvalidateHandlerFunc
;;;     gdk_window_set_invalidate_handler
;;;     gdk_window_invalidate_rect
;;;     gdk_window_invalidate_region
;;;     GdkWindowChildFunc
;;;     gdk_window_invalidate_maybe_recurse
;;;     gdk_window_get_update_area
;;;     gdk_window_freeze_updates
;;;     gdk_window_thaw_updates
;;;     gdk_window_process_all_updates                     deprecated
;;;     gdk_window_process_updates                         deprecated
;;;     gdk_window_set_debug_updates                       deprecated
;;;     gdk_window_enable_synchronized_configure           deprecated
;;;     gdk_window_configure_finished                      deprecated
;;;     gdk_window_get_frame_clock
;;;     gdk_window_set_user_data
;;;     gdk_window_set_override_redirect
;;;     gdk_window_set_accept_focus
;;;     gdk_window_get_accept_focus
;;;     gdk_window_set_focus_on_map
;;;     gdk_window_get_focus_on_map
;;;     gdk_window_add_filter
;;;     gdk_window_remove_filter
;;;     gdk_window_shape_combine_region
;;;     gdk_window_set_child_shapes
;;;     gdk_window_merge_child_shapes
;;;     gdk_window_input_shape_combine_region
;;;     gdk_window_set_child_input_shapes
;;;     gdk_window_merge_child_input_shapes
;;;     gdk_window_set_static_gravities                    deprecated
;;;     gdk_window_set_title
;;;     gdk_window_set_background                          deprecated
;;;     gdk_window_set_background_rgba                     deprecated
;;;     gdk_window_set_background_pattern                  deprecated
;;;     gdk_window_get_background_pattern                  deprecated
;;;     gdk_window_set_cursor                              Accessor
;;;     gdk_window_get_cursor                              Accessor
;;;     gdk_window_get_user_data
;;;     gdk_window_get_geometry
;;;     gdk_window_set_geometry_hints
;;;     gdk_window_get_width
;;;     gdk_window_get_height
;;;     gdk_window_set_icon_list
;;;     gdk_window_set_modal_hint
;;;     gdk_window_get_modal_hint
;;;     gdk_window_set_type_hint
;;;     gdk_window_get_type_hint
;;;     gdk_window_set_shadow_width ()
;;;     gdk_window_set_skip_taskbar_hint
;;;     gdk_window_set_skip_pager_hint
;;;     gdk_window_set_urgency_hint
;;;     gdk_window_get_position
;;;     gdk_window_get_root_origin
;;;     gdk_window_get_frame_extents
;;;     gdk_window_get_origin
;;;     gdk_window_get_root_coords
;;;     gdk_window_get_pointer                             deprecated
;;;     gdk_window_get_device_position
;;;     gdk_window_get_device_position_double ()
;;;     gdk_window_get_parent
;;;     gdk_window_get_toplevel
;;;     gdk_window_get_children
;;;     gdk_window_get_children_with_user_data ()
;;;     gdk_window_peek_children
;;;     gdk_window_get_events
;;;     gdk_window_set_events
;;;     gdk_window_set_icon_name
;;;     gdk_window_set_transient_for
;;;     gdk_window_set_role
;;;     gdk_window_set_startup_id
;;;     gdk_window_set_group
;;;     gdk_window_get_group
;;;     gdk_window_set_decorations
;;;     gdk_window_get_decorations
;;;     gdk_window_set_functions
;;;     gdk_get_default_root_window
;;;     gdk_window_get_support_multidevice
;;;     gdk_window_set_support_multidevice
;;;     gdk_window_get_device_cursor
;;;     gdk_window_set_device_cursor
;;;     gdk_window_get_device_events
;;;     gdk_window_set_device_events
;;;     gdk_window_get_source_events
;;;     gdk_window_set_source_events
;;;     gdk_window_get_event_compression ()
;;;     gdk_window_set_event_compression ()
;;;     gdk_offscreen_window_get_surface
;;;     gdk_offscreen_window_set_embedder
;;;     gdk_offscreen_window_get_embedder
;;;     gdk_window_geometry_changed
;;;     gdk_window_coords_from_parent
;;;     gdk_window_coords_to_parent
;;;     gdk_window_get_effective_parent
;;;     gdk_window_get_effective_toplevel
;;;
;;; Properties
;;;
;;;     cursor
;;;
;;; Signals
;;;
;;;     create-surface
;;;     from-embedder
;;;     moved-to-rect
;;;     pick-embedded-child
;;;     to-embedder
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkWindow
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; enum GdkWindowType
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkWindowType" window-type
  (:export t
   :type-initializer "gdk_window_type_get_type")
  (:root 0)
  (:toplevel 1)
  (:child 2)
  (:temp 3)
  (:foreign 4)
  (:offscreen 5)
  (:subsurface 6))

#+liber-documentation
(setf (liber:alias-for-symbol 'window-type)
      "GEnum"
      (liber:symbol-documentation 'window-type)
 "@version{2023-2-26}
  @short{Describes the kind of the window.}
  @begin{pre}
(define-g-enum \"GdkWindowType\" window-type
  (:export t
   :type-initializer \"gdk_window_type_get_type\")
  (:root 0)
  (:toplevel 1)
  (:child 2)
  (:temp 3)
  (:foreign 4)
  (:offscreen 5)
  (:subsurface 6))
  @end{pre}
  @begin[code]{table}
    @entry[:root]{Root window, this window has no parent, covers the entire
      screen, and is created by the window system.}
    @entry[:toplevel]{Toplevel window, used to implement the @class{gtk:window}
      class.}
    @entry[:child]{Child window, used to implement e.g. the @class{gtk:entry}
      class.}
    @entry[:temp]{Override redirect temporary window, used to implement the
      @class{gtk:menu} class.}
    @entry[:foreign]{Foreign window.}
    @entry[:offscreen]{Offscreen window, see the section called
      \"Offscreen Windows\".}
    @entry[:subsurface]{Subsurface-based window. This window is visually tied
      to a toplevel, and is moved/stacked with it. Currently this window type
      is only implemented in Wayland.}
  @end{table}
  @see-class{gdk:window}
  @see-class{gtk:window}
  @see-class{gtk:entry}
  @see-class{gtk:menu}")

;;; ----------------------------------------------------------------------------
;;; enum GdkWindowWindowClass
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkWindowWindowClass" window-window-class
  (:export t
   :type-initializer "gdk_window_window_class_get_type")
  (:input-output 0)
  (:input-only 1))

#+liber-documentation
(setf (liber:alias-for-symbol 'window-window-class)
      "GEnum"
      (liber:symbol-documentation 'window-window-class)
 "@version{2023-2-26}
  @begin{short}
    @code{:input-output} windows are the standard kind of window you might
    expect.
  @end{short}
  Such windows receive events and are also displayed on screen.
  @code{:input-only} windows are invisible. They are usually placed above other
  windows in order to trap or filter the events. You cannot draw on
  @code{:input-only} windows.
  @begin{pre}
(define-g-enum \"GdkWindowWindowClass\" window-window-class
  (:export t
   :type-initializer \"gdk_window_window_class_get_type\")
  (:input-output 0)
  (:input-only 1))
  @end{pre}
  @begin[code]{table}
    @entry[:input-output]{Window for graphics and events.}
    @entry[:input-only]{Window for events only.}
  @end{table}
  @see-class{gdk:window}")

;;; ----------------------------------------------------------------------------
;;; enum GdkWindowHints
;;; ----------------------------------------------------------------------------

(define-g-flags "GdkWindowHints" window-hints
  (:export t
   :type-initializer "gdk_window_hints_get_type")
  (:pos 1)
  (:min-size 2)
  (:max-size 4)
  (:base-size 8)
  (:aspect 16)
  (:resize-inc 32)
  (:win-gravity 64)
  (:user-pos 128)
  (:user-size 256))

#+liber-documentation
(setf (liber:alias-for-symbol 'window-hints)
      "GFlags"
      (liber:symbol-documentation 'window-hints)
 "@version{2023-2-26}
  @begin{short}
    Used to indicate which fields of a @symbol{gdk:geometry} instance should
    be paid attention to.
  @end{short}
  Also, the presence/absence of the @code{:pos}, @code{:user-pos}, and
  @code{:user-size} values is significant, though they do not directly refer to
  @symbol{gdk:geometry} fields. The @code{:user-pos} value will be set
  automatically by the @class{gtk:window} widget if you call the
  @fun{gtk:window-move} function. The @code{:user-pos} and @code{:user-size}
  values should be set if the user specified a size/position using a - geometry
  command-line argument. The @fun{gtk:window-parse-geometry} function
  automatically sets these flags.
  @begin{pre}
(define-g-flags \"GdkWindowHints\" window-hints
  (:export t
   :type-initializer \"gdk_window_hints_get_type\")
  (:pos 1)
  (:min-size 2)
  (:max-size 4)
  (:base-size 8)
  (:aspect 16)
  (:resize-inc 32)
  (:win-gravity 64)
  (:user-pos 128)
  (:user-size 256))
  @end{pre}
  @begin[code]{table}
    @entry[:pos]{Indicates that the program has positioned the window.}
    @entry[:min-size]{Min size fields are set.}
    @entry[:max-size]{Max size fields are set.}
    @entry[:base-size]{Base size fields are set.}
    @entry[:aspect]{Aspect ratio fields are set.}
    @entry[:resize-inc]{Resize increment fields are set.}
    @entry[:win-gravity]{Window gravity field is set.}
    @entry[:user-pos]{Indicates that the window's position was explicitly set
      by the user.}
    @entry[:user-size]{Indicates that the window's size was explicitly set by
      the user.}
  @end{table}
  @see-symbol{gdk:geometry}
  @see-class{gtk:window}
  @see-function{gtk:window-parse-geometry}")

;;; ----------------------------------------------------------------------------
;;; enum GdkGravity
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkGravity" gravity
  (:export t
   :type-initializer "gdk_gravity_get_type")
  (:north-west 1)
  :north
  :north-east
  :west
  :center
  :east
  :south-west
  :south
  :south-east
  :static)

#+liber-documentation
(setf (liber:alias-for-symbol 'gravity)
      "GEnum"
      (liber:symbol-documentation 'gravity)
 "@version{2023-2-26}
  @begin{short}
    Defines the reference point of a window and the meaning of coordinates
    passed to the @fun{gtk:window-move} function.
  @end{short}
  See the @fun{gtk:window-move} function and the \"implementation notes\"
  section of the Extended Window Manager Hints specification for more details.
  @begin{pre}
(define-g-enum \"GdkGravity\" gravity
  (:export t
   :type-initializer \"gdk_gravity_get_type\")
  (:north-west 1)
  :north
  :north-east
  :west
  :center
  :east
  :south-west
  :south
  :south-east
  :static)
  @end{pre}
  @begin[code]{table}
    @entry[:north-west]{The reference point is at the top left corner.}
    @entry[:north]{The reference point is in the middle of the top edge.}
    @entry[:north-east]{The reference point is at the top right corner.}
    @entry[:west]{The reference point is at the middle of the left edge.}
    @entry[:center]{The reference point is at the center of the window.}
    @entry[:east]{The reference point is at the middle of the right edge.}
    @entry[:south-west]{The reference point is at the lower left corner.}
    @entry[:south]{The reference point is at the middle of the lower edge.}
    @entry[:south-east]{The reference point is at the lower right corner.}
    @entry[:static]{The reference point is at the top left corner of the
      window itself, ignoring window manager decorations.}
  @end{table}
  @see-class{gdk:window}
  @see-function{gtk:window-move}")

;;; ----------------------------------------------------------------------------
;;; struct GdkGeometry
;;; ----------------------------------------------------------------------------

(defcstruct geometry
  (min-width :int)
  (min-height :int)
  (max-width :int)
  (max-height :int)
  (base-width :int)
  (base-height :int)
  (width-increment :int)
  (height-increment :int)
  (min-aspect :double)
  (max-aspect :double)
  (win-gravity gravity))

#+liber-documentation
(setf (liber:alias-for-symbol 'geometry)
      "CStruct"
      (liber:symbol-documentation 'geometry)
 "@version{2023-2-26}
  @begin{short}
    The @sym{gdk:geometry} structure gives the window manager information about
    a window's geometry constraints.
  @end{short}
  Normally you would set these on the GTK level using the
  @fun{gtk:window-set-geometry-hints} function. The @class{gtk:window} widget
  then sets the hints on the @class{gdk:window} object it creates.

  The @fun{gdk:window-set-geometry-hints} function expects the hints to be fully
  valid already and simply passes them to the window manager. In contrast, the
  @fun{gtk:window-set-geometry-hints} function performs some interpretation.
  For example, the @class{gtk:window} widget will apply the hints to the
  geometry widget instead of the toplevel window, if you set a geometry widget.
  Also, the @code{min-width}/@code{min-height}/@code{max-width}/@code{max-height}
  fields may be set to -1, and the @class{gtk:window} widget will substitute the
  size request of the window or geometry widget. If the minimum size hint is not
  provided, the @class{gtk:window} widget will use its requisition as the
  minimum size. If the minimum size is provided and a geometry widget is set,
  the @class{gtk:window} widget will take the minimum size as the minimum size
  of the geometry widget rather than the entire window. The base size is treated
  similarly.

  The canonical use-case for the @fun{gtk:window-set-geometry-hints} function
  is to get a terminal widget to resize properly. Here, the terminal text area
  should be the geometry widget. The @class{gtk:window} widget will then
  automatically set the base size to the size of other widgets in the terminal
  window, such as the menubar and scrollbar. Then, the @code{width-increment}
  and @code{height-incement} fields should be set to the size of one character
  in the terminal. Finally, the base size should be set to the size of one
  character. The net effect is that the minimum size of the terminal will have
  a 1 x 1 character terminal area, and only terminal sizes on the
  \"character grid\" will be allowed.

  Here is an example of how the terminal example would be implemented, assuming
  a terminal area widget called \"terminal\" and a toplevel window \"toplevel\":
  @begin{pre}
GdkGeometry hints;

hints.base_width = terminal->char_width;
hints.base_height = terminal->char_height;
hints.min_width = terminal->char_width;
hints.min_height = terminal->char_height;
hints.width_inc = terminal->char_width;
hints.height_inc = terminal->char_height;

gtk_window_set_geometry_hints (GTK_WINDOW (toplevel),
                               GTK_WIDGET (terminal),
                               &hints,
                               GDK_HINT_RESIZE_INC |
                               GDK_HINT_MIN_SIZE |
                               GDK_HINT_BASE_SIZE);
  @end{pre}
  The other useful fields are the @code{min-aspect} and @code{max-aspect}
  fields. These contain a width/height ratio as a floating point number. If a
  geometry widget is set, the aspect applies to the geometry widget rather than
  the entire window. The most common use of these hints is probably to set
  @code{min-aspect} and @code{max-aspect} to the same value, thus forcing the
  window to keep a constant aspect ratio.
  @begin{pre}
(defcstruct geometry
  (min-width :int)
  (min-height :int)
  (max-width :int)
  (max-height :int)
  (base-width :int)
  (base-height :int)
  (width-increment :int)
  (height-increment :int)
  (min-aspect :double)
  (max-aspect :double)
  (win-gravity gravity))
  @end{pre}
  @begin[code]{table}
    @entry[min-width]{An integer with the minimum width of window or -1 to use
      requisition, with the @class{gtk:window} widget only.}
    @entry[min-height]{An integer with the minimum height of window or -1 to
      use requisition, with the @class{gtk:window} widget only.}
    @entry[max-width]{An integer with the maximum width of window or -1 to use
      requisition, with the @class{gtk:window} widget only.}
    @entry[max-height]{An integer with the maximum height of window or -1 to
      use requisition, with the @class{gtk:window} widget only.}
    @entry[base-width]{An integer with the allowed window widths are
      @code{base-width} + @code{width-inc} * @code{N} where @code{N} is any
      integer, -1 is allowed with the @class{gtk:window} widget.}
    @entry[base-height]{An integer with the allowed window widths are
      @code{base-height} + @code{height-inc} * @code{N} where @code{N} is any
      integer, -1 is allowed with the @class{gtk:window} widget.}
    @entry[width-increment]{An integer with the width resize increment.}
    @entry[height-increment]{An integer with the height resize increment.}
    @entry[min-aspect]{A double float with the minimum width/height ratio.}
    @entry[max-aspect]{A double float with the maximum width/height ratio.}
    @entry[win-gravity]{A @symbol{gdk:gravity} value for the window gravity,
      see the @fun{gtk:window-gravity} function.}
  @end{table}
  @see-class{gdk:window}
  @see-class{gtk:window}
  @see-function{gdk:window-set-geometry-hints}
  @see-function{gtk:window-set-geometry-hints}
  @see-function{gtk:window-gravity}")

(export 'geometry)

;;; ----------------------------------------------------------------------------
;;; enum GdkAnchorHints
;;; ----------------------------------------------------------------------------

(define-g-flags "GdkAnchorHints" anchor-hints
  (:export t
   :type-initializer "gdk_anchor_hints_get_type")
  (:flip-x   #.(ash 1 0))
  (:flip-y   #.(ash 1 1))
  (:slide-x  #.(ash 1 2))
  (:slide-y  #.(ash 1 3))
  (:resize-x #.(ash 1 4))
  (:resize-y #.(ash 1 5))
  (:flip    3)  ; :flip-x   | :flip-y
  (:slide  12)  ; :slide-x  | :slide-y
  (:resize 48)) ; :resize-x | :resize-y

#+liber-documentation
(setf (liber:alias-for-symbol 'anchor-hints)
      "GFlags"
      (liber:symbol-documentation 'anchor-hints)
 "@version{2023-3-13}
  @begin{short}
    Positioning hints for aligning a window relative to a rectangle.
  @end{short}
  These hints determine how the window should be positioned in the case that
  the window would fall off-screen if placed in its ideal position. For example,
  @code{:flip-x} will replace @code{:north-west} with @code{:north-east} and
  vice versa if the window extends beyond the left or right edges of the
  monitor.

  If @code{:slide-x} is set, the window can be shifted horizontally to fit
  on-screen. If @code{:resize-x} is set, the window can be shrunken
  horizontally to fit.

  In general, when multiple flags are set, flipping should take precedence
  over sliding, which should take precedence over resizing.
  @begin{pre}
(define-g-flags \"GdkAnchorHints\" anchor-hints
  (:export t
   :type-initializer \"gdk_anchor_hints_get_type\")
  (:flip-x   #.(ash 1 0))
  (:flip-y   #.(ash 1 1))
  (:slide-x  #.(ash 1 2))
  (:slide-y  #.(ash 1 3))
  (:resize-x #.(ash 1 4))
  (:resize-y #.(ash 1 5))
  (:flip    3)  ; :flip-x   | :flip-y
  (:slide  12)  ; :slide-x  | :slide-y
  (:resize 48)) ; :resize-x | :resize-y
  @end{pre}
  @begin[code]{table}
    @entry[:flip-x]{Allow flipping anchors horizontally.}
    @entry[:fliy-y]{Allow flipping anchors vertically.}
    @entry[:slide-x]{Allow sliding window horizontally.}
    @entry[:slide-y]{Allow sliding window vertically.}
    @entry[:resize-x]{Allow resizing window horizontally.}
    @entry[:resize-y]{Allow resizing window vertically.}
    @entry[:flip]{Allow flipping anchors on both axes.}
    @entry[:slide]{Allow sliding window on both axes.}
    @entry[:resize]{Allow resizing window on both axes.}
  @end{table}
  @see-class{gdk:window}")

;;; ----------------------------------------------------------------------------
;;; enum GdkWindowEdge
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkWindowEdge" window-edge
  (:export t
   :type-initializer "gdk_window_edge_get_type")
  (:north-west 0)
  (:north 1)
  (:north-east 2)
  (:west 3)
  (:east 4)
  (:south-west 5)
  (:south 6)
  (:south-east 7))

#+liber-documentation
(setf (liber:alias-for-symbol 'window-edge)
      "GEnum"
      (liber:symbol-documentation 'window-edge)
 "@version{2023-2-26}
  @begin{short}
    Determines a window edge or corner.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkWindowEdge\" window-edge
  (:export t
   :type-initializer \"gdk_window_edge_get_type\")
  (:north-west 0)
  (:north 1)
  (:north-east 2)
  (:west 3)
  (:east 4)
  (:south-west 5)
  (:south 6)
  (:south-east 7))
  @end{pre}
  @begin[code]{table}
    @entry[:north-west]{The top left corner.}
    @entry[:north]{The top edge.}
    @entry[:north-east]{The top right corner.}
    @entry[:west]{The left edge.}
    @entry[:east]{The right edge.}
    @entry[:south-west]{The lower left corner.}
    @entry[:south]{The lower edge.}
    @entry[:south-east]{The lower right corner.}
  @end{table}
  @see-class{gdk:window}")

;;; ----------------------------------------------------------------------------
;;; enum GdkWindowTypeHint
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkWindowTypeHint" window-type-hint
  (:export t
   :type-initializer "gdk_window_type_hint_get_type")
  (:normal 0)
  (:dialog 1)
  (:menu 2)
  (:toolbar 3)
  (:splashscreen 4)
  (:utility 5)
  (:dock 6)
  (:desktop 7)
  (:dropdown-menu 8)
  (:popup-menu 9)
  (:tooltip 10)
  (:notification 11)
  (:combo 12)
  (:dnd 13))

#+liber-documentation
(setf (liber:alias-for-symbol 'window-type-hint)
      "GEnum"
      (liber:symbol-documentation 'window-type-hint)
 "@version{2023-2-26}
  @begin{short}
    These are hints for the window manager that indicate what type of function
    the window has.
  @end{short}
  The window manager can use this when determining decoration and behaviour of
  the window. The hint must be set before mapping the window.

  See the Extended Window Manager Hints specification for more details about
  window types.
  @begin{pre}
(define-g-enum \"GdkWindowTypeHint\" window-type-hint
  (:export t
   :type-initializer \"gdk_window_type_hint_get_type\")
  (:normal 0)
  (:dialog 1)
  (:menu 2)
  (:toolbar 3)
  (:splashscreen 4)
  (:utility 5)
  (:dock 6)
  (:desktop 7)
  (:dropdown-menu 8)
  (:popup-menu 9)
  (:tooltip 10)
  (:notification 11)
  (:combo 12)
  (:dnd 13))
  @end{pre}
  @begin[code]{table}
    @entry[:normal]{Normal toplevel window.}
    @entry[:dialog]{Dialog window.}
    @entry[:menu]{Window used to implement a menu. GTK uses this hint only for
      the deprecated torn-off menus.}
    @entry[:toolbar]{Window used to implement toolbars.}
    @entry[:splashscreen]{Window used to display a splash screen during
      application startup.}
    @entry[:utility]{Utility windows which are not detached toolbars or
      dialogs.}
    @entry[:dock]{Used for creating dock or panel windows.}
    @entry[:desktop]{Used for creating the desktop background window.}
    @entry[:dropdown-menu]{A menu that belongs to a menubar.}
    @entry[:popup-menu]{A menu that does not belong to a menubar, e.g. a
      context menu.}
    @entry[:tooltip]{A tooltip.}
    @entry[:notification]{A notification - typically a \"bubble\" that belongs
      to a status icon.}
    @entry[:combo]{A popup from a combo box.}
    @entry[:dnd]{A window that is used to implement a DND cursor.}
  @end{table}
  @see-class{gdk:window}")

;;; ----------------------------------------------------------------------------
;;; struct GdkWindowAttr
;;; ----------------------------------------------------------------------------

(defcstruct window-attr
  (title :string)
  (event-mask event-mask)
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (wclass window-window-class)
  (visual (g:object visual))
  (window-type window-type)
  (cursor (g:object cursor))
  (wmclass-name :string)
  (wmclass-class :string)
  (override-redirect :boolean)
  (type-hint window-type-hint))

#+liber-documentation
(setf (liber:alias-for-symbol 'window-attr)
      "CStruct"
      (liber:symbol-documentation 'window-attr)
 "@version{2023-2-26}
  @begin{short}
    Attributes to use for a newly created window.
  @end{short}
  @begin{pre}
(defcstruct window-attr
  (title :string)
  (event-mask event-mask)
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (wclass window-window-class)
  (visual (g:object visual))
  (window-type window-type)
  (cursor (g:object cursor))
  (wmclass-name :string)
  (wmclass-class :string)
  (override-redirect :boolean)
  (type-hint window-type-hint))
  @end{pre}
  @begin[code]{table}
    @entry[title]{A string with the title of the window for toplevel windows.}
    @entry[event-mask]{a @symbol{gdk:event-mask} value, see the
      @fun{gdk:window-events} function.}
    @entry[x]{An integer with the x coordinate relative to parent window,
      see the @fun{gdk:window-move} function.}
    @entry[y]{An integer with the y coordinate relative to parent window,
      see the @fun{gdk:window-move} function.}
    @entry[width]{An integer with the width of the window.}
    @entry[height]{An integer with the height of window.}
    @entry[wclass]{A @symbol{gdk:window-window-class} value,
      @code{:input-output} for a normal window or @code{:input-only} for an
      invisible window that receives events.}
    @entry[visual]{A @class{gdk:visual} object for the window.}
    @entry[window-type]{A @symbol{gdk:window-type} value.}
    @entry[cursor]{A @class{gdk:cursor} object for the window, see the
      @fun{gdk:window-cursor} function.}
    @entry[wmclass-name]{A string, do not use.}
    @entry[wmclass-class]{A string, do not use.}
    @entry[override-redirect]{@em{True} to bypass the window manager.}
    @entry[type-hint]{A @symbol{gdk:window-type-hint} value of the function of
      the window.}
  @end{table}
  @see-class{gdk:visual}
  @see-function{gdk:window-move}
  @see-function{gdk:window-cursor}
  @see-function{gdk:window-events}")

(export 'window-attr)

;;; ----------------------------------------------------------------------------
;;; enum GdkWindowAttributesType
;;; ----------------------------------------------------------------------------

(define-g-flags "GdkWindowAttributesType" window-attributes-type
  (:export t
   :type-initializer "gdk_window_attributes_type_get_type")
  (:title 2)
  (:x 4)
  (:y 8)
  (:cursor 16)
  (:visual 32)
  (:wmclass 64)
  (:noredir 128)
  (:type-hint 256))

#+liber-documentation
(setf (liber:alias-for-symbol 'window-attributes-type)
      "GFlags"
      (liber:symbol-documentation 'window-attributes-type)
 "@version{2023-2-26}
  @begin{short}
    Used to indicate which fields in the @symbol{gdk:window-attr} structure
    should be honored.
  @end{short}
  For example, if you filled in the @code{cursor} and @code{x} fields of the
  @symbol{gdk:window-attr} structure, pass @code{'(:x :cursor)} to the
  @fun{gdk:window-new} function. Fields in the @symbol{gdk:window-attr}
  structure not covered by a bit in this enumeration are required. For example,
  the @code{width}/@code{height}, @code{wclass}, and @code{window-type} fields
  are required, they have no corresponding flag in the
  @symbol{gdk:window-attributes-type} flags.
  @begin{pre}
(define-g-flags \"GdkWindowAttributesType\" window-attributes-type
  (:export t
   :type-initializer \"gdk_window_attributes_type_get_type\")
  (:title 2)
  (:x 4)
  (:y 8)
  (:cursor 16)
  (:visual 32)
  (:wmclass 64)
  (:noredir 128)
  (:type-hint 256))
  @end{pre}
  @begin[code]{table}
    @entry[:title]{Honor the @code{title} field.}
    @entry[:x]{Honor the @code{x} coordinate field.}
    @entry[:y]{Honor the @code{y} coordinate field.}
    @entry[:cursor]{Honor the @code{cursor} field.}
    @entry[:visual]{Honor the @code{visual} field.}
    @entry[:wmclass]{Honor the @code{wmclass-class} and @code{wmclass-name}
      fields.}
    @entry[:noredir]{Honor the @code{override-redirect} field.}
    @entry[:type-hint]{Honor the @code{type-hint} field.}
  @end{table}
  @see-symbol{gdk:window-attr}
  @see-function{gdk:window-new}")

;;; ----------------------------------------------------------------------------
;;; enum GdkFullscreenMode
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkFullscreenMode" fullscreen-mode
  (:export t
   :type-initializer "gdk_fullscreen_mode_get_type")
  (:current-monitor 0)
  (:all-monitors 1))

#+liber-documentation
(setf (liber:alias-for-symbol 'fullscreen-mode)
      "GEnum"
      (liber:symbol-documentation 'fullscreen-mode)
 "@version{2023-2-26}
  @begin{short}
    Indicates which monitor (in a multi-head setup) a window should span over
    when in fullscreen mode.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkFullscreenMode\" fullscreen-mode
  (:export t
   :type-initializer \"gdk_fullscreen_mode_get_type\")
  (:on-current-monitor 0)
  (:on-all-monitors 1))
  @end{pre}
  @begin[code]{table}
    @entry[:current-monitor]{Fullscreen on current monitor only.}
    @entry[:all-monitors]{Span across all monitors when fullscreen.}
  @end{table}
  @see-class{gdk:window}")

;;; ----------------------------------------------------------------------------
;;; enum GdkFilterReturn                                   not exported
;;; ----------------------------------------------------------------------------

;; TODO: The filter functionality is not implemented. Consider to remove
;; this code. The enumeration is not exported.

(define-g-enum "GdkFilterReturn" filter-return
  (:export nil
   :type-initializer "gdk_filter_return_get_type")
  (:continue 0)
  (:translate 1)
  (:remove 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'filter-return)
      "GEnum"
      (liber:symbol-documentation 'filter-return)
 "@version{2023-2-26}
  @begin{short}
    Specifies the result of applying a @code{GdkFilterFunc} to a native event.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkFilterReturn\" filter-return
  (:export t
   :type-initializer \"gdk_filter_return_get_type\")
  (:continue 0)
  (:translate 1)
  (:remove 2))
  @end{pre}
  @begin[code]{table}
    @entry[:continue]{Event not handled, continue processing.}
    @entry[:translate]{Native event translated into a GDK event and stored in
      the event structure that was passed in.}
    @entry[:remove]{Event handled, terminate processing.}
  @end{table}
  @see-class{gdk:window}")

;;; ----------------------------------------------------------------------------
;;; enum GdkModifierIntent
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkModifierIntent" modifier-intent
  (:export t
   :type-initializer "gdk_modifier_intent_get_type")
  (:primary-accelerator 0)
  (:context-menu 1)
  (:extend-selection 2)
  (:modify-selection 3)
  (:no-text-input 4)
  (:shift-group 5)
  (:default-mod-mask 6))

#+liber-documentation
(setf (liber:alias-for-symbol 'modifier-intent)
      "GEnum"
      (liber:symbol-documentation 'modifier-intent)
 "@version{2023-2-26}
  @begin{short}
    This enumeration is used with the @fun{gdk:keymap-modifier-mask} function
    in order to determine what modifiers the currently used windowing system
    backend uses for particular purposes.
  @end{short}
  For example, on X11/Windows, the Control key is used for invoking menu
  shortcuts (accelerators), whereas on Apple computers it is the Command key,
  which correspond to the @code{:control-mask} and @code{:mod2-mask} values of
  the @symbol{gdk:modifier-type} flags, respectively.
  @begin{pre}
(define-g-enum \"GdkModifierIntent\" modifier-intent
  (:export t
   :type-initializer \"gdk_modifier_intent_get_type\")
  (:primary-accelerator 0)
  (:context-menu 1)
  (:extend-selection 2)
  (:modify-selection 3)
  (:no-text-input 4)
  (:shift-group 5)
  (:default-mod-mask 6))
  @end{pre}
  @begin[code]{table}
    @entry[:primary-accelerator]{The primary modifier used to invoke menu
      accelerators.}
    @entry[:context-menu]{The modifier used to invoke context menus. Note that
      mouse button 3 always triggers context menus. When this modifier is not
      0, it additionally triggers context menus when used with mouse button 1.}
    @entry[:extend-selextion]{The modifier used to extend selections using
      <modifier>-click or <modifier>-cursor-key.}
    @entry[:modify-selection]{The modifier used to modify selections, which in
      most cases means toggling the clicked item into or out of the selection.}
    @entry[:no-text-input]{When any of these modifiers is pressed, the key
      event cannot produce a symbol directly. This is meant to be used for
      input methods, and for use cases like typeahead search.}
    @entry[:shift-group]{The modifier that switches between keyboard groups,
      the @kbd{AltGr} key on X11/Windows and the @kbd{Option/Alt} key on OS X.}
    @entry[:default-mod-mask]{The set of modifier masks accepted as modifiers
      in accelerators. Needed because the @kbd{Command} key is mapped to
      @code{MOD2} on OSX, which is widely used, but on X11 @code{MOD2} is the
      @kbd{NumLock} key and using that for a mod key is problematic at best.}
  @end{table}
  @see-symbol{gdk:modifier-type}
  @see-function{gdk:keymap-modifier-mask}")

;;; ----------------------------------------------------------------------------
;;; enum GdkWMDecoration
;;; ----------------------------------------------------------------------------

(define-g-flags "GdkWMDecoration" wm-decoration
  (:export t
   :type-initializer "gdk_wm_decoration_get_type")
  (:all 1)
  (:border 2)
  (:resizeh 4)
  (:title 8)
  (:menu 16)
  (:minimize 32)
  (:maximize 64))

#+liber-documentation
(setf (liber:alias-for-symbol 'wm-decoration)
      "GFlags"
      (liber:symbol-documentation 'wm-decoration)
 "@version{2023-2-26}
  @begin{short}
    These are hints originally defined by the Motif toolkit.
  @end{short}
  The window manager can use them when determining how to decorate the window.
  The hint must be set before mapping the window.
  @begin{pre}
(define-g-flags \"GdkWMDecoration\" wm-decoration
  (:export t
   :type-initializer \"gdk_wm_decoration_get_type\")
  (:all 1)
  (:border 2)
  (:resizeh 4)
  (:title 8)
  (:menu 16)
  (:minimize 32)
  (:maximize 64))
  @end{pre}
  @begin[code]{table}
    @entry[:all]{All decorations should be applied.}
    @entry[:border]{A frame should be drawn around the window.}
    @entry[:resizeh]{The frame should have resize handles.}
    @entry[:title]{A titlebar should be placed above the window.}
    @entry[:menu]{A button for opening a menu should be included.}
    @entry[:minimize]{A minimize button should be included.}
    @entry[:maximize]{A maximize button should be included.}
  @end{table}
  @see-class{gdk:window}")

;;; ----------------------------------------------------------------------------
;;; enum GdkWMFunction
;;; ----------------------------------------------------------------------------

(define-g-flags "GdkWMFunction" wm-function
  (:export t
   :type-initializer "gdk_wm_function_get_type")
  (:all 1)
  (:resize 2)
  (:move 4)
  (:minimize 8)
  (:maximize 16)
  (:close 32))

#+liber-documentation
(setf (liber:alias-for-symbol 'wm-function)
      "GFlags"
      (liber:symbol-documentation 'wm-function)
 "@version{2023-2-26}
  @begin{short}
    These are hints originally defined by the Motif toolkit.
  @end{short}
  The window manager can use them when determining the functions to offer for
  the window. The hint must be set before mapping the window.
  @begin{pre}
(define-g-flags \"GdkWMFunction\" wm-function
  (:export t
   :type-initializer \"gdk_wm_function_get_type\")
  (:all 1)
  (:resize 2)
  (:move 4)
  (:minimize 8)
  (:maximize 16)
  (:close 32))
  @end{pre}
  @begin[code]{table}
    @entry[:all]{All functions should be offered.}
    @entry[:resize]{The window should be resizable.}
    @entry[:move]{The window should be movable.}
    @entry[:minimize]{The window should be minimizable.}
    @entry[:maximize]{The window should be maximizable.}
    @entry[:close]{The window should be closable.}
  @end{table}
  @see-class{gdk:window}
  @see-function{gdk:window-set-functions}")

;;; ----------------------------------------------------------------------------
;;; GdkWindow
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkWindow" window
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_window_get_type")
  ((cursor
    window-cursor
    "cursor" "GdkCursor" t t)))

(setf (documentation 'window 'type)
 "@version{2023-2-26}
  @begin{short}
    Onscreen display areas in the target window system.
  @end{short}
  A @sym{gdk:window} object is a usually rectangular region on the screen.
  It is a low-level object, used to implement high-level objects such as
  @class{gtk:widget} and @class{gtk:window} widgets on the GTK level. A
  @class{gtk:window} widget is a toplevel window, the thing a user might think
  of as a \"window\" with a titlebar and so on. A @class{gtk:window} widget
  may contain many @sym{gdk:window} objects. For example, each
  @class{gtk:button} widget has a @sym{gdk:window} object associated with it.

  @subheading{Composited Windows}
    Normally, the windowing system takes care of rendering the contents of a
    child window onto its parent window. This mechanism can be intercepted by
    calling the @fun{gdk:window-composited} function on the child window.
    For a composited window it is the responsibility of the application to
    render the window contents at the right spot.

  @subheading{Offscreen Windows}
    Offscreen windows are more general than composited windows, since they
    allow not only to modify the rendering of the child window onto its parent,
    but also to apply coordinate transformations.

    To integrate an offscreen window into a window hierarchy, one has to call
    the @fun{gdk:offscreen-window-embedder} function and handle a number of
    signals. The \"pick-embedded-child\" signal on the embedder window is used
    to select an offscreen child at given coordinates, and the \"to-embedder\"
    and \"from-embedder\" signals on the offscreen window are used to translate
    coordinates between the embedder and the offscreen window.

    For rendering an offscreen window onto its embedder, the contents of the
    offscreen window are available as a surface, via the
    @fun{gdk:offscreen-window-surface} function.
  @begin[Signal Details]{dictionary}
    @subheading{The \"create-surface\" signal}
      @begin{pre}
lambda (window width height)    :run-last
      @end{pre}
      The signal is emitted when an offscreen window needs its surface
      (re)created, which happens either when the the window is first drawn to,
      or when the window is being resized. The first signal handler that returns
      a non-@code{nil} surface will stop any further signal emission, and its
      surface will be used. Note that it is not possible to access the window's
      previous surface from within any callback of this signal. Calling the
      @fun{gdk:offscreen-window-surface} function will lead to a crash.
      @begin[code]{table}
        @entry[window]{The @sym{gdk:window} offscreen window on which the
          signal is emitted.}
        @entry[width]{An integer with the width of the offscreen surface to
          create.}
        @entry[height]{An integer with the height of the offscreen surface
          to create.}
        @entry[Returns]{The newly created @symbol{cairo:surface-t} instance
          for the offscreen window.}
      @end{table}
    @subheading{The \"from-embedder\" signal}
      @begin{pre}
lambda (window xembedder yembedder xoffscreen yoffscreen)    :run-last
      @end{pre}
      The signal is emitted to translate coordinates in the embedder of an
      offscreen window to the offscreen window. See also the \"to-embedder\"
      signal.
      @begin[code]{table}
        @entry[window]{The @sym{gdk:window} offscreen window on which the
          signal is emitted.}
        @entry[xembedder]{An double float with the x coordinate in the
          embedder window.}
        @entry[yembedder]{An double float with the y coordinate in the
          embedder window.}
        @entry[xoffscreen]{A double float return location for the x coordinate
          in the offscreen window.}
        @entry[yoffscreen]{A double float return location for the y coordinate
          in the offscreen window.}
      @end{table}
    @subheading{The \"moved-to-rect\" signal}
      @begin{pre}
lambda (window flipped final xflipped yflipped)    :run-first
      @end{pre}
      Emitted when the position of window is finalized after being moved to a
      destination rectangle. @arg{window} might be flipped over the destination
      rectangle in order to keep it on-screen, in which case @arg{xflipped}
      and @arg{yflipped} will be set to @em{true} accordingly. @arg{flipped} is
      the ideal position of window after any possible flipping, but before any
      possible sliding. @arg{final} is @arg{flipped}, but possibly translated
      in the case that flipping is still ineffective in keeping window
      on-screen. Since 3.22
      @begin[code]{table}
        @entry[window]{The @sym{gdk:window} object that moved.}
        @entry[flipped]{The position of @arg{window} after any possible flipping
          or @code{nil} if the backend cannot obtain it.}
        @entry[final]{The final position of @code{window} or @code{nil} if the
          backend cannot obtain it.}
        @entry[xflipped]{@em{True} if the anchors were flipped horizontally.}
        @entry[yflipped]{@em{True} if the anchors were flipped vertically.}
      @end{table}
    @subheading{The \"pick-embedded-child\" signal}
      @begin{pre}
lambda (window x y)    :run-last
      @end{pre}
      The signal is emitted to find an embedded child at the given position.
      @begin[code]{table}
        @entry[window]{The @sym{gdk:window} object on which the signal is
          emitted.}
        @entry[x]{A double float with the x coordinate in the window.}
        @entry[y]{A double float with the y coordinate in the window.}
        @entry[Returns]{The @class{gdk:window} object of the embedded child at
          @arg{x}, @arg{y}, or @code{nil}.}
      @end{table}
    @subheading{The \"to-embedder\" signal}
      @begin{pre}
lambda (window xoffscreen yoffscreen xembedder yembedder)    :run-last
      @end{pre}
      The signal is emitted to translate coordinates in an offscreen window to
      its embedder. See also the \"from-embedder\" signal.
      @begin[code]{table}
        @entry[window]{The @sym{gdk:window} object with the offscreen window on
          which the signal is emitted.}
        @entry[xoffscreen]{A double float with the x coordinate in the
          offscreen window.}
        @entry[yoffscreen]{A double float with the y coordinate in the
          offscreen window.}
        @entry[xembedder]{A double float return location for the x coordinate
          in the embedder window.}
        @entry[yembedder]{A double float return location for the y coordinate
          in the embedder window.}
      @end{table}
  @end{dictionary}
  @see-constructor{gdk:window-new}
  @see-slot{gdk:window-cursor}
  @see-class{gtk:widget}
  @see-class{gtk:window}
  @see-function{gdk:window-composited}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "cursor" 'window) t)
 "The @code{cursor} property of type @class{gdk:cursor} (Read / Write) @br{}
  The mouse pointer for a GDK window.")

#+liber-documentation
(setf (liber:alias-for-function 'window-cursor)
      "Accessor"
      (documentation 'window-cursor 'function)
 "@version{2023-2-26}
  @syntax[]{(gdk:window-cursor object) => cursor}
  @syntax[]{(setf (gdk:window-cursor object) cursor)}
  @argument[object]{a @class{gdk:window} object}
  @argument[cursor]{a @class{gdk:cursor} object}
  @begin{short}
    Accessor of the @slot[gdk:window]{cursor} slot of the @class{gdk:window}
    class.
  @end{short}
  The @sym{gdk:window-cursor} function retrieves a @class{gdk:cursor} pointer
  for the cursor currently set on the specified window, or @code{nil}. If the
  return value is @code{nil} then there is no custom cursor set on the specified
  window, and it is using the cursor for its parent window. The
  @sym{(setf gdk:window-cursor)} function sets the default mouse pointer for a
  window.

  Use the @fun{gdk:cursor-new-for-display}, @fun{gdk:cursor-new-from-name}, or
  @fun{gdk:cursor-new-from-pixbuf} functions to create the cursor. To make the
  cursor invisible, use the @code{:blank-cursor} value of the
  @symbol{gdk:cursor-type} enumeration. Passing @code{nil} for the @arg{cursor}
  argument means that the GDK window will use the cursor of its parent window.
  Most windows should use this default.
  @see-class{gdk:window}
  @see-class{gdk:cursor}
  @see-symbol{gdk:cursor-type}
  @see-function{gdk:window-cursor}
  @see-function{gdk:cursor-new-for-display}
  @see-function{gdk:cursor-new-from-name}
  @see-function{gdk:cursor-new-from-pixbuf}")

;;; ----------------------------------------------------------------------------
;;; gdk_window_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_new" window-new) (g:object window :already-referenced)
 #+liber-documentation
 "@version{2023-2-26}
  @argument[parent]{a @class{gdk:window} object, or @code{nil} to create the
    window as a child of the default root window for the default display}
  @argument[attributes]{a @symbol{gdk:window-attr} instance with the attributes
    of type  of the new window}
  @argument[mask]{a @symbol{gdk:window-attributes-type} mask indicating which
    fields in @arg{attributes} are valid}
  @return{The new @class{gdk:window} object.}
  @begin{short}
    Creates a new window using the attributes from @arg{attributes}.
  @end{short}
  See the @symbol{gdk:window-attr} and @symbol{gdk:window-attributes-type}
  documentation for more details.

  Note: To use this on displays other than the default display, @arg{parent}
  must be specified.
  @see-class{gdk:window}
  @see-symbol{gdk:window-attr}
  @see-symbol{gdk:window-attributes-type}"
  (parent (g:object window))
  (attributes (:pointer (:struct window-attr)))
  (mask window-attributes-type))

(export 'window-new)

;;; ----------------------------------------------------------------------------
;;; gdk_window_destroy ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_destroy" window-destroy) :void
 #+liber-documentation
 "@version{2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @begin{short}
    Destroys the window system resources associated with @arg{window} and
    decrements @arg{window}'s reference count.
  @end{short}
  The window system resources for all children of @arg{window} are also
  destroyed, but the children's reference counts are not decremented.

  Note that a window will not be destroyed automatically when its reference
  count reaches zero. You must call this function yourself before that
  happens.
  @see-class{gdk:window}"
  (window (g:object window)))

(export 'window-destroy)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_window_type () -> window-window-type
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_window_type" window-window-type) window-type
 #+liber-documentation
 "@version{2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @return{The @symbol{gdk:window-type} value of @arg{window}.}
  @begin{short}
    Gets the type of the window.
  @end{short}
  See the @symbol{gdk:window-type} enumeration.
  @see-class{gdk:window}
  @see-symbol{gdk:window-type}"
  (window (g:object window)))

(export 'window-window-type)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_display () -> window-display
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_display" window-display) (g:object display)
 #+liber-documentation
 "@version{2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @return{The @class{gdk:display} object associated with @arg{window}.}
  @begin{short}
    Gets the display associated with a window.
  @end{short}
  @see-class{gdk:window}
  @see-class{gdk:display}"
  (window (g:object window)))

(export 'window-display)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_screen () -> window-screen
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_screen" window-screen) (g:object screen)
 #+liber-documentation
 "@version{2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @return{The @class{gdk:screen} object associated with @arg{window}.}
  @begin{short}
    Gets the screen associated with a window.
  @end{short}
  @see-class{gdk:window}
  @see-class{gdk:screen}"
  (window (g:object window)))

(export 'window-screen)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_visual () -> window-visual
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_visual" window-visual) (g:object visual)
 #+liber-documentation
 "@version{2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @return{A @class{gdk:visual} object.}
  @begin{short}
    Gets the visual describing the pixel format of a window.
  @end{short}
  @see-class{gdk:window}
  @see-class{gdk:visual}"
  (window (g:object window)))

(export 'window-visual)

;;; ----------------------------------------------------------------------------
;;; gdk_window_at_pointer ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_at_pointer" %window-at-pointer) (g:object window)
  (xwin (:pointer :int))
  (ywin (:pointer :int)))

(defun window-at-pointer ()
 #+liber-documentation
 "@version{#2023-2-26}
  @begin{return}
    @code{window} -- a @class{gdk:window} object under the mouse pointer @br{}
    @code{xwin} -- an integer with the origin of the window under the pointer
    @br{}
    @code{ywin} -- an integer with the origin of the window under the pointer
  @end{return}
  @begin{short}
    Obtains the window underneath the mouse pointer, returning the location of
    that window in @arg{xwin}, @arg{ywin}.
  @end{short}
  Returns @code{nil} if the window under the mouse pointer is not known to GDK.
  @begin[Warning]{dictionary}
    The @sym{gdk:window-at-pointer} function has been deprecated since version
    3.0 and should not be used in newly written code. Use the
    @fun{gdk:device-window-at-position} function instead.
  @end{dictionary}
  @see-class{gdk:window}
  @see-function{gdk:device-window-at-position}"
  (with-foreign-objects ((x :int) (y :int))
    (let ((window (%window-at-pointer x y)))
      (when window
        (values window
                (cffi:mem-ref x :int)
                (cffi:mem-ref y :int))))))

(export 'window-at-pointer)

;;; ----------------------------------------------------------------------------
;;; gdk_window_show ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_show" window-show) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @begin{short}
    Like the @fun{gdk:window-show-unraised} function, but also raises the window
    to the top of the window stack, moves the window to the front of the
    z-order.
  @end{short}
  This function maps a window so it is visible onscreen. Its opposite is the
  @fun{gdk:window-hide} function.

  When implementing a @class{gtk:widget} widget, you should call this function
  on the widget's @class{gdk:window} object as part of the \"map\" method.
  @see-class{gdk:window}
  @see-class{gtk:widget}
  @see-function{gdk:window-hide}
  @see-function{gdk:window-show-unraised}"
  (window (g:object window)))

(export 'window-show)

;;; ----------------------------------------------------------------------------
;;; gdk_window_show_unraised ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_show_unraised" window-show-unraised) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @begin{short}
    Shows a window onscreen, but does not modify its stacking order.
  @end{short}
  In contrast, the @fun{gdk:window-show} function will raise the window to the
  top of the window stack.

  On the X11 platform, in Xlib terms, this function calls the
  @code{XMapWindow()} function, it also updates some internal GDK state, which
  means that you cannot really use the @code{XMapWindow()} function directly on
  a GDK window.
  @see-class{gdk:window}
  @see-function{gdk:window-show}"
  (window (g:object window)))

(export 'window-show-unraised)

;;; ----------------------------------------------------------------------------
;;; gdk_window_hide ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_hide" window-hide) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @begin{short}
    For toplevel windows, withdraws them, so they will no longer be known to
    the window manager.
  @end{short}
  For all windows, unmaps them, so they will not be displayed. Normally done
  automatically as part of the @fun{gtk:widget-hide} function.
  @see-class{gdk:window}
  @see-function{gtk:widget-hide}"
  (window (g:object window)))

(export 'window-hide)

;;; ----------------------------------------------------------------------------
;;; gdk_window_is_destroyed ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_is_destroyed" window-is-destroyed) :boolean
 #+liber-documentation
 "@version{2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @return{@em{True} if @arg{window} is destroyed.}
  @begin{short}
    Check to see if a window is destroyed.
  @end{short}
  @see-class{gdk:window}
  @see-function{gdk:window-destroy}"
  (window (g:object window)))

(export 'window-is-destroyed)

;;; ----------------------------------------------------------------------------
;;; gdk_window_is_visible ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_is_visible" window-is-visible) :boolean
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @return{@em{True} if @arg{window} is mapped.}
  @begin{short}
    Checks whether the window has been mapped with the @fun{gdk:window-show} or
    @fun{gdk:window-show-unraised} functions.
  @end{short}
  @see-class{gdk:window}
  @see-function{gdk:window-show}
  @see-function{gdk:window-show-unraised}"
  (window (g:object window)))

(export 'window-is-visible)

;;; ----------------------------------------------------------------------------
;;; gdk_window_is_viewable ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_is_viewable" window-is-viewable) :boolean
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @return{@em{True} if @arg{window} is viewable.}
  @begin{short}
    Check if the window and all ancestors of the window are mapped.
  @end{short}
  This is not necessarily \"viewable\" in the X sense, since we only check as
  far as we have GDK window parents, not to the root window.
  @see-class{gdk:window}"
  (window (g:object window)))

(export 'window-is-viewable)

;;; ----------------------------------------------------------------------------
;;; gdk_window_is_input_only ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_is_input_only" window-is-input-only) :boolean
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @return{@em{True} if @arg{window} is input only.}
  @begin{short}
    Determines whether or not the window is an input only window.
  @end{short}
  @see-class{gdk:window}"
  (window (g:object window)))

(export 'window-is-input-only)

;;; ----------------------------------------------------------------------------
;;; gdk_window_is_shaped ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_is_shaped" window-is-shaped) :boolean
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @return{@em{True} if @arg{window} is shaped.}
  @begin{short}
    Determines whether or not the window is shaped.
  @end{short}
  @see-class{gdk:window}"
  (window (g:object window)))

(export 'window-is-shaped)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_state () -> window-state
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_state" window-state) window-state
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @return{The @symbol{gdk:window-state} value of the window state.}
  @begin{short}
    Gets the bitwise OR of the currently active window state flags, from the
    @symbol{gdk:window-state} enumeration.
  @end{short}
  @see-class{gdk:window}
  @see-symbol{gdk:window-state}"
  (window (g:object window)))

(export 'window-state)

;;; ----------------------------------------------------------------------------
;;; gdk_window_withdraw ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_withdraw" window-withdraw) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @begin{short}
    Withdraws a window, that is, unmaps it and asks the window manager to
    forget about it.
  @end{short}
  This function is not really useful as the @fun{gdk:window-hide} function
  automatically withdraws toplevel windows before hiding them.
  @see-class{gdk:window}
  @see-function{gdk:window-hide}"
  (window (g:object window)))

(export 'window-withdraw)

;;; ----------------------------------------------------------------------------
;;; gdk_window_iconify ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_iconify" window-iconify) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @begin{short}
    Asks to iconify (minimize) window.
  @end{short}
  The window manager may choose to ignore the request, but normally will honor
  it. Using the @fun{gtk:window-iconify} function is preferred, if you have a
  @class{gtk:window} widget.

  This function only makes sense when @arg{window} is a toplevel window.
  @see-class{gdk:window}
  @see-class{gtk:window}
  @see-function{gdk:window-deiconify}
  @see-function{gtk:window-iconify}"
  (window (g:object window)))

(export 'window-iconify)

;;; ----------------------------------------------------------------------------
;;; gdk_window_deiconify ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_deiconify" window-deiconify) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @begin{short}
    Attempt to deiconify (unminimize) window.
  @end{short}
  On X11 the window manager may choose to ignore the request to deiconify. When
  using GTK, use the @fun{gtk:window-deiconify} function instead of the
  @class{gdk:window} variant. Or better yet, you probably want to use
  the @fun{gtk:window-present} function, which raises the window, focuses
  it, unminimizes it, and puts it on the current desktop.
  @see-class{gdk:window}
  @see-class{gtk:window}
  @see-function{gdk:window-iconify}
  @see-function{gtk:window-deiconify}
  @see-function{gtk:window-present}"
  (window (g:object window)))

(export 'window-deiconify)

;;; ----------------------------------------------------------------------------
;;; gdk_window_stick ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_stick" window-stick) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @begin{short}
    \"Pins\" a window such that it is on all workspaces and does not scroll with
    viewports, for window managers that have scrollable viewports.
  @end{short}
  When using the @class{gtk:window} widget, the @fun{gtk:window-stick} function
  may be more useful.

  On the X11 platform, this function depends on window manager support, so may
  have no effect with many window managers. However, GDK will do the best it
  can to convince the window manager to stick the window. For window managers
  that do not support this operation, there is nothing you can do to force it
  to happen.
  @see-class{gdk:window}
  @see-class{gtk:window}
  @see-function{gtk:window-stick}
  @see-function{gdk:window-unstick}"
  (window (g:object window)))

(export 'window-stick)

;;; ----------------------------------------------------------------------------
;;; gdk_window_unstick ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_unstick" window-unstick) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @begin{short}
    Reverse operation for the @fun{gdk:window-stick} function.
  @end{short}
  See the @fun{gdk:window-stick} and @fun{gtk:window-unstick} functions.
  @see-class{gdk:window}
  @see-function{gdk:window-stick}
  @see-function{gtk:window-unstick}"
  (window (g:object window)))

(export 'window-unstick)

;;; ----------------------------------------------------------------------------
;;; gdk_window_maximize ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_maximize" window-maximize) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @begin{short}
    Maximizes the window.
  @end{short}
  If the window was already maximized, then this function does nothing.

  On X11, asks the window manager to maximize window, if the window manager
  supports this operation. Not all window managers support this, and some
  deliberately ignore it or do not have a concept of \"maximized\"; so you
  cannot rely on the maximization actually happening. But it will happen with
  most standard window managers, and GDK makes a best effort to get it to
  happen.

  On Windows, reliably maximizes the window.
  @see-class{gdk:window}
  @see-function{gdk:window-unmaximize}"
  (window (g:object window)))

(export 'window-maximize)

;;; ----------------------------------------------------------------------------
;;; gdk_window_unmaximize ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_unmaximize" window-unmaximize) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @begin{short}
    Unmaximizes the window.
  @end{short}
  If the window was not maximized, then this function does nothing.

  On X11, asks the window manager to unmaximize window, if the window manager
  supports this operation. Not all window managers support this, and some
  deliberately ignore it or do not have a concept of \"maximized\"; so you
  cannot rely on the unmaximization actually happening. But it will happen with
  most standard window managers, and GDK makes a best effort to get it to
  happen.

  On Windows, reliably unmaximizes the window.
  @see-class{gdk:window}
  @see-function{gdk:window-maximize}"
  (window (g:object window)))

(export 'window-unmaximize)

;;; ----------------------------------------------------------------------------
;;; gdk_window_fullscreen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_fullscreen" window-fullscreen) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @begin{short}
    Moves the window into fullscreen mode.
  @end{short}
  This means the window covers the entire screen and is above any panels or
  task bars.

  If the window was already fullscreen, then this function does nothing.

  On X11, asks the window manager to put window in a fullscreen state, if the
  window manager supports this operation. Not all window managers support
  this, and some deliberately ignore it or do not have a concept of
  \"fullscreen\"; so you cannot rely on the fullscreenification actually
  happening. But it will happen with most standard window managers, and GDK
  makes a best effort to get it to happen.
  @see-class{gdk:window}
  @see-function{gdk:window-unfullscreen}"
  (window (g:object window)))

(export 'window-fullscreen)

;;; ----------------------------------------------------------------------------
;;; gdk_window_fullscreen_on_monitor ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_fullscreen_on_monitor" window-fullscreen-on-monitor) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @argument[monitor]{an integer with the monitor to display fullscreen on}
  @begin{short}
    Moves the window into fullscreen mode on the given monitor.
  @end{short}
  This means the window covers the entire screen and is above any panels or
  task bars.

  If the window was already fullscreen, then this function does nothing.
  @see-class{gdk:window}
  @see-function{gdk:window-fullscreen}
  @see-function{gdk:window-unfullscreen}"
  (window (g:object window))
  (monitor :int))

(export 'window-fullscreen-on-monitor)

;;; ----------------------------------------------------------------------------
;;; gdk_window_unfullscreen ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_unfullscreen" window-unfullscreen) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @begin{short}
    Moves the window out of fullscreen mode.
  @end{short}
  If the window was not fullscreen, does nothing.

  On X11, asks the window manager to move window out of the fullscreen state,
  if the window manager supports this operation. Not all window managers
  support this, and some deliberately ignore it or do not have a concept of
  \"fullscreen\"; so you cannot rely on the unfullscreenification actually
  happening. But it will happen with most standard window managers, and GDK
  makes a best effort to get it to happen.
  @see-class{gdk:window}
  @see-function{gdk:window-fullscreen}"
  (window (g:object window)))

(export 'window-unfullscreen)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_fullscreen_mode ()
;;; gdk_window_set_fullscreen_mode () -> window-fullscreen-mode
;;; ----------------------------------------------------------------------------

(defun (setf window-fullscreen-mode) (mode window)
  (cffi:foreign-funcall "gdk_window_set_fullscreen_mode"
                        (g:object window) window
                        fullscreen-mode mode
                        :void)
  mode)

(defcfun ("gdk_window_get_fullscreen_mode" window-fullscreen-mode)
    fullscreen-mode
 #+liber-documentation
 "@version{#2023-2-26}
  @syntax[]{(gdk:window-fullscreen-mode window) => mode}
  @syntax[]{(setf (gdk:window-fullscreen-mode window) mode)}
  @argument[window]{a toplevel @class{gdk:window} object}
  @argument[mode]{a value of the @symbol{gdk:fullscreen-mode} enumeration}
  @begin{short}
    Accessor of the fullscreen mode applied to the window when fullscreen.
  @end{short}
  The @sym{gdk:window-fullscreen-mode} function obtains the fullscreen mode of
  the window. The @sym{(setf gdk:window-fullscreen-mode)} function specifies
  whether the window should span over all monitors (in a multi-head setup) or
  only the current monitor when in fullscreen mode.

  The mode argument is from the @symbol{gdk:fullscreen-mode} enumeration. If
  @code{:on-all-monitors} is specified, the fullscreen window will span over
  all monitors from the @class{gdk:screen} object.

  On X11, searches through the list of monitors from the @class{gdk:screen}
  object the ones which delimit the 4 edges of the entire @class{gdk:screen}
  object and will ask the window manager to span the window over these monitors.

  If the XINERAMA extension is not available or not usable, this function has
  no effect.

  Not all window managers support this, so you can not rely on the fullscreen
  window to span over the multiple monitors when @code{:on-all-monitors} is
  specified.
  @see-class{gdk:window}
  @see-class{gdk:screen}
  @see-symbol{gdk:fullscreen-mode}"
  (window (g:object window)))

(export 'window-fullscreen-mode)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_keep_above ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_keep_above" window-set-keep-above) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @argument[setting]{a boolean whether to keep @arg{window} above other windows}
  @begin{short}
    Set if the window must be kept above other windows.
  @end{short}
  If the window was already above, then this function does nothing.

  On X11, asks the window manager to keep the window above, if the window
  manager supports this operation. Not all window managers support this, and
  some deliberately ignore it or do not have a concept of \"keep above\"; so
  you cannot rely on the window being kept above. But it will happen with most
  standard window managers, and GDK makes a best effort to get it to happen.
  @see-class{gdk:window}
  @see-function{gdk:window-set-keep-below}"
  (window (g:object window))
  (setting :boolean))

(export 'window-set-keep-above)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_keep_below ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_keep_below" window-set-keep-below) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @argument[setting]{a boolean whether to keep @arg{window} below other windows}
  @begin{short}
    Set if the window must be kept below other windows.
  @end{short}
  If the window was already below, then this function does nothing.

  On X11, asks the window manager to keep the window below, if the window
  manager supports this operation. Not all window managers support this, and
  some deliberately ignore it or do not have a concept of \"keep below\"; so you
  cannot rely on the window being kept below. But it will happen with most
  standard window managers, and GDK makes a best effort to get it to happen.
  @see-class{gdk:window}
  @see-function{gdk:window-set-keep-above}"
  (window (g:object window))
  (setting :boolean))

(export 'window-set-keep-below)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_opacity ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_opacity" window-set-opacity) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @argument[opacity]{a double float with the opacity}
  @begin{short}
    Request the windowing system to make window partially transparent, with
    opacity 0 being fully transparent and 1 fully opaque.
  @end{short}
  Values of the opacity parameter are clamped to the [0,1] range.

  On X11, this works only on X screens with a compositing manager running.

  For setting up per-pixel alpha, see the @fun{gdk:screen-rgba-visual}
  function. For making non-toplevel windows translucent, see the
  @fun{gdk:window-composited} function.
  @see-class{gdk:window}
  @see-function{gdk:screen-rgba-visual}
  @see-function{gdk:window-composited}"
  (window (g:object window))
  (opacity :double))

(export 'window-set-opacity)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_composited ()
;;; gdk_window_set_composited () -> window-composited
;;; ----------------------------------------------------------------------------

(defun (setf window-composited) (composited window)
  (cffi:foreign-funcall "gdk_window_set_composited"
                        (g:object window) window
                        :boolean composited
                        :void)
  composited)

(defcfun ("gdk_window_get_composited" window-composited) :boolean
 #+liber-documentation
 "@version{#2023-2-26}
  @syntax[]{(gdk:window-composited window) => composited}
  @syntax[]{(setf (gdk:window-composited window) composited)}
  @argument[window]{a @class{gdk:window} object}
  @argument[composited]{@em{true} to set @arg{window} as composited}
  @begin{short}
    The @sym{gdk:window-composited} function determines whether @arg{window} is
    composited.
  @end{short}
  The @sym{(setf gdk:window-composited)} function sets a window as composited,
  or unsets it.

  Composited windows do not automatically have their contents drawn to the
  screen. Drawing is redirected to an offscreen buffer and an expose event is
  emitted on the parent of the composited window. It is the responsibility of
  the parent's expose handler to manually merge the off-screen content onto the
  screen in whatever way it sees fit.

  It only makes sense for child windows to be composited. See the
  @fun{gdk:window-set-opacity} function if you need translucent toplevel
  windows.

  An additional effect of this call is that the area of this window is no
  longer clipped from regions marked for invalidation on its parent. Draws
  done on the parent window are also no longer clipped by the child.

  This call is only supported on some systems, currently, only X11 with new
  enough Xcomposite and Xdamage extensions. You must call the
  @fun{gdk:display-supports-composite} function to check if setting a window as
  composited is supported before attempting to do so.
  @begin[Warning]{dictionary}
    The @sym{gdk:window-composited} function has been deprecated since version
    3.16 and should not be used in newly written code. Compositing is an
    outdated technology that only ever worked on X11.
  @end{dictionary}
  @see-class{gdk:window}
  @see-function{gdk:window-set-opacity}
  @see-function{gdk:display-supports-composite}"
  (window (g:object window)))

(export 'window-composited)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_pass_through ()
;;; gdk_window_set_pass_through () -> window-pass-through
;;; ----------------------------------------------------------------------------

(defun (setf window-pass-through) (pass-through window)
  (cffi:foreign-funcall "gdk_window_set_pass_through"
                        (g:object window) window
                        :boolean
                        :void)
  pass-through)

(defcfun ("gdk_window_get_pass_through" window-pass-through) :boolean
 #+liber-documentation
 "@version{#2023-3-13}
  @syntax[]{(gdk:window-pass-through window) => pass-through}
  @syntax[]{(setf (gdk:window-pass-through window) pass-through)}
  @argument[window]{a @class{gdk:window} object}
  @argument[pass-through]{a boolean}
  @begin{short}
    The @sym{gdk:window-pass-through} function returns whether input to the
    window is passed through to the window below.
  @end{short}
  The @sym{(setf gdk:window-pass-through)} function sets whether input to the
  window is passed through to the window below.

  The default value of this is @em{false}, which means that pointer events that
  happen inside the window are send first to the window, but if the event is
  not selected by the event mask then the event is sent to the parent window,
  and so on up the hierarchy.

  If @arg{pass-through} is @em{true} then such pointer events happen as if the
  window was not there at all, and thus will be sent first to any windows below
  @arg{window}. This is useful if the window is used in a transparent fashion.
  In the terminology of the web this would be called \"pointer-events: none\".

  Note that a window with @arg{pass-through} @em{true} can still have a
  subwindow without pass through, so you can get events on a subset of a window.
  And in that cases you would get the in-between related events such as the
  pointer enter/leave events on its way to the destination window.
  @see-class{gdk:window}"
  (window (g:object window)))

(export 'window-pass-through)

;;; ----------------------------------------------------------------------------
;;; gdk_window_move ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_move" window-move) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[x]{an integer with the x coordinate relative to window's parent}
  @argument[y]{an integer with the y coordinate relative to window's parent}
  @begin{short}
    Repositions a window relative to its parent window.
  @end{short}
  For toplevel windows, window managers may ignore or modify the move. You
  should probably use the @fun{gtk:window-move} function on a @class{gtk:window}
  widget anyway, instead of using GDK functions. For child windows, the move
  will reliably succeed.

  If you are also planning to resize the window, use the
  @fun{gdk:window-move-resize} function to both move and resize simultaneously,
  for a nicer visual effect.
  @see-class{gdk:window}
  @see-class{gtk:window}
  @see-function{gtk:window-move}
  @see-function{gdk:window-move-resize}"
  (window (g:object window))
  (x :int)
  (y :int))

(export 'window-move)

;;; ----------------------------------------------------------------------------
;;; gdk_window_resize ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_resize" window-resize) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[width]{an integer with the new width of the window}
  @argument[height]{an integer with the new height of the window}
  @begin{short}
    Resizes the window, for toplevel windows, asks the window manager to
    resize the window.
  @end{short}
  The window manager may not allow the resize. When using GTK, use the
  @fun{gtk:window-resize} function instead of this low-level GDK function.

  Windows may not be resized below 1x1.

  If you are also planning to move the window, use the
  @fun{gdk:window-move-resize} function to both move and resize simultaneously,
  for a nicer visual effect.
  @see-class{gdk:window}
  @see-class{gtk:window}
  @see-function{gtk:window-resize}
  @see-function{gdk:window-move-resize}"
  (window (g:object window))
  (width :int)
  (height :int))

(export 'window-resize)

;;; ----------------------------------------------------------------------------
;;; gdk_window_move_resize ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_move_resize" window-move-resize) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[x]{an integer with the new x position relative to window's parent}
  @argument[y]{an integer with the new y position relative to window's parent}
  @argument[width]{an integer with the new width}
  @argument[height]{an integer with the new height}
  @begin{short}
    Equivalent to calling the @fun{gdk:window-move} and @fun{gdk:window-resize}
    functions, except that both operations are performed at once, avoiding
    strange visual effects.
  @end{short}
  I.e. the user may be able to see the window first move, then resize, if you
  do not use the @sym{gdk:window-move-resize} function.
  @see-class{gdk:window}
  @see-function{gdk:window-move}
  @see-function{gdk:window-resize}"
  (window (g:object window))
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(export 'window-move-resize)

;;; ----------------------------------------------------------------------------
;;; gdk_window_scroll ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_scroll" window-scroll) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[dx]{an integer with the amount to scroll in the x direction}
  @argument[dy]{an integer with the amount to scroll in the y direction}
  @begin{short}
    Scroll the contents of window, both pixels and children, by the given
    amount.
  @end{short}
  @arg{window} itself does not move. Portions of the window that the scroll
  operation brings in from offscreen areas are invalidated. The invalidated
  region may be bigger than what would strictly be necessary.

  For X11, a minimum area will be invalidated if the window has no subwindows,
  or if the edges of the window's parent do not extend beyond the edges of the
  window. In other cases, a multi-step process is used to scroll the window
  which may produce temporary visual artifacts and unnecessary invalidations.
  @see-class{gdk:window}"
  (window (g:object window))
  (dx :int)
  (dy :int))

(export 'window-scroll)

;;; ----------------------------------------------------------------------------
;;; gdk_window_move_to_rect ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_move_to_rect" window-move-to-rect) :void
 #+liber-documentation
 "@version{#2023-3-13}
  @argument[window]{a @class{gdk:window} object to move}
  @argument[rect]{a @class{gdk:rectangle} instance with the destination to
    align @arg{window} with}
  @argument[ranchor]{a @symbol{gdk:gravity} value with the point on @arg{rect}
    to align with @arg{window}'s anchor point}
  @argument[wanchor]{a @symbol{gdk:gravity} value with the point on @arg{window}
    to align with @arg{rect}'s anchor point}
  @argument[hints]{a @symbol{gdk:anchor-hints} value with the positioning hints
    to use when limited on space}
  @argument[dx]{an integer with the horizontal offset to shift @arg{window},
    i.e. @arg{rect}'s anchor point}
  @argument[dy]{an integer with the vertical offset to shift @arg{window},
    i.e. @arg{rect}'s anchor point}
  @begin{short}
    Moves the window to @arg{rect}, aligning their anchor points.
  @end{short}
  @arg{rect} is relative to the top-left corner of the window that window is
  transient for. @arg{ranchor} and @arg{wanchor} determine anchor points on
  @arg{rect} and @arg{window} to pin together. @arg{rect}'s anchor point can
  optionally be offset by @arg{dx} and @arg{dy}, which is equivalent to
  offsetting the position of @arg{window}.

  @arg{hints} determines how window will be moved if the anchor points cause it
  to move off-screen. For example, @code{:flip-x} will replace
  @code{:north-west} with @code{:north-east} and vice versa if @arg{window}
  extends beyond the left or right edges of the monitor.

  Connect to the \"moved-to-rect\" signal to find out how it was actually
  positioned.
  @see-class{gdk:window}
  @see-class{gdk:rectangle}
  @see-symbol{gdk:gravity}
  @see-symbol{gdk:anchor-hints}"
  (window (g:object window))
  (rect (g:boxed rectangle))
  (ranchor gravity)
  (wanchor gravity)
  (hints anchor-hints)
  (dx :int)
  (dy :int))

(export 'window-move-to-rect)

;;; ----------------------------------------------------------------------------
;;; gdk_window_move_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_move_region" window-move-region) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[region]{a @symbol{cairo:region-t} to move}
  @argument[dx]{an integer with the amount to move in the x direction}
  @argument[dy]{an integer with the amount to move in the y direction}
  @begin{short}
    Move the part of window indicated by region by dy pixels in the y direction
    and dx pixels in the x direction.
  @end{short}
  The portions of region that not covered by the new position of region are
  invalidated.

  Child windows are not moved.
  @see-class{gdk:window}
  @see-symbol{cairo:region-t}"
  (window (g:object window))
  (region (:pointer (:struct cairo:region-t)))
  (dx :int)
  (dy :int))

(export 'window-move-region)

;;; ----------------------------------------------------------------------------
;;; gdk_window_flush ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_flush" window-flush) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @begin{short}
    This function does nothing.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gdk:window-flush} function has been deprecated since version 3.14
    and should not be used in newly written code.
  @end{dictionary}
  @see-class{gdk:window}"
  (window (g:object window)))

(export 'window-flush)

;;; ----------------------------------------------------------------------------
;;; gdk_window_has_native ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_has_native" window-has-native) :boolean
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @return{@em{True} if the window has a native window, @em{false} otherwise.}
  @begin{short}
    Checks whether the window has a native window or not.
  @end{short}
  Note that you can use the @fun{gdk:window-ensure-native} function if a native
  window is needed.
  @see-class{gdk:window}
  @see-function{gdk:window-ensure-native}"
  (window (g:object window)))

(export 'window-has-native)

;;; ----------------------------------------------------------------------------
;;; gdk_window_ensure_native ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_ensure_native" window-ensure-native) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @return{@em{True} if the window has a native window, @em{false} otherwise.}
  @begin{short}
    Tries to ensure that there is a window-system native window for this
    window.
  @end{short}
  This may fail in some situations, returning @em{false}.

  Offscreen window and children of them can never have native windows.

  Some backends may not support native child windows.
  @see-class{gdk:window}
  @see-function{gdk:window-has-native}"
  (window (g:object window)))

(export 'window-ensure-native)

;;; ----------------------------------------------------------------------------
;;; gdk_window_reparent ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_reparent" window-reparent) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[parent]{a new @class{gdk:window} parent to move @arg{window} into}
  @argument[x]{an integer with the x location inside the new parent}
  @argument[y]{an integer with the y location inside the new parent}
  @begin{short}
    Reparents @arg{window} into the given @arg{parent}.
  @end{short}
  The window being reparented will be unmapped as a side effect.
  @see-class{gdk:window}"
  (window (g:object window))
  (parent (g:object window))
  (x :int)
  (y :int))

(export 'window-reparent)

;;; ----------------------------------------------------------------------------
;;; gdk_window_raise ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_raise" window-raise) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @begin{short}
    Raises the window to the top of the z-order (stacking order), so that other
    windows with the same parent window appear below the window.
  @end{short}
  This is true whether or not the windows are visible.

  If @arg{window} is a toplevel, the window manager may choose to deny the
  request to move the window in the z-order, the @sym{gdk:window-raise} function
  only requests the restack, does not guarantee it.
  @see-class{gdk:window}
  @see-function{gdk:window-lower}"
  (window (g:object window)))

(export 'window-raise)

;;; ----------------------------------------------------------------------------
;;; gdk_window_lower ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_lower" window-lower) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @begin{short}
    Lowers the window to the bottom of the z-order (stacking order), so that
    other windows with the same parent window appear above the window.
  @end{short}
  This is true whether or not the other windows are visible.

  If @arg{window} is a toplevel, the window manager may choose to deny the
  request to move the window in the z-order, the @sym{gdk:window-lower} function
  only requests the restack, does not guarantee it.

  Note that the @fun{gdk:window-show} function raises the window again, so do
  not call this function before the @fun{gdk:window-show} function.
  Try the @fun{gdk:window-show-unraised} function.
  @see-class{gdk:window}
  @see-function{gdk:window-raise}
  @see-function{gdk:window-show}
  @see-function{gdk:window-show-unraised}"
  (window (g:object window)))

(export 'window-lower)

;;; ----------------------------------------------------------------------------
;;; gdk_window_restack ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_restack" window-restack) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[sibling]{a @class{gdk:window} object that is a sibling of
    @arg{window}, or @code{nil}}
  @argument[above]{a boolean}
  @begin{short}
    Changes the position of the window in the z-order (stacking order), so
    that it is above @arg{sibling}, if @arg{above} is @em{true}, or below
    sibling, if @arg{above} is @em{false}.
  @end{short}

  If @arg{sibling} is @code{nil}, then this either raises, if @arg{above} is
  @em{true}, or lowers the window.

  If @arg{window} is a toplevel, the window manager may choose to deny the
  request to move the window in the z-order, the @sym{gdk:window-restack}
  function only requests the restack, does not guarantee it.
  @see-class{gdk:window}
  @see-function{gdk:window-raise}
  @see-function{gdk:window-lower}"
  (window (g:object window))
  (sibling (g:object window))
  (above :boolean))

(export 'window-restack)

;;; ----------------------------------------------------------------------------
;;; gdk_window_focus ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_focus" window-focus) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[timestamp]{an unsigned integer with the timestamp of the event
    triggering the window focus}
  @begin{short}
    Sets keyboard focus to @arg{window}.
  @end{short}
  In most cases, the @fun{gtk:window-present} function should be used on a
  @class{gtk:window} widget, rather than calling this function.
  @see-class{gdk:window}
  @see-class{gtk:window}
  @see-function{gtk:window-present}"
  (window (g:object window))
  (timestamp :uint32))

(export 'window-focus)

;;; ----------------------------------------------------------------------------
;;; gdk_window_register_dnd ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_register_dnd" window-register-dnd) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @shot{Registers a window as a potential drop destination.}
  @see-class{gdk:window}"
  (window (g:object window)))

(export 'window-register-dnd)

;;; ----------------------------------------------------------------------------
;;; gdk_window_begin_resize_drag ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_begin_resize_drag" window-begin-resize-drag) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @argument[edge]{a @symbol{gdk:window-edge} value with the edge or corner from
    which the drag is started}
  @argument[button]{an integer with the button being used to drag}
  @argument[xroot]{an integer with the root window x coordinate of mouse click
    that began the drag}
  @argument[yroot]{an integer with the root window y coordinate of mouse click
    that began the drag}
  @argument[timestamp]{an unsigned integer with the timestamp of mouse click
    that began the drag, use the @fun{gdk:event-time} function}
  @begin{short}
    Begins a window resize operation for a toplevel window.
  @end{short}
  This function assumes that the drag is controlled by the client pointer
  device, use the @fun{gdk:window-begin-resize-drag-for-device} function to
  begin a drag with a different device.
  @see-class{gdk:window}
  @see-symbol{gdk:window-edge}
  @see-function{gdk:event-time}
  @see-function{gdk:window-begin-resize-drag-for-device}"
  (window (g:object window))
  (edge window-edge)
  (button :int)
  (xroot :int)
  (yroot :int)
  (timestamp :uint32))

(export 'window-begin-resize-drag)

;;; ----------------------------------------------------------------------------
;;; gdk_window_begin_resize_drag_for_device ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_begin_resize_drag_for_device"
           window-begin-resize-drag-for-device) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @argument[edge]{a @symbol{gdk:window-edge} value with the edge or corner from
    which the drag is started}
  @argument[device]{a @class{gdk:device} object used for the operation}
  @argument[button]{an integer with the button being used to drag}
  @argument[xroot]{an integer with the root window x coordinate of mouse click
    that began the drag}
  @argument[yroot]{an integer with the root window y coordinate of mouse click
    that began the drag}
  @argument[timestamp]{an unsigned integer with the timestamp of mouse click
    that began the drag, use the @fun{gdk:event-time} function}
  @begin{short}
    Begins a window resize operation for a toplevel window.
  @end{short}
  You might use this function to implement a \"window resize grip\", for
  example. In fact @class{gtk:statusbar} widgets uses it. The function works
  best with window managers that support the Extended Window Manager Hints, but
  has a fallback implementation for other window managers.
  @see-class{gdk:window}
  @see-class{gdk:device}
  @see-class{gtk:statusbar}
  @see-function{gdk:event-time}"
  (window (g:object window))
  (edge window-edge)
  (device (g:object device))
  (button :int)
  (xroot :int)
  (yroot :int)
  (timestamp :uint32))

(export 'window-begin-resize-drag-for-device)

;;; ----------------------------------------------------------------------------
;;; gdk_window_begin_move_drag ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_begin_move_drag" window-begin-move-drag) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @argument[button]{an integer with the button being used to drag}
  @argument[xroot]{an integer with the root window x coordinate of mouse click
    that began the drag}
  @argument[yroot]{an integer with the root window y coordinate of mouse click
    that began the drag}
  @argument[timestamp]{an unsigned integer with the timestamp of mouse click
    that began the drag}
  @begin{short}
    Begins a window move operation for a toplevel window.
  @end{short}
  This function assumes that the drag is controlled by the client pointer
  device, use the @fun{gdk:window-begin-move-drag-for-device} function to begin
  a drag with a different device.
  @see-class{gdk:window}
  @see-function{gdk:window-begin-move-drag-for-device}"
  (window (g:object window))
  (button :int)
  (xroot :int)
  (yroot :int)
  (timestamp :uint32))

(export 'window-begin-move-drag)

;;; ----------------------------------------------------------------------------
;;; gdk_window_begin_move_drag_for_device ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_begin_move_drag_for_device"
           window-begin-move-drag-for-device) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @argument[device]{a @class{gdk:device} object used for the operation}
  @argument[button]{an integer with the button being used to drag}
  @argument[xroot]{an integer with the root window x coordinate of mouse click
    that began the drag}
  @argument[yroot]{an integer with the root window y coordinate of mouse click
    that began the drag}
  @argument[timestamp]{an unsigned integer with the timestamp of mouse click
    that began the drag}
  @begin{short}
    Begins a window move operation for a toplevel window.
  @end{short}
  You might use this function to implement a \"window move grip\", for example.
  The function works best with window managers that support the Extended Window
  Manager Hints, but has a fallback implementation for other window managers.
  @see-class{gdk:window}
  @see-class{gdk:device}
  @see-function{gdk:window-begin-move-drag}"
  (window (g:object window))
  (device (g:object device))
  (button :int)
  (xroot :int)
  (yroot :int)
  (timestamp :uint32))

(export 'window-begin-move-drag-for-device)

;;; ----------------------------------------------------------------------------
;;; gdk_window_show_window_menu ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_show_window_menu" window-show-window-menu) :boolean
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[event]{a @class{gdk:event} event to show the menu for}
  @return{@em{True} if the window menu was shown and @em{false} otherwise.}
  @begin{short}
    Asks the windowing system to show the window menu.
  @end{short}
  The window menu is the menu shown when right-clicking the titlebar on
  traditional windows managed by the window manager. This is useful for windows
  using client-side decorations, activating it with a right-click on the window
  decorations.
  @see-class{gdk:window}
  @see-class{gdk:event}"
  (window (g:object window))
  (event (g:boxed event)))

(export 'window-show-window-menu)

;;; ----------------------------------------------------------------------------
;;; gdk_window_constrain_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_constrain_size" %window-constrain-size) :void
  (geometry (:pointer (:struct geometry)))
  (flags window-hints)
  (width :int)
  (height :int)
  (new-width (:pointer :int))
  (new-height (:pointer :int)))

(defun window-constrain-size (geometry flags width height)
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[geometry]{a @symbol{gdk:geometry} instance}
  @argument[flags]{a @symbol{gdk:window-hints} mask indicating what portions of
    geometry are set}
  @argument[width]{an integer with the desired width of the window}
  @argument[height]{an integer with the desired height of the window}
  @begin{return}
    @code{new-width}  -- an integer with the resulting width @br{}
    @code{new-height} -- an integer with the resulting height
  @end{return}
  @begin{short}
    Constrains a desired @arg{width} and @arg{height} according to a set of
    geometry hints such as minimum and maximum size.
  @end{short}
  @see-class{gdk:window}
  @see-symbol{gdk:geometry}
  @see-symbol{gdk:window-hints}"
  (with-foreign-objects ((new-width :int) (new-height :int))
    (%window-constrain-size geometry
                            flags
                            width
                            height
                            new-width
                            new-height)
    (values (cffi:mem-ref new-width :int)
            (cffi:mem-ref new-height :int))))

(export 'window-constrain-size)

;;; ----------------------------------------------------------------------------
;;; gdk_window_beep ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_beep" window-beep) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @begin{short}
    Emits a short beep associated to @arg{window} in the appropriate display,
    if supported.
  @end{short}
  Otherwise, emits a short beep on the display just as the
  @fun{gdk:display-beep} function.
  @see-class{gdk:window}
  @see-function{gdk:display-beep}"
  (window (g:object window)))

(export 'window-beep)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_scale_factor () -> window-scale-factor
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_scale_factor" window-scale-factor) :int
 #+liber-documentation
 "@version{2023-2-26}
  @argument[window]{a @class{gdk:window} object to get scale factor for}
  @return{An integer with the scale factor.}
  @begin{short}
    Returns the internal scale factor that maps from window coordinates to the
    actual device pixels.
  @end{short}
  On traditional systems this is 1, but on very high density outputs this can
  be a higher value (often 2).

  A higher value means that drawing is automatically scaled up to a higher
  resolution, so any code doing drawing will automatically look nicer. However,
  if you are supplying pixel-based data the scale value can be used to
  determine whether to use a pixel resource with higher resolution data.

  The scale of a window may change during runtime, if this happens a configure
  event will be sent to the toplevel window.
  @see-class{gdk:window}"
  (window (g:object window)))

(export 'window-scale-factor)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_opaque_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_opaque_region" window-set-opaque-region) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel or non-native @class{gdk:window} object}
  @argument[region]{a @symbol{cairo:region-t} instance, or @code{nil}}
  @begin{short}
    For optimisation purposes, compositing window managers may like to not draw
    obscured regions of windows, or turn off blending during for these regions.
  @end{short}
  With RGB windows with no transparency, this is just the shape of the window,
  but with ARGB32 windows, the compositor does not know what regions of the
  window are transparent or not.

  This function only works for toplevel windows.

  GTK will update this property automatically if the window background is
  opaque, as we know where the opaque regions are. If your window background
  is not opaque, please update this property in your \"style-updated\" handler.
  @see-class{gdk:window}
  @see-symbol{cairo:region-t}"
  (window (g:object window))
  (region (:pointer (:struct cairo:region-t))))

(export 'window-set-opaque-region)

;;; ----------------------------------------------------------------------------
;;; gdk_window_create_gl_context ()
;;;
;;; GdkGLContext *
;;; gdk_window_create_gl_context (GdkWindow *window,
;;;                               GError **error);
;;;
;;; Creates a new GdkGLContext matching the framebuffer format to the visual of
;;; the GdkWindow. The context is disconnected from any particular window or
;;; surface.
;;;
;;; If the creation of the GdkGLContext failed, error will be set.
;;;
;;; Before using the returned GdkGLContext, you will need to call
;;; gdk_gl_context_make_current() or gdk_gl_context_realize().
;;;
;;; Parameters
;;;
;;; window
;;;     a GdkWindow
;;;
;;; error
;;;     return location for an error
;;;
;;; Returns
;;;     the newly created GdkGLContext, or NULL on error.
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_window_mark_paint_from_clip ()
;;;
;;; void
;;; gdk_window_mark_paint_from_clip (GdkWindow *window,
;;;                                  cairo_t *cr);
;;;
;;; If you call this during a paint (e.g. between
;;; gdk_window_begin_paint_region() and gdk_window_end_paint() then GDK will
;;; mark the current clip region of the window as being drawn. This is required
;;; when mixing GL rendering via gdk_cairo_draw_from_gl() and cairo rendering,
;;; as otherwise GDK has no way of knowing when something paints over the
;;; GL-drawn regions.
;;;
;;; This is typically called automatically by GTK and you don't need to care
;;; about this.
;;;
;;; Parameters
;;;
;;; window
;;;     a GdkWindow
;;;
;;; cr
;;;     a cairo_t
;;;
;;; Since 3.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_clip_region () -> window-clip-region
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_clip_region" window-clip-region)
    (:pointer (:struct cairo:region-t))
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @begin{return}
    A @symbol{cairo:region-t} instance. This must be freed with the
    @fun{cairo:region-destroy} function when you are done.
  @end{return}
  @begin{short}
    Computes the region of a window that potentially can be written to by
    drawing primitives.
  @end{short}
  This region may not take into account other factors such as if the window is
  obscured by other windows, but no area outside of this region will be affected
  by drawing primitives.
  @see-class{gdk:window}
  @see-symbol{cairo:region-t}
  @see-function{cairo:region-destroy}"
  (window (g:object window)))

(export 'window-clip-region)

;;; ----------------------------------------------------------------------------
;;; gdk_window_begin_paint_rect ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_begin_paint_rect" window-begin-paint-rect) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[rectangle]{a @class{gdk:rectangle} instance you intend to draw to}
  @begin{short}
    A convenience wrapper around the @fun{gdk:window-begin-paint-region}
    function which creates a rectangular region for you.
  @end{short}
  See the @fun{gdk:window-begin-paint-region} function for details.
  @begin[Warning]{dictionary}
    The @sym{gdk:window-begin-paint-rect} function has been deprecated since
    version 3.22 and should not be used in newly written code. Use the
    @fun{gdk:window-begin-draw-frame} function instead.
  @end{dictionary}
  @see-class{gdk:window}
  @see-class{gdk:rectangle}
  @see-function{gdk:window-begin-paint-region}
  @see-function{gdk:window-begin-draw-frame}"
  (window (g:object window))
  (rectangle (g:boxed rectangle)))

(export 'window-begin-paint-rect)

;;; ----------------------------------------------------------------------------
;;; gdk_window_begin_paint_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_begin_paint_region" window-begin-paint-region) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[region]{a @symbol{cairo:region-t} instance you intend to draw to}
  @begin{short}
    Indicates that you are beginning the process of redrawing region.
  @end{short}
  A backing store (offscreen buffer) large enough to contain @arg{region} will
  be created. The backing store will be initialized with the background color or
  background surface for @arg{window}. Then, all drawing operations performed on
  @arg{window} will be diverted to the backing store. When you call the
  @fun{gdk:window-end-paint} function, the backing store will be copied to
  @arg{window}, making it visible onscreen. Only the part of @arg{window}
  contained in @arg{region} will be modified; that is, drawing operations are
  clipped to @arg{region}.

  The net result of all this is to remove flicker, because the user sees the
  finished product appear all at once when you call the
  @fun{gdk:window-end-paint} function. If you draw to @arg{window} directly
  without calling the @sym{gdk:window-begin-paint-region} function, the user may
  see flicker as individual drawing operations are performed in sequence. The
  clipping and background-initializing features of the
  @sym{gdk:window-begin-paint-region} function are conveniences for the
  programmer, so you can avoid doing that work yourself.

  When using GTK, the widget system automatically places calls to the
  @sym{gdk:window-begin-paint-region} and @fun{gdk:window-end-paint} functions
  around emissions of the \"expose-event\" signal. That is, if you are writing
  an expose event handler, you can assume that the exposed area in the
  @class{gdk:event-expose} event has already been cleared to the window
  background, is already set as the clip region, and already has a backing
  store. Therefore in most cases, application code need not call the
  @sym{gdk:window-begin-paint-region} function. You can disable the automatic
  calls around expose events on a widget-by-widget basis by calling the
  @fun{gtk:widget-double-buffered} function.

  If you call this function multiple times before calling the matching
  @fun{gdk:window-end-paint} function, the backing stores are pushed onto a
  stack. The @fun{gdk:window-end-paint} function copies the topmost backing
  store onscreen, subtracts the topmost region from all other regions in the
  stack, and pops the stack. All drawing operations affect only the topmost
  backing store in the stack. One matching call to the
  @fun{gdk:window-end-paint} function is required for each call to the
  @sym{gdk:window-begin-paint-region} function.
  @begin[Warning]{dictionary}
    The @sym{gdk:window-begin-paint-region} function has been deprecated since
    version 3.22 and should not be used in newly written code. Use the
    @fun{gdk:window-begin-draw-frame} function instead.
  @end{dictionary}
  @see-class{gdk:window}
  @see-class{gdk:event-expose}
  @see-function{gdk:window-end-paint}
  @see-function{gtk:widget-double-buffered}
  @see-function{gdk:window-begin-draw-frame}"
  (window (g:object window))
  (region (:pointer (:struct cairo:region-t))))

(export 'window-begin-paint-region)

;;; ----------------------------------------------------------------------------
;;; gdk_window_end_paint ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_end_paint" window-end-paint) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @begin{short}
    Indicates that the backing store created by the most recent call to the
    @fun{gdk:window-begin-paint-region} function should be copied onscreen and
    deleted, leaving the next-most-recent backing store or no backing store at
    all as the active paint region.
  @end{short}
  See the @fun{gdk:window-begin-paint-region} function for full details.

  It is an error to call this function without a matching call to the
  @fun{gdk:window-begin-paint-region} function first.
  @begin[Warning]{dictionary}
    The @sym{gdk:window-end-paint} function is deprecated and should not be used
    in newly written code.
  @end{dictionary}
  @see-class{gdk:window}
  @see-function{gdk:window-begin-paint-region}"
  (window (g:object window)))

(export 'window-end-paint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_begin_draw_frame ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_begin_draw_frame" window-begin-draw-frame)
    (g:object drawing-context)
 #+liber-documentation
 "@version{2023-3-13}
  @argument[window]{a @class{gdk:window} object}
  @argument[region]{a @symbol{cairo:region-t} instance}
  @return{A @class{gdk:drawing-context} object that should be used to draw the
    contents of the window, the returned context is owned by GDK.}
  @begin{short}
    Indicates that you are beginning the process of redrawing @arg{region} on
    @arg{window}, and provides you with a @class{gdk:drawing-context} object.
  @end{short}

  If @arg{window} is a toplevel @class{gdk:window} object, backed by a native
  window implementation, a backing store (offscreen buffer) large enough to
  contain @arg{region} will be created. The backing store will be initialized
  with the background color or background surface for @arg{window}. Then, all
  drawing operations performed on @arg{window} will be diverted to the backing
  store. When you call the @fun{gdk:window-end-draw-frame} function, the
  contents of the backing store will be copied to @arg{window}, making it
  visible on screen. Only the part of @arg{window} contained in @arg{region}
  will be modified, that is, drawing operations are clipped to @arg{region}.

  The net result of all this is to remove flicker, because the user sees the
  finished product appear all at once when you call the
  @fun{gdk:window-end-draw-frame} function. If you draw to the window directly
  without calling the @sym{gdk:window-begin-draw-frame} function, the user may
  see flicker as individual drawing operations are performed in sequence.

  When using GTK, the widget system automatically places calls to the
  @sym{gdk:window-begin-draw-frame} and @fun{gdk:window-end-draw-frame}
  functions around emissions of the \"draw\" signal. That is, if you are drawing
  the contents of the widget yourself, you can assume that the widget has a
  cleared background, is already set as the clip region, and already has a
  backing store. Therefore in most cases, application code in GTK does not need
  to call the @sym{gdk:window-begin-draw-frame} function explicitly.
  @see-class{gdk:window}
  @see-class{gdk:drawing-context}
  @see-symbol{cairo:region-t}
  @see-function{gdk:window-end-draw-frame}"
  (window (g:object window))
  (region (:pointer (:struct cairo:region-t))))

(export 'window-begin-draw-frame)

;;; ----------------------------------------------------------------------------
;;; gdk_window_end_draw_frame ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_end_draw_frame" window-end-draw-frame) :void
 #+liber-documentation
 "@version{2023-3-13}
  @argument[window]{a @class{gdk:window} object}
  @argument[context]{a @class{gdk:drawing-context} object created by the
   @fun{gdk:window-begin-draw-frame} function}
  @begin{short}
    Indicates that the drawing of the contents of @arg{window} started with
    the @fun{gdk:window-begin-draw-frame} function has been completed.
  @end{short}
  This function will take care of destroying the @class{gdk:drawing-context}
  object. It is an error to call this function without a matching call of the
  @fun{gdk:window-begin-draw-frame} function first.
  @see-class{gdk:window}
  @see-class{gdk:drawing-context}
  @see-function{gdk:window-begin-draw-frame}"
  (window (g:object window))
  (context (g:object drawing-context)))

(export 'window-end-draw-frame)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_visible_region () -> window-visible-region
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_visible_region" window-visible-region)
    (:pointer (:struct cairo:region-t))
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @begin{return}
    A @symbol{cairo:region-t} instance. This must be freed with the
    @fun{cairo:region-destroy} function when you are done.
  @end{return}
  Computes the region of the window that is potentially visible. This does not
  necessarily take into account if the window is obscured by other windows,
  but no area outside of this region is visible.
  @see-class{gdk:window}
  @see-symbol{cairo:region-t}
  @see-function{cairo:region-destroy}"
  (window (g:object window)))

(export 'window-visible-region)

;;; ----------------------------------------------------------------------------
;;; GdkWindowInvalidateHandlerFunc ()
;;;
;;; void
;;; (*GdkWindowInvalidateHandlerFunc) (GdkWindow *window,
;;;                                    cairo_region_t *region);
;;;
;;; Whenever some area of the window is invalidated (directly in the window or
;;; in a child window) this gets called with region in the coordinate space of
;;; window . You can use region to just keep track of the dirty region, or you
;;; can actually change region in case you are doing display tricks like showing
;;; a child in multiple places.
;;;
;;; Parameters
;;;
;;; window
;;;     a GdkWindow
;;;
;;; region
;;;     a cairo_region_t
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_invalidate_handler ()
;;;
;;; void
;;; gdk_window_set_invalidate_handler (GdkWindow *window,
;;;                                    GdkWindowInvalidateHandlerFunc handler);
;;;
;;; Registers an invalidate handler for a specific window. This will get called
;;; whenever a region in the window or its children is invalidated.
;;;
;;; This can be used to record the invalidated region, which is useful if you
;;; are keeping an offscreen copy of some region and want to keep it up to date.
;;; You can also modify the invalidated region in case you’re doing some effect
;;; where e.g. a child widget appears in multiple places.
;;;
;;; Parameters
;;;
;;; window
;;;     a GdkWindow
;;;
;;; handler
;;;     a GdkWindowInvalidateHandlerFunc callback function
;;;
;;; Since 3.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_window_invalidate_rect ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_invalidate_rect" window-invalidate-rect) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[rect]{a @class{gdk:rectangle} instance to invalidate or @code{nil}
    to invalidate the whole window}
  @argument[invalidate]{a boolean whether to also invalidate child windows}
  @begin{short}
    A convenience wrapper around the @fun{gdk:window-invalidate-region} function
    which invalidates a rectangular region.
  @end{short}
  See the @fun{gdk:window-invalidate-region} function for details.
  @see-class{gdk:window}
  @see-class{gdk:rectangle}
  @see-function{gdk:window-invalidate-region}"
  (window (g:object window))
  (rectangle (g:boxed rectangle))
  (invalidate :boolean))

(export 'window-invalidate-rect)

;;; ----------------------------------------------------------------------------
;;; gdk_window_invalidate_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_invalidate_region" window-invalidate-region) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[region]{a @symbol{cairo:region-t} instance}
  @argument[invalidate]{@em{true} to also invalidate child windows}
  @begin{short}
    Adds @arg{region} to the update area for @arg{window}.
  @end{short}
  The update area is the region that needs to be redrawn, or \"dirty region\".
  The call of the @fun{gdk:window-process-updates} function sends one or more
  expose events to the window, which together cover the entire update area. An
  application would normally redraw the contents of the window in response to
  those expose events.

  GDK will call the @fun{gdk:window-process-all-updates} function on your behalf
  whenever your program returns to the main loop and becomes idle, so normally
  there is no need to do that manually, you just need to invalidate regions that
  you know should be redrawn.

  The @arg{invalidate} parameter controls whether the region of each child
  window that intersects @arg{region} will also be invalidated. If @code{nil},
  then the update area for child windows will remain unaffected. See the
  @fun{gdk:window-invalidate-maybe-recurse} function if you need fine grained
  control over which children are invalidated.
  @see-class{gdk:window}
  @see-symbol{cairo:region-t}
  @see-function{gdk:window-process-updates}
  @see-function{gdk:window-process-all-updates}
  @see-function{gdk:window-invalidate-maybe-recurse}"
  (window (g:object window))
  (region (:pointer (:struct cairo:region-t)))
  (invalidate :boolean))

(export 'window-invalidate-region)

;;; ----------------------------------------------------------------------------
;;; GdkWindowChildFunc ()
;;; ----------------------------------------------------------------------------

(defcallback window-child-func :boolean
    ((window (g:object window))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (funcall func window)))

#+liber-documentation
(setf (liber:alias-for-symbol 'window-child-func)
      "Callback"
      (liber:symbol-documentation 'window-child-func)
 "@version{#2023-2-26}
  @begin{short}
    A callback function of this type is passed to the
    @fun{gdk:window-invalidate-maybe-recurse} function.
  @end{short}
  It gets called for each child of the window to determine whether to
  recursively invalidate it or now.
  @begin{pre}
lambda (window)
  @end{pre}
  @begin[code]{table}
    @entry[window]{A @class{gdk:window} object.}
    @entry[Returns]{@em{True} to invalidate @arg{window} recursively.}
  @end{table}
  @see-class{gdk:window}
  @see-class{gdk:window-invalidate-maybe-recurse}")

(export 'window-child-func)

;;; ----------------------------------------------------------------------------
;;; gdk_window_invalidate_maybe_recurse ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_invalidate_maybe_recurse"
          %window-invalidate-maybe-recurse) :void
  (window (g:object window))
  (region (:pointer (:struct cairo:region-t)))
  (func :pointer)
  (user-data :pointer))

(defun window-invalidate-maybe-recurse (window region func)
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[region]{a @symbol{cairo:region-t} instance}
  @argument[func]{a @symbol{gdk:window-child-func} callback function to use to
    decide if to recurse to a child, @code{nil} means never recurse}
  @begin{short}
    Adds region to the update area for window.
  @end{short}
  The update area is the region that needs to be redrawn, or \"dirty region\".
  The call of the @fun{gdk:window-process-updates} function sends one or more
  expose events to the window, which together cover the entire update area. An
  application would normally redraw the contents of @arg{window} in response to
  those expose events.

  GDK will call the @fun{gdk:window-process-all-updates} function on your behalf
  whenever your program returns to the main loop and becomes idle, so normally
  there is no need to do that manually, you just need to invalidate regions that
  you know should be redrawn.

  The @arg{func} parameter controls whether the region of each child
  window that intersects region will also be invalidated. Only children for
  which @arg{func} returns @em{true} will have the area invalidated.
  @see-class{gdk:window}
  @see-symbol{cairo:region-t}
  @see-function{gdk:window-process-updates}
  @see-function{gdk:window-process-all-updates}"
  (with-stable-pointer (ptr func)
    (%window-invalidate-maybe-recurse window
                                      region
                                      (cffi:callback window-child-func)
                                      ptr)))

(export 'window-invalidate-maybe-recurse)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_update_area () -> window-update-area
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_update_area" window-update-area)
    (:pointer (:struct cairo:region-t))
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @return{The @symbol{cairo:region-t} update area for window.}
  @begin{short}
    Transfers ownership of the update area from @arg{window} to the caller of
    the function.
  @end{short}
  That is, after calling this function, @arg{window} will no longer have an
  invalid/dirty region; the update area is removed from @arg{window} and handed
  to you. If a window has no update area, the @sym{gdk:window-update-area}
  function returns @code{nil}. You are responsible for calling the
  @fun{cairo:region-destroy} function on the returned region if it is
  non-@code{nil}.
  @see-class{gdk:window}
  @see-symbol{cairo:region-t}
  @see-function{cairo:region-destroy}"
  (window (g:object window)))

(export 'window-update-area)

;;; ----------------------------------------------------------------------------
;;; gdk_window_freeze_updates ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_freeze_updates" window-freeze-updates) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @begin{short}
    Temporarily freezes a window such that it will not receive expose events.
  @end{short}
  The window will begin receiving expose events again when the
  @fun{gdk:window-thaw-updates} function is called. If the
  @sym{gdk:window-freeze-updates} function has been called more than once, the
  @fun{gdk:window-thaw-updates} function must be called an equal number of
  times to begin processing exposes.
  @see-class{gdk:window}
  @see-function{gdk:window-thaw-updates}"
  (window (g:object window)))

(export 'window-freeze-updates)

;;; ----------------------------------------------------------------------------
;;; gdk_window_thaw_updates ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_thaw_updates" window-thaw-updates) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @begin{short}
    Thaws a window frozen with the @fun{gdk:window-freeze-updates} function.
  @end{short}
  @see-class{gdk:window}
  @see-function{gdk:window-freeze-updates}"
  (window (g:object window)))

(export 'window-thaw-updates)

;;; ----------------------------------------------------------------------------
;;; gdk_window_process_all_updates ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_process_all_updates" window-process-all-updates) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @begin{short}
    Calls the @fun{gdk:window-process-updates} function for all windows.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gdk:window-process-all-updates} function has been deprecated since
    version 3.22 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gdk:window}
  @see-function{gdk:window-process-updates}")

(export 'window-process-all-updates)

;;; ----------------------------------------------------------------------------
;;; gdk_window_process_updates ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_process_updates" window-process-updates) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[update]{a boolean whether to also process updates for child windows}
  @begin{short}
    Sends one or more expose events to window.
  @end{short}
  The areas in each expose event will cover the entire update area for the
  window, see the @fun{gdk:window-invalidate-region} function for details.
  Normally GDK calls the @fun{gdk:window-process-all-updates} function on your
  behalf, so there is no need to call this function unless you want to force
  expose events to be delivered immediately and synchronously, vs. the usual
  case, where GDK delivers them in an idle handler. Occasionally this is useful
  to produce nicer scrolling behavior, for example.
  @begin[Warning]{dictionary}
    The @sym{gdk:window-process-updates} function has been deprecated since
    version 3.22 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gdk:window}
  @see-function{gdk:window-invalidate-region}
  @see-function{gdk:window-process-all-updates}"
  (window (g:object window))
  (update :boolean))

(export 'window-process-updates)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_debug_updates ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_debug_updates" window-set-debug-updates) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[setting]{@em{true} to turn on update debugging}
  @begin{short}
    With update debugging enabled, calls to the
    @fun{gdk:window-invalidate-region} function clear the invalidated region of
    the screen to a noticeable color, and GDK pauses for a short time before
    sending exposes to windows during the @fun{gdk:window-process-updates}
    function.
  @end{short}
  The net effect is that you can see the invalid region for each window and
  watch redraws as they occur. This allows you to diagnose inefficiencies in
  your application.

  In essence, because the GDK rendering model prevents all flicker, if you are
  redrawing the same region 400 times you may never notice, aside from
  noticing a speed problem. Enabling update debugging causes GTK to flicker
  slowly and noticeably, so you can see exactly what is being redrawn when, in
  what order.

  The @code{--gtk-debug=updates} command line option passed to GTK programs
  enables this debug option at application startup time. That is usually more
  useful than calling the @fun{gdk:window-set-debug-updates} function yourself,
  though you might want to use this function to enable updates sometime after
  application startup time.
  @begin[Warning]{dictionary}
    The @sym{gdk:window-set-debug-updates} function has been deprecated since
    version 3.22 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gdk:window}
  @see-function{gdk:window-invalidate-region}
  @see-function{gdk:window-process-updates}"
  (setting :boolean))

(export 'window-set-debug-updates)

;;; ----------------------------------------------------------------------------
;;; gdk_window_enable_synchronized_configure ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_enable_synchronized_configure"
           window-enable-synchronized-configure) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @begin{short}
    Does nothing, present only for compatiblity.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gdk:window-enable-synchronized-configure} function has been
    deprecated since version 3.8 and should not be used in newly written code.
    This function is no longer needed.
  @end{dictionary}
  @see-class{gdk:window}
  @see-function{gdk:window-configure-finished}"
  (window (g:object window)))

(export 'window-enable-synchronized-configure)

;;; ----------------------------------------------------------------------------
;;; gdk_window_configure_finished ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_configure_finished" window-configure-finished) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @begin{short}
    Does nothing, present only for compatiblity.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gdk:window-configure-finished} function has been deprecated since
    version 3.8 and should not be used in newly written code. This function is
    no longer needed.
  @end{dictionary}
  @see-class{gdk:window}
  @see-function{gdk:window-enable-synchronized-configure}"
  (window (g:object window)))

(export 'window-configure-finished)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_frame_clock () -> window-frame-clock
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_frame_clock" window-frame-clock)
    (g:object frame-clock)
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} obeject}
  @return{The @class{gdk:frame-clock} object.}
  @begin{short}
    Gets the frame clock for the window.
  @end{short}
  The frame clock for a window never changes unless the window is reparented to
  a new toplevel window.
  @see-class{gdk:window}
  @see-class{gdk:frame-clock}"
  (window (g:object window)))

(export 'window-frame-clock)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_user_data ()
;;; gdk_window_set_user_data () -> window-user-data
;;; ----------------------------------------------------------------------------

(defun (setf window-user-data) (data window)
  (cffi:foreign-funcall "gdk_window_set_user_data"
                        (g:object window) window
                        :pointer data
                        :void)
  data)

(defcfun ("gdk_window_get_user_data" %window-user-data) :void
  (window (g:object window))
  (data :pointer))

(defun window-user-data (window)
 #+liber-documentation
 "@version{#2023-2-26}
  @syntax[]{(gdk:window-user-data window) => data}
  @syntax[]{(setf (gdk:window-user-data window) data)}
  @argument[window]{a @class{gdk:window} object}
  @argument[data]{a pointer with the user data}
  @begin{short}
    The @sym{gdk:window-user-data} function retrieves the user data for
    @arg{window}, which is normally the widget that @arg{window} belongs to.
  @end{short}
  The @sym{(setf gdk:window-user-data)} function sets the user data.

  For most purposes this function is deprecated in favor of the
  @fun{g:object-data} function. However, for historical reasons GTK stores the
  @class{gtk:widget} widget that owns a @class{gdk:window} object as user data
  on the @class{gdk:window} object. So, custom widget implementations should use
  this function for that. If GTK receives an event for a @class{gdk:window}
  object, and the user data for the window is non-@code{nil}, GTK will assume
  the user data is a @class{gtk:widget} widget, and forward the event to that
  widget.
  @see-class{gdk:window}
  @see-class{gtk:widget}
  @see-function{g:object-data}"
  (with-foreign-object (data :pointer)
    (%window-user-data window data)
    (cffi:mem-ref data :pointer)))

(export 'window-user-data)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_override_redirect ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_override_redirect" window-set-override-redirect) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @argument[redirect]{@em{true} if @arg{window} should be override redirect}
  @begin{short}
    An override redirect window is not under the control of the window manager.
  @end{short}
  This means it will not have a titlebar, will not be minimizable, etc. - it
  will be entirely under the control of the application. The window manager
  cannot see the override redirect window at all.

  Override redirect should only be used for short-lived temporary windows,
  such as popup menus. The @class{gtk:menu} widget uses an override redirect
  window in its implementation, for example.
  @see-class{gdk:window}
  @see-class{gtk:menu}"
  (window (g:object window))
  (override-redirect :boolean))

(export 'window-set-override-redirect)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_accept_focus ()
;;; gdk_window_set_accept_focus () -> window-accept-focus
;;; ----------------------------------------------------------------------------

(defun (setf window-accept-focus) (setting window)
  (cffi:foreign-funcall "gdk_window_set_accept_focus"
                        (g:object window) window
                        :boolean setting
                        :void)
  setting)

(defcfun ("gdk_window_get_accept_focus" window-accept-focus) :boolean
 #+liber-documentation
 "@version{#2023-2-26}
  @syntax[]{(gdk:window-accept-focus window) => setting}
  @syntax[]{(setf (gdk:window-accept-focus window) setting)}
  @argument[window]{a toplevel @class{gdk:window} object}
  @argument[setting]{@em{true} if @arg{window} should receive input focus}
  @begin{short}
    Determines whether or not the desktop environment should be hinted that the
    window does not want to receive input focus.
  @end{short}
  Setting @arg{setting} to @em{false} hints the desktop environment that the
  window does not want to receive input focus.

  On X, it is the responsibility of the window manager to interpret this hint.
  ICCCM-compliant window manager usually respect it.
  @see-class{gdk:window}"
  (window (g:object window)))

(export 'window-accept-focus)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_focus_on_map ()
;;; gdk_window_set_focus_on_map () -> window-focus-on-map
;;; ----------------------------------------------------------------------------

(defun (setf window-focus-on-map) (setting window)
  (cffi:foreign-funcall "gdk_window_set_focus_on_map"
                        (g:object window) window
                        :boolean setting
                        :void)
  setting)

(defcfun ("gdk_window_get_focus_on_map" window-focus-on-map) :boolean
 #+liber-documentation
 "@version{#2023-2-26}
  @syntax[]{(gdk:window-focus-on-map window) => setting}
  @syntax[]{(setf gdk:window-focus-on-map window) setting)}
  @argument[window]{a toplevel @class{gdk:window} object}
  @argument[setting]{@em{true} if the window should receive input focus when
    mapped}
  @begin{short}
    Whether or not the window wants to receive input focus when it is
    mapped.
  @end{short}
  The @sym{gdk:window-focus-on-map} function determines whether or not the
  desktop environment should be hinted that the window does not want to receive
  input focus when it is mapped.

  Setting @arg{setting} to @em{false} hints the desktop environment that the
  window does not want to receive input focus when it is mapped. @arg{setting}
  should be turned off for windows that are not triggered interactively, such
  as popups from network activity.

  On X, it is the responsibility of the window manager to interpret this hint.
  Window managers following the freedesktop.org window manager extension
  specification should respect it.
  @see-class{gdk:window}"
  (window (g:object window)))

(export 'window-focus-on-map)

;;; ----------------------------------------------------------------------------
;;; GdkXEvent
;;;
;;; typedef void GdkXEvent;      /* Can be cast to window system specific
;;;
;;; Used to represent native events (XEvents for the X11 backend, MSGs for
;;; Win32).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GdkFilterFunc ()
;;;
;;; GdkFilterReturn (*GdkFilterFunc) (GdkXEvent *xevent,
;;;                                   GdkEvent *event,
;;;                                   gpointer data);
;;;
;;; Specifies the type of function used to filter native events before they are
;;; converted to GDK events.
;;;
;;; When a filter is called, event is unpopulated, except for event->window. The
;;; filter may translate the native event to a GDK event and store the result in
;;; event, or handle it without translation. If the filter translates the event
;;; and processing should continue, it should return GDK_FILTER_TRANSLATE.
;;;
;;; xevent :
;;;     the native event to filter.
;;;
;;; event :
;;;     the GDK event to which the X event will be translated.
;;;
;;; data :
;;;     user data set when the filter was installed.
;;;
;;; Returns :
;;;     a GdkFilterReturn value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_window_add_filter ()
;;;
;;; void gdk_window_add_filter (GdkWindow *window,
;;;                             GdkFilterFunc function,
;;;                             gpointer data);
;;;
;;; Adds an event filter to window, allowing you to intercept events before they
;;; reach GDK. This is a low-level operation and makes it easy to break GDK
;;; and/or GTK, so you have to know what you're doing. Pass NULL for window to
;;; get all events for all windows, instead of events for a specific window.
;;;
;;; If you are interested in X GenericEvents, bear in mind that XGetEventData()
;;; has been already called on the event, and XFreeEventData() must not be
;;; called within function.
;;;
;;; window :
;;;     a GdkWindow
;;;
;;; function :
;;;     filter callback
;;;
;;; data :
;;;     data to pass to filter callback
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_window_remove_filter ()
;;;
;;; void gdk_window_remove_filter (GdkWindow *window,
;;;                                GdkFilterFunc function,
;;;                                gpointer data);
;;;
;;; Remove a filter previously added with gdk_window_add_filter().
;;;
;;; window :
;;;     a GdkWindow
;;;
;;; function :
;;;     previously-added filter function
;;;
;;; data :
;;;     user data for previously-added filter function
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_window_shape_combine_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_shape_combine_region" window-shape-combine-region) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[region]{a @symbol{cairo:region-t} region of @arg{window} to be
    non-transparent}
  @argument[xoffset]{an integer with the x position of @arg{region} in window
    coordinates}
  @argument[yoffset]{an integer with the y position of @arg{region} in window
    coordinates}
  @begin{short}
    Makes pixels in the window outside @arg{region} be transparent, so that the
    window may be nonrectangular.
  @end{short}
  If @arg{region} is @code{nil}, the shape will be unset, so the whole window
  will be opaque again. @arg{xoffset} and @arg{yoffset} are ignored if
  @arg{region} is @code{nil}.

  On the X11 platform, this uses an X server extension which is widely
  available on most common platforms, but not available on very old X servers,
  and occasionally the implementation will be buggy. On servers without the
  shape extension, this function will do nothing.

  This function works on both toplevel and child windows.
  @see-class{gdk:window}
  @see-symbol{cairo:region-t}"
  (window (g:object window))
  (region (:pointer (:struct cairo:region-t)))
  (xoffset :int)
  (yoffset :int))

(export 'window-shape-combine-region)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_child_shapes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_child_shapes" window-set-child-shapes) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @begin{short}
    Sets the shape mask of the window to the union of shape masks for all
    children of the window, ignoring the shape mask of the window itself.
  @end{short}
  Contrast with the @fun{gdk:window-merge-child-shapes} function which
  includes the shape mask of the window in the masks to be merged.
  @see-class{gdk:window}
  @see-function{gdk:window-merge-child-shapes}"
  (window (g:object window)))

(export 'window-set-child-shapes)

;;; ----------------------------------------------------------------------------
;;; gdk_window_merge_child_shapes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_merge_child_shapes" window-merge-child-shapes) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @begin{short}
    Merges the shape masks for any child windows into the shape mask for the
    window.
  @end{short}
  I.e. the union of all masks for the window and its children will become the
  new mask for the window. See the @fun{gdk:window-shape-combine-region}
  function.

  This function is distinct from the @fun{gdk:window-set-child-shapes} function
  because it includes window's shape mask in the set of shapes to be merged.
  @see-class{gdk:window}
  @see-function{gdk:window-shape-combine-region}
  @see-function{gdk:window-set-child-shapes}"
  (window (g:object window)))

(export 'window-merge-child-shapes)

;;; ----------------------------------------------------------------------------
;;; gdk_window_input_shape_combine_region ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_input_shape_combine_region"
           window-input-shape-combine-region) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[region]{a @symbol{cairo:region-t} region of @arg{window} to be
    non-transparent}
  @argument[xoffset]{an integer with the x position of @arg{region} in window
    coordinates}
  @argument[yoffset]{an integer with the y position of @arg{region} in window
    coordinates}
  @begin{short}
    Like the @fun{gdk:window-shape-combine-region} function, but the shape
    applies only to event handling.
  @end{short}
  Mouse events which happen while the pointer position corresponds to an unset
  bit in the mask will be passed on the window below window.

  An input shape is typically used with RGBA windows. The alpha channel of the
  window defines which pixels are invisible and allows for nicely antialiased
  borders, and the input shape controls where the window is \"clickable\".

  On the X11 platform, this requires version 1.1 of the shape extension.

  On the Win32 platform, this functionality is not present and the function
  does nothing.
  @see-class{gdk:window}
  @see-symbol{cairo:region-t}
  @see-function{gdk:window-shape-combine-region}"
  (window (g:object window))
  (region (:pointer (:struct cairo:region-t)))
  (xoffset :int)
  (yoffset :int))

(export 'window-input-shape-combine-region)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_child_input_shapes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_child_input_shapes" window-set-child-input-shapes)
    :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @begin{short}
    Sets the input shape mask of the window to the union of input shape masks
    for all children of window, ignoring the input shape mask of window itself.
  @end{short}
  Contrast with the @fun{gdk:window-merge-child-input-shapes} function which
  includes the input shape mask of the window in the masks to be merged.
  @see-class{gdk:window}
  @see-function{gdk:window-merge-child-input-shapes}"
  (window (g:object window)))

(export 'window-set-child-input-shapes)

;;; ----------------------------------------------------------------------------
;;; gdk_window_merge_child_input_shapes ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_merge_child_input_shapes"
           window-merge-child-input-shapes) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @begin{short}
    Merges the input shape masks for any child windows into the input shape mask
    for @arg{window}.
  @end{short}
  I.e. the union of all input masks for the window and its children will become
  the new input mask for the window. See the
  @fun{gdk:window-input-shape-combine-region} function.

  This function is distinct from the @fun{gdk:window-set-child-input-shapes}
  function because it includes window's input shape mask in the set of shapes
  to be merged.
  @see-class{gdk:window}
  @see-function{gdk:window-input-shape-combine-region}
  @see-function{gdk:window-set-child-input-shapes}"
  (window (g:object window)))

(export 'window-merge-child-input-shapes)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_static_gravities ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_static_gravities" window-set-static-gravities)
    :boolean
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[use-static]{@em{true} to turn on static gravity}
  @return{@em{True} if the server supports static gravity.}
  @begin{short}
    Set the bit gravity of the given window to static, and flag it so all
    children get static subwindow gravity.
  @end{short}
  This is used if you are implementing scary features that involve deep
  knowledge of the windowing system. Do not worry about it unless you have to.
  @begin[Warning]{dictionary}
    The @sym{gdk:window-set-static-gravities} function has been deprecated since
    version 3.16 and should not be used in newly written code. Static gravities
    have not worked on anything but X11 for a long time.
  @end{dictionary}
  @see-class{gdk:window}"
  (window (g:object window))
  (use-static :boolean))

(export 'window-set-static-gravities)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_title ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_title" window-set-title) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @argument[title]{a string with the title of @arg{window}}
  @begin{short}
    Sets the title of a toplevel window, to be displayed in the titlebar.
  @end{short}
  If you have not explicitly set the icon name for the window, using the
  @fun{gdk:window-set-icon-name} function, the icon name will be set to title
  as well. @arg{title} must be in UTF-8 encoding, as with all user-readable
  strings in GDK/GTK. @arg{title} may not be @code{nil}.
  @see-class{gdk:window}
  @see-function{gdk:window-set-icon-name}"
  (window (g:object window))
  (title :string))

(export 'window-set-title)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_background ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_background" window-set-background) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[color]{a @class{gdk:color} instance}
  @begin{short}
    Sets the background color of the window.
  @end{short}
  However, when using GTK, set the background of a widget with the
  @fun{gtk:widget-modify-bg} function, if you are implementing an application,
  or the @fun{gtk:style-context-set-background} function, if you are
  implementing a custom widget.

  See also the @fun{gdk:window-background-pattern} function.
  @begin[Warning]{dictionary}
    The @sym{gdk:window-set-background} function has been deprecated since
    version 3.4 and should not be used in newly written code. Use the
    @fun{gdk:window-set-background-rgba} function instead.
  @end{dictionary}
  @see-class{gdk:window}
  @see-function{gtk:widget-modify-bg}
  @see-function{gdk:window-set-background-rgba}
  @see-function{gtk:style-context-set-background}
  @see-function{gdk:window-background-pattern}"
  (window (g:object window))
  (color (g:boxed color)))

(export 'window-set-background)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_background_rgba ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_background_rgba" window-set-background-rgba) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[rgba]{a @class{gdk:rgba} color}
  @begin{short}
    Sets the background color of the window.
  @end{short}
  See also the @fun{gdk:window-background-pattern} function.
  @begin[Warning]{dictionary}
    The @sym{gdk:window-set-background-rgba} function has been deprecated since
    version 3.22 and should not be used in newly written code. Do not use this
    function.
  @end{dictionary}
  @see-class{gdk:window}
  @see-function{gdk:window-background-pattern}"
  (window (g:object window))
  (rgba (g:boxed rgba)))

(export 'window-set-background-rgba)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_background_pattern ()
;;; gdk_window_set_background_pattern () -> window-background-pattern
;;; ----------------------------------------------------------------------------

(defun (setf window-background-pattern) (pattern window)
  (cffi:foreign-funcall "gdk_window_set_background_pattern"
                        (g:object window) window
                        (:pointer (:struct cairo:pattern-t)) pattern
                        :void)
  pattern)

(defcfun ("gdk_window_get_background_pattern" window-background-pattern)
    (:pointer (:struct cairo:pattern-t))
 #+liber-documentation
 "@version{#2023-2-26}
  @syntax[]{(gdk:window-background-pattern window) => pattern}
  @syntax[]{(setf (gdk:window-background-pattern window) pattern)}
  @argument[window]{a @class{gdk:window} object}
  @argument[pattern]{a @symbol{cairo:pattern-t} pattern to use, or @code{nil}}
  @begin{short}
    The pattern to use for the background or @code{nil} to use the parent's
    background.
  @end{short}
  The @sym{gdk:window-background-pattern} function gets the pattern used to
  clear the background on the window. If the window does not have its own
  background and reuses the parent's, @code{nil} is returned and you will have
  to query it yourself. The @sym{(setf gdk:window-background-pattern)} function
  sets the background of the window.

  A background of @code{nil} means that the window will inherit its background
  form its parent window.

  The windowing system will normally fill a window with its background when
  the window is obscured then exposed.
  @begin[Warning]{dictionary}
    The @sym{gdk:window-background-pattern} function has been deprecated since
    version 3.22 and should not be used in newly written code. Do not use this
    function.
  @end{dictionary}
  @see-class{gdk:window}
  @see-symbol{cairo:pattern-t}"
  (window (g:object window)))

(export 'window-background-pattern)

;;; ----------------------------------------------------------------------------
;;; GDK_PARENT_RELATIVE
;;; ----------------------------------------------------------------------------

;; We do not export this constant

(defconstant +gdk-parent-relative+ 1
 #+liber-documentation
 "@version{#2013-9-1}
  @variable-value{1}
  A special value, indicating that the background for a window should be
  inherited from the parent window.
  @see-class{gdk:window}")

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_geometry () -> window-geometry
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_geometry" %window-geometry) :void
  (window (g:object window))
  (x (:pointer :int))
  (y (:pointer :int))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun window-geometry (window)
 #+liber-documentation
 "@version{2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @begin{return}
    @code{x} -- an integer with the x coordinate of @arg{window}, relative to
    its parent @br{}
    @code{y} -- an integer with the y coordinate of @arg{window}, relative to
    its parent @br{}
    @code{width} -- an integer with the width of @arg{window} @br{}
    @code{height} -- an integer with the height of @arg{window}
  @end{return}
  @begin{short}
    The @arg{x} and @arg{y} coordinates returned are relative to the parent
    window of @arg{window}, which for toplevels usually means relative to the
    window decorations (titlebar, etc.) rather than relative to the root window
    (screen-size background window).
  @end{short}

  On the X11 platform, the geometry is obtained from the X server, so reflects
  the latest position of the window. This may be out-of-sync with the position
  of the window delivered in the most-recently-processed
  @class{gdk:event-configure} event. The @fun{gdk:window-position} function in
  contrast gets the position from the most recent configure event.
  @begin[Note]{dictionary}
    If @arg{window} is not a toplevel, it is much better to call the
    @fun{gdk:window-position}, @fun{gdk:window-width} and
    @fun{gdk:window-height} functions instead, because it avoids the roundtrip
    to the X server and because these functions support the full 32-bit
    coordinate space, whereas the @sym{gdk:window-geometry} function is
    restricted to the 16-bit coordinates of X11.
  @end{dictionary}
  @see-class{gdk:window}
  @see-class{gdk:event-configure}
  @see-function{gdk:window-position}
  @see-function{gdk:window-width}
  @see-function{gdk:window-height}"
  (with-foreign-objects ((x :int) (y :int) (width :int) (height :int))
    (%window-geometry window x y width height)
    (values (cffi:mem-ref x :int)
            (cffi:mem-ref y :int)
            (cffi:mem-ref width :int)
            (cffi:mem-ref height :int))))

(export 'window-geometry)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_geometry_hints ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_geometry_hints" window-set-geometry-hints) :void
 #+liber-documentation
 "@version{2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @argument[geometry]{a @symbol{gdk:geometry} instance with the geometry hints}
  @argument[mask]{a @symbol{gdk:window-hints} bitmask indicating fields
    of @arg{geometry} to pay attention to}
  @begin{short}
    Sets the geometry hints for the window.
  @end{short}
  Hints flagged in @arg{mask} are set, hints not flagged  are unset.

  This function provides hints to the windowing system about acceptable sizes
  for a toplevel window. The purpose of this is to constrain user resizing,
  but the windowing system will typically, but is not required to, also
  constrain the current size of the window to the provided values and
  constrain programatic resizing via the @fun{gdk:window-resize} or
  @fun{gdk:window-move-resize} functions.

  Note that on X11, this effect has no effect on windows of type @code{:temp}
  or windows where override redirect has been turned on via the
  @fun{gdk:window-set-override-redirect} function since these windows are not
  resizable by the user.

  Since you cannot count on the windowing system doing the constraints for
  programmatic resizes, you should generally call the
  @fun{gdk:window-constrain-size} function yourself to determine appropriate
  sizes.
  @see-class{gdk:window}
  @see-symbol{gdk:geometry}
  @see-symbol{gdk:window-hints}
  @see-function{gdk:window-resize}
  @see-function{gdk:window-move-resize}
  @see-function{gdk:window-set-override-redirect}
  @see-function{gdk:window-constrain-size}"
  (window (g:object window))
  (geometry (:pointer (:struct geometry)))
  (mask window-hints))

(export 'window-set-geometry-hints)

;;; -------------------------------------------------------------_---------------
;;; gdk_window_get_width () -> window-width
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_width" window-width) :int
 #+liber-documentation
 "@version{2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @return{An integer with the width of @arg{window}.}
  @begin{short}
    Returns the width of the given window.
  @end{short}
  On the X11 platform the returned size is the size reported in the
  most-recently-processed configure event, rather than the current size on the
  X server.
  @see-class{gdk:window}
  @see-function{gdk:window-height}
  @see-function{gdk:window-position}"
  (window (g:object window)))

(export 'window-width)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_height () -> window-height
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_height" window-height) :int
 #+liber-documentation
 "@version{2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @return{An integer with the height of @arg{window}.}
  @begin{short}
    Returns the height of the given window.
  @end{short}
  On the X11 platform the returned size is the size reported in the
  most-recently-processed configure event, rather than the current size on the
  X server.
  @see-class{gdk:window}
  @see-function{gdk:window-width}
  @see-function{gdk:window-position}"
  (window (g:object window)))

(export 'window-height)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_icon_list ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_icon_list" window-set-icon-list) :void
 #+liber-documentation
 "@version{#2023-3-12}
  @argument[window]{a toplevel @class{gdk:window} window to set the icon of}
  @argument[pixbufs]{a list of @class{gdk-pixbuf:pixbuf} objects, of different
    sizes}
  @begin{short}
    Sets a list of icons for the window.
  @end{short}
  One of these will be used to represent the window when it has been iconified.
  The icon is usually shown in an icon box or some sort of task bar. Which icon
  size is shown depends on the window manager. The window manager can scale the
  icon but setting several size icons can give better image quality since the
  window manager may only need to scale the icon by a small amount or not at
  all.
  @see-class{gdk:window}
  @see-class{gdk-pixbuf:pixbuf}"
  (window (g:object window))
  (pixbufs (g:list-t (g:object gdk-pixbuf:pixbuf))))

(export 'window-set-icon-list)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_modal_hint ()
;;; gdk_window_set_modal_hint () -> window-modal-hint
;;; ----------------------------------------------------------------------------

(defun (setf window-modal-hint) (modal window)
  (cffi:foreign-funcall "gdk_window_set_modal_hint"
                        (g:object window) window
                        :boolean modal
                        :void)
  modal)

(defcfun ("gdk_window_get_modal_hint" window-modal-hint) :boolean
 #+liber-documentation
 "@version{#2023-2-26}
  @syntax[]{(gdk:window-modal-hint window) => modal}
  @syntax[]{(setf (gdk:window-modal-hint window) modal)}
  @argument[window]{a toplevel @class{gdk:window} object}
  @argument[modal]{@em{true} if the window is modal, @em{false} otherwise}
  @begin{short}
    Whether or not the window has the modal hint set.
  @end{short}
  The @sym{gdk:window-modal-hint} function determines whether or not the window
  manager is hinted that window has modal behaviour.

  The application can use this hint to tell the window manager that a certain
  window has modal behaviour. The window manager can use this information to
  handle modal windows in a special way.

  You should only use this on windows for which you have previously called
  the @fun{gdk:window-set-transient-for} function.
  @see-class{gdk:window}
  @see-function{gdk:window-set-transient-for}"
  (window (g:object window)))

(export 'window-modal-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_type_hint ()
;;; gdk_window_set_type_hint () -> window-type-hint
;;; ----------------------------------------------------------------------------

(defun (setf window-type-hint) (hint window)
  (cffi:foreign-funcall "gdk_window_set_type-hint"
                        (g:object window) window
                        window-type-hint hint
                        :void)
  hint)

(defcfun ("gdk_window_get_type_hint" window-type-hint) window-type-hint
 #+liber-documentation
 "@version{#2023-2-26}
  @syntax[]{(gdk:window-type-hint window) => hint}
  @syntax[]{(setf (gdk:window-type-hint window) hint)}
  @argument[window]{a @class{gdk:window} toplevel object}
  @argument[hint]{a @symbol{gdk:window-type-hint} value with the type hint this
    window will have}
  @begin{short}
    The type hint set for the window.
  @end{short}
  The @sym{gdk:window-type-hint} function returns the type hint set for a
  the window. The @sym{(setf gdk:window-type-hint)} function sets the type hint.

  The application can use this call to provide a hint to the window manager
  about the functionality of a window. The window manager can use this
  information when determining the decoration and behaviour of the window.

  The hint must be set before the window is mapped.
  @see-class{gdk:window}
  @see-symbol{gdk:window-type-hint}"
  (window (g:object window)))

(export 'window-type-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_shadow_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_shadow_width" window-set-shadow-width) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[left]{an integer with the left extent}
  @argument[right]{an integer with the right extent}
  @argument[top]{an integer with the top extent}
  @argument[bottom]{an integer with the bottom extent}
  @begin{short}
    Newer GTK windows using client-side decorations use extra geometry around
    their frames for effects like shadows and invisible borders.
  @end{short}
  Window managers that want to maximize windows or snap to edges need to know
  where the extents of the actual frame lie, so that users do not feel like
  windows are snapping against random invisible edges.

  Note that this property is automatically updated by GTK, so this function
  should only be used by applications which do not use GTK to create toplevel
  windows.
  @see-class{gdk:window}"
  (window (g:object window))
  (left :int)
  (right :int)
  (top :int)
  (bottom :int))

(export 'window-set-shadow-width)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_skip_taskbar_hint ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_skip_taskbar_hint" window-set-skip-taskbar-hint) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @argument[skips-taskbar]{@em{true} to skip the taskbar}
  @begin{short}
    Toggles whether a window should appear in a task list or window list.
  @end{short}
  If a window's semantic type as specified with the @fun{gdk:window-type-hint}
  function already fully describes the window, this function should not be
  called in addition, instead you should allow the window to be treated
  according to standard policy for its semantic type.
  @see-class{gdk:window}
  @see-function{gdk:window-type-hint}"
  (window (g:object window))
  (skips-taskbar :boolean))

(export 'window-set-skip-taskbar-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_skip_pager_hint ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_skip_pager_hint" window-set-skip-pager-hint) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @argument[skips-pager]{@em{true} to skip the pager}
  @begin{short}
    Toggles whether a window should appear in a pager, workspace switcher, or
    other desktop utility program that displays a small thumbnail representation
    of the windows on the desktop.
  @end{short}
  If a window's semantic type as specified with the @fun{gdk:window-type-hint}
  function already fully describes the window, this function should not be
  called in addition, instead you should allow the window to be treated
  according to standard policy for its semantic type.
  @see-class{gdk:window}
  @see-function{gdk:window-type-hint}"
  (window (g:object window))
  (skips-pager :boolean))

(export 'window-set-skip-pager-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_urgency_hint ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_urgency_hint" window-set-urgency-hint) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @argument[urgent]{@em{true} if the window is urgent}
  @begin{short}
    Toggles whether a window needs the user's urgent attention.
  @end{short}
  @see-class{gdk:window}"
  (window (g:object window))
  (urgent :boolean))

(export 'window-set-urgency-hint)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_position () -> window-position
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_position" %window-position) :void
  (window (g:object window))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun window-position (window)
 #+liber-documentation
 "@version{2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @begin{return}
    @code{x} -- an integer with the x coordinate of @arg{window} @br{}
    @code{y} -- an integer with the y coordinate of @arg{window}
  @end{return}
  @begin{short}
    Obtains the position of the window as reported in the
    most-recently-processed @class{gdk:event-configure} event.
  @end{short}
  Contrast with the @fun{gdk:window-geometry} function which queries the X
  server for the current window position, regardless of which events have been
  received or processed. The position coordinates are relative to the window's
  parent window.
  @see-class{gdk:window}
  @see-class{gdk:event-configure}
  @see-function{gdk:window-width}
  @see-function{gdk:window-height}
  @see-function{gdk:window-geometry}"
  (with-foreign-objects ((x :int) (y :int))
    (%window-position window x y)
    (values (cffi:mem-ref x :int)
            (cffi:mem-ref y :int))))

(export 'window-position)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_root_origin () -> window-root-origin
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_root_origin" %window-root-origin) :void
  (window (g:object window))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun window-root-origin (window)
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @begin{return}
    @code{x} -- an integer with the x position of window frame @br{}
    @code{y} -- an integer with the y position of window frame
  @end{return}
  @begin{short}
    Obtains the top-left corner of the window manager frame in root window
    coordinates.
  @end{short}
  @see-class{gdk:window}"
  (with-foreign-objects ((x :int) (y :int))
    (%window-root-origin window x y)
    (values (cffi:mem-ref x :int)
            (cffi:mem-ref y :int))))

(export 'window-root-origin)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_frame_extents () -> window-frame-extents
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_frame_extents" %window-frame-extents) :void
  (window (g:object window))
  (rectangle (g:boxed rectangle)))

(defun window-frame-extents (window)
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @begin{return}
    @code{rect} -- a @class{gdk:rectangle} instance with bounding box of the
    window frame
  @end{return}
  @begin{short}
    Obtains the bounding box of the window, including window manager
    titlebar/borders if any.
  @end{short}
  The frame position is given in root window coordinates. To get the position
  of the window itself, rather than the frame, in root window coordinates, use
  the @fun{gdk:window-origin} function.
  @see-class{gdk:window}
  @see-class{gdk:rectangle}
  @see-function{gdk:window-origin}"
  (let ((rectangle (rectangle-new)))
    (%window-frame-extents window rectangle)
    rectangle))

(export 'window-frame-extents)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_origin () -> window-origin
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_origin" %window-origin) :int
  (window (g:object window))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun window-origin (window)
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @begin{return}
    @code{x} -- an integer with the x coordinate @br{}
    @code{y} -- an integer with the y coordinate
  @end{return}
  @begin{short}
    Obtains the position of a window in root window coordinates.
  @end{short}
  Compare with the @fun{gdk:window-position} and @fun{gdk:window-geometry}
  functions which return the position of a window relative to its parent window.
  @see-class{gdk:window}
  @see-function{gdk:window-position}
  @see-function{gdk:window-geometry}"
  (with-foreign-objects ((x :int) (y :int))
    (%window-origin window x y)
    (values (cffi:mem-ref x :int)
            (cffi:mem-ref y :int))))

(export 'window-origin)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_root_coords () -> window-root-coords
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_root_coords" %window-root-coords) :void
  (window (g:object window))
  (x :int)
  (y :int)
  (root-x :int)
  (root-y :int))

(defun window-root-coords (window x y)
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[x]{an integer with the x coordinate in window}
  @argument[y]{an integer with the y coordinate in window}
  @begin{return}
    @code{root-x} -- an integer with the x root coordinate @br{}
    @code{root-y} -- an integer with the y root coordinate
  @end{return}
  @begin{short}
    Obtains the position of a window position in root window coordinates.
  @end{short}
  This is similar to the @fun{gdk:window-origin} function but allows you to
  pass in any position in the window, not just the origin.
  @see-class{gdk:window}
  @see-function{gdk:window-origin}"
  (with-foreign-objects ((root-x :int) (root-y :int))
    (%window-root-coords window x y root-x root-y)
    (values (cffi:mem-ref root-x :int)
            (cffi:mem-ref root-y :int))))

(export 'window-root-coords)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_pointer () -> window-pointer        not exported
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_pointer" %window-pointer) (g:object window)
  (window (g:object window))
  (x (:pointer :int))
  (y (:pointer :int))
  (mask (:pointer modifier-type)))

(defun window-pointer (window)
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @begin{return}
    @code{win} -- a @class{gdk:window} object containing the pointer as with
    the @fun{gdk:window-at-pointer} function, or @code{nil} if the window
    containing the pointer is not known to GDK @br{}
    @code{x} -- an integer with the x coordinate of pointer @br{}
    @code{y} -- an integer with the y coordinate of pointer @br{}
    @code{mask} -- a @symbol{gdk:modifier-type} value with the modifier mask
  @end{return}
  @begin{short}
    Obtains the current pointer position and modifier state. The position is
    given in coordinates relative to the upper left corner of window.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{gdk:window-pointer} function has been deprecated since version
    3.0 and should not be used in newly written code. Use the
    @fun{gdk:window-device-position} function instead.
  @end{dictionary}
  @see-class{gdk:window}
  @see-symbol{gdk:modifier-type}
  @see-function{gdk:window-at-pointer}
  @see-function{gdk:window-device-position}"
  (with-foreign-objects ((x :int) (y :int) (mask 'modifier-type))
    (let ((window (%window-pointer window x y mask)))
      (values window
              (cffi:mem-ref x :int)
              (cffi:mem-ref y :int)
              (cffi:mem-ref mask 'modifier-type)))))

(export 'window-pointer)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_device_position () -> window-device-position
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_device_position" %window-device-position)
    (g:object window)
  (window (g:object window))
  (device (g:object device))
  (x (:pointer :int))
  (y (:pointer :int))
  (mask (:pointer modifier-type)))

(defun window-device-position (window device)
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[device]{a @class{gdk:device} object to query}
  @begin{return}
    @code{win}  -- a @class{gdk:window} object underneath @arg{device}, as
    with the @fun{gdk:device-window-at-position} function, or @code{nil} if the
    window is not known to GDK @br{}
    @code{x} -- an integer with the x coordinate of the device @br{}
    @code{y} -- an integer with the y coordinate of the device @br{}
    @code{mask} -- a @symbol{gdk:modifier-type} value
  @end{return}
  @begin{short}
    Obtains the current device position and modifier state.
  @end{short}
  The position is given in coordinates relative to the upper left corner of
  window. Use the @fun{gdk:window-device-position-double} function if you need
  subpixel precision.
  @see-class{gdk:window}
  @see-class{gdk:device}
  @see-symbol{gdk:modifier-type}
  @see-function{gdk:device-window-at-position}
  @see-function{gdk:window-device-position-double}"
  (with-foreign-objects ((x :int) (y :int) (mask 'modifier-type))
    (let ((window (%window-device-position window device x y mask)))
      (values window
              (cffi:mem-ref x :int)
              (cffi:mem-ref y :int)
              (cffi:mem-ref mask 'modifier-type)))))

(export 'window-device-position)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_device_position_double ()
;;; -> window-device-position-double
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_device_position_double"
          %window-device-position-double) (g:object window)
  (window (g:object window))
  (device (g:object device))
  (x (:pointer :double))
  (y (:pointer :double))
  (mask (:pointer modifier-type)))

(defun window-device-position-double (window device)
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[device]{a @class{gdk:device} object to query to}
  @begin{return}
    @code{win} -- a @class{gdk:window} object underneath device, as with the
    @fun{gdk:device-window-at-position} function, or @code{nil} if the window
    is not known to GDK @br{}
    @code{x} -- a double float with the x coordinate of the device @br{}
    @code{y} -- a double float with the y coordinate of the device  @br{}
    @code{mask} -- a @symbol{gdk:modifier-type} value
  @end{return}
  @begin{short}
    Obtains the current device position in doubles and the modifier state.
  @end{short}
  The position is given in coordinates relative to the upper left corner of
  the window.
  @see-class{gdk:window}
  @see-class{gdk:device}
  @see-symbol{gdk:modifier-type}
  @see-function{gdk:device-window-at-position}"
  (with-foreign-objects ((x :double) (y :double) (mask 'modifier-type))
    (let ((window (%window-device-position-double window device x y mask)))
      (values window
              (cffi:mem-ref x :double)
              (cffi:mem-ref y :double)
              (cffi:mem-ref mask 'modifier-type)))))

(export 'window-device-position-double)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_parent () -> window-parent
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_parent" window-parent) (g:object window)
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @return{The parent @class{gdk:window} object of @arg{window}}
  @begin{short}
    Obtains the parent of window, as known to GDK.
  @end{short}
  Does not query the X server. Thus this returns the parent as passed to the
  @fun{gdk:window-new} function, not the actual parent. This should never
  matter unless you are using Xlib calls mixed with GDK calls on the X11
  platform. It may also matter for toplevel windows, because the window manager
  may choose to reparent them.

  Note that you should use the @fun{gdk:window-effective-parent} function when
  writing generic code that walks up a window hierarchy, because the
  @sym{gdk:window-parent} function will most likely not do what you expect if
  there are offscreen windows in the hierarchy.
  @see-class{gdk:window}
  @see-function{gdk:window-new}
  @see-function{gdk:window-children}
  @see-function{gdk:window-effective-parent}"
  (window (g:object window)))

(export 'window-parent)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_toplevel () -> window-toplevel
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_toplevel" window-toplevel) (g:object window)
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @return{The toplevel @class{gdk:window} window containing @arg{window}.}
  @begin{short}
    Gets the toplevel window that is an ancestor of the window.
  @end{short}
  Any window type but @code{:child} is considered a toplevel window, as is
  a @code{:child} window that has a root window as parent.

  Note that you should use the @fun{gdk:window-effective-toplevel} function
  when you want to get to a window's toplevel as seen on screen, because
  the @sym{gdk:window-toplevel} function will most likely not do what you
  expect if there are offscreen windows in the hierarchy.
  @see-class{gdk:window}
  @see-function{gdk:window-effective-toplevel}"
  (window (g:object window)))

(export 'window-toplevel)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_children () -> window-children
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_children" window-children)
    (g:list-t (g:object window))
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @return{List of @class{gdk:window} child windows inside @arg{window}.}
  @begin{short}
    Gets the list of children of the window known to GDK.
  @end{short}
  This function only returns children created via GDK, so for example it is
  useless when used with the root window. It only returns windows an application
  created itself.
  @see-class{gdk:window}
  @see-function{gdk:window-parent}"
  (window (g:object window)))

(export 'window-children)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_children_with_user_data ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_children_with_user_data"
           window-children-with-user-data) (g:list-t (g:object window))
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[data]{user data to look for as a pointer}
  @return{list of @class{gdk:window} child windows inside @arg{window}}
  @begin{short}
    Gets the list of children of the window known to GDK with a particular
    @arg{data} set on it.
  @end{short}
  The returned list must be freed, but the elements in the list need not be.

  The list is returned in (relative) stacking order, i.e. the lowest window
  is first.
  @see-class{gdk:window}"
  (window (g:object window))
  (data :pointer))

(export 'window-children-with-user-data)

;;; ----------------------------------------------------------------------------
;;; gdk_window_peek_children ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_peek_children" window-peek-children)
    (g:list-t (g:object window) :free-from-foreign nil)
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @return{A list of @class{gdk:window} child windows in @arg{window}.}
  @begin{short}
    Like the @fun{gdk:window-children} function, but does not copy the list
    of children, so the list does not need to be freed.
  @end{short}
  @see-class{gdk:window}
  @see-function{gdk:window-children}"
  (window (g:object window)))

(export 'window-peek-children)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_events ()
;;; gdk_window_set_events () -> window-events
;;; ----------------------------------------------------------------------------

(defun (setf window-events) (event-mask window)
  (cffi:foreign-funcall "gdk_window_set_events"
                        (g:object window) window
                        event-mask event-mask
                        :void)
  event-mask)

(defcfun ("gdk_window_get_events" window-events) event-mask
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[event-mask]{a @symbol{gdk:event-mask} event mask for @arg{window}}
  @begin{short}
    Accessor of the event mask for the window.
  @end{short}
  The @sym{gdk:window-events} function gets the event mask for the window for
  all master input devices. The @sym{(setf gdk:window-events)} function sets
  the event mask.

  The event mask for a window determines which events will be reported for that
  window from all master input devices. For example, an event mask including
  @code{:button-press-mask} means the window should report button press events.
  The event mask is the bitwise OR of values from the @symbol{gdk:event-mask}
  flags.
  @see-class{gdk:window}
  @see-symbol{gdk:event-mask}"
  (window (g:object window)))

(export 'window-events)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_icon_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_icon_name" window-set-icon-name) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @argument[name]{a string with the name of @arg{window} while iconified
    (minimized)}
  @begin{short}
    Windows may have a name used while minimized, distinct from the name they
    display in their titlebar.
  @end{short}
  Most of the time this is a bad idea from a user interface standpoint. But you
  can set such a name with this function, if you like.

  After calling this with a non-@code{nil} name, calls to the
  @fun{gdk:window-set-title} function will not update the icon title.

  Using @code{nil} for name unsets the icon title. Further calls to the
  @fun{gdk:window-set-title} function will again update the icon title as well.
  @see-class{gdk:window}
  @see-function{gdk:window-set-title}"
  (window (g:object window))
  (name :string))

(export 'window-set-icon-name)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_transient_for ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_transient_for" window-set-transient-for) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @argument[parent]{another toplevel @class{gdk:window} object}
  @begin{short}
    Indicates to the window manager that @arg{window} is a transient dialog
    associated with the application window parent.
  @end{short}
  This allows the window manager to do things like center @arg{window} on
  @arg{parent} and keep @arg{window} above @arg{parent}.

  See the @fun{gtk:window-transient-for} function if you are using
  @class{gtk:window} or @class{gtk:dialog} widgets.
  @see-class{gdk:window}
  @see-class{gtk:window}
  @see-class{gtk:dialog}
  @see-function{gtk:window-transient-for}"
  (window (g:object window))
  (parent (g:object window)))

(export 'window-set-transient-for)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_role ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_role" window-set-role) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @argument[role]{a string indicating its role}
  @begin{short}
    When using GTK, typically you should use the @fun{gtk:window-role} function
    instead of this low-level function.
  @end{short}

  The window manager and session manager use a window's role to distinguish it
  from other kinds of window in the same application. When an application is
  restarted after being saved in a previous session, all windows with the same
  title and role are treated as interchangeable. So if you have two windows
  with the same title that should be distinguished for session management
  purposes, you should set the role on those windows. It does not matter what
  string you use for the role, as long as you have a different role for each
  non-interchangeable kind of window.
  @see-class{gdk:window}
  @see-function{gtk:window-role}"
  (window (g:object window))
  (role :string))

(export 'window-set-role)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_startup_id ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_startup_id" window-set-startup-id) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @argument[startup-id]{a string with startup-notification identifier}
  @begin{short}
    When using GTK, typically you should use the @fun{gtk:window-startup-id}
    function instead of this low-level function.
  @end{short}
  @see-class{gdk:window}
  @see-function{gtk:window-startup-id}"
  (window (g:object window))
  (startup-id :string))

(export 'window-set-startup-id)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_group ()
;;; gdk_window_set_group () -> window-group
;;; ----------------------------------------------------------------------------

(defun (setf window-group) (leader window)
  (cffi:foreign-funcall "gdk_window_set_group"
                        (g:object window) window
                        (g:object window) leader
                        :void)
  leader)

(defcfun ("gdk_window_get_group" window-group) (g:object window)
 #+liber-documentation
 "@version{#2023-2-26}
  @syntax[]{(gdk:window-group window) => leader}
  @syntax[]{(setf (gdk:window-group window) leader)}
  @argument[window]{a toplevel @class{gdk:window} object}
  @argument[leader]{a group leader @class{gdk:window} object, or @code{nil} to
    restore the default group leader window}
  @begin{short}
    The group leader window for @arg{window}.
  @end{short}
  The @sym{gdk:window-group} function returns the group leader window for
  @arg{window}. The @sym{(setf gdk:window-group)} function sets the group leader
  window for @arg{window}.

  By default, GDK sets the group leader for all toplevel windows to a global
  window implicitly created by GDK. With this function you can override this
  default.

  The group leader window allows the window manager to distinguish all windows
  that belong to a single application. It may for example allow users to
  minimize/unminimize all windows belonging to an application at once. You
  should only set a non-default group window if your application pretends to
  be multiple applications.
  @see-class{gdk:window}"
  (window (g:object window)))

(export 'window-group)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_decorations ()
;;; gdk_window_set_decorations () -> window-decorations
;;; ----------------------------------------------------------------------------

(defun (setf window-decorations) (decorations window)
  (cffi:foreign-funcall "gdk_window_set_decorations"
                        (g:object window) window
                        wm-decoration decorations
                        :void)
  decorations)

(defcfun ("gdk_window_get_decorations" %window-decorations) :boolean
  (window (g:object window))
  (decorations (:pointer wm-decoration)))

(defun window-decorations (window)
 #+liber-documentation
 "@version{#2023-2-26}
  @syntax[]{(gdk:window-decorations window) => decorations}
  @syntax[]{(setf (gdk:window-decorations window) decorations)}
  @argument[window]{a toplevel @class{gdk:window} object}
  @argument[decorations]{a @symbol{gdk:wm-decoration} value with the decoration
    hint mask}
  @begin{short}
    \"Decorations\" are the features the window manager adds to a toplevel
    @class{gdk:window} object.
  @end{short}
  This function sets the traditional Motif window manager hints that tell the
  window manager which decorations you would like your window to have. Usually
  you should use the @fun{gtk:window-decorated} function on a @class{gtk:window}
  widget instead of using the GDK function directly.

  The decorations argument is the logical OR of the values of the
  @symbol{gdk:wm-decoration} flags. If @code{:all} is included in the mask, the
  other bits indicate which decorations should be turned off. If @code{:all}
  is not included, then the other bits indicate which decorations should be
  turned on.

  Most window managers honor a decorations hint of 0 to disable all
  decorations, but very few honor all possible combinations of bits.
  @see-class{gdk:window}
  @see-symbol{gdk:wm-decoration}
  @see-class{gtk:window}
  @see-function{gtk:window-decorated}"
  (with-foreign-object (decorations 'wm-decoration)
    (when (%window-decorations window decorations)
      (cffi:mem-ref decorations 'wm-decoration))))

(export 'window-decorations)

;;; ----------------------------------------------------------------------------
;;; gdk_window_set_functions ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_set_functions" window-set-functions) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a toplevel @class{gdk:window} object}
  @argument[functions]{a @symbol{gdk:wm-function} bitmask of operations to
    allow on @arg{window}}
  @begin{short}
    Sets hints about the window management functions to make available via
    buttons on the window frame.
  @end{short}
  On the X backend, this function sets the traditional Motif window manager
  hint for this purpose. However, few window managers do anything reliable or
  interesting with this hint. Many ignore it entirely.

  The functions argument is the logical OR of values from the
  @symbol{gdk:wm-function} flags. If the bitmask includes @code{:all}, then the
  other bits indicate which functions to disable; if it does not include
  @code{:all}, it indicates which functions to enable.
  @see-class{gdk:window}
  @see-symbol{gdk:wm-function}"
  (window (g:object window))
  (functions wm-function))

(export 'window-set-functions)

;;; ----------------------------------------------------------------------------
;;; gdk_get_default_root_window () -> default-root-window
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_get_default_root_window" default-root-window) (g:object window)
 #+liber-documentation
 "@version{2023-2-26}
  @return{The @class{gdk:window} default root window.}
  @begin{short}
    Obtains the root window, parent all other windows are inside, for the
    default display and screen.
  @end{short}
  @see-class{gdk:window}")

(export 'default-root-window)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_support_multidevice ()
;;; gdk_window_set_support_multidevice () -> window-support-multidevice
;;; ----------------------------------------------------------------------------

(defun (setf window-support-multidevice) (support-multidevice window)
  (cffi:foreign-funcall "gdk_window_set_support_multidevice"
                        (g:object window) window
                        :boolean support-multidevice
                        :void)
  support-multidevice)

(defcfun ("gdk_window_get_support_multidevice" window-support-multidevice)
    :boolean
 #+liber-documentation
 "@version{#2023-2-26}
  @syntax[]{(gdk:window-support-multidevice window) => support-multidevice}
  @syntax[]{(setf (gdk:window-support-multidevice window) support-multidevice)}
  @argument[window]{a @class{gdk:window} object}
  @argument[support-multidevice]{@em{true} to enable multidevice support in
    @arg{window}}
  @begin{short}
    The @sym{gdk:window-support-multidevice} function returns @em{true} if the
    window is aware of the existence of multiple devices.
  @end{short}
  The @sym{(setf gdk:window-support-multidevice)} function will enable
  multidevice features in @arg{window}.

  Multidevice aware windows will need to handle properly multiple, per device
  enter/leave events, device grabs and grab ownerships.
  @see-class{gdk:window}"
  (window (g:object window)))

(export 'window-support-multidevice)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_device_cursor ()
;;; gdk_window_set_device_cursor () -> window-device-cursor
;;; ----------------------------------------------------------------------------

(defun (setf window-device-cursor) (cursor window device)
  (cffi:foreign-funcall "gdk_window_set_device_cursor"
                        (g:object window) window
                        (g:object device) device
                        (g:object cursor) cursor
                        :void)
  cursor)

(defcfun ("gdk_window_get_device_cursor" window-device-cursor) (g:object cursor)
 #+liber-documentation
 "@version{#2023-2-26}
  @syntax[]{(gdk:window-device-cursor window device) => cursor}
  @syntax[]{(setf (gdk:window-device-cursor window device) cursor)}
  @argument[window]{a @class{gdk:window} object}
  @argument[device]{a master @class{gdk:device} object}
  @argument[cursor]{a @class{gdk:cursor} object}
  @return{ A @class{gdk:cursor} object, or @code{nil}.}
  @begin{short}
    The @sym{gdk:window-device-cursor} function retrieves a @class{gdk:cursor}
    pointer for the device currently set on the specified @class{gdk:window}
    object, or @code{nil}.
  @end{short}
  If the return value is @code{nil} then there is no custom cursor set on the
  specified window, and it is using the cursor for its parent window.

  The @sym{(setf gdk:window-device-cursor)} function sets a specific
  @class{gdk:cursor} object for a given device when it gets inside window.
  Use the @fun{gdk:cursor-new-for-display} or @fun{gdk:cursor-new-from-pixbuf}
  functions to create the cursor. To make the cursor invisible, use
  @code{:blank-cursor}. Passing @code{nil} for the cursor argument to the
  @fun{gdk:window-cursor} function means that the window will
  use the cursor of its parent window. Most windows should use this default.
  @see-class{gdk:window}
  @see-class{gdk:device}
  @see-class{gdk:cursor}
  @see-function{gdk:cursor-new-for-display}
  @see-function{gdk:cursor-new-from-pixbuf}"
  (window (g:object window))
  (device (g:object device)))

(export 'window-device-cursor)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_device_events ()
;;; gdk_window_set_device_events () -> window-device-events
;;; ----------------------------------------------------------------------------

(defun (setf window-device-events) (event-mask window device)
  (cffi:foreign-funcall "gdk_window_set_device_events"
                        (g:object window) window
                        (g:object device) device
                        event-mask event-mask
                        :void)
  event-mask)

(defcfun ("gdk_window_get_device_events" window-device-events) event-mask
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @argument[device]{a @class{gdk:device} object}
  @argument[event-mask]{a @symbol{gdk:event-mask} event mask for @arg{window}}
  @begin{short}
    The @sym{gdk:window-device-events} function returns the event mask for
    the window corresponding to an specific device.
  @end{short}
  The @sym{(setf gdk:window-device-events)} function sets the event mask for a
  given device.

  Normally a floating device, not attached to any visible pointer to the window.
  For example, an event mask including @code{:button-press-mask} means the
  window should report button press events. The event mask is the bitwise OR of
  values from the @symbol{gdk:event-mask} flags.
  @see-class{gdk:window}
  @see-class{gdk:device}
  @see-symbol{gdk:event-mask}"
  (window (g:object window))
  (device (g:object device)))

(export 'window-device-events)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_source_events ()
;;; gdk_window_set_source_events () -> window-source-events
;;; ----------------------------------------------------------------------------

(defun (setf window-source-events) (event-mask window source)
  (cffi:foreign-funcall "gdk_window_set_source_events"
                        (g:object window) window
                        input-source source
                        event-mask event-mask
                        :void)
  event-mask)

(defcfun ("gdk_window_get_source_events" window-source-events) event-mask
 #+liber-documentation
 "@version{#2023-2-26}
  @syntax[]{(gdk:window-source-events window source) => event-mask}
  @syntax[]{(setf (gdk:window-source-events window source) event-mask)}
  @argument[window]{a @class{gdk:window} object}
  @argument[source]{a @symbol{gdk:input-source} value to define the source
    class}
  @argument[event-mask]{a @symbol{gdk:event-mask} event mask for @arg{window}}
  @begin{short}
    The @sym{gdk:window-source-events} function returns the event mask for
    the window corresponding to the device class specified by @arg{source}.
  @end{short}
  The @sym{(setf gdk:window-source-events)} function sets the event mask for
  any floating device, i.e. not attached to any visible pointer, that has
  @arg{source} defined as source.

  This event mask will be applied both to currently existing, newly added
  devices after this call, and devices being attached/detached.
  @see-class{gdk:window}
  @see-symbol{gdk:input-source}
  @see-symbol{gdk:event-mask}"
  (window (g:object window))
  (source input-source))

(export 'window-source-events)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_event_compression ()
;;; gdk_window_set_event_compression () -> window-event-compression
;;; ----------------------------------------------------------------------------

(defun (setf window-event-compression) (event-compression window)
  (cffi:foreign-funcall "gdk_window_set_event_compression"
                        (g:object window) window
                        :boolean event-compression
                        :void)
  event-compression)

(defcfun ("gdk_window_get_event_compression" window-event-compression) :boolean
 #+liber-documentation
 "@version{#2023-2-26}
  @syntax[]{(gdk:window-event-compression window) => event-compression}
  @syntax[]{(setf (gdk:window-event-compression window) event-compression)}
  @argument[window]{a @class{gdk:window} object}
  @argument[event-compression]{a @em{true} if motion events will be compressed}
  @begin{short}
    Determines whether or not extra unprocessed motion events in the event
    queue can be discarded.
  @end{short}
  If @em{true} only the most recent event will be delivered.

  Some types of applications, e.g. paint programs, need to see all motion
  events and will benefit from turning off event compression. By default, event
  compression is enabled.
  @see-class{gdk:window}"
  (window (g:object window)))

(export 'window-event-compression)

;;; ----------------------------------------------------------------------------
;;; gdk_offscreen_window_get_surface () -> offscreen-window-surface
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_offscreen_window_get_surface" offscreen-window-surface)
    (:pointer (:struct cairo:surface-t))
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @return{The offscreen @symbol{cairo:surface-t} instance, or @code{nil} if
    not offscreen.}
  @begin{short}
    Gets the offscreen surface that an offscreen window renders into.
  @end{short}
  If you need to keep this around over window resizes, you need to add a
  reference to it.
  @see-class{gdk:window}
  @see-symbol{cairo:surface-t}"
  (window (g:object window)))

(export 'offscreen-window-surface)

;;; ----------------------------------------------------------------------------
;;; gdk_offscreen_window_get_embedder ()
;;; gdk_offscreen_window_set_embedder () -> offscreen-window-embedder
;;; ----------------------------------------------------------------------------

(defun (setf offscreen-window-embedder) (embedder window)
  (cffi:foreign-funcall "gdk_offscreen_window_set_embedder"
                        (g:object window) window
                        (g:object window) embedder
                        :void)
  embedder)

(defcfun ("gdk_offscreen_window_get_embedder" offscreen-window-embedder)
    (g:object window)
 #+liber-documentation
 "@version{#2023-2-26}
  @syntax[]{gdk:offscreen-window-embedder window) => embedder}
  @syntax[]{(setf gdk:offscreen-window-embedded window) embedder)}
  @argument[window]{a @class{gdk:window} object}
  @argument[embedder]{a @class{gdk:window} object that @arg{window} gets
    embedded in}
  @begin{short}
    The embedding @class{gdk:window} object, or @code{nil} if @arg{window} is
    not an embedded offscreen window.
  @end{short}
  The @sym{gdk:offscreen-window-embedder} function gets the window that
  @arg{window} is embedded in. The @sym{(setf gdk:offscreen-window-embedder)}
  function sets @arg{window} to be embedded in @arg{embedder}.

  To fully embed an offscreen window, in addition to calling this function, it
  is also necessary to handle the \"pick-embedded-child\" signal on
  @arg{embedder} and the \"to-embedder\" and \"from-embedder\" signals on
  @arg{window}.
  @see-class{gdk:window}"
  (window (g:object window)))

(export 'offscreen-window-embedder)

;;; ----------------------------------------------------------------------------
;;; gdk_window_geometry_changed ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_geometry_changed" window-geometry-changed) :void
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{an embedded offscreen @class{gdk:window} object}
  @begin{short}
    This function informs GDK that the geometry of an embedded offscreen window
    has changed.
  @end{short}
  This is necessary for GDK to keep track of which offscreen window the pointer
  is in.
  @see-class{gdk:window}"
  (window (g:object window)))

(export 'window-geometry-changed)

;;; ----------------------------------------------------------------------------
;;; gdk_window_coords_from_parent ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_coords_from_parent" %window-coords-from-parent) :void
  (window (g:object window))
  (xparent :double)
  (yparent :double)
  (x (:pointer :double))
  (y (:pointer :double)))

(defun window-coords-from-parent (window xparent yparent)
 #+liber-documentation
 "@version{2023-2-26}
  @argument[window]{a @class{gdk:window} child window}
  @argument[xparent]{a number coerced to a double float with the x coordinate
    in parent's coordinate system}
  @argument[yparent]{a number coerced to a double float with the y coordinate
    in parent's coordinate system}
  @begin{return}
    @code{x} -- a double float with the x coordinate in child's coordinate
                system @br{}
    @code{y} -- a double float with the y coordinate in child's coordinate
                system
  @end{return}
  @begin{short}
    Transforms window coordinates from a parent window to a child window, where
    the parent window is the normal parent as returned by the
    @fun{gdk:window-parent} function for normal windows, and the window's
    embedder as returned by the @fun{gdk:offscreen-window-embedder} function
    for offscreen windows.
  @end{short}

  For normal windows, calling this function is equivalent to subtracting the
  return values of the @fun{gdk:window-position} function from the parent
  coordinates. For offscreen windows however, which can be arbitrarily
  transformed, this function calls the \"from-embedder\" signal to translate
  the coordinates.

  You should always use this function when writing generic code that walks
  down a window hierarchy.

  See also the @fun{gdk:window-coords-to-parent} function.
  @see-function{gdk:window}
  @see-function{gdk:window-parent}
  @see-function{gdk:window-position}
  @see-function{gdk:offscreen-window-embedder}
  @see-function{gdk:window-coords-to-parent}"
  (with-foreign-objects ((x :double) (y :double))
    (%window-coords-from-parent window
                                (coerce xparent 'double-float)
                                (coerce yparent 'double-float)
                                x
                                y)
    (values (cffi:mem-ref x :double)
            (cffi:mem-ref y :double))))

(export 'window-coords-from-parent)

;;; ----------------------------------------------------------------------------
;;; gdk_window_coords_to_parent ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_coords_to_parent" %window-coords-to-parent) :void
  (window (g:object window))
  (x :double)
  (y :double)
  (xparent (:pointer :double))
  (yparent (:pointer :double)))

(defun window-coords-to-parent (window x y)
 #+liber-documentation
 "@version{2023-2-26}
  @argument[window]{a @class{gdk:window} child window}
  @argument[x]{a number coerced to a double float with the x coordinate in
    child's coordinate system}
  @argument[y]{a number coerced to a double float with the y coordinate in
    child's coordinate system}
  @begin{return}
    @code{xparent} -- a double float with the x coordinate in parent's
                       coordinate system, or @code{nil} @br{}
    @code{yparent} -- a double float with the y coordinate in parent's
                       coordinate system, or @code{nil}
  @end{return}
  @begin{short}
    Transforms window coordinates from a child window to its parent window,
    where the parent window is the normal parent as returned by the
    @fun{gdk:window-parent} function for normal windows, and the window's
    embedder as returned by the @fun{gdk:offscreen-window-embedder} function
    for offscreen windows.
  @end{short}

  For normal windows, calling this function is equivalent to adding the return
  values of the @fun{gdk:window-position} function to the child coordinates.
  For offscreen windows however, which can be arbitrarily transformed, this
  function calls the \"to-embedder\" signal to translate the coordinates.

  You should always use this function when writing generic code that walks up
  a window hierarchy.

  See also the @fun{gdk:window-coords-from-parent} function.
  @see-class{gdk:window}
  @see-function{gdk:window-parent}
  @see-function{gdk:window-position}
  @see-function{gdk:offscreen-window-embedder}
  @see-function{gdk:window-coords-from-parent}"
  (with-foreign-objects ((xparent :double) (yparent :double))
    (%window-coords-to-parent window
                              (coerce x 'double-float)
                              (coerce y 'double-float)
                              xparent
                              yparent)
    (values (cffi:mem-ref xparent :double)
            (cffi:mem-ref yparent :double))))

(export 'window-coords-to-parent)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_effective_parent () -> window-effective-parent
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_effective_parent" window-effective-parent)
    (g:object window)
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @return{Effective @class{gdk:window} parent of @arg{window}.}
  @begin{short}
    Obtains the parent of window, as known to GDK.
  @end{short}
  Works like the @fun{gdk:window-parent} function for normal windows, but
  returns the window's embedder for offscreen windows.

  See also the @fun{gdk:offscreen-window-embedder} function.
  @see-class{gdk:window}
  @see-function{gdk:window-parent}
  @see-function{gdk:offscreen-window-embedder}"
  (window (g:object window)))

(export 'window-effective-parent)

;;; ----------------------------------------------------------------------------
;;; gdk_window_get_effective_toplevel () -> window-effective-toplevel
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_window_get_effective_toplevel" window-effective-toplevel)
    (g:object window)
 #+liber-documentation
 "@version{#2023-2-26}
  @argument[window]{a @class{gdk:window} object}
  @return{The effective @class{gdk:window} toplevel containing @arg{window}.}
  @begin{short}
    Gets the toplevel window that is an ancestor of @arg{window}.
  @end{short}
  Works like the @fun{gdk:window-toplevel} function, but treats an offscreen
  window's embedder as its parent, using the @fun{gdk:window-effective-parent}
  function.

  See also the @fun{gdk:offscreen-window-embedder} function.
  @see-class{gdk:window}
  @see-function{gdk:window-toplevel}
  @see-function{gdk:window-effective-parent}
  @see-function{gdk:offscreen-window-embedder}"
  (window (g:object window)))

(export 'window-effective-toplevel)

;;; --- End of file gdk3.window.lisp -------------------------------------------
