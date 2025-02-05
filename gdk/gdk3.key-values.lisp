;;; ----------------------------------------------------------------------------
;;; gdk3.key-values.lisp
;;;
;;; The documentation in this file is taken from the GDK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GDK library,
;;; see <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------
;;;
;;; Key Values
;;;
;;;     Functions for manipulating keyboard codes
;;;
;;; Types and Values
;;;
;;;     GdkKeymap
;;;     GdkKeymapKey
;;;
;;; Functions
;;;
;;;     gdk_keymap_get_default
;;;     gdk_keymap_get_for_display
;;;     gdk_keymap_lookup_key
;;;     gdk_keymap_translate_keyboard_state
;;;     gdk_keymap_get_entries_for_keyval
;;;     gdk_keymap_get_entries_for_keycode
;;;     gdk_keymap_get_direction
;;;     gdk_keymap_have_bidi_layouts
;;;     gdk_keymap_get_caps_lock_state
;;;     gdk_keymap_get_num_lock_state
;;;     gdk_keymap_get_scroll_lock_state
;;;     gdk_keymap_get_modifier_state
;;;     gdk_keymap_add_virtual_modifiers
;;;     gdk_keymap_map_virtual_modifiers
;;;     gdk_keymap_get_modifier_mask
;;;
;;;     gdk_keyval_name
;;;     gdk_keyval_from_name
;;;     gdk_keyval_convert_case
;;;     gdk_keyval_to_upper
;;;     gdk_keyval_to_lower
;;;     gdk_keyval_is_upper
;;;     gdk_keyval_is_lower
;;;     gdk_keyval_to_unicode
;;;     gdk_unicode_to_keyval
;;;
;;; Signals
;;;
;;;     direction-changed
;;;     keys-changed
;;;     state-changed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkKeymap
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkKeymapKey                                            not exported
;;; ----------------------------------------------------------------------------

(cffi:defcstruct %keymap-key
  (keycode :uint)
  (group :int)
  (level :int))

;;; ----------------------------------------------------------------------------
;;; GdkKeymap
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkKeymap" keymap
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_keymap_get_type")
  nil)

#+liber-documentation
(setf (documentation 'keymap 'type)
 "@version{2023-3-4}
  @begin{short}
    A @class{gdk:keymap} object defines the translation from keyboard state,
    including a hardware key, a modifier mask, and active keyboard group, to a
    keyval.
  @end{short}
  This translation has two phases. The first phase is to determine the effective
  keyboard group and level for the keyboard state. The second phase is to look
  up the keycode/group/level triplet in the keymap and see what keyval it
  corresponds to.

  Key values are the codes which are sent whenever a key is pressed or released.
  They appear in the @code{keyval} field of the @class{gdk:event-key}
  structure, which is passed to signal handlers for the
  @code{\"key-press-event\"} and @code{\"key-release-event\"} signals. The
  complete list of key values can be found in the @file{gdk/gdkkeysyms.h} header
  file.

  Key values are regularly updated from the upstream X.org X11 implementation,
  so new values are added regularly. They will be prefixed with GDK_KEY_
  rather than XF86XK_ or XK_ (for older symbols).

  Key values can be converted into a string representation using the
  @fun{gdk:keyval-name} function. The reverse function, converting a string to
  a key value, is provided by the @fun{gdk:keyval-from-name} function.

  The case of key values can be determined using the @fun{gdk:keyval-is-upper}
  and @fun{gdk:keyval-is-lower} functions. Key values can be converted to upper
  or lower case using the @fun{gdk:keyval-to-upper} and
  @fun{gdk:keyval-to-lower} functions.

  When it makes sense, key values can be converted to and from Unicode
  characters with the @fun{gdk:keyval-to-unicode} and
  @fun{gdk:unicode-to-keyval} functions.

  @subheading{Groups}
  One @class{gdk:keymap} object exists for each user display. To obtain the keymap
  for the display, use the @fun{gdk:keymap-for-display} function. A keymap is a
  mapping from a keycode, group, and level to key values. You can think of these
  values as a representation of a symbol printed on a physical keyboard key.
  That is, it contains three pieces of information. First, it contains the
  hardware keycode. This is an identifying number for a physical key. Second, it
  contains the \"level\" of the key. The level indicates which symbol on the key
  will be used, in a vertical direction. So on a standard US keyboard, the key
  with the number \"1\" on it also has the exclamation point \"!\" character on
  it. The level indicates whether to use the \"1\" or the \"!\" symbol. The
  letter keys are considered to have a lowercase letter at level 0, and an
  uppercase letter at level 1, though only the uppercase letter is printed.
  Third, there is the group. Groups are not used on standard US keyboards, but
  are used in many other countries. On a keyboard with groups, there can be 3
  or 4 symbols printed on a single key. The group indicates movement in a
  horizontal direction. Usually groups are used for two different languages. In
  group 0, a key might have two English characters, and in group 1 it might have
  two Hebrew characters. The Hebrew characters will be printed on the key next
  to the English characters.

  In order to use a keymap to interpret a key event, it is necessary to first
  convert the keyboard state into an effective group and level. This is done
  via a set of rules that varies widely according to type of keyboard and user
  configuration. The @fun{gdk:keymap-translate-keyboard-state} function accepts
  a keyboard state - consisting of hardware keycode pressed, active modifiers,
  and active group - applies the appropriate rules, and returns the group/level
  to be used to index the keymap, along with the modifiers which did not affect
  the group and level, i.e. it returns \"unconsumed modifiers\". The keyboard
  group may differ from the effective group used for keymap lookups because
  some keys do not have multiple groups - e.g. the @kbd{Enter} key is always in
  group 0 regardless of keyboard state.

  Note that the @fun{gdk:keymap-translate-keyboard-state} function also returns
  the keyval, i.e. it goes ahead and performs the keymap lookup in addition to
  telling you which effective group/level values were used for the lookup.
  The @class{gdk:event-key} event already contains this keyval, however, so you
  do not normally need to call the @fun{gdk:keymap-translate-keyboard-state}
  function just to get the keyval.
  @begin[Signal Details]{dictionary}
    @subheading{The \"direction-changed\" signal}
      @begin{pre}
lambda (keymap)    :run-last
      @end{pre}
      The signal gets emitted when the direction of the keymap changes.
      @begin[code]{table}
        @entry[keymap]{The @class{gdk:keymap} object on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"keys-changed\" signal}
      @begin{pre}
lambda (keymap)    :run-last
      @end{pre}
      The signal is emitted when the mapping represented by keymap changes.
      @begin[code]{table}
        @entry[keymap]{The @class{gdk:keymap} object on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"state-changed\" signal}
      @begin{pre}
lambda (keymap)    :run-last
      @end{pre}
      The signal is emitted when the state of the keyboard changes, e.g. when
      Caps Lock is turned on or off. See the @fun{gdk:keymap-caps-lock-state}
      function.
      @begin[code]{table}
        @entry[keymap]{The @class{gdk:keymap} object on which the signal is
          emitted.}
      @end{table}
  @end{dictionary}
  @see-class{gdk:event-key}")

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_default
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keymap_get_default" keymap-default) (g:object keymap)
 #+liber-documentation
 "@version{2023-3-4}
  @return{The @class{gdk:keymap} object attached to the default display.}
  @begin{short}
    Returns the keymap attached to the default display.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gdk:keymap-default} function has been deprecated since version
    3.22 and should not be used in newly written code. Use the
    @fun{gdk:keymap-for-display} function instead.
  @end{dictionary}
  @see-class{gdk:keymap}
  @see-function{gdk:keymap-for-display}")

(export 'keymap-default)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_for_display
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keymap_get_for_display" keymap-for-display) (g:object keymap)
 #+liber-documentation
 "@version{2023-3-4}
  @argument[display]{a @class{gdk:display} object}
  @return{The @class{gdk:keymap} object attached to @arg{display}.}
  @short{Returns the keymap attached to the display.}
  @see-class{gdk:keymap}
  @see-class{gdk:display}"
  (display (g:object display)))

(export 'keymap-for-display)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_lookup_key
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keymap_lookup_key" %keymap-lookup-key) :uint
  (keymap (g:object keymap))
  (key (:pointer (:struct %keymap-key))))

(defun keymap-lookup-key (keymap keycode group level)
 "@version{2023-3-4}
  @argument[keymap]{a @class{gdk:keymap} object}
  @argument[keycode]{an unsigned integer for the hardware keycode}
  @argument[group]{an integer which indicates movement in a horizontal
    direction}
  @argument[level]{an integer which indicates which symbol on the key will be
    used}
  @return{The unsigned integer with the keyval, or 0 if none was mapped to the
    given key.}
  @begin{short}
    Looks up the keyval mapped to a keycode/group/level triplet.
  @end{short}
  If no keyval is bound to @arg{key}, returns 0. For normal user input, you want
  to use the @fun{gdk:keymap-translate-keyboard-state} function instead of this
  function, since the effective group/level may not be the same as the current
  keyboard state.
  @begin[Example]{dictionary}
    @begin{pre}
(defvar keymap (gdk:keymap-default)) => KEYMAP
(gdk:keyval-name (gdk:keymap-lookup-key keymap  35 0 0)) => \"plus\"
(gdk:keyval-name (gdk:keymap-lookup-key keymap  35 0 1)) => \"asterisk\"
(gdk:keyval-name (gdk:keymap-lookup-key keymap  35 0 2)) => \"asciitilde\"
(gdk:keyval-name (gdk:keymap-lookup-key keymap  35 0 3)) => \"macron\"
    @end{pre}
  @end{dictionary}
  @see-class{gdk:keymap}
  @see-function{gdk:keymap-translate-keyboard-state}"
  (cffi:with-foreign-object (key '(:struct %keymap-key))
    (setf (cffi:foreign-slot-value key '(:struct %keymap-key) 'keycode) keycode)
    (setf (cffi:foreign-slot-value key '(:struct %keymap-key) 'group) group)
    (setf (cffi:foreign-slot-value key '(:struct %keymap-key) 'level) level)
    (%keymap-lookup-key keymap key)))

(export 'keymap-lookup-key)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_translate_keyboard_state
;;; ----------------------------------------------------------------------------

;; TODO: Translate the example to Lisp code

(cffi:defcfun ("gdk_keymap_translate_keyboard_state"
          %keymap-translate-keyboard-state) :boolean
  (keymap (g:object keymap))
  (keycode :uint)
  (state modifier-type)
  (group :int)
  (keyval (:pointer :uint))
  (effective (:pointer :int))
  (level (:pointer :int))
  (consumed (:pointer modifier-type)))

(defun keymap-translate-keyboard-state (keymap keycode state group)
 #+liber-documentation
 "@version{2023-3-4}
  @argument[keymap]{a @class{gdk:keymap} object}
  @argument[keycode]{an unsigned integer for the keycode}
  @argument[state]{a @symbol{gdk:modifier-type} modifier state}
  @argument[group]{an integer for the active keyboard group}
  @begin{return}
    @arg{keyval} -- an unsigned integer with the keyval @br{}
    @arg{effective} -- an integer with the effective group @br{}
    @arg{level} -- an integer with the level @br{}
    @arg{consumed} -- a @symbol{gdk:modifier-type} value with the flags that
    were used to determine the group or level
  @end{return}
  @begin{short}
    Translates the contents of the @arg{keycode}, @arg{state}, and @arg{group}
    arguments into a keyval, effective group, and level.
  @end{short}
  Modifiers that affected the translation and are thus unavailable for
  application use are returned in @arg{consumed}. The @arg{effective} return
  value is the group that was actually used for the translation. Some keys such
  as the @kbd{Enter} key are not affected by the active keyboard group. The
  level is derived from @rg{state}. For convenience, the @class{gdk:event-key}
  event already contains the translated keyval, so this function is not as
  useful as you might think.

  The @arg{consumed} flags gives modifiers that should be masked out from
  @arg{state} when comparing this key press to a hot key. For instance, on a
  US keyboard, the @kbd{Plus} symbol is shifted, so when comparing a key press
  to a @kbd{<Control>Plus} accelerator the @kbd{Shift} key should be masked out.
  @begin{pre}
/* We want to ignore irrelevant modifiers like ScrollLock */
#define ALL_ACCELS_MASK (GDK_CONTROL_MASK | GDK_SHIFT_MASK |
                         GDK_MOD1_MASK)
gdk_keymap_translate_keyboard_state (keymap, event->hardware_keycode,
                                     event->state, event->group,
                                     &keyval, NULL, NULL, &consumed);
if (keyval == GDK_PLUS &&
    (event->state & ~consumed & ALL_ACCELS_MASK) == GDK_CONTROL_MASK)
  /* Control was pressed */
  @end{pre}
  @see-class{gdk:keymap}
  @see-class{gdk:event-key}
  @see-symbol{gdk:modifier-type}"
  (cffi:with-foreign-objects ((keyval :uint)
                              (effective :int)
                              (level :int)
                              (consumed 'modifier-type))
    (when (%keymap-translate-keyboard-state keymap
                                            keycode
                                            state
                                            group
                                            keyval
                                            effective
                                            level
                                            consumed)
      (values (cffi:mem-ref keyval :uint)
              (cffi:mem-ref effective :int)
              (cffi:mem-ref level :int)
              (cffi:mem-ref consumed 'modifier-type)))))

(export 'keymap-translate-keyboard-state)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_entries_for_keyval
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keymap_get_entries_for_keyval" %keymap-entries-for-keyval)
    :boolean
  (keymap (g:object keymap))
  (keyval :uint)
  (keys :pointer)
  (n-keys (:pointer :int)))

(defun keymap-entries-for-keyval (keymap keyval)
 #+liber-documentation
 "@version{2023-3-4}
  @argument[keymap]{a @class{gdk:keymap} object}
  @argument[keyval]{a keyval, such as @code{GDK_a}, @code{GDK_Up},
    @code{GDK_Return}, etc.}
  @return{A list of keycode, group, and level values.}
  @begin{short}
    Obtains a list of keycode/group/level combinations that will generate
    @arg{keyval}.
  @end{short}
  Groups and levels are two kinds of keyboard mode. In general, the level
  determines whether the top or bottom symbol on a key is used, and the
  group determines whether the left or right symbol is used. On US keyboards,
  the shift key changes the keyboard level, and there are no groups. A group
  switch key might convert a keyboard between Hebrew to English modes, for
  example. A @class{gdk:event-key} event contains a group field that indicates
  the active keyboard group. The level is computed from the modifier mask.
  @begin[Example]{dictionary}
    @begin{pre}
(defvar keymap (gdk:keymap-default)) => KEYMAP
(gdk:keymap-entries-for-keyval keymap (gdk:keyval-from-name \"plus\"))
=> ((35 0 0))
(gdk:keymap-entries-for-keyval keymap (gdk:keyval-from-name \"asciitilde\"))
=> ((35 0 2))
    @end{pre}
  @end{dictionary}
  @see-class{gdk:keymap}
  @see-class{gdk:event-key}"
  (cffi:with-foreign-objects ((keys :pointer) (n-keys :int))
    (when (%keymap-entries-for-keyval keymap keyval keys n-keys)
      (let ((keys (cffi:mem-ref keys :pointer))
            (n-keys (cffi:mem-ref n-keys :int)))
        (loop for i from 0 below n-keys
              for key = (cffi:mem-aptr keys '(:struct %keymap-key) i)
              collect (cffi:with-foreign-slots ((keycode group level)
                                                key
                                                (:struct %keymap-key))
                      (list keycode group level))
              finally (g:free keys))))))

(export 'keymap-entries-for-keyval)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_entries_for_keycode
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keymap_get_entries_for_keycode" %keymap-entries-for-keycode)
    :boolean
  (keymap (g:object keymap))
  (keycode :uint)
  (keys :pointer)
  (keyvals :pointer)
  (n-entries :pointer))

(defun keymap-entries-for-keycode (keymap keycode)
 #+liber-documentation
 "@version{2023-3-4}
  @argument[keymap]{a @class{gdk:keymap} object}
  @argument[keycode]{an integer for a hardware keycode}
  @begin{return}
    The list of keyval, keycode, group, and level values.
  @end{return}
  @begin{short}
    Returns the keyvals bound to @arg{keycode}.
  @end{short}
  When a keycode is pressed by the user, the keyval from this list
  of entries is selected by considering the effective keyboard group and
  level. See the @fun{gdk:keymap-translate-keyboard-state} function.
  @begin[Example]{dictionary}
    @begin{pre}
(defvar keymap (gdk:keymap-default)) => KEYMAP
(gdk:keymap-entries-for-keycode keymap 35)
=> ((43 35 0 0) (42 35 0 1) (126 35 0 2) (175 35 0 3))
(mapcar #'gdk:keyval-name (mapcar #'first *))
=> (\"plus\" \"asterisk\" \"asciitilde\" \"macron\")
    @end{pre}
  @end{dictionary}
  @see-class{gdk:keymap}
  @see-function{gdk:keymap-translate-keyboard-state}"
  (cffi:with-foreign-objects ((keys :pointer) (keyvals :pointer) (n-keys :int))
    (when (%keymap-entries-for-keycode keymap keycode keys keyvals n-keys)
        (let ((keys (cffi:mem-ref keys :pointer))
              (keyvals (cffi:mem-ref keyvals :pointer))
              (n-keys (cffi:mem-ref n-keys :int)))
          (loop for i from 0 below n-keys
                for keyval = (cffi:mem-aref keyvals :uint i)
                for key = (cffi:mem-aptr keys '(:struct %keymap-key) i)
                collect (cffi:with-foreign-slots ((keycode group level)
                                                  key
                                                  (:struct %keymap-key))
                          (list keyval keycode group level))
                finally (g:free keys)
                        (g:free keyvals))))))

(export 'keymap-entries-for-keycode)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_direction
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keymap_get_direction" keymap-direction) pango:direction
 #+liber-documentation
 "@version{2023-3-4}
  @argument[keymap]{a @class{gdk:keymap} object}
  @begin{return}
    The @code{:ltr} or @code{:rtl} value if it can determine the direction.
    The @code{:neutral} value otherwise.
  @end{return}
  @begin{short}
    Returns the Pango direction of the effective layout of the keymap.
  @end{short}
  @see-class{gdk:keymap}
  @see-symbol{pango:direction}"
  (keymap (g:object keymap)))

(export 'keymap-direction)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_have_bidi_layouts
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keymap_have_bidi_layouts" keymap-have-bidi-layouts) :boolean
 #+liber-documentation
 "@version{2023-3-4}
  @argument[keymap]{a @class{gdk:keymap} object}
  @return{@em{True} if there are layouts in both directions, @em{false}
    otherwise.}
  @begin{short}
    Determines if keyboard layouts for both right-to-left and left-to-right
    languages are in use.
  @end{short}
  @see-class{gdk:keymap}"
  (keymap (g:object keymap)))

(export 'keymap-have-bidi-layouts)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_caps_lock_state
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keymap_get_caps_lock_state" keymap-caps-lock-state) :boolean
 #+liber-documentation
 "@version{2023-3-4}
  @argument[keymap]{a @class{gdk:keymap} object}
  @return{@em{True} if the @kbd{Caps Lock} key is on.}
  @short{Returns whether the @kbd{Caps Lock} modifier is locked.}
  @see-class{gdk:keymap}"
  (keymap (g:object keymap)))

(export 'keymap-caps-lock-state)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_num_lock_state
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keymap_get_num_lock_state" keymap-num-lock-state) :boolean
 #+liber-documentation
 "@version{2023-3-4}
  @argument[keymap]{a @class{gdk:keymap} object}
  @return{@em{True} if the @kbd{Num Lock} key is on.}
  @short{Returns whether the @kbd{Num Lock} modifier is locked.}
  @see-class{gdk:keymap}"
  (keymap (g:object keymap)))

(export 'keymap-num-lock-state)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_scroll_lock_state
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keymap_get_scroll_lock_state" keymap-scroll-lock-state)
    :boolean
 #+liber-documentation
 "@version{2023-3-13}
  @argument[keymap]{a @class{gdk:keymap} object}
  @return{@em{True} if the @kbd{Scroll Lock} key is on.}
  @short{Returns whether the @kbd{Scroll Lock} modifier is locked.}
  @see-class{gdk:keymap}"
  (keymap (g:object keymap)))

(export 'keymap-scroll-lock-state)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_modifier_state
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keymap_get_modifier_state" keymap-modifier-state)
    modifier-type
 #+liber-documentation
 "@version{2023-3-4}
  @argument[keymap]{a @class{gdk:keymap} object}
  @return{The current @symbol{gdk:modifier-type} modifier state.}
  @short{Returns the current modifier state.}
  @see-class{gdk:keymap}
  @see-symbol{gdk:modifier-type}"
  (keymap (g:object keymap)))

(export 'keymap-modifier-state)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_add_virtual_modifiers
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keymap_add_virtual_modifiers" %keymap-add-virtual-modifiers)
    :void
  (keymap (g:object keymap))
  (state (:pointer modifier-type)))

(defun keymap-add-virtual-modifiers (keymap state)
 #+liber-documentation
 "@version{2023-3-4}
  @argument[keymap]{a @class{gdk:keymap} object}
  @argument[state]{a value of the @symbol{gdk:modifier-type} flags}
  @return{The @symbol{gdk:modfier-type} flags.}
  @begin{short}
    Adds virtual modifiers (i.e. Super, Hyper and Meta) which correspond to the
    real modifiers (i.e. Mod2, Mod3, ...) in modifiers and set the corresponding
    bits in state.
  @end{short}

  GDK already does this before delivering key events, but for compatibility
  reasons, it only sets the first virtual modifier it finds, whereas this
  function sets all matching virtual modifiers.

  This function is useful when matching key events against accelerators.
  @begin[Example]{dictionary}
    @begin{pre}
(defvar keymap (gdk:keymap-default)) => KEYMAP
(gdk:keymap-add-virtual-modifiers keymap :mod4-mask)
=> (:MOD4-MASK :SUPER-MASK :HYPER-MASK)
    @end{pre}
  @end{dictionary}
  @see-class{gdk:keymap}
  @see-symbol{gdk:modifier-type}"
  (cffi:with-foreign-object (modifier 'modifier-type)
    (setf (cffi:mem-ref modifier 'modifier-type) state)
    (%keymap-add-virtual-modifiers keymap modifier)
    (cffi:mem-ref modifier 'modifier-type)))

(export 'keymap-add-virtual-modifiers)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_map_virtual_modifiers
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keymap_map_virtual_modifiers" %keymap-map-virtual-modifiers)
    :boolean
  (keymap (g:object keymap))
  (state (:pointer modifier-type)))

(defun keymap-map-virtual-modifiers (keymap state)
 #+liber-documentation
 "@version{2023-3-4}
  @argument[keymap]{a @class{gdk:keymap} object}
  @argument[state]{a @symbol{gdk:modifier-type} value}
  @return{The @symbol{gdk:modifier-type} value.}
  @begin{short}
    Maps the virtual modifiers, that is Super, Hyper and Meta, which are set in
    @arg{state} to their non-virtual counterparts, that is Mod2, Mod3, ..., and
    set the corresponding bits in the return value.
  @end{short}
  This function is useful when matching key events against accelerators.
  @begin[Example]{dictionary}
    @begin{pre}
(gdk:keymap-map-virtual-modifiers keymap :super-mask)
=> (:MOD4-MASK :SUPER-MASK)
    @end{pre}
  @end{dictionary}
  @see-class{gdk:keymap}
  @see-symbol{gdk:modifier-type}"
  (cffi:with-foreign-object (mods 'modifier-type)
    (setf (cffi:mem-ref mods 'modifier-type) state)
    (when (%keymap-map-virtual-modifiers keymap mods)
      (cffi:mem-ref mods 'modifier-type))))

(export 'keymap-map-virtual-modifiers)

;;; ----------------------------------------------------------------------------
;;; gdk_keymap_get_modifier_mask
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keymap_get_modifier_mask" keymap-modifier-mask) modifier-type
 #+liber-documentation
 "@version{2023-3-4}
  @argument[keymap]{a @class{gdk:keymap} object}
  @argument[intent]{a value of the @symbol{gdk:modifier-intent} enumeration for
    the use case for the modifier mask}
  @return{The @symbol{gdk:modifier-type} modifier mask used for @arg{intent}.}
  @begin{short}
    Returns the modifier mask the windowing system backend of the keymap uses
    for a particular purpose.
  @end{short}

  Note that this function always returns real hardware modifiers, not virtual
  ones, e.g. it will return @code{:mod1-mask} rather than @code{:meta-mask} if
  the backend maps @code{MOD1} to @code{META}), so there are use cases where
  the return value of this function has to be transformed by the
  @fun{gdk:keymap-add-virtual-modifiers} function in order to contain the
  expected result.
  @see-class{gdk:keymap}
  @see-symbol{gdk:modifier-type}
  @see-symbol{gdk:modifier-intent}
  @see-function{gdk:keymap-add-virtual-modifiers}"
  (keymap (g:object keymap))
  (intent modifier-intent))

(export 'keymap-modifier-mask)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keyval_name" keyval-name) (:string :free-from-foreign nil)
 #+liber-documentation
 "@version{2023-3-4}
  @argument[keyval]{an unsigned integer for a key value}
  @begin{return}
    The string containing the name of the key, or @code{nil} if @arg{keyval} is
    not a valid key.
  @end{return}
  @begin{short}
    Converts a key value into a symbolic name.
  @end{short}
  The names are the same as those in the @file{gdk/gdkkeysyms.h} header file
  but without the leading @code{\"GDK_KEY_\"}.
  @begin[Examples]{dictionary}
    @begin{pre}
(gdk:keyval-name 97) => \"a\"
(gdk:keyval-name 61) => \"equal\"
(gdk:keyval-name 65470) => \"F1\"
(gdk:keyval-from-name 16777215) => \"0xffffff\"
    @end{pre}
  @end{dictionary}
  @see-class{gdk:keymap}"
  (keyval :uint))

(export 'keyval-name)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_from_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keyval_from_name" keyval-from-name) :uint
 #+liber-documentation
 "@version{2023-3-4}
  @argument[name]{a string for the key name}
  @begin{return}
    The unsigned integer with the corresponding key value, or the
    @code{#xffffff} value if the key name is not a valid key.
  @end{return}
  @begin{short}
    Converts a key name to a key value.
  @end{short}
  The names are the same as those in the @file{gdk/gdkkeysyms.h} header file
  but without the leading @code{\"GDK_KEY_\"}.
  @begin[Examples]{dictionary}
    @begin{pre}
(gdk:keyval-from-name \"a\") => 97
(gdk:keyval-from-name \"equal\") => 61
(gdk:keyval-from-name \"F1\") => 65470
(gdk:keyval-from-name \"unknown\") => 16777215
    @end{pre}
  @end{dictionary}
  @see-class{gdk:keymap}"
  (name :string))

(export 'keyval-from-name)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_convert_case
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keyval_convert_case" %keyval-convert-case) :void
  (keyval :uint)
  (lower (:pointer :uint))
  (upper (:pointer :uint)))

(defun keyval-convert-case (keyval)
 #+liber-documentation
 "@version{2023-3-4}
  @argument[keyval]{an unsigned integer for the keyval}
  @begin{return}
    @arg{lower} -- an unsigned integer with the lowercase version @br{}
    @arg{upper} -- an unsigned integer with the uppercase version
  @end{return}
  @begin{short}
    Obtains the upper-case and lower-case versions of @arg{keyval}.
  @end{short}
  @see-class{gdk:keymap}"
  (cffi:with-foreign-objects ((lower :uint) (upper :uint))
    (%keyval-convert-case keyval lower upper)
    (values (cffi:mem-ref lower :uint)
            (cffi:mem-ref upper :uint))))

(export 'keyval-convert-case)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_to_upper
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keyval_to_upper" keyval-to-upper) :uint
 #+liber-documentation
 "@version{2023-3-4}
  @argument[keyval]{an unsigned integer for a key value}
  @return{The upper case form of @arg{keyval}, or @arg{keyval} itself if it is
    already in upper case or it is not subject to case conversion.}
  @begin{short}
    Converts a key value to upper case, if applicable.
  @end{short}
  @see-class{gdk:keymap}
  @see-function{gdk:keyval-to-lower}"
  (keyval :uint))

(export 'keyval-to-upper)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_to_lower
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keyval_to_lower" keyval-to-lower) :uint
 #+liber-documentation
 "@version{2023-3-4}
  @argument[keyval]{an unsigned integer for a key value}
  @return{The lower case form of @arg{keyval}, or @arg{keyval} itself if it is
    already in lower case or it is not subject to case conversion.}
  @begin{short}
    Converts a key value to lower case, if applicable.
  @end{short}
  @see-class{gdk:keymap}
  @see-function{gdk:keyval-to-upper}"
  (keyval :uint))

(export 'keyval-to-lower)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_is_upper
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keyval_is_upper" keyval-is-upper) :boolean
 #+liber-documentation
 "@version{2023-3-4}
  @argument[keyval]{an unsigned integer for a key value}
  @return{@em{True} if @arg{keyval} is in upper case, or if @arg{keyval} is not
    subject to case conversion.}
  @begin{short}
    Returns @em{true} if the given key value is in upper case.
  @end{short}
  @see-class{gdk:keymap}
  @see-function{gdk:keyval-is-lower}"
  (keyval :uint))

(export 'keyval-is-upper)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_is_lower
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keyval_is_lower" keyval-is-lower) :boolean
 #+liber-documentation
 "@version{2023-3-4}
  @argument[keyval]{an unsigned integer for a key value}
  @return{@em{True} if @arg{keyval} is in lower case, or if @arg{keyval} is not
    subject to case conversion.}
  @begin{short}
    Returns @em{true} if the given key value is in lower case.
  @end{short}
  @see-class{gdk:keymap}
  @see-function{gdk:keyval-is-upper}"
  (keyval :uint))

(export 'keyval-is-lower)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_to_unicode
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keyval_to_unicode" keyval-to-unicode) g:unichar
 #+liber-documentation
 "@version{2023-3-4}
  @argument[keyval]{an unsigned integer for a GDK key symbol}
  @return{The corresponding unicode character, or @code{#\\Nul} if there is no
    corresponding character.}
  @begin{short}
    Convert from a GDK key symbol to the corresponding ISO10646 (Unicode)
    character.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(mapcar 'gdk:keyval-to-unicode '(65 66 67 68 69 70 71))
=> (#\\A #\\B #\\C #\\D #\\E #\\F #\\G)
    @end{pre}
  @end{dictionary}
  @see-class{gdk:keymap}
  @see-function{gdk:unicode-to-keyval}"
  (keyval :uint))

(export 'keyval-to-unicode)

;;; ----------------------------------------------------------------------------
;;; gdk_unicode_to_keyval
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_unicode_to_keyval" unicode-to-keyval) :uint
 #+liber-documentation
 "@version{2023-3-4}
  @argument[unichar]{a ISO10646 encoded character}
  @return{The unsigned integer with the corresponding GDK key symbol, if one
    exists, or, if there is no corresponding symbol, @code{@arg{unichar} |
    0x01000000}.}
  @short{Convert from a ISO10646 character to a key symbol.}
  @begin[Examples]{dictionary}
    @begin{pre}
(mapcar 'gdk:unicode-to-keyval '(#\\a #\\b #\\c))
=> (97 98 99)
    @end{pre}
  @end{dictionary}
  @see-class{gdk:keymap}
  @see-function{gdk:keyval-to-unicode}"
  (unichar g:unichar))

(export 'unicode-to-keyval)

;;; --- End of file gdk3.key-values.lisp ---------------------------------------
