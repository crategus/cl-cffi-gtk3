;;; ----------------------------------------------------------------------------
;;; gtk3.action-group.lisp
;;;
;;; The documentation in this file is taken from the GTK 3 Reference Manual
;;; version 3.24 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
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
;;; GtkActionGroup
;;;
;;;     A group of actions
;;;
;;; Types and Values
;;;
;;;     GtkActionGroup
;;;
;;;     GtkActionEntry                                      not needed
;;;     GtkToggleActionEntry                                not needed
;;;     GtkRadioActionEntry                                 not needed
;;;
;;; Accessors
;;;
;;;     gtk_action_group_get_name
;;;     gtk_action_group_get_sensitive
;;;     gtk_action_group_set_sensitive
;;;     gtk_action_group_get_visible
;;;     gtk_action_group_set_visible
;;;     gtk_action_group_get_accel_group
;;;     gtk_action_group_set_accel_group
;;;
;;; Functions
;;;
;;;     gtk_action_group_new
;;;
;;;     gtk_action_group_get_action
;;;     gtk_action_group_list_actions
;;;     gtk_action_group_add_action
;;;     gtk_action_group_add_action_with_accel             not exported
;;;     gtk_action_group_remove_action
;;;     gtk_action_group_add_actions
;;;     gtk_action_group_add_actions_full                  not implemented
;;;     gtk_action_group_add_toggle_actions
;;;     gtk_action_group_add_toggle_actions_full           not implemented
;;;     gtk_action_group_add_radio_actions
;;;     gtk_action_group_add_radio_actions_full            not implemented
;;;
;;;     GtkTranslateFunc                                   not exported
;;;
;;;     gtk_action_group_set_translate_func                not exported
;;;     gtk_action_group_set_translation_domain            not exported
;;;     gtk_action_group_translate_string                  not exported
;;;
;;; Properties
;;;
;;;     accel-group
;;;     name
;;;     sensitive
;;;     visible
;;;
;;; Signals
;;;
;;;     connect-proxy
;;;     disconnect-proxy
;;;     post-activate
;;;     pre-activate
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkActionGroup
;;;
;;; Implemented Interfaces
;;;
;;;     GtkActionGroup implements GtkBuildable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkActionGroup
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkActionGroup" action-group
  (:superclass g:object
   :export t
   :interfaces ("GtkBuildable")
   :type-initializer "gtk_action_group_get_type")
  ((accel-group
    action-group-accel-group
    "accel-group" "GtkAccelGroup" t t)
   (name
    action-group-name
    "name" "gchararray" t nil)
   (sensitive
    action-group-sensitive
    "sensitive" "gboolean" t t)
   (visible
    action-group-visible
    "visible" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'action-group 'type)
 "@version{2025-06-27}
  @begin{short}
    Actions are organised into groups. An action group is essentially a map
    from names to @class{gtk:action} objects.
  @end{short}

  All actions that would make sense to use in a particular context should be
  in a single group. Multiple action groups may be used for a particular user
  interface. In fact, it is expected that most nontrivial applications will
  make use of multiple groups. For example, in an application that can edit
  multiple documents, one group holding global actions, for example, Quit,
  About, New, and one group per document holding actions that act on that
  document, for example, Save, Cut, Copy, Paste. Each of the menus of the
  window would be constructed from a combination of two action groups.

  @subheading{Accelerators}
  Accelerators are handled by the GTK accelerator map. All actions are assigned
  an accelerator path, which normally has the form
  @code{<Actions>/group-name/action-name}, and a shortcut is associated with
  this accelerator path. All menu items and tool items take on this accelerator
  path. The GTK accelerator map code makes sure that the correct shortcut is
  displayed next to the menu item.
  @begin[Warning]{dictionary}
    The @class{gtk:action-group} class has been deprecated since version 3.10
    and should not be used in newly written code.
  @end{dictionary}
  @begin[GtkActionGroup as GtkBuildable]{dictionary}
    The @class{gtk:action-group} implementation of the @class{gtk:buildable}
    interface accepts @class{gtk:action} objects as @code{<child>} elements in
    UI definitions.

    Note that it is probably more common to define actions and action groups in
    the code, since they are directly related to what the code can do.

    The @class{gtk:action-group} implementation of the @class{gtk:buildable}
    interface supports a custom @code{<accelerator>} element, which has
    attributes named @code{key} and @code{modifiers} and allows to specify
    accelerators. This is similar to the @code{<accelerator>} element of a
    @class{gtk:widget} object, the main difference is that it does not allow
    you to specify a signal.

    @b{Example:} A @class{gtk:action-group} UI definition fragment.
    @begin{pre}
<object class=\"GtkActionGroup\" id=\"actiongroup\">
  <child>
    <object class=\"GtkAction\" id=\"About\">
      <property name=\"name\">About</property>
      <property name=\"stock_id\">gtk-about</property>
      <signal handler=\"about_activate\" name=\"activate\"/>
    </object>
    <accelerator key=\"F1\" modifiers=\"GDK_CONTROL_MASK | GDK_SHIFT_MASK\"/>
  </child>
</object>
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[action-group::connect-proxy]{signal}
      @begin{pre}
lambda (group action proxy)
      @end{pre}
      @begin[code]{simple-table}
        @entry[group]{The @class{gtk:action-group} object.}
        @entry[action]{The @class{gtk:action} object.}
        @entry[proxy]{The @class{gtk:widget} proxy.}
      @end{simple-table}
      The signal is emitted after connecting a proxy to an action in the group.
      Note that the proxy may have been connected to a different action before.
      This is intended for simple customizations for which a custom action class
      would be too clumsy, for example, showing tooltips for menu items in the
      statusbar.
    @end{signal}
    @begin[action-group::disconnect-proxy]{signal}
      @begin{pre}
lambda (group action proxy)
      @end{pre}
      @begin[code]{simple-table}
        @entry[group]{The @class{gtk:action-group} object.}
        @entry[action]{The @class{gtk:action} object.}
        @entry[proxy]{The @class{gtk:widget} proxy.}
      @end{simple-table}
      The signal is emitted after disconnecting a proxy from an action in the
      group.
    @end{signal}
    @begin[action-group::post-activate]{signal}
      @begin{pre}
lambda (group action)
      @end{pre}
      @begin[code]{simple-table}
        @entry[group]{The @class{gtk:action-group} object.}
        @entry[action]{The @class{gtk:action} object.}
      @end{simple-table}
      The signal is emitted just after the action in the action group is
      activated.
    @end{signal}
    @begin[action-group::pre-activate]{signal}
      @begin{pre}
lambda (group action)
      @end{pre}
      @begin[code]{simple-table}
        @entry[group]{The @class{gtk:action-group} object.}
        @entry[action]{The @class{gtk:action} object.}
      @end{simple-table}
      The signal is emitted just before the action in the action group is
      activated.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:action-group-new}
  @see-slot{gtk:action-group-accel-group}
  @see-slot{gtk:action-group-name}
  @see-slot{gtk:action-group-sensitive}
  @see-slot{gtk:action-group-visible}
  @see-class{gtk:action}
  @see-class{gtk:accel-group}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:action-group-accel-group -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accel-group" 'action-group) t)
 "The @code{accel-group} property of type @class{gtk:accel-group} (Read / Write)
  @br{}
  The accelerator group the actions of this group should use.")

#+liber-documentation
(setf (liber:alias-for-function 'action-group-accel-group)
      "Accessor"
      (documentation 'action-group-accel-group 'function)
 "@version{2024-9-24}
  @syntax{(gtk:action-group-accel-group object) => group}
  @syntax{(setf (gtk:action-group-accel-group object) group)}
  @argument[object]{a @class{gtk:action-group} object}
  @argument[group]{a @class{gtk:accel-group} object to set or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:action-group]{accel-group} slot of the
    @class{gtk:action-group} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:action-group-accel-group} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action-group}
  @see-class{gtk:accel-group}")

;;; --- gtk:action-group-name --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "name" 'action-group) t)
 "The @code{name} property of type @code{:string}
  (Read / Write / Construct only) @br{}
  The name for the action group. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'action-group-name)
      "Accessor"
      (documentation 'action-group-name 'function)
 "@version{2025-07-02}
  @syntax{(gtk:action-group-name object) => name}
  @argument[object]{a @class{gtk:action-group} object}
  @argument[name]{a string for the name of the action group}
  @begin{short}
    Accessor of the @slot[gtk:action-group]{name} slot of the
    @class{gtk:action-group} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:action-group-name} function has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action-group}")

;;; --- gtk:action-group-sensitive ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "sensitive" 'action-group) t)
 "The @code{sensitive} property of type @code{:boolean} (Read / Write) @br{}
  Whether the action group is enabled. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'action-group-sensitive)
      "Accessor"
      (documentation 'action-group-sensitive 'function)
 "@version{2025-07-02}
  @syntax{(gtk:action-group-sensitive object) => sensitive}
  @syntax{(setf (gtk:action-group-sensitive object) sensitive)}
  @argument[object]{a @class{gtk:action-group} object}
  @argument[sensitive]{a boolean for the sensitivity}
  @begin{short}
    Accessor of the @slot[gtk:action-group]{sensitive} slot of the
    @class{gtk:action-group} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:action-group-sensitive} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action-group}")

;;; --- gtk:action-group-visible -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "visible" 'action-group) t)
 "The @code{visible} property of type @code{:boolean} (Read / Write) @br{}
  Whether the action group is visible. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'action-group-visible)
      "Accessor"
      (documentation 'action-group-visible 'function)
 "@version{2025-07-02}
  @syntax{(gtk:action-group-visible object) => visible}
  @syntax{(setf (gtk:action-group-visible object) visible)}
  @argument[object]{a @class{gtk:action-group} object}
  @argument[visible]{a boolean for the visibility}
  @begin{short}
    Accessor of the @slot[gtk:action-group]{visible} slot of the
    @class{gtk:action-group} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:action-group-visible} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action-group}")

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_new
;;; ----------------------------------------------------------------------------

(declaim (inline action-group-new))

(defun action-group-new (name)
 #+liber-documentation
 "@version{2025-07-02}
  @argument[name]{a string for the name of the action group}
  @return{The new @class{gtk:action-group} object.}
  @begin{short}
    Creates a new action group.
  @end{short}
  The name of the action group is used when associating keybindings with the
  actions.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-group-new} function has been deprecated since version
    3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action-group}"
  (make-instance 'action-group
                 :name name))

(export 'action-group-new)

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_get_action
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_action_group_get_action" action-group-action) g:object
 #+liber-documentation
 "@version{2025-07-02}
  @argument[group]{a @class{gtk:action-group} object}
  @argument[name]{a string for the name of the action}
  @return{The @class{gtk:action} object, or @code{nil} if no action by that
    name exists.}
  @short{Looks up an action in the action group by name.}
  @begin[Warning]{dictionary}
    The @fun{gtk:action-group-action} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action-group}
  @see-function{gtk:action-group-add-action}"
  (group (g:object action-group))
  (name :string))

(export 'action-group-action)

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_list_actions
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_action_group_list_actions" action-group-list-actions)
    (g:list-t g:object :free-from-foreign t)
 #+liber-documentation
 "@version{2024-9-24}
  @argument[group]{a @class{gtk:action-group} object}
  @return{The list of the @class{gtk:action} objects in the action group.}
  @short{Lists the actions in the action group.}
  @begin[Warning]{dictionary}
    The @fun{gtk:action-group-list-actions} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action-group}
  @see-class{gtk:action}
  @see-function{gtk:action-group-add-action}"
  (group (g:object action-group)))

(export 'action-group-list-actions)

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_add_action
;;; gtk_action_group_add_action_with_accel                  not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_action_group_add_action_with_accel"
               %action-group-add-action-with-accel) :void
  (group (g:object action-group))
  (action (g:object action))
  (accelerator :string))

(defun action-group-add-action (group action &optional accelerator)
 #+liber-documentation
 "@version{2025-07-02}
  @argument[group]{a @class{gtk:action-group} object}
  @argument[action]{a @class{gtk:action} object to add}
  @argument[accelerator]{a string for the optional accelerator for the action,
    in the format understood by the @fun{gtk:accelerator-parse} function, or
    \"\" for no accelerator, or @code{nil} to use the stock accelerator}
  @begin{short}
    Adds an action to the action group and sets up the accelerator.
  @end{short}
  If the @arg{accelerator} argument is @code{nil}, this is the default value,
  attempts to use the accelerator associated with the stock ID of the action.
  Accel paths are set to @code{<Actions>/group-name/action-name}.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-group-add-action} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action-group}
  @see-function{gtk:accelerator-parse}"
  (%action-group-add-action-with-accel group
                                       action
                                       (or accelerator (cffi:null-pointer))))

(export 'action-group-add-action)

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_remove_action
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_action_group_remove_action" action-group-remove-action)
    :void
 #+liber-documentation
 "@version{2024-9-24}
  @argument[group]{a @class{gtk:action-group} object}
  @argument[action]{a @class{gtk:action} object}
  @short{Removes an action from the action group.}
  @begin[Warning]{dictionary}
    The @fun{gtk:action-group-remove-action} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action-group}
  @see-class{gtk:action}
  @see-function{gtk:action-group-add-action}"
  (group (g:object action-group))
  (action (g:object action)))

(export 'action-group-remove-action)

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_add_actions
;;; gtk_action_group_add_actions_full                       not implemented
;;; ----------------------------------------------------------------------------

(defun action-group-add-actions (group entries)
 #+liber-documentation
 "@version{2025-07-16}
  @argument[group]{a @class{gtk:action-group} object}
  @argument[entries]{a list of action descriptions}
  @begin{short}
    This is a convenience function to create a number of actions and add them
    to the action group.
  @end{short}
  The @sig[gtk:action]{activate} signals of the actions are connected to the
  callback functions and their accel paths are set to
  @code{<Actions>/group-name/action-name}.
  @begin[Examples]{dictionary}
    @begin{pre}
(let ((group (gtk:action-group-new \"AppWindowActions\"))
      (actions '((\"Open\"            ; name
                  \"gtk-stock-open\"  ; stock-id
                  \"_Open\"           ; label
                  \"<ctrl>o\"         ; accelerator
                  \"Open a file\"     ; tooltip
                  nil               ; callback function
                ))))
  (gtk:action-group-add-actions group actions)
  ... )
    @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @fun{gtk:action-group-add-actions} function has been deprecated since
    version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action-group}
  @see-class{gtk:action}
  @see-function{gtk:action-group-add-action}"
  (dolist (entry entries)
    (let ((action (make-instance 'action
                                 :name (first entry)
                                 :stock-id
                                 (or (second entry) (cffi:null-pointer))
                                 :label
                                 (or (third entry) (cffi:null-pointer))
                                 :tooltip
                                 (or (fifth entry) (cffi:null-pointer)))))
      (action-group-add-action group action (fourth entry))
      (let ((func (sixth entry)))
        (when func
          (g:signal-connect action "activate" func))))))

(export 'action-group-add-actions)

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_add_toggle_actions
;;; gtk_action_group_add_toggle_actions_full                not implemented
;;; ----------------------------------------------------------------------------

(defun action-group-add-toggle-actions (group entries)
 #+liber-documentation
 "@version{2025-0716}
  @argument[group]{a @class{gtk:action-group} object}
  @argument[entries]{a list of toggle action descriptions}
  @begin{short}
    This is a convenience function to create a number of toggle actions and add
    them to the action group.
  @end{short}
  The @sig[gtk:action]{activate} signals of the actions are connected to the
  callback functions and their accel paths are set to
  @code{<Actions>/group-name/action-name}.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-group-add-toggle-actions} function has been deprecated
    since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action-group}
  @see-class{gtk:toggle-action}
  @see-function{gtk:action-group-add-action}"
  (dolist (entry entries)
    (let ((action (make-instance 'toggle-action
                                 :name (first entry)
                                 :stock-id
                                 (or (second entry) (cffi:null-pointer))
                                 :label
                                 (or (third entry) (cffi:null-pointer))
                                 :tooltip
                                 (or (fifth entry) (cffi:null-pointer))
                                 :active (seventh entry))))
      (action-group-add-action group action (fourth entry))
      (let ((func (sixth entry)))
        (when func
          (g:signal-connect action "activate" func))))))

(export 'action-group-add-toggle-actions)

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_add_radio_actions
;;; gtk_action_group_add_radio_actions_full                 not implemented
;;; ----------------------------------------------------------------------------

;; TODO: This Lisp implementation is not complete.
;;       See the C code for missing features.

(defun action-group-add-radio-actions (group entries value on-change)
 #+liber-documentation
 "@version{2025-07-02}
  @argument[group]{a @class{gtk:action-group} object}
  @argument[entries]{a list of radio action descriptions}
  @argument[value]{an integer for the value of the action to activate
    initially, or -1 if no action should be activated}
  @argument[on-change]{a callback function to connect to the changed signal}
  @begin{short}
    This is a convenience function to create a group of radio actions and add
    them to the action group.
  @end{short}
  The @sig[gtk:action]{changed} signal of the first radio action is connected to
  the @arg{on-change} callback function and the accel paths of the actions are
  set to @code{<Actions>/group-name/action-name}.
  @begin[Examples]{dictionary}
    @begin{pre}
(let ((group (gtk:action-group-new \"AppWindowActions\"))
      (actions '((\"Red\" nil                      ; name, stock id
                  \"_Red\" \"<control>R\"            ; label, accelerator
                  \"Blood\" 0)                     ; tooltip, value
                 (\"Green\" nil                    ; name, stock id
                  \"_Green\" \"<control>G\"          ; label, accelerator
                  \"Grass\" 1)                     ; tooltip, value
                 (\"Blue\" nil                     ; name, stock id
                  \"_Blue\" \"<control>B\"           ; label, accelerator
                  \"Sky\" 2))))
    (gtk:action-group-add-radio-actions group actions 0 nil)
    ... )
    @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @fun{gtk:action-group-add-radio-actions} function has been deprecated
    since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action-group}
  @see-class{gtk:radio-action}
  @see-function{gtk:action-group-add-action}"
  (let ((last-action nil)
        (first-action nil))
    (dolist (entry entries)
      (let ((action (make-instance 'radio-action
                                   :name (first entry)
                                   :stock-id
                                   (or(second entry) (cffi:null-pointer))
                                   :label
                                   (or (third entry) (cffi:null-pointer))
                                   :tooltip
                                   (or (fifth entry) (cffi:null-pointer))
                                   :value (sixth entry)
                                   :active nil)))
        (unless first-action
          (setf first-action action))
        (radio-action-join-group action last-action)
        (setf last-action action)
        (if (= value (sixth entry))
            (setf (toggle-action-active action) t)
            (setf (toggle-action-active action) nil))
        (action-group-add-action group action (fourth entry))))
    (when on-change
      (g:signal-connect first-action "changed" on-change))))

(export 'action-group-add-radio-actions)

;;; ----------------------------------------------------------------------------
;;; GtkTranslateFunc ()                                    not exported
;;; ----------------------------------------------------------------------------

(cffi:defcallback translate-func (:string :free-to-foreign nil
                                          :free-from-foreign nil)
    ((path (:string :free-from-foreign nil))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func path)
      (return-untranslated () :report "Untranslated" path))))

#+liber-documentation
(setf (liber:alias-for-symbol 'translate-func)
      "Callback"
      (liber:symbol-documentation 'translate-func)
 "@version{#2025-07-02}
  @syntax{lambda (path) => result}
  @argument[path]{a string for the ID of the message}
  @argument[result]{a string for the translated message}
  @begin{short}
    A callback function used to translate messages.
  @end{short}
  @see-function{gtk:action-group-set-translate-func}")

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_set_translate_func ()                 not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_action_group_set_translate_func"
               %action-group-set-translate-func) :void
  (group (g:object action-group))
  (func :pointer)
  (data :pointer)
  (destroy :pointer))

(defun action-group-set-translate-func (group func)
 #+liber-documentation
 "@version{#2025-07-03}
  @argument[group]{a @class{gtk:action-group} object}
  @argument[func]{a @sym{gtk:translate-func} callback function}
  @begin{short}
    Sets a function to be used for translating the label and tooltip of
    action entires added by the @fun{gtk:action-group-add-actions} function.
  @end{short}
  If you are using GNU gettext, it is enough to set the translation domain with
  the @fun{gtk:action-group-set-translation-domain} function.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-group-set-translate-func} function has been deprecated
    since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action-group}
  @see-symbol{gtk:translate-func}
  @see-function{gtk:action-group-add-actions}
  @see-function{gtk:action-group-set-translation-domain}"
  (%action-group-set-translate-func
          group
          (cffi:callback translate-func)
          (glib:allocate-stable-pointer func)
          (cffi:callback glib:stable-pointer-destroy-notify)))

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_set_translation_domain                not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_action_group_set_translation_domain"
               action-group-set-translation-domain) :void
 #+liber-documentation
 "@version{#2025-07-02}
  @argument[group]{a @class{gtk:action-group} object}
  @argument[domain]{a string for the translation domain to use for
    GLIB gettext calls}
  @begin{short}
    Sets the translation domain and uses GLIB gettext for translating the label
    and tooltip of action entries added by the
    @fun{gtk:action-group-add-actions} function.
  @end{short}
  If you are not using GNU gettext for localization, see the
  @fun{gtk:action-group-set-translate-func} function.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-group-set-translate-domain} function has been deprecated
    since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action-group}
  @see-function{gtk:action-group-add-actions}
  @see-function{gtk:action-group-set-translate-func}"
  (group (g:object action-group))
  (domain :string))

;;; ----------------------------------------------------------------------------
;;; gtk_action_group_translate_string                      not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_action_group_translate_string"
               action-group-translate-string) (:string :free-from-foreign nil)
 #+liber-documentation
 "@version{#2025-07-02}
  @argument[group]{a @class{gtk:action-group} object}
  @argument[string]{a string}
  @return{The string with the translation.}
  @begin{short}
    Translates a string using the specified @sym{gtk:translate-func} callback
    function.
  @end{short}
  This is mainly intended for language bindings.
  @begin[Warning]{dictionary}
    The @fun{gtk:action-group-translate-string} function has been deprecated
    since version 3.10 and should not be used in newly written code.
  @end{dictionary}
  @see-class{gtk:action-group}
  @see-symbol{gtk:translate-func}"
  (group (g:object action-group))
  (string (:string :free-to-foreign nil)))

;;; --- End of file gtk3.action-group.lisp -------------------------------------
