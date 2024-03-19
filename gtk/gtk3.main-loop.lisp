;;; ----------------------------------------------------------------------------
;;; gtk3.main-loop.lisp
;;;
;;; The documentation of this file is taken from the GTK 3 Reference Manual
;;; Version 3.24 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk3/>.
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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
;;; Main loop and Events
;;;
;;;     Library initialization, main event loop, and events
;;;
;;; Types and Values
;;;
;;;     GTK_PRIORITY_RESIZE
;;;
;;; Functions
;;;
;;;     gtk_disable_setlocale                    not exported
;;;     gtk_get_default_language
;;;     gtk_get_locale_direction
;;;     gtk_parse_args                           not implemented
;;;     gtk_init                                 not exported
;;;     gtk_init_check                           not exported
;;;     gtk_init_with_args                       not implemented
;;;     gtk_get_option_group
;;;     gtk_events_pending
;;;     gtk_main
;;;     gtk_main_level
;;;     gtk_main_quit
;;;     gtk_main_iteration
;;;     gtk_main_iteration_do
;;;     gtk_main_do_event
;;;     gtk_true                                 not implemented
;;;     gtk_false                                not implemented
;;;     gtk_grab_add
;;;     gtk_grab_get_current
;;;     gtk_grab_remove
;;;     gtk_device_grab_add
;;;     gtk_device_grab_remove
;;;
;;;     gtk_key_snooper_install                  deprecated
;;;     gtk_key_snooper_remove                   deprecated
;;;
;;;     gtk_get_current_event
;;;     gtk_get_current_event_time
;;;     gtk_get_current_event_state
;;;     gtk_get_current_event_device
;;;     gtk_get_event_widget
;;;     gtk_propagate_event
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GTK_PRIORITY_RESIZE
;;;
;;; #define GTK_PRIORITY_RESIZE (G_PRIORITY_HIGH_IDLE + 10)
;;;
;;; Use this priority for functionality related to size allocation.
;;;
;;; It is used internally by GTK to compute the sizes of widgets. This priority
;;; is higher than GDK_PRIORITY_REDRAW to avoid resizing a widget which was just
;;; redrawn.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_disable_setlocale ()                               not exported
;;; ----------------------------------------------------------------------------

;; TODO: Because GTK is initialized, when loading the Lisp library, this
;; function should have no effect. We do not export the implementation.

(cffi:defcfun ("gtk_disable_setlocale" %disable-setlocale) :void
 #+liber-documentation
 "@version{#2012-12-23}
  @begin{short}
    Prevents @code{gtk_init()}, @code{gtk_init_check()},
    @code{gtk_init_with_args()} and @code{gtk_parse_args()} from automatically
    calling @code{setlocale (LC_ALL, \"\")}.
  @end{short}
  You would want to use this function if you wanted to set the locale for your
  program to something other than the user's locale, or if you wanted to set
  different values for different locale categories.

  Most programs should not need to call this function.
  @begin[Lisp Implemention]{dictionary}
    In the Lisp implementation the @code{gtk:%init} function is called
    automatically when loading the library @code{cl-cffi-gtk3}. Therefore
    @fun{gtk:disable-setlocale} does not have any effect.
  @end{dictionary}
  @see-function{gtk:%init-check}")

;;; ----------------------------------------------------------------------------
;;; gtk_get_default_language () -> default-language
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_get_default_language" default-language)
    (g:boxed pango:language)
 #+liber-documentation
 "@version{2023-3-5}
  @return{The default language as a @class{pango:language} instance.}
  @begin{short}
    Returns the Pango language instance for the default language currently in
    effect.
  @end{short}
  The default language is derived from the current locale. Note that this can
  change over the life of an application. It determines, for example, whether
  GTK uses the right-to-left or left-to-right text direction.

  This function is equivalent to the @fun{pango:language-default} function.
  @begin{examples}
    @begin{pre}
(setq lang (gtk:default-language))
=> #<PANGO-LANGUAGE {C7B3C51@}>
(pango:language-to-string lang)
=> \"de-de\"
    @end{pre}
  @end{examples}
  @see-class{pango:language}
  @see-function{pango:language-default}")

(export 'default-language)

;;; ----------------------------------------------------------------------------
;;; gtk_get_locale_direction () -> locale-direction
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_get_locale_direction" locale-direction) text-direction
 #+liber-documentation
 "@version{2023-3-5}
  @return{The @symbol{gtk:text-direction} value with the current locale.}
  @begin{short}
    Gets the direction of the current locale.
  @end{short}
  This is the expected reading direction for text and UI.

  This function depends on the current locale and will default to setting the
  @code{:ltr} direction of the @symbol{gtk:text-direction} enumeration
  otherwise. The value @code{:none} will never be returned.

  GTK sets the default text direction according to the locale during the
  execution of the @code{gtk_init()} function, and you should normally use the
  @fun{gtk:widget-direction} or @fun{gtk:widget-default-direction} function to
  obtain the current direction.

  This function is only needed rare cases when the locale is changed after GTK
  has already been initialized.
  @begin{examples}
    You can use the @fun{gtk:locale-direction} function to update the default
    text direction as follows:
    @begin{pre}
(setf (gtk:widget-default-direction) (gtk:locale-direction))
=> :LTR
    @end{pre}
  @end{examples}
  @see-symbol{gtk:text-direction}
  @see-function{gtk:widget-default-direction}")

(export 'locale-direction)

;;; ----------------------------------------------------------------------------
;;; gtk_parse_args ()
;;;
;;; gboolean gtk_parse_args (int *argc, char ***argv);
;;;
;;; Parses command line arguments, and initializes global attributes of GTK,
;;; but does not actually open a connection to a display. (See
;;; gdk_display_open(), gdk_get_display_arg_name())
;;;
;;; Any arguments used by GTK or GDK are removed from the array and argc and
;;; argv are updated accordingly.
;;;
;;; There is no need to call this function explicitely if you are using
;;; gtk_init(), or gtk_init_check().
;;;
;;; argc :
;;;     a pointer to the number of command line arguments
;;;
;;; argv :
;;;     a pointer to the array of command line arguments
;;;
;;; Returns :
;;;     TRUE if initialization succeeded, otherwise FALSE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_init ()                                            not exported
;;; ----------------------------------------------------------------------------

;;; TODO: The function is for internal use and not exported.
;;; Rework the handling of command line parameters in this function and
;;; the function %gtk-init-check.

(defun %gtk-init ()
 #+liber-documentation
 "@version{#2012-12-23}
  @argument[argc]{Address of the @arg{argc} parameter of your @code{main()}
    function (or 0 if @arg{argv} is @code{NULL}). This will be changed if any
    arguments were handled.}
  @argument[argv]{Address of the @arg{argv} parameter of @code{main()}, or
    @code{NULL}. Any options understood by GTK are stripped before return.}
  @begin{short}
    Call this function before using any other GTK functions in your GUI
    applications.
  @end{short}
  It will initialize everything needed to operate the toolkit and parses some
  standard command line options.

  Although you are expected to pass the @arg{argc}, @arg{argv} parameters from
  @code{main()} to this function, it is possible to pass @code{NULL} if
  @arg{argv} is not available or command line handling is not required.

  @arg{argc} and @arg{argv} are adjusted accordingly so your own code will
  never see those standard arguments.

  Note that there are some alternative ways to initialize GTK: if you are
  calling @code{gtk_parse_args()}, @fun{%gtk-init-check},
  @code{gtk_init_with_args()} or @code{g_option_context_parse()} with the
  option group returned by the @fun{gtk:option-group} function, you do not have
  to call the @code{gtk_init()} function.
  @begin{notes}
    This function will terminate your program if it was unable to initialize the
    windowing system for some reason. If you want your program to fall back to a
    textual interface you want to call @fun{%gtk-init-check} instead.

    Since 2.18, GTK calls the @code{(SIGPIPE, SIG_IGN)} signal during
    initialization, to ignore @code{SIGPIPE} signals, since these are almost
    never wanted in graphical applications. If you do need to handle
    @code{SIGPIPE} for some reason, reset the handler after @code{gtk_init()},
    butt notice that other libraries (e.g. @code{libdbus} or @code{gvfs}) might
    do similar things.
  @end{notes}
  @begin[Lisp Implemention]{dictionary}
    In the Lisp implementation @code{%gtk-init} calls the
    @code{gtk_init_check()} C function which is implemented through the
    @fun{%gtk-init-check} function. Both functions are never called directly.
    The @code{%gtk-init} function is called automatically when loading the
    @code{cl-cffi-gtk3} library.
  @end{dictionary}
  @see-function{%gtk-init-check}"
  (%gtk-init-check (cffi:foreign-alloc :int :initial-element 0)
                   (cffi:foreign-alloc :string :initial-contents
                                       '("/usr/bin/sbcl")))
  #+(and nil sbcl (not win32))
  (sb-unix::enable-interrupt sb-unix:sigpipe #'sb-unix::sigpipe-handler)
  #+nil(cffi:with-foreign-objects ((argc :int)
                         (argv '(:pointer :string) 1))
    (setf (cffi:mem-ref argc :int) 0
          (cffi:mem-ref argv '(:pointer :string))
          (cffi:foreign-alloc :string
                              :count 1
                              :initial-element "/usr/bin/sbcl"))
    (unwind-protect
      (unless (%gtk-init-check argc argv)
        (error "Cannot initialize GTK"))
      (cffi:foreign-free (cffi:mem-ref argv '(:pointer :string))))))

;;; ----------------------------------------------------------------------------
;;; gtk_init_check ()
;;; ----------------------------------------------------------------------------

;;; TODO: The function is for internal use and not exported.
;;; Rework the handling of command line parameters in this function and
;;; the function %gtk-init.

(cffi:defcfun ("gtk_init_check" %gtk-init-check) :boolean
 #+liber-documentation
 "@version{#2012-12-23}
  @argument[argc]{Address of the @code{argc} parameter of your @code{main()}
    function (or 0 if @code{argv} is @code{NULL}). This will be changed if any
    arguments were handled.}
  @argument[argv]{Address of the @arg{argv} parameter of @code{main()}, or
    @code{nil}. Any options understood by GTK are stripped before return.}
  @return{@em{true} if the windowing system has been successfully initialized,
    @code{nil} otherwise}
  @begin{short}
    This function does the same work as gtk_init() with only a single change: It
    does not terminate the program if the windowing system can't be initialized.
    Instead it returns FALSE on failure.
  @end{short}

  This way the application can fall back to some other means of communication
  with the user - for example a curses or command line interface.
  @begin[Lisp Implemention]{dictionary}
    In the Lisp implementation @code{%gtk-init-check} is called from the
    @code{%gtk-init} function. Both functions are never called directly. The
    @code{%gtk-init} function is called automatically when loading the
    @code{cl-cffi-gtk3} library.
  @end{dictionary}"
  (argc (:pointer :int))
  (argv (:pointer (:pointer :string))))

;;; ----------------------------------------------------------------------------
;;; gtk_init_with_args ()
;;;
;;; gboolean gtk_init_with_args (gint *argc,
;;;                              gchar ***argv,
;;;                              const gchar *parameter_string,
;;;                              const GOptionEntry *entries,
;;;                              const gchar *translation_domain,
;;;                              GError **error);
;;;
;;; This function does the same work as gtk_init_check(). Additionally, it
;;; allows you to add your own command line options, and it automatically
;;; generates nicely formatted --help output. Note that your program will be
;;; terminated after writing out the help output.
;;;
;;; argc :
;;;     Address of the argc parameter of your main() function (or 0 if argv is
;;;     NULL). This will be changed if any arguments were handled.
;;;
;;; argv :
;;;     Address of the argv parameter of main(), or NULL. Any options understood
;;;     by GTK are stripped before return.
;;;
;;; parameter_string :
;;;     a string which is displayed in the first line of --help output, after
;;;     programname [OPTION...]
;;;
;;; entries :
;;;     a NULL-terminated array of GOptionEntrys describing the options of your
;;;     program
;;;
;;; translation_domain :
;;;     a translation domain to use for translating the --help output for the
;;;     options in entries and the parameter_string with gettext(), or NULL
;;;
;;; error :
;;;     a return location for errors
;;;
;;; Returns :
;;;     TRUE if the windowing system has been successfully initialized, FALSE
;;;     otherwise
;;;
;;; Since 2.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_get_option_group () -> option-group
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_get_option_group" option-group)
    (:pointer (:struct g:option-group))
 #+liber-documentation
 "@version{#2023-3-5}
  @argument[default]{a boolean whether to open the default display when parsing
    the command line arguments}
  @begin{return}
    A @type{g:option-group} instance for the command line arguments recognized
    by GTK.
  @end{return}
  @begin{short}
    Returns an option group for the command line arguments recognized by GTK
    and GDK.
  @end{short}
  You should add this group to your @type{g:option-context} instance with the
  @fun{g:option-context-add-group} function, if you are using the
  @fun{g:option-context-parse} function to parse your command line arguments.
  @see-type{g:option-group}
  @see-type{g:option-context}
  @see-function{g:option-context-add-group}
  @see-function{g:option-context-parse}"
  (default :boolean))

(export 'option-group)

;;; ----------------------------------------------------------------------------
;;; gtk_events_pending ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_events_pending" events-pending) :boolean
 #+liber-documentation
 "@version{#2023-3-5}
  @return{@em{True} if any events are pending, @em{false} otherwise.}
  @begin{short}
    Checks if any events are pending.
  @end{short}
  This can be used to update the UI and invoke timeouts etc. while doing some
  time intensive computation.
  @begin{examples}
    Updating the UI during a long computation.
    @begin{pre}
;; computation going on ...
(loop while (gtk:events-pending)
      do (gtk:main-iteration))
;; ... computation continued
    @end{pre}
  @end{examples}
  @see-function{gtk:main-iteration}
  @see-function{gtk:main-iteration-do}")

(export 'events-pending)

;;; ----------------------------------------------------------------------------
;;; gtk_main ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_main" %main) :void)

(defun main ()
 #+liber-documentation
 "@version{#2023-3-5}
  @begin{short}
    Runs the main loop until the @fun{gtk:main-quit} function is called.
  @end{short}
  You can nest calls to the @fun{gtk:main} function. In that case the
  @fun{gtk:main-quit} function will make the innermost invocation of the main
  loop return.
  @begin[Lisp Implementation]{dictionary}
    In the Lisp binding to GTK the @fun{gtk:main} function is not called
    directly but through the @fun{gtk:within-main-loop} macro. The
    @fun{gtk:within-main-loop} macro does some additional bookkeeping, to run
    the Lisp program in a separate thread.
  @end{dictionary}
  @begin{examples}
    In this example an idle source is excecuted from the main loop. The
    @fun{gtk:main-quit} function is called in the idle callback to quit the
    main loop.
    @begin{pre}
(defun main-idle-cb ()
  (format t \"~&Execute main-idle-cb in level ~A.~%\" (gtk:main-level))
  ;; Quit the main loop.
  (gtk:main-quit)
  ;; Remove the idle source.
  glib:+g-source-remove+)

(defun main ()
  ;; Add an idle source to the main loop.
  (g:idle-add #'main-idle-cb)
  ;; Start the main loop.
  ;; We return when gtk:main-quit is called in the idle callback.
  (gtk:main))
    @end{pre}
  @end{examples}
  @see-function{gtk:within-main-loop}
  @see-function{gtk:main-quit}"
  (gdk:with-gdk-threads-lock
    (%main)))

(export 'main)

;;; ----------------------------------------------------------------------------
;;; gtk_main_level ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_main_level" main-level) :uint
 #+liber-documentation
 "@version{#2023-3-5}
  @return{The unsigned integer with the nesting level of the current invocation
    of the main loop.}
  @begin{short}
    Asks for the current nesting level of the main loop.
  @end{short}
  @see-function{gtk:main}")

(export 'main-level)

;;; ----------------------------------------------------------------------------
;;; gtk_main_quit ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_main_quit" main-quit) :void
 #+liber-documentation
 "@version{#2023-3-5}
  @begin{short}
    Makes the innermost invocation of the main loop return when it regains
    control.
  @end{short}
  See the @fun{gtk:main} function for an example.
  @begin[Lisp Implementation]{dictionary}
    In the Lisp binding to GTK the @fun{gtk:main-quit} function is not called,
    but the @fun{gtk:leave-gtk-main} function. The @fun{gtk:leave-gtk-main}
    function does some additional bookkeeping, which is necessary to destroy
    the separate thread for a Lisp program.
  @end{dictionary}
  @see-function{gtk:main}
  @see-function{gtk:within-main-loop}
  @see-function{gtk:leave-gtk-main}")

(export 'main-quit)

;;; ----------------------------------------------------------------------------
;;; gtk_main_iteration ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_main_iteration" main-iteration) :boolean
 #+liber-documentation
 "@version{#2023-3-5}
  @return{@em{True} if the @fun{gtk:main-quit} function has been called for the
    innermost main loop.}
  @short{Runs a single iteration of the main loop.}
  If no events are waiting to be processed GTK will block until the next
  event is noticed. If you do not want to block look at the
  @fun{gtk:main-iteration-do} function or check if any events are pending with
  the @fun{gtk:events-pending} function first.
  @see-function{gtk:main-quit}
  @see-function{gtk:events-pending}
  @see-function{gtk:main-iteration-do}")

(export 'main-iteration)

;;; ----------------------------------------------------------------------------
;;; gtk_main_iteration_do ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_main_iteration_do" main-iteration-do) :boolean
 #+liber-documentation
 "@version{#2023-3-5}
  @argument[blocking]{@em{true} if you want GTK to block if no events are
    pending}
  @return{@em{True} if the @fun{gtk:main-quit} function has been called for the
    innermost main loop.}
  @begin{short}
    Runs a single iteration of the main loop.
  @end{short}
  If no events are available either return or block depending on the value of
  @arg{blocking}.
  @see-function{gtk:main-iteration}
  @see-function{gtk:main-quit}"
  (blocking :boolean))

(export 'main-iteration-do)

;;; ----------------------------------------------------------------------------
;;; gtk_main_do_event ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_main_do_event" main-do-event) :void
 #+liber-documentation
 "@version{#2023-3-5}
  @argument[event]{a @class{gdk:event} instance to process normally passed by
    GDK}
  @begin{short}
    Processes a single GDK event.
  @end{short}
  This is public only to allow filtering of events between GDK and GTK. You
  will not usually need to call this function directly.

  While you should not call this function directly, you might want to know how
  exactly events are handled. So here is what this function does with the
  event:
  @begin{itemize}
    @begin{item}
      Compress enter/leave notify events. If the event passed build an
      enter/leave pair together with the next event peeked from GDK, both
      events are thrown away. This is to avoid a backlog of (de-)highlighting
      widgets crossed by the pointer.
    @end{item}
    @begin{item}
      Find the widget which got the event. If the widget cannot be determined
      the event is thrown away unless it belongs to a @code{INCR} transaction.
      In that case it is passed to the @code{gtk_selection_incr_event()}
      function.
    @end{item}
    @begin{item}
      Then the event is pushed onto a stack so you can query the currently
      handled event with the @fun{gtk:current-event} function.
    @end{item}
    @begin{item}
      The event is sent to a widget. If a grab is active all events for
      widgets that are not in the contained grab widget are sent to the
      latter with a few exceptions:
      @begin{itemize}
        @begin{item}
          Deletion and destruction events are still sent to the event widget
          for obvious reasons.
        @end{item}
        @begin{item}
          Events which directly relate to the visual representation of the
          event widget.
        @end{item}
        @begin{item}
          Leave events are delivered to the event widget if there was an enter
          event delivered to it before without the paired leave event.
        @end{item}
        @begin{item}
          Drag events are not redirected because it is unclear what the
          semantics of that would be.
        @end{item}
      @end{itemize}
    @end{item}
    @begin{item}
      Another point of interest might be that all key events are first passed
      through the key snooper functions if there are any. Read the description
      of the @code{gtk_key_snooper_install()} function if you need this feature.
    @end{item}
    @begin{item}
      After finishing the delivery the event is popped from the event stack.
    @end{item}
  @end{itemize}
  @see-function{gtk:main-iteration}
  @see-function{gtk:main-iteration-do}
  @see-function{gtk:current-event}"
  (event (g:boxed gdk:event)))

(export 'main-do-event)

;;; ----------------------------------------------------------------------------
;;; GtkModuleInitFunc ()
;;;
;;; void (*GtkModuleInitFunc) (gint *argc, gchar ***argv);
;;;
;;; Each GTK module must have a function gtk_module_init() with this prototype.
;;; This function is called after loading the module.
;;;
;;; argc :
;;;     GTK always passes NULL for this argument. [allow-none]
;;;
;;; argv :
;;;     GTK always passes NULL for this argument
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkModuleDisplayInitFunc ()
;;;
;;; void (*GtkModuleDisplayInitFunc) (GdkDisplay *display);
;;;
;;; A multihead-aware GTK module may have a gtk_module_display_init() function
;;; with this prototype. GTK calls this function for each opened display.
;;;
;;; display :
;;;     an open GdkDisplay
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_true ()
;;;
;;; gboolean gtk_true (void);
;;;
;;; All this function does it to return TRUE.
;;;
;;; This can be useful for example if you want to inhibit the deletion of a
;;; window. Of course you should not do this as the user expects a reaction from
;;; clicking the close icon of the window...
;;;
;;; Example 8. A persistent window
;;;
;;;   #include <gtk/gtk.h><
;;;
;;;   int
;;;   main (int argc, char **argv)
;;;   {
;;;     GtkWidget *win, *but;
;;;
;;;     gtk_init (&argc, &argv);
;;;
;;;     win = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;;;     g_signal_connect (win, "delete-event",
;;;                       G_CALLBACK (gtk_true), NULL);
;;;     g_signal_connect (win, "destroy",
;;;                       G_CALLBACK (gtk_main_quit), NULL);
;;;
;;;     but = gtk_button_new_with_label ("Close yourself. I mean it!");
;;;     g_signal_connect_swapped (but, "clicked",
;;;                               G_CALLBACK (gtk_object_destroy), win);
;;;     gtk_container_add (GTK_CONTAINER (win), but);
;;;
;;;     gtk_widget_show_all (win);
;;;
;;;     gtk_main ();
;;;
;;;     return 0;
;;;   }
;;;
;;; Returns :
;;;     TRUE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_false ()
;;;
;;; gboolean gtk_false (void);
;;;
;;; Analogical to gtk_true(), this function does nothing but always returns
;;; FALSE.
;;;
;;; Returns :
;;;     FALSE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_grab_add ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_grab_add" grab-add) :void
 #+liber-documentation
 "@version{#2023-3-5}
  @argument[widget]{a @class{gtk:widget} widget that grabs keyboard and
    pointer events}
  @short{Makes @arg{widget} the current grabbed widget.}
  This means that interaction with other widgets in the same application is
  blocked and mouse as well as keyboard events are delivered to this
  @arg{widget}. If @arg{widget} is not sensitive, it is not set as the current
  grabbed widget and this function does nothing.
  @see-class{gtk:widget}
  @see-function{gtk:grab-remove}
  @see-function{gtk:grab-current}
  @see-function{gtk:device-grab-add}"
  (widget (g:object widget)))

(export 'grab-add)

;;; ----------------------------------------------------------------------------
;;; gtk_grab_get_current () -> grab-current
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_grab_get_current" grab-current) (g:object widget)
 #+liber-documentation
 "@version{#2023-3-5}
  @return{The @class{gtk:widget} widget which currently has the grab or
    @code{nil} if no grab is active.}
  @begin{short}
    Queries the current grab of the default window group.
  @end{short}
  @see-class{gtk:widget}
  @see-function{gtk:grab-add}
  @see-function{gtk:grab-remove}
  @see-function{gtk:device-grab-add}")

(export 'grab-current)

;;; ----------------------------------------------------------------------------
;;; gtk_grab_remove ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_grab_remove" grab-remove) :void
 #+liber-documentation
 "@version{#2023-3-5}
  @argument[widget]{a @class{gtk:widget} widget which gives up the grab}
  @short{Removes the grab from the given @arg{widget}.}
  You have to pair calls to the @fun{gtk:grab-add} and @fun{gtk:grab-remove}
  functions. If @arg{widget} does not have the grab, this function does nothing.
  @see-class{gtk:widget}
  @see-function{gtk:grab-add}
  @see-function{gtk:grab-current}"
  (widget g:object))

(export 'grab-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_device_grab_add ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_device_grab_add" device-grab-add) :void
 #+liber-documentation
 "@version{#2023-3-5}
  @argument[widget]{a @class{gtk:widget} widget}
  @argument[device]{a @class{gdk:device} object to grab on}
  @argument[blocking]{@em{true} to prevent other devices to interact with
    @arg{widget}}
  @begin{short}
    Adds a grab on the device, so all the events on the device and its
    associated pointer or keyboard (if any) are delivered to the widget.
  @end{short}
  If the @arg{blocking} parameter is @em{true}, any other devices will be
  unable to interact with @arg{widget} during the grab.
  @see-class{gtk:widget}
  @see-class{gdk:device}
  @see-function{gtk:grab-add}
  @see-function{gtk:grab-current}
  @see-function{gtk:device-grab-remove}"
  (widget (g:object widget))
  (device (g:object gdk:device))
  (blocking :boolean))

(export 'device-grab-add)

;;; ----------------------------------------------------------------------------
;;; gtk_device_grab_remove ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_device_grab_remove" device-grab-remove) :void
 #+liber-documentation
 "@version{#2023-3-5}
  @argument[widget]{a @class{gtk:widget} widget}
  @argument[device]{a @class{gdk:device} object}
  @begin{short}
    Removes a device grab from the given widget.
  @end{short}
  You have to pair calls to the @fun{gtk:device-grab-add} and
  @fun{gtk:device-grab-remove} functions.
  @see-class{gtk:widget}
  @see-class{gdk:device}
  @see-function{gtk:device-grab-add}
  @see-function{gtk:grab-current}"
  (widget (g:object widget))
  (device (g:object gdk:device)))

(export 'device-grab-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_key_snooper_install ()
;;;
;;; guint gtk_key_snooper_install (GtkKeySnoopFunc snooper, gpointer func_data);
;;;
;;; Warning
;;;
;;; gtk_key_snooper_install has been deprecated since version 3.4 and should not
;;; be used in newly written code. Key snooping should not be done. Events
;;; should be handled by widgets.
;;;
;;; Installs a key snooper function, which will get called on all key events
;;; before delivering them normally.
;;;
;;; snooper :
;;;     a GtkKeySnoopFunc
;;;
;;; func_data :
;;;     data to pass to snooper
;;;
;;; Returns :
;;;     a unique id for this key snooper for use with gtk_key_snooper_remove().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkKeySnoopFunc ()
;;;
;;; gint (*GtkKeySnoopFunc) (GtkWidget *grab_widget,
;;;                          GdkEventKey *event,
;;;                          gpointer func_data);
;;;
;;; Key snooper functions are called before normal event delivery. They can be
;;; used to implement custom key event handling.
;;;
;;; grab_widget :
;;;     the widget to which the event will be delivered
;;;
;;; event :
;;;     the key event
;;;
;;; func_data :
;;;     data supplied to gtk_key_snooper_install()
;;;
;;; Returns :
;;;     TRUE to stop further processing of event, FALSE to continue.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_key_snooper_remove ()
;;;
;;; void gtk_key_snooper_remove (guint snooper_handler_id);
;;;
;;; Warning
;;;
;;; gtk_key_snooper_remove has been deprecated since version 3.4 and should not
;;; be used in newly written code. Key snooping should not be done. Events
;;; should be handled by widgets.
;;;
;;; Removes the key snooper function with the given id.
;;;
;;; snooper_handler_id :
;;;     Identifies the key snooper to remove
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_get_current_event () -> current-event
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_get_current_event" current-event)
    (g:boxed gdk:event :return)
 #+liber-documentation
 "@version{#2023-3-5}
  @return{The copy of the current @class{gdk:event} instance, or @code{nil} if
    there is no current event.}
  @short{Obtains a copy of the event currently being processed by GTK.}
  For example, if you are handling a @code{\"clicked\"} signal, the current
  event will be the @class{gdk:event-button} event that triggered the
  @code{\"clicked\"} signal.
  @begin{examples}
    In this example the @fun{gtk:current-event} function is used in a signal
    handler to check for a button press event. This code is part of the GTK
    demo for popovers.
    @begin{pre}
(g:signal-connect calendar \"day-selected\"
    (lambda (calendar)
      (let ((event (gtk:current-event)))
        (when (eq :button-press (gdk:event-type event))
          (multiple-value-bind (x y)
              (gdk:window-coords-to-parent (gdk:event-window event)
                                           (gdk:event-button-x event)
                                           (gdk:event-button-y event))
            (let ((rect (gtk:widget-allocation calendar)))
              (setf (gdk:rectangle-x rect)
                    (- (truncate x) (gdk:rectangle-x rect)))
              (setf (gdk:rectangle-y rect)
                    (- (truncate y) (gdk:rectangle-y rect)))
              (setf (gdk:rectangle-width rect) 1)
              (setf (gdk:rectangle-height rect) 1)
              (let ((popover (create-popover calendar
                                             (make-instance 'gtk:entry)
                                             :bottom)))
                (setf (gtk:popover-pointing-to popover) rect)
                (gtk:widget-show popover))))))))
    @end{pre}
  @end{examples}
  @see-class{gdk:event}
  @see-function{gtk:current-event-time}
  @see-function{gtk:current-event-state}
  @see-function{gtk:current-event-device}")

(export 'current-event)

;;; ----------------------------------------------------------------------------
;;; gtk_get_current_event_time () -> current-event-time
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_get_current_event_time" current-event-time) :uint32
 #+liber-documentation
 "@version{#2023-3-5}
  @return{The unsigned integer with the timestamp from the current event, or the
    @var{gdk:+gdk-current-time+} value.}
  @begin{short}
    If there is a current event and it has a timestamp, return that timestamp.
  @end{short}
  Otherwise return the @var{gdk:+gdk-current-time+} value.
  @see-function{gtk:current-event}
  @see-variable{gdk:+gdk-current-time+}")

(export 'current-event-time)

;;; ----------------------------------------------------------------------------
;;; gtk_get_current_event_state () -> current-event-state
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_get_current_event_state" %current-event-state) :boolean
  (state (:pointer gdk:modifier-type)))

(defun current-event-state ()
 #+liber-documentation
 "@version{#2023-3-5}
  @begin{return}
    The state as a value of the @symbol{gdk:modifier-type} flags of the current
    event or @code{nil} if there is no current event.
  @end{return}
  @begin{short}
    If there is a current event and it has a state field, return that state
    field, otherwise return @code{nil}.
  @end{short}
  @see-symbol{gdk:modifier-type}
  @see-function{gtk:current-event}"
  (cffi:with-foreign-object (state 'gdk:modifier-type)
    (when (%current-event-state state)
      (cffi:mem-ref state 'gdk:modifier-type))))

(export 'current-event-state)

;;; ----------------------------------------------------------------------------
;;; gtk_get_current_event_device () -> current-event-device
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_get_current_event_device" current-event-device)
    (g:object gdk:device)
 #+liber-documentation
 "@version{#2023-3-5}
  @return{The @class{gdk:device} object, or @code{nil}.}
  @begin{short}
    If there is a current event and it has a device, return that device,
    otherwise return @code{nil}.
  @end{short}
  @see-class{gdk:device}
  @see-function{gtk:current-event}")

(export 'current-event-device)

;;; ----------------------------------------------------------------------------
;;; gtk_get_event_widget () -> event-widget
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_get_event_widget" event-widget) (g:object widget)
 #+liber-documentation
 "@version{#2023-3-5}
  @argument[event]{a @class{gdk:event} instance}
  @return{The @class{gtk:widget} widget that originally received @arg{event},
    or @code{nil}.}
  @begin{short}
    If @arg{event} is @code{nil} or @arg{event} was not associated with any
    widget, returns @code{nil}, otherwise returns the widget that received
    @arg{event} originally.
  @end{short}
  @see-class{gdk:event}
  @see-class{gtk:widget}
  @see-function{gtk:propagate-event}"
  (event (g:boxed gdk:event)))

(export 'event-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_propagate_event ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_propagate_event" propagate-event) :void
 #+liber-documentation
 "@version{#2023-3-5}
  @argument[widget]{a @class{gtk:widget} widget}
  @argument[event]{a @class{gdk:event} instance}
  @begin{short}
    Sends an event to a widget, propagating the event to parent widgets if the
    event remains unhandled.
  @end{short}
  Events received by GTK from GDK normally begin in the @fun{gtk:main-do-event}
  function. Depending on the type of event, existence of modal dialogs, grabs,
  etc., the event may be propagated, if so, this function is used.

  The @fun{gtk:propagate-event} function calls the @fun{gtk:widget-event}
  function on each widget it decides to send the event to. So the
  @fun{gtk:widget-event} function is the lowest level function. It simply emits
  the event and possibly an event specific signal on a widget. The
  @fun{gtk:propagate-event} function is a bit higher-level, and the
  @fun{gtk:main-do-event} function is the highest level.

  All that said, you most likely do not want to use any of these functions.
  Synthesizing events is rarely needed. There are almost certainly better ways
  to achieve your goals. For example, use the @fun{gdk:window-invalidate-rect}
  or @fun{gtk:widget-queue-draw} functions instead of making up expose events.
  @see-class{gtk:widget}
  @see-class{gdk:event}
  @see-function{gtk:widget-event}
  @see-function{gtk:main-do-event}
  @see-function{gdk:window-invalidate-rect}
  @see-function{gtk:widget-queue-draw}"
  (widget g:object)
  (event (g:boxed gdk:event)))

(export 'propagate-event)

;;; --- End of file gtk3.main.loop.lisp ----------------------------------------
