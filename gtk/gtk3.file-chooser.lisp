;;; ----------------------------------------------------------------------------
;;; gtk3.file-chooser.lisp
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
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------
;;;
;;; GtkFileChooser
;;;
;;;     File chooser interface used by GtkFileChooserWidget and
;;;     GtkFileChooserDialog
;;;
;;; Types and Values
;;;
;;;     GtkFileChooser
;;;     GtkFileChooserAction
;;;     GtkFileChooserConfirmation
;;;     GtkFileChooserError
;;;
;;; Functions
;;;
;;;     gtk_file_chooser_set_action                        Accessor
;;;     gtk_file_chooser_get_action                        Accessor
;;;     gtk_file_chooser_set_local_only                    Accessor
;;;     gtk_file_chooser_get_local_only                    Accessor
;;;     gtk_file_chooser_set_select_multiple               Accessor
;;;     gtk_file_chooser_get_select_multiple               Accessor
;;;     gtk_file_chooser_set_show_hidden                   Accessor
;;;     gtk_file_chooser_get_show_hidden                   Accessor
;;;     gtk_file_chooser_set_do_overwrite_confirmation     Accessor
;;;     gtk_file_chooser_get_do_overwrite_confirmation     Accessor
;;;     gtk_file_chooser_set_create_folders                Accessor
;;;     gtk_file_chooser_get_create_folders                Accessor
;;;     gtk_file_chooser_set_current_name
;;;     gtk_file_chooser_get_current_name
;;;     gtk_file_chooser_get_filename
;;;     gtk_file_chooser_set_filename
;;;     gtk_file_chooser_select_filename
;;;     gtk_file_chooser_unselect_filename
;;;     gtk_file_chooser_select_all
;;;     gtk_file_chooser_unselect_all
;;;     gtk_file_chooser_get_filenames
;;;     gtk_file_chooser_set_current_folder
;;;     gtk_file_chooser_get_current_folder
;;;     gtk_file_chooser_get_uri
;;;     gtk_file_chooser_set_uri
;;;     gtk_file_chooser_select_uri
;;;     gtk_file_chooser_unselect_uri
;;;     gtk_file_chooser_get_uris
;;;     gtk_file_chooser_set_current_folder_uri
;;;     gtk_file_chooser_get_current_folder_uri
;;;     gtk_file_chooser_set_preview_widget                Accessor
;;;     gtk_file_chooser_get_preview_widget                Accessor
;;;     gtk_file_chooser_set_preview_widget_active         Accessor
;;;     gtk_file_chooser_get_preview_widget_active         Accessor
;;;     gtk_file_chooser_set_use_preview_label             Accessor
;;;     gtk_file_chooser_get_use_preview_label             Accessor
;;;     gtk_file_chooser_get_preview_filename
;;;     gtk_file_chooser_get_preview_uri
;;;     gtk_file_chooser_set_extra_widget                  Accessor
;;;     gtk_file_chooser_get_extra_widget                  Accessor
;;;     gtk_file_chooser_add_filter
;;;     gtk_file_chooser_remove_filter
;;;     gtk_file_chooser_list_filters
;;;     gtk_file_chooser_set_filter                        Accessor
;;;     gtk_file_chooser_get_filter                        Accessor
;;;     gtk_file_chooser_add_shortcut_folder
;;;     gtk_file_chooser_remove_shortcut_folder
;;;     gtk_file_chooser_list_shortcut_folders
;;;     gtk_file_chooser_add_shortcut_folder_uri
;;;     gtk_file_chooser_remove_shortcut_folder_uri
;;;     gtk_file_chooser_list_shortcut_folder_uris
;;;     gtk_file_chooser_get_current_folder_file
;;;     gtk_file_chooser_get_file
;;;     gtk_file_chooser_get_files
;;;     gtk_file_chooser_get_preview_file
;;;     gtk_file_chooser_select_file
;;;     gtk_file_chooser_set_current_folder_file
;;;     gtk_file_chooser_set_file
;;;     gtk_file_chooser_unselect_file
;;;
;;; Properties
;;;
;;;     action
;;;     create-folders
;;;     do-overwrite-confirmation
;;;     extra-widget
;;;     filter
;;;     local-only
;;;     preview-widget
;;;     preview-widget-active
;;;     select-multiple
;;;     show-hidden
;;;     use-preview-label
;;;
;;; Signals
;;;
;;;     confirm-overwrite
;;;     current-folder-changed
;;;     file-activated
;;;     selection-changed
;;;     update-preview
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkFileChooser
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFileChooserAction
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkFileChooserAction" file-chooser-action
  (:export t
   :type-initializer "gtk_file_chooser_action_get_type")
  (:open 0)
  (:save 1)
  (:select-folder 2)
  (:create-folder 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'file-chooser-action)
      "GEnum"
      (liber:symbol-documentation 'file-chooser-action)
 "@version{2024-3-21}
  @begin{declaration}
(gobject:define-genum \"GtkFileChooserAction\" file-chooser-action
  (:export t
   :type-initializer \"gtk_file_chooser_action_get_type\")
  (:open 0)
  (:save 1)
  (:select-folder 2)
  (:create-folder 3))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:open]{Indicates Open mode. The file chooser will only let the
        user pick an existing file.}
      @entry[:save]{Indicates Save mode. The file chooser will let the user
        pick an existing file, or type in a new filename.}
      @entry[:select-folder]{Indicates an Open mode for selecting folders. The
        file chooser will let the user pick an existing folder.}
      @entry[:create-folder]{Indicates a mode for creating a new folder. The
        file chooser will let the user name an existing or new folder.}
    @end{table}
  @end{values}
  @begin{short}
    Describes whether a @class{gtk:file-chooser} widget is being used to
    open existing files or to save to a possibly new file.
  @end{short}
  @see-class{gtk:file-chooser}")

;;; ----------------------------------------------------------------------------
;;; GtkFileChooserConfirmation
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkFileChooserConfirmation" file-chooser-confirmation
  (:export t
   :type-initializer "gtk_file_chooser_confirmation_get_type")
  (:confirm 0)
  (:accept-filename 1)
  (:select-again 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'file-chooser-confirmation)
      "GEnum"
      (liber:symbol-documentation 'file-chooser-confirmation)
 "@version{2024-3-22}
  @begin{declaration}
(gobject:define-genum \"GtkFileChooserConfirmation\" gtk:file-chooser-confirmation
  (:export t
   :type-initializer \"gtk_file_chooser_confirmation_get_type\")
  (:confirm 0)
  (:accept-filename 1)
  (:select-again 2))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:confirm]{The file chooser will present its stock dialog to confirm
        about overwriting an existing file.}
      @entry[:accept-filename]{The file chooser will terminate and accept the
        user's choice of a file name.}
      @entry[:select-again]{The file chooser will continue running, so as to let
        the user select another file name.}
    @end{table}
  @end{values}
  @begin{short}
    Used as a return value of handlers for the @code{\"confirm-overwrite\"}
    signal of a @class{gtk:file-chooser} widget.
  @end{short}
  This value determines whether the file chooser will present the stock
  confirmation dialog, accept the user's choice of a filename, or let the user
  choose another filename.
  @class{gtk:file-chooser}")

;;; ----------------------------------------------------------------------------
;;; GtkFileChooserError
;;; ----------------------------------------------------------------------------

;; GtkFileChooserError is not exported

(gobject:define-genum "GtkFileChooserError" file-chooser-error
  (:export nil
   :type-initializer "gtk_file_chooser_error_get_type")
  (:nonexistent 0)
  (:bad-filename 1)
  (:already-exists 2)
  (:incomplete-hostname 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'file-chooser-error)
      "GEnum"
      (liber:symbol-documentation 'file-chooser-error)
 "@version{#2024-3-21}
  @begin{declaration}
(gobject:define-genum \"GtkFileChooserError\" file-chooser-error
  (:export t
   :type-initializer \"gtk_file_chooser_error_get_type\")
  (:nonexistent 0)
  (:bad-filename 1)
  (:already-exists 2)
  (:incomplete-hostname 3))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:nonexistent]{Indicates that a file does not exist.}
      @entry[:bad-filename]{Indicates a malformed filename.}
      @entry[:already-exists]{Indicates a duplicate path, e.g. when adding a
        bookmark.}
      @entry[:incomplete-hostname]{Indicates an incomplete hostname, e.g.
        \"http://foo\" without a slash after that.}
    @end{table}
  @end{values}
  @begin{short}
    These identify the various errors that can occur while calling
    @class{gtk:file-chooser} interface functions.
  @end{short}
  @class{gtk:file-chooser}")

;;; ----------------------------------------------------------------------------
;;; GtkFileChooser
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GtkFileChooser" file-chooser
  (:export t
   :type-initializer "gtk_file_chooser_get_type")
  ((action
    file-chooser-action
    "action" "GtkFileChooserAction" t t)
   (create-folders
    file-chooser-create-folders
    "create-folders" "gboolean" t t)
   (do-overwrite-confirmation
    file-chooser-do-overwrite-confirmation
    "do-overwrite-confirmation" "gboolean" t t)
   (extra-widget
    file-chooser-extra-widget
    "extra-widget" "GtkWidget" t t)
   (filter
    file-chooser-filter
    "filter" "GtkFileFilter" t t)
   (local-only
    file-chooser-local-only
    "local-only" "gboolean" t t)
   (preview-widget
    file-chooser-preview-widget
    "preview-widget" "GtkWidget" t t)
   (preview-widget-active
    file-chooser-preview-widget-active
    "preview-widget-active" "gboolean" t t)
   (select-multiple
    file-chooser-select-multiple
    "select-multiple" "gboolean" t t)
   (show-hidden
    file-chooser-show-hidden
    "show-hidden" "gboolean" t t)
   (use-preview-label
    file-chooser-use-preview-label
    "use-preview-label" "gboolean" t t)))

#+liber-documentation
(setf (liber:alias-for-class 'file-chooser)
      "Interface"
      (documentation 'file-chooser 'type)
 "@version{2023-5-16}
  @begin{short}
    The @class{gtk:file-chooser} interface is an interface that can be
    implemented by file selection widgets.
  @end{short}
  The main widgets that implement this interface are the
  @class{gtk:file-chooser-widget}, @class{gtk:file-chooser-dialog}, and
  @class{gtk:file-chooser-button} widgets. You do not need to write a widget
  that implements the @class{gtk:file-chooser} interface unless you are trying to
  adapt an existing file selector to expose a standard programming interface.

  The @class{gtk:file-chooser} interface allows for shortcuts to various places
  in the filesystem. In the default implementation these are displayed in the
  left pane. It may be a bit confusing at first that these shortcuts come from
  various sources and in various flavours, so lets explain the terminology here:
  @begin{table}
    @begin[Bookmarks]{entry}
      are created by the user, by dragging folders from the right pane to the
      left pane, or by using the \"Add\". Bookmarks can be renamed and deleted
      by the user.
    @end{entry}
    @begin[Shortcuts]{entry}
      can be provided by the application or by the underlying filesystem
      abstraction, e.g. both the gnome-vfs and the Windows filesystems provide
      \"Desktop\" shortcuts. Shortcuts cannot be modified by the user.
    @end{entry}
    @begin[Volumes]{entry}
      are provided by the underlying filesystem abstraction. Volumes are the
      \"roots\" of the filesystem.
    @end{entry}
  @end{table}
  @subheading{File Names and Encodings}
    When the user is finished selecting files in a @class{gtk:file-chooser}
    widget, the program can get the selected names either as filenames or as
    URIs. For URIs, the normal escaping rules are applied if the URI contains
    non-ASCII characters. However, filenames are always returned in the
    character set specified by the @code{G_FILENAME_ENCODING} environment
    variable. Please see the GLib documentation for more details about this
    variable.

    This means that while you can pass the result of the the
    @fun{gtk:file-chooser-filename} function to the @code{open()} or
    @code{fopen()} functions, you may not be able to directly set it as the
    text of a @class{gtk:label} widget unless you convert it first to UTF-8,
    which all GTK widgets expect. You should use the @code{g_filename_to_utf8()}
    function to convert filenames into strings that can be passed to GTK
    widgets.

  @subheading{Adding a Preview Widget}
    You can add a custom preview widget to a file chooser and then get
    notification about when the preview needs to be updated. To install a
    preview widget, use the @fun{gtk:file-chooser-preview-widget} function.
    Then, connect to the @code{\"update-preview\"} signal to get notified
    when you need to update the contents of the preview.

    Your callback should use the @fun{gtk:file-chooser-preview-filename}
    function to see what needs previewing. Once you have generated the preview
    for the corresponding file, you must call the
    @fun{gtk:file-chooser-preview-widget-active} function with a boolean
    flag that indicates whether your callback could successfully generate a
    preview.

    @b{Example:} Sample Usage
    @begin{pre}
(defun create-file-chooser-preview ()
  (let ((response nil)
        (preview-width 256)
        (preview-height 256)
        (chooser (gtk:file-chooser-dialog-new \"Example File Chooser Preview\"
                                              nil
                                              :open
                                              \"gtk-open\" :accept
                                              \"gtk-cancel\" :cancel))
        (preview (make-instance 'gtk:image
                                :margin 24)))
    ;; Handler for the signal \"upadate-preview\"
    (g:signal-connect chooser \"update-preview\"
        (lambda (chooser)
          (let* ((filename (gtk:file-chooser-preview-filename chooser))
                 (pixbuf (when filename
                           (gdk:pixbuf-new-from-file-at-size filename
                                                             preview-width
                                                             preview-height))))
            (if pixbuf
                (progn
                  (gtk:image-set-from-pixbuf preview pixbuf)
                  (setf (gtk:file-chooser-preview-widget-active chooser) t))
                (setf (gtk:file-chooser-preview-widget-active chooser) nil)))))
    ;; Set the preview widget
    (setf (gtk:file-chooser-preview-widget chooser) preview)
    ;; Run the file chooser dialog
    (when (eq :accept
              (setf response
                    (gtk:dialog-run chooser)))
      (format t \"Save to file ~A~%\"
                (gtk:file-chooser-filename chooser)))
    (gtk:widget-destroy chooser)
    response))
    @end{pre}
  @subheading{Adding Extra Widgets}
    You can add extra widgets to a file chooser to provide options that are not
    present in the default design. For example, you can add a toggle button to
    give the user the option to open a file in read-only mode. You can use the
    @fun{gtk:file-chooser-extra-widget} function to insert additional widgets
    in a file chooser.

    @b{Example:} Sample Usage
    @begin{pre}
(defun create-file-chooser-widget ()
  (let ((response nil)
        (chooser (gtk:file-chooser-dialog-new \"Example File Chooser Widget\"
                                              nil
                                              :open
                                              \"gtk-open\" :accept
                                              \"gtk-cancel\" :cancel))
        (extra-widget (make-instance 'gtk:box
                                     :orientation :horizontal
                                     :spacing 12))
        (local-only (gtk:check-button-new-with-label \"Local only\"))
        (select-multiple (gtk:check-button-new-with-label \"Select Multiple\"))
        (show-hidden (gtk:check-button-new-with-label \"Show hidden\")))
    ;; Connect signal handlers to the toggle buttons
    (g:signal-connect local-only \"toggled\"
                      (lambda (button)
                        (setf (gtk:file-chooser-local-only chooser)
                              (gtk:toggle-button-active button))))
    (g:signal-connect select-multiple \"toggled\"
                      (lambda (button)
                        (setf (gtk:file-chooser-select-multiple chooser)
                              (gtk:toggle-button-active button))))
    (g:signal-connect show-hidden \"toggled\"
                      (lambda (button)
                        (setf (gtk:file-chooser-show-hidden chooser)
                              (gtk:toggle-button-active button))))
    ;; Put the extra widgets in a box
    (gtk:box-pack-start extra-widget local-only)
    (setf (gtk:toggle-button-active local-only) t) ; default is true
    (gtk:box-pack-start extra-widget select-multiple)
    (gtk:box-pack-start extra-widget show-hidden)
    (setf (gtk:file-chooser-extra-widget chooser) extra-widget)
    ;; Show the extra widgets
    (gtk:widget-show-all extra-widget)
    ;; Run the file chooser dialog
    (when (eq :accept
              (setf response
                    (gtk:dialog-run chooser)))
      (format t \"Open file ~A~%\"
                (gtk:file-chooser-filename chooser)))
    (gtk:widget-destroy chooser)
    response))
    @end{pre}
    If you want to set more than one extra widget in the file chooser, you can
    add a container such as a @class{gtk:box} or a @class{gtk:grid} widget and
    include your widgets in it. Then, set the container as the whole extra
    widget.
  @begin[Signal Details]{dictionary}
    @subheading{The \"confirm-overwrite\" signal}
      @begin{pre}
lambda (chooser)    :run-last
      @end{pre}
      The signal gets emitted whenever it is appropriate to present a
      confirmation dialog when the user has selected a file name that already
      exists. The signal only gets emitted when the file chooser is in
      @code{:save} mode.

      Most applications just need to turn on the
      @code{do-overwrite-confirmation} property, and they will automatically
      get a stock confirmation dialog. Applications which need to customize
      this behavior should do that, and also connect to the
      @code{\"confirm-overwrite\"} signal.

      A signal handler for this signal must return a value of the
      @symbol{gtk:file-chooser-confirmation} enumeration, which indicates the
      action to take. If the handler determines that the user wants to select
      a different filename, it should return the @code{:select-again} value. If
      it determines that the user is satisfied with his choice of file name, it
      should return the @code{:accept-filename} value. On the other hand, if it
      determines that the stock confirmation dialog should be used, it should
      return the @code{:confirm} value. The following example illustrates this.

      @b{Example:} Custom confirmation
      @begin{pre}
(defun confirm-overwrite (chooser)
  (let ((uri (gtk:file-chooser-uri chooser)))
    ;; Check for read-only file
    (if (is-uri-read-only uri)
        (if (user-wants-to-replace-read-only-file uri)
            ;; User accepts overwriting
            :accept-filename
            ;; User rejects overwriting
            :select-again)
         ;; Fall back to the default dialog
         :confirm)))
   ...
  (let ((chooser (gtk:file-choose-dialog-new ...)))
    ...
    (setf (gtk:file-chooser-do-overwrite-confirmation chooser) t)
    (g:signal-connect chooser \"confirm-overwrite\" #'confirm-overwrite)
    ... )
      @end{pre}
      @begin[code]{table}
        @entry[chooser]{The @class{gtk:file-chooser} widget which received the
          signal.}
        @entry[Returns]{A @symbol{gtk:file-chooser-confirmation} value that
          indicates which action to take after emitting the signal.}
      @end{table}
    @subheading{The \"current-folder-changed\" signal}
      @begin{pre}
lambda (chooser)    :run-last
      @end{pre}
      The signal is emitted when the current folder in a file chooser changes.
      This can happen due to the user performing some action that changes
      folders, such as selecting a bookmark or visiting a folder on the file
      list. It can also happen as a result of calling a function to explicitly
      change the current folder in a file chooser. Normally you do not need to
      connect to this signal, unless you need to keep track of which folder a
      file chooser is showing.
      @begin[code]{table}
        @entry[chooser]{The @class{gtk:file-chooser} widget which received the
          signal.}
      @end{table}
    @subheading{The \"file-activated\" signal}
      @begin{pre}
lambda (chooser)    :run-last
      @end{pre}
      The signal is emitted when the user \"activates\" a file in the file
      chooser. This can happen by double-clicking on a file in the file list,
      or by pressing the @kbd{Enter} key. Normally you do not need to connect to
      the signal. It is used internally by the @class{gtk:file-chooser-dialog}
      class to know when to activate the default button in the dialog.
      @begin[code]{table}
        @entry[chooser]{The @class{gtk:file-chooser} widget which received the
          signal.}
      @end{table}
    @subheading{The \"selection-changed\" signal}
      @begin{pre}
lambda (chooser)    :run-last
      @end{pre}
      The signal is emitted when there is a change in the set of selected files
      in a file chooser. This can happen when the user modifies the selection
      with the mouse or the keyboard, or when explicitly calling functions to
      change the selection. Normally you do not need to connect to the signal,
      as it is easier to wait for the file chooser to finish running.
      @begin[code]{table}
        @entry[chooser]{The @class{gtk:file-chooser} widget which received the
          signal.}
      @end{table}
    @subheading{The \"update-preview\" signal}
      @begin{pre}
lambda (chooser)    :run-last
      @end{pre}
      The signal is emitted when the preview in a file chooser should be
      regenerated. For example, this can happen when the currently selected
      file changes. You should use this signal if you want your file chooser
      to have a preview widget. Once you have installed a preview widget with
      the @fun{gtk:file-chooser-preview-widget} function, you should update it
      when the signal is emitted. You can use the
      @fun{gtk:file-chooser-preview-filename} or
      @fun{gtk:file-chooser-preview-uri} functions to get the name of the file
      to preview. Your widget may not be able to preview all kinds of files.
      Your callback function must call the
      @fun{gtk:file-chooser-preview-widget-active} function to inform the file
      chooser about whether the preview was generated successfully or not.
      @begin[code]{table}
        @entry[chooser]{The @class{gtk:file-chooser} widget which received the
          signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:file-chooser-action}
  @see-slot{gtk:file-chooser-create-folders}
  @see-slot{gtk:file-chooser-do-overwrite-confirmation}
  @see-slot{gtk:file-chooser-extra-widget}
  @see-slot{gtk:file-chooser-filter}
  @see-slot{gtk:file-chooser-local-only}
  @see-slot{gtk:file-chooser-preview-widget}
  @see-slot{gtk:file-chooser-preview-widget-active}
  @see-slot{gtk:file-chooser-select-multiple}
  @see-slot{gtk:file-chooser-show-hidden}
  @see-slot{gtk:file-chooser-use-preview-label}
  @see-class{gtk:file-chooser-button}
  @see-class{gtk:file-chooser-dialog}
  @see-class{gtk:file-chooser-widget}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;: --- gtk:file-chooser-action ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "action" 'file-chooser) t)
 "The @code{action} property of type @symbol{gtk:file-chooser-action}
  (Read / Write) @br{}
  The type of operation that the file selector is performing. @br{}
  Default value: @code{:action-open}")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-action)
      "Accessor"
      (documentation 'file-chooser-action 'function)
 "@version{2023-6-11}
  @syntax{(gtk:file-chooser-action object) => action}
  @syntax{(setf (gtk:file-chooser-action object) action)}
  @argument[object]{a @class{gtk:file-chooser} widget}
  @argument[action]{a value of the @symbol{gtk:file-chooser-action} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser]{action} slot of the
    @class{gtk:file-chooser} interface.
  @end{short}
  The @fun{gtk:file-chooser-action} function gets the type of operation that the
  file chooser is performing. The @setf{gtk:file-chooser-action} function sets
  the type of operation. The user interface is adapted to suit the selected
  action. For example, an option to create a new folder might be shown if the
  action is @code{:save} but not if the action is @code{:open}.
  @see-class{gtk:file-chooser}
  @see-symbol{gtk:file-chooser-action}")

;;; --- gtk:file-chooser-create-folders ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "create-folders"
                                               'file-chooser) t)
 "The @code{create-folders} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether a file chooser not in @code{:action-open} mode will offer the user to
  create new folders. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-create-folders)
      "Accessor"
      (documentation 'file-chooser-create-folders 'function)
 "@version{2023-6-11}
  @syntax{(gtk:file-chooser-create-folders object) => create-folders}
  @syntax{(setf (gtk:file-chooser-create-folders object) create-folders)}
  @argument[object]{a @class{gtk:file-chooser} widget}
  @argument[create-folders]{@em{true} if the New Folder button should be
    displayed}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser]{create-folders} slot of the
    @class{gtk:file-chooser} interface.
  @end{short}
  The @fun{gtk:file-chooser-create-folders} function gets whether the file
  chooser will offer to create new folders. The
  @setf{gtk:file-chooser-create-folders} function sets the property.

  This is only relevant if the action of the file chooser is not set to be
  @code{:open}.
  @see-class{gtk:file-chooser}
  @see-symbol{gtk:file-chooser-action}")

;;; --- gtk:file-chooser-do-overwrite-confirmation -----------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "do-overwrite-confirmation"
                                               'file-chooser) t)
 "The @code{do-overwrite-confirmation} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether a file chooser in @code{:action-save} mode will present an overwrite
  confirmation dialog if the user selects a file name that already exists. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-do-overwrite-confirmation)
      "Accessor"
      (documentation 'file-chooser-do-overwrite-confirmation 'function)
 "@version{2023-6-11}
  @syntax{(gtk:file-chooser-do-overwrite-confirmation object) => confirm}
  @syntax{(setf (gtk:file-chooser-do-overwrite-confirmation object) confirm)}
  @argument[object]{a @class{gtk:file-chooser} widget}
  @argument[confirm]{a boolean whether to confirm overwriting in @code{:save}
    mode}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser]{do-overwrite-confirmation} slot of
    the @class{gtk:file-chooser} interface.
  @end{short}
  The @fun{gtk:file-chooser-do-overwrite-confirmation} function queries whether
  the file chooser in @code{:save} mode is set to confirm for overwriting when
  the user types a file name that already exists. The
  @setf{gtk:file-chooser-do-overwrite-confirmation} function sets whether the
  file chooser will present a confirmation dialog. This is @em{false} by
  default.

  Regardless of this setting, the chooser will emit the
  @code{\"confirm-overwrite\"} signal when appropriate.

  If all you need is the stock confirmation dialog, set this property to
  @em{true}. You can override the way confirmation is done by actually handling
  the @code{\"confirm-overwrite\"} signal. Please refer to its documentation for
  the details.
  @see-class{gtk:file-chooser}
  @see-symbol{gtk:file-chooser-action}")

;;; --- gtk:file-chooser-extra-widget ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "extra-widget" 'file-chooser) t)
 "The @code{extra-widget} property of type @class{gtk:widget} (Read / Write)
  @br{}
  Application supplied widget for extra options.")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-extra-widget)
      "Accessor"
      (documentation 'file-chooser-extra-widget 'function)
 "@version{2023-6-11}
  @syntax{(gtk:file-chooser-extra-widget object) => extra-widget}
  @syntax{(setf (gtk:file-chooser-extra-widget object) extra-widget)}
  @argument[object]{a @class{gtk:file-chooser} widget}
  @argument[extra-widget]{a @class{gtk:widget} widget for extra options}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser]{extra-widget} slot of the
    @class{gtk:file-chooser} interface.
  @end{short}
  The @fun{gtk:file-chooser-extra-widget} function gets the application
  supplied widget to provide extra options to the user. The
  @setf{gtk:file-chooser-extra-widget} function sets the widget.
  @see-class{gtk:file-chooser}
  @see-class{gtk:widget}")

;;; --- gtk:file-chooser-filter ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "filter" 'file-chooser) t)
 "The @code{filter} property of type @class{gtk:file-filter} (Read / Write)
  @br{}
  The current filter for selecting which files are displayed.")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-filter)
      "Accessor"
      (documentation 'file-chooser-filter 'function)
 "@version{2023-6-11}
  @syntax{(gtk:file-chooser-filter object) => filter}
  @syntax{(setf (gtk:file-chooser-filter object) filter)}
  @argument[object]{a @class{gtk:file-chooser} widget}
  @argument[filter]{a @class{gtk:file-filter} object}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser]{filter} slot of the
    @class{gtk:file-chooser} interface.
  @end{short}
  The @fun{gtk:file-chooser-filter} function gets the current filter. The
  @setf{gtk:file-chooser-filter} function sets the current filter. Only the
  files that pass the filter will be displayed.

  If the user-selectable list of filters is non-empty, then the filter should be
  one of the filters in that list. Setting the current filter when the list of
  filters is empty is useful if you want to restrict the displayed set of files
  without letting the user change it.
  @see-class{gtk:file-chooser}
  @see-class{gtk:file-filter}")

;;; --- gtk:file-chooser-local-only --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "local-only" 'file-chooser) t)
 "The @code{local-only} property of type @code{:boolean} (Read / Write) @br{}
  Whether the selected file(s) should be limited to local file URLs. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-local-only)
      "Accessor"
      (documentation 'file-chooser-local-only 'function)
 "@version{2023-6-11}
  @syntax{(gtk:file-chooser-local-only object) => local-only}
  @syntax{(setf (gtk:file-chooser-local-only object) local-only)}
  @argument[object]{a @class{gtk:file-chooser} widget}
  @argument[local-only]{@em{true} if only local files can be selected}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser]{local-only} slot of the
    @class{gtk:file-chooser} interface.
  @end{short}
  The @fun{gtk:file-chooser-local-only} function gets whether only local files
  can be selected in the file selector. The @setf{gtk:file-chooser-local-only}
  function sets whether only local files can be selected.

  If @arg{local-only} is @em{true}, the default, then the selected files are
  guaranteed to be accessible through the operating systems native file
  system and therefore the application only needs to worry about the filename
  functions in the file chooser, like the @fun{gtk:file-chooser-filename}
  function, rather than the URI functions like the @fun{gtk:file-chooser-uri}
  function.
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-filename}
  @see-function{gtk:file-chooser-uri}")

;;; --- gtk:file-chooser-preview-widget ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "preview-widget"
                                               'file-chooser) t)
 "The @code{preview-widget} property of type @class{gtk:widget} (Read / Write)
  @br{}
  Application supplied widget for custom previews.")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-preview-widget)
      "Accessor"
      (documentation 'file-chooser-preview-widget 'function)
 "@version{#2023-3-14}
  @syntax{(gtk:file-chooser-preview-widget object) => widget}
  @syntax{(setf (gtk:file-chooser-preview-widget object) widget)}
  @argument[object]{a @class{gtk:file-chooser} widget}
  @argument[widget]{a @class{gtk:widget} for displaying preview}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser]{preview-widget} slot of the
    @class{gtk:file-chooser} interface.
  @end{short}
  The @fun{gtk:file-chooser-preview-widget} function gets the current preview
  widget. The @setf{gtk:file-chooser-preview-widget} function sets an
  application supplied widget to use to display a custom preview of the
  currently selected file.

  To implement a preview, after setting the preview widget, you connect to the
  @code{\"update-preview\"} signal, and call the
  @fun{gtk:file-chooser-preview-filename} or @fun{gtk:file-chooser-preview-uri}
  functions on each change. If you can display a preview of the new file, update
  your widget and set the preview active using the
  @fun{gtk:file-chooser-preview-widget-active} function. Otherwise, set the
  preview inactive.

  When there is no application supplied preview widget, or the application
  supplied preview widget is not active, the file chooser may display an
  internally generated preview of the current file or it may display no preview
  at all.
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-preview-filename}
  @see-function{gtk:file-chooser-preview-uri}
  @see-function{gtk:file-chooser-preview-widget-active}")

;;; --- gtk:file-chooser-preview-widget-active ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "preview-widget-active"
                                               'file-chooser) t)
 "The @code{preview-widget-active} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the application supplied widget for custom previews should be
  shown. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-preview-widget-active)
      "Accessor"
      (documentation 'file-chooser-preview-widget-active 'function)
 "@version{2023-6-11}
  @syntax{(gtk:file-chooser-preview-widget-active object) => active}
  @syntax{(setf (gtk:file-chooser-preview-widget-active object) active)}
  @argument[object]{a @class{gtk:file-chooser} widget}
  @argument[active]{a boolean whether to display the user-specified preview
    widget}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser]{preview-widget-active} slot of the
    @class{gtk:file-chooser} interface.
  @end{short}
  The @fun{gtk:file-chooser-preview-widget-active} function gets whether the
  preview widget should be shown for the current filename. The
  @setf{gtk:file-chooser-preview-widget-active} function sets whether the
  preview widget should be shown.

  When @arg{active} is set to @em{false}, the file chooser may display an
  internally generated preview of the current file or it may display no preview
  at all. See the @fun{gtk:file-chooser-preview-widget} function for more
  details.
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-preview-widget}")

;;; --- gtk:file-chooser-select-multiple ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "select-multiple"
                                               'file-chooser) t)
 "The @code{select-multiple} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to allow multiple files to be selected. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-select-multiple)
      "Accessor"
      (documentation 'file-chooser-select-multiple 'function)
 "@version{2023-6-11}
  @syntax{(gtk:file-chooser-select-multiple object) => multiple}
  @syntax{(setf (gtk:file-chooser-select-multiple object) multiple)}
  @argument[object]{a @class{gtk:file-chooser} widget}
  @argument[multiple]{@em{true} if multiple files can be selected}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser]{select-multiple} slot of the
    @class{gtk:file-chooser} interface.
  @end{short}
  The @fun{gtk:file-chooser-select-multiple} function gets whether multiple
  files can be selected in the file selector. The
  @setf{gtk:file-chooser-select-multiple} function sets whether multiple files
  can be selected.

  This is only relevant if the action of the file chooser is set to be
  @code{:open} or @code{:select-folder}.
  @see-class{gtk:file-chooser}
  @see-symbol{gtk:file-chooser-action}")

;;; --- gtk:file-chooser-show-hidden -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-hidden"
                                               'file-chooser) t)
 "The @code{show-hidden} property of type @code{:boolean} (Read / Write) @br{}
  Whether the hidden files and folders should be displayed. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-show-hidden)
      "Accessor"
      (documentation 'file-chooser-show-hidden 'function)
 "@version{2023-6-11}
  @syntax{(gtk:file-chooser-show-hidden object) => setting}
  @syntax{(setf (gtk:file-chooser-show-hidden object) setting)}
  @argument[object]{a @class{gtk:file-chooser} widget}
  @argument[setting]{@em{true} if hidden files and folders should be displayed}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser]{show-hidden} slot of the
    @class{gtk:file-chooser} interface.
  @end{short}
  The @fun{gtk:file-chooser-show-hidden} function gets whether hidden files and
  folders are displayed in the file selector. The
  @setf{gtk:file-chooser-show-hidden} function sets the property.
  @see-class{gtk:file-chooser}")

;;; --- gtk:file-chooser-use-preview-label -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-preview-label"
                                               'file-chooser) t)
 "The @code{use-preview-label} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to display a stock label with the name of the previewed file. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'file-chooser-use-preview-label)
      "Accessor"
      (documentation 'file-chooser-use-preview-label 'function)
 "@version{2023-6-11}
  @syntax{(gtk:file-chooser-use-preview-label object) => setting}
  @syntax{(setf (gtk:file-chooser-use-preview-label object) setting)}
  @argument[object]{a @class{gtk:file-chooser} widget}
  @argument[setting]{a boolean whether to display a stock label with the name
    of the previewed file}
  @begin{short}
    Accessor of the @slot[gtk:file-chooser]{use-preview-label} slot of the
    @class{gtk:file-chooser} interface.
  @end{short}
  The @fun{gtk:file-chooser-use-preview-label} function gets whether a stock
  label should be drawn with the name of the previewed file. The
  @setf{gtk:file-chooser-use-preview-label} function sets whether the file
  chooser should display a stock label. The default is @em{true}.

  Applications that want to draw the whole preview area themselves should set
  this to @em{false} and display the name themselves in their preview widget.
  See also the @fun{gtk:file-chooser-preview-widget} function.
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-preview-widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_current_name
;;; gtk_file_chooser_set_current_name
;;; ----------------------------------------------------------------------------

(defun (setf file-chooser-current-name) (name chooser)
  (cffi:foreign-funcall "gtk_file_chooser_set_current_name"
                        (g:object file-chooser) chooser
                        (:string :free-to-foreign t :encoding :utf-8) name
                        :void)
  name)

(cffi:defcfun ("gtk_file_chooser_get_current_name" file-chooser-current-name)
    (:string :free-from-foreign t :encoding :utf-8)
 #+liber-documentation
 "@version{2023-6-11}
  @syntax{(gtk:file-chooser-current-name chooser) => name}
  @syntax{(setf (gtk:file-chooser-current-name chooser) name)}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[name]{a string with the filename to use, as a UTF-8 string}
  @begin{short}
    Accessor of the current name of a file chooser widget.
  @end{short}
  The @fun{gtk:file-chooser-current-name} function gets the current name in the
  file selector, as entered by the user in the text entry for \"Name\". The
  @setf{gtk:file-chooser-current-name} function sets the current name.

  This is meant to be used in save dialogs, to get the currently typed filename
  when the file itself does not exist yet. For example, an application that adds
  a custom extra widget to the file chooser for \"file format\" may want to
  change the extension of the typed filename based on the chosen format, say,
  from \".jpg\" to \".png\".

  Note that the name passed in here is a UTF-8 string rather than a filename.
  This function is meant for such uses as a suggested name in a \"Save As...\"
  dialog. You can pass \"Untitled.doc\" or a similarly suitable suggestion for
  the name.

  If you want to preselect a particular existing file, you should use the
  @fun{gtk:file-chooser-filename} or @fun{gtk:file-chooser-uri} functions
  instead. Please see the documentation for those functions for an
  example of using the @fun{gtk:file-chooser-current-name} function as well.
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-filename}
  @see-function{gtk:file-chooser-uri}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-current-name)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_filename
;;; gtk_file_chooser_set_filename
;;; ----------------------------------------------------------------------------

(defun (setf file-chooser-filename) (filename chooser)
  (cffi:foreign-funcall "gtk_file_chooser_set_filename"
                        (g:object file-chooser) chooser
                        :string filename
                        :boolean)
  filename)

(cffi:defcfun ("gtk_file_chooser_get_filename" file-chooser-filename)
    (:string :free-from-foreign t)
 #+liber-documentation
 "@version{2023-6-11}
  @syntax{(gtk:file-chooser-filename chooser) => filename}
  @syntax{(setf (gtk:file-chooser-filename chooser) filename)}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[filename]{a string with the filename to set as current}
  @begin{short}
    Accessor of the filename of a file chooser widget.
  @end{short}
  The @fun{gtk:file-chooser-filename} function gets the filename for the
  currently selected file in the file selector. If multiple files are selected,
  one of the filenames will be returned at random. If the file chooser is in
  folder mode, this function returns the selected folder.

  The @setf{gtk:file-chooser-filename} function sets @arg{filename} as the
  current filename for the file chooser, by changing to the file's parent
  folder and actually selecting the file in the list. All other files will be
  unselected. If the chooser is in @code{:save} mode, the file's base name will
  also appear in the dialog's file name entry. Note that the file must exist,
  or nothing will be done except for the directory change.

  You should use this function only when implementing a \"File/Save As...\"
  dialog for which you already have a file name to which the user may save. For
  example, when the user opens an existing file and then does
  \"File/Save As...\" on it to save a copy or a modified version. If you do not
  have a file name already - for example, if the user just created a new file
  and is saving it for the first time, do not call this function. Instead, use
  something similar to this:
  @begin{pre}
(if (document-is-new)
    ;; the user just created a new document
    (setf (gtk:file-chooser-current-name chooser) \"Untitled document\")
    ;; the user edited an existing document
    (setf (gtk:file-chooser-filename chooser) existing-filename))
  @end{pre}
  In the first case, the file chooser will present the user with useful
  suggestions as to where to save his new file. In the second case, the file's
  existing location is already known, so the file chooser will use it.
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-current-name}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-filename)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_select_filename
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_select_filename" file-chooser-select-filename)
    :boolean
 #+liber-documentation
 "@version{#2023-3-14}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[filename]{a string with the filename to select}
  @begin{short}
    Selects a filename in the file chooser.
  @end{short}
  If the filename is not in the current folder of the file chooser, then the
  current folder of the file chooser will be changed to the folder containing
  @arg{filename}.
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-unselect-filename}"
  (chooser (g:object file-chooser))
  (filename :string))

(export 'file-chooser-select-filename)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_unselect_filename
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_unselect_filename"
                file-chooser-unselect-filename) :void
 #+liber-documentation
 "@version{#2023-3-14}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[filename]{a string with the filename to unselect}
  @begin{short}
    Unselects a currently selected filename.
  @end{short}
  If the filename is not in the current directory, does not exist, or is
  otherwise not currently selected, does nothing.
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-select-filename}"
  (chooser (g:object file-chooser))
  (filenname :string))

(export 'file-chooser-unselect-filename)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_select_all
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_select_all" file-chooser-select-all) :void
 #+liber-documentation
 "@version{#2023-3-14}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @begin{short}
    Selects all files in the current folder of the file chooser.
  @end{short}
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-unselect-all}
  @see-function{gtk:file-chooser-select-filename}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-select-all)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_unselect_all
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_unselect_all" file-chooser-unselect-all) :void
 #+liber-documentation
 "@version{#2023-3-14}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @begin{short}
    Unselects all files in the current folder of the file chooser.
  @end{short}
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-select-all}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-unselect-all)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_filenames
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_get_filenames" file-chooser-filenames)
    (g:slist-t :string)
 #+liber-documentation
 "@version{#2023-3-14}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @begin{return}
    A list containing the filenames of all selected files and subfolders
    in the current folder.
  @end{return}
  @begin{short}
    Lists all the selected files and subfolders in the current folder of
    the file chooser.
  @end{short}
  The returned names are full absolute paths. If the files in the current
  folder cannot be represented as local filenames they will be ignored. See
  the @fun{gtk:file-chooser-uris} function.
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-uris}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-filenames)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_current_folder
;;; gtk_file_chooser_set_current_folder
;;; ----------------------------------------------------------------------------

(defun (setf file-chooser-current-folder) (filename chooser)
  (cffi:foreign-funcall "gtk_file_chooser_set_current_folder"
                        (g:object file-chooser) chooser
                        :string (or filename (cffi:null-pointer))
                        :boolean)
  filename)

(cffi:defcfun ("gtk_file_chooser_get_current_folder"
                file-chooser-current-folder) :string
 #+liber-documentation
 "@version{2024-12-29}
  @syntax{(gtk:file-chooser-current-folder chooser) => filename}
  @syntax{(setf (gtk:file-chooser-current-folder chooser) filename)}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[filename]{a string with the full path of the new current folder}
  @begin{short}
    The @fun{gtk:file-chooser-current-folder} function gets the current folder
    of the file chooser as a local filename.
  @end{short}
  The @setf{gtk:file-chooser-current-folder} function sets the current folder.
  The user will be shown the full contents of the current folder, plus user
  interface elements for navigating to other folders.

  Note that this is the folder that the file chooser is currently displaying,
  for example @file{/home/username/Documents}, which is not the same as the
  currently selected folder if the chooser is in @code{:select-folder} mode,
  for example @file{/home/username/Documents/selected-folder/}. To get the
  currently selected folder in that mode, use the @fun{gtk:file-chooser-uri}
  function as the usual way to get the selection.

  In general, you should not use this function. See the section on setting up
  a file chooser dialog for the rationale behind this.
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-uri}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-current-folder)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_uri
;;; gtk_file_chooser_set_uri
;;; ----------------------------------------------------------------------------

(defun (setf file-chooser-uri) (uri chooser)
  (cffi:foreign-funcall "gtk_file_chooser_set_uri"
                        (g:object file-chooser) chooser
                        :string uri
                        :boolean)
  uri)

(cffi:defcfun ("gtk_file_chooser_get_uri" file-chooser-uri) :string
 #+liber-documentation
 "@version{#2023-3-14}
  @syntax{(gtk:file-chooser-uri chooser) => uri}
  @syntax{(setf (gtk:file-chooser-uri chooser) uri)}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[uri]{a string with the URI to set as current}
  @begin{short}
    Accessor of the currently selected URI.
  @end{short}
  The @fun{gtk:file-chooser-uri} function gets the URI for the currently
  selected file in the file selector. If multiple files are selected, one of
  the filenames will be returned at random. If the file chooser is in folder
  mode, this function returns the selected folder.

  The @setf{gtk:file-chooser-uri} function sets the file referred to by
  @arg{uri} as the current file for the file chooser, by changing to the URI's
  parent folder and actually selecting the URI in the list. If the chooser is
  in @code{:save} mode, the URI's base name will also appear in the dialog's
  file name entry.

  Note that the URI must exist, or nothing will be done except for the
  directory change.

  You should use this function only when implementing a File/Save As... dialog
  for which you already have a file name to which the user may save. For
  example, when the user opens an existing file and then does File/Save As...
  on it to save a copy or a modified version. If you do not have a file name
  already - for example, if the user just created a new file and is saving it
  for the first time, do not call this function. Instead, use something
  similar to this:
  @begin{pre}
(if (document-is-new)
    ;; the user just created a new document
    (setf (gtk:file-chooser-current-name chooser) \"Untitled document\")
    ;; the user edited an existing document
    (setf (gtk:file-chooser-uri chooser) existing-uri))
  @end{pre}
  In the first case, the file chooser will present the user with useful
  suggestions as to where to save his new file. In the second case, the file's
  existing location is already known, so the file chooser will use it.
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-current-name}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_select_uri
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_select_uri" file-chooser-select-uri) :boolean
 #+liber-documentation
 "@version{#2023-3-14}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[uri]{a string with the URI to select}
  @begin{short}
    Selects the file by @arg{uri}.
  @end{short}
  If the URI does not refer to a file in the current folder of the file chooser,
  then the current folder of the file chooser will be changed to the folder
  containing @arg{uri}.
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-unselect-uri}"
  (chooser (g:object file-chooser))
  (uri :string))

(export 'file-chooser-select-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_unselect_uri
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_unselect_uri" file-chooser-unselect-uri) :void
 #+liber-documentation
 "@version{#2023-3-14}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[uri]{a string with the URI to unselect}
  @begin{short}
    Unselects the file referred to by @arg{uri}.
  @end{short}
  If the file is not in the current directory, does not exist, or is otherwise
  not currently selected, does nothing.
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-select-uri}"
  (chooser (g:object file-chooser))
  (uri :string))

(export 'file-chooser-unselect-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_uris
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_get_uris" file-chooser-uris)
    (g:slist-t :string)
 #+liber-documentation
 "@version{#2023-3-14}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @begin{return}
    A list containing strings with the URIs of all selected files and subfolders
    in the current folder.
  @end{return}
  @begin{short}
    Lists all the selected files and subfolders in the current folder of the
    file chooser.
  @end{short}
  The returned names are full absolute URIs.
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-filenames}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-uris)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_set_current_folder_uri
;;; gtk_file_chooser_get_current_folder_uri
;;; ----------------------------------------------------------------------------

(defun (setf file-chooser-current-folder-uri) (uri chooser)
  (when (cffi:foreign-funcall "gtk_file_chooser_set_current_folder_uri"
                              (g:object file-chooser) chooser
                              :string uri
                              :boolean)
    uri))

(cffi:defcfun ("gtk_file_chooser_get_current_folder_uri"
                file-chooser-current-folder-uri) :string
 #+liber-documentation
 "@version{#2023-3-14}
  @syntax{(gtk:file-chooser-current-folder-uri chooser) => uri}
  @syntax{(setf (gtk:file-chooser-current-folder-uri chooser) uri)}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[uri]{a string with the URI for the current folder}
  @begin{short}
    Accessor of the URI for the current folder of the file chooser.
  @end{short}
  The @fun{gtk:file-chooser-current-folder-uri} function gets the current
  folder of the file chooser as an URI. This function will also return
  @code{nil} if the file chooser was unable to load the last folder that was
  requested from it. For example, as would be for calling this function on a
  nonexistent folder.

  Note that this is the folder that the file chooser is currently displaying,
  e.g. @file{file:///home/username/Documents}, which is not the same as the
  currently selected folder if the chooser is in @code{:select-folder}, e.g.
  @file{file:///home/username/Documents/selected-folder/}. To get the
  currently selected folder in that mode, use the @fun{gtk:file-chooser-uri}
  function as the usual way to get the selection.

  The @setf{gtk:file-chooser-current-folder-uri} function sets the current
  folder for the file chooser from an URI. The user will be shown the full
  contents of the current folder, plus user interface elements for navigating
  to other folders.

  In general, you should not use this function. See the section on setting up
  a file chooser dialog for the rationale behind this.
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-uri}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-current-folder-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_preview_filename
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_get_preview_filename"
                file-chooser-preview-filename) (:string :free-from-foreign t)
 #+liber-documentation
 "@version{#2023-3-14}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @begin{return}
    A string with the filename to preview, or @code{nil} if no file is
    selected, or if the selected file cannot be represented as a local filename.
  @end{return}
  @begin{short}
    Gets the filename that should be previewed in a custom preview widget.
  @end{short}
  See the @fun{gtk:file-chooser-preview-widget} function.
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-preview-widget}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-preview-filename)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_preview_uri
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_get_preview_uri" file-chooser-preview-uri)
    :string
 #+liber-documentation
 "@version{#2023-3-14}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @begin{return}
    A string with the URI for the file to preview, or @code{nil} if no file is
    selected.
  @end{return}
  @begin{short}
    Gets the URI that should be previewed in a custom preview widget.
  @end{short}
  See the @fun{gtk:file-chooser-preview-widget} function.
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-preview-widget}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-preview-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_add_filter
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_add_filter" file-chooser-add-filter) :void
 #+liber-documentation
 "@version{#2023-3-14}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[filter]{a @class{gtk:file-filter} object}
  @begin{short}
    Adds a filter to the list of filters that the user can select between.
  @end{short}
  When a filter is selected, only files that are passed by that filter are
  displayed.
  @see-class{gtk:file-chooser}
  @see-class{gtk:file-filter}
  @see-function{gtk:file-chooser-remove-filter}"
  (chooser (g:object file-chooser))
  (filter (g:object file-filter)))

(export 'file-chooser-add-filter)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_remove_filter
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_remove_filter" file-chooser-remove-filter)
    :void
 #+liber-documentation
 "@version{#2023-3-14}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[filter]{a @class{gtk:file-filter} object}
  @begin{short}
    Removes a filter from the list of filters that the user can select between.
  @end{short}
  @see-class{gtk:file-chooser}
  @see-class{gtk:file-filter}
  @see-function{gtk:file-chooser-add-filter}"
  (chooser (g:object file-chooser))
  (filter (g:object file-filter)))

(export 'file-chooser-remove-filter)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_list_filters
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_list_filters" file-chooser-list-filters)
    (g:slist-t (g:object file-filter))
 #+liber-documentation
 "@version{#2023-3-14}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @begin{return}
    A list containing the current set of user selectable @class{gtk:file-filter}
    objects.
  @end{return}
  @begin{short}
    Lists the current set of user-selectable filters.
  @end{short}
  See the @fun{gtk:file-chooser-add-filter} and
  @fun{gtk:file-chooser-remove-filter} functions.
  @see-class{gtk:file-chooser}
  @see-class{gtk:file-filter}
  @see-function{gtk:file-chooser-add-filter}
  @see-function{gtk:file-chooser-remove-filter}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-list-filters)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_add_shortcut_folder
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_add_shortcut_folder"
               %file-chooser-add-shortcut-folder) :boolean
  (chooser (g:object file-chooser))
  (folder :string)
  (err :pointer))

(defun file-chooser-add-shortcut-folder (chooser folder)
 #+liber-documentation
 "@version{2024-11-20}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[folder]{a string with a filename of the folder to add}
  @begin{return}
    @em{True} if the folder could be added successfully, @em{false} otherwise.
  @end{return}
  @begin{short}
    Adds a folder to be displayed with the shortcut folders in a file chooser.
  @end{short}
  Note that shortcut folders do not get saved, as they are provided by the
  application. For example, you can use this to add a
  @file{\"/usr/share/mydrawprogram/Clipart\"} folder to the volume list.
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-remove-shortcut-folder}"
  (glib:with-error (err)
    (%file-chooser-add-shortcut-folder chooser folder err)))

(export 'file-chooser-add-shortcut-folder)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_remove_shortcut_folder
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_remove_shortcut_folder"
               %file-chooser-remove-shortcut-folder) :boolean
  (chooser (g:object file-chooser))
  (folder :string)
  (err :pointer))

(defun file-chooser-remove-shortcut-folder (chooser folder)
 #+liber-documentation
 "@version{2024-11-20}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[folder]{a string with the filename of the folder to remove}
  @begin{return}
    @em{True} if the operation succeeds, @em{false} otherwise.
  @end{return}
  @begin{short}
    Removes a folder from a file chooser's list of shortcut folders.
  @end{short}
  See also the @fun{gtk:file-chooser-add-shortcut-folder} function.
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-add-shortcut-folder}"
  (glib:with-error (err)
    (%file-chooser-remove-shortcut-folder chooser folder err)))

(export 'file-chooser-remove-shortcut-folder)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_list_shortcut_folders
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_list_shortcut_folders"
                file-chooser-list-shortcut-folders) (g:slist-t :string)
 #+liber-documentation
 "@version{2023-6-11}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @begin{return}
    A list of strings with the folder filenames, or @code{nil} if there are no
    shortcut folders.
  @end{return}
  @begin{short}
    Queries the list of shortcut folders in the file chooser, as set by the
    @fun{gtk:file-chooser-add-shortcut-folder} function.
  @end{short}
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-add-shortcut-folder}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-list-shortcut-folders)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_add_shortcut_folder_uri
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_add_shortcut_folder_uri"
               %file-chooser-add-shortcut-folder-uri) :boolean
  (chooser (g:object file-chooser))
  (uri :string)
  (err :pointer))

(defun file-chooser-add-shortcut-folder-uri (chooser uri)
 #+liber-documentation
 "@version{2024-11-20}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[uri]{a string with the URI of the folder to add}
  @begin{return}
    @em{True} if the folder could be added successfully, @em{false} otherwise.
  @end{return}
  @begin{short}
    Adds a folder URI to be displayed with the shortcut folders in a file
    chooser.
  @end{short}
  Note that shortcut folders do not get saved, as they are provided by the
  application. For example, you can use this to add a
  @file{file:///usr/share/mydrawprogram/Clipart} folder to the volume list.
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-remove-shortcut-folder-uri}
  @see-function{gtk:file-chooser-add-shortcut-folder}"
  (glib:with-error (err)
    (%file-chooser-add-shortcut-folder-uri chooser uri err)))

(export 'file-chooser-add-shortcut-folder-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_remove_shortcut_folder_uri
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_remove_shortcut_folder_uri"
               %file-chooser-remove-shortcut-folder-uri) :boolean
  (chooser (g:object file-chooser))
  (uri :string)
  (err :pointer))

(defun file-chooser-remove-shortcut-folder-uri (chooser uri)
 #+liber-documentation
 "@version{2024-11-20}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[uri]{a string with the URI of the folder to remove}
  @begin{return}
    @em{True} if the operation succeeds, @em{false} otherwise.
  @end{return}
  @begin{short}
    Removes a folder URI from a file chooser's list of shortcut folders.
  @end{short}
  See also the @fun{gtk:file-chooser-add-shortcut-folder-uri} function.
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-add-shortcut-folder-uri}"
  (glib:with-error (err)
    (%file-chooser-remove-shortcut-folder-uri chooser uri err)))

(export 'file-chooser-remove-shortcut-folder-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_list_shortcut_folder_uris
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_list_shortcut_folder_uris"
                file-chooser-list-shortcut-folder-uris) (g:slist-t :string)
 #+liber-documentation
 "@version{2023-6-11}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @begin{return}
    A list of strings with the folder URIs, or @code{nil} if there are no
    shortcut folders.
  @end{return}
  @begin{short}
    Queries the list of shortcut folders in the file chooser, as set by the
    @fun{gtk:file-chooser-add-shortcut-folder-uri} function.
  @end{short}
  @see-class{gtk:file-chooser}
  @see-function{gtk:file-chooser-add-shortcut-folder-uri}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-list-shortcut-folder-uris)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_current_folder_file
;;; gtk_file_chooser_set_current_folder_file
;;; ----------------------------------------------------------------------------

(defun (setf file-chooser-current-folder-file) (file chooser)
  (glib:with-error (err)
    (when (cffi:foreign-funcall "gtk_file_chooser_set_current_folder_file"
                                (g:object file-chooser) chooser
                                (g:object g:file) file
                                :pointer err
                                :boolean)
      file)))

(cffi:defcfun ("gtk_file_chooser_get_current_folder_file"
                file-chooser-current-folder-file) (g:object g:file)
 #+liber-documentation
 "@version{#2024-11-20}
  @syntax{(gtk:file-chooser-current-folder-file chooser) => file}
  @syntax{(setf (gtk:file-chooser-current-folder-file chooser) file)}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[file]{a @class{g:file} object for the folder}
  @begin{short}
    The @fun{gtk:file-chooser-current-folder-file} function gets the current
    folder of the file chooser as a @class{g:file} object.
  @end{short}
  The @setf{gtk:file-chooser-current-folder-file} function sets the current
  folder. This function returns @code{nil}, if the folder could not be changed
  successfully.

  Internal function, see the @fun{gtk:file-chooser-current-folder-uri} function.
  @see-class{gtk:file-chooser}
  @see-class{g:file}
  @see-function{gtk:file-chooser-current-folder-uri}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-current-folder-file)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_file
;;; gtk_file_chooser_set_file
;;; ----------------------------------------------------------------------------

(defun (setf file-chooser-file) (file chooser)
  (glib:with-error (err)
    (cffi:foreign-funcall "gtk_file_chooser_set_file"
                          (g:object file-chooser) chooser
                          (g:object g:file) file
                          :pointer err
                          :boolean)
     file))

(cffi:defcfun ("gtk_file_chooser_get_file" file-chooser-file) (g:object g:file)
 #+liber-documentation
 "@version{#2024-11-20}
  @syntax{(gtk:file-chooser-file chooser) => file}
  @syntax{(setf (gtk:file-chooser-file chooser) file)}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[file]{a @class{g:file} object for the file}
  @begin{short}
    The @fun{gtk:file-choose-file} function gets the @class{g:file} object for
   the currently selected file in the file selector.
  @end{short}
  If multiple files are selected, one of the files will be returned at random.
  If the file chooser is in folder mode, this function returns the selected
  folder.

  The @setf{gtk:file-chooser-file} function sets @arg{file} as the current
  filename for the file chooser, by changing to the file's parent folder and
  actually selecting the file in list. If the chooser is in @code{:save} mode,
  the file's base name will also appear in the dialog's file name entry.

  If the file name is not in the current folder of the file chooser, then the
  current folder of the file chooser will be changed to the folder containing
  @arg{file}. This is equivalent to a sequence of the
  @fun{gtk:file-chooser-unselect-all} function followed by the
  @fun{gtk:file-chooser-select-filename} function.

  Note that the file must exist, or nothing will be done except for the
  directory change.

  If you are implementing a File/Save As... dialog, you should use this
  function if you already have a file name to which the user may save. For
  example, when the user opens an existing file and then does File/Save As...
  on it. If you do not have a file name already - for example, if the user just
  created a new file and is saving it for the first time, do not call this
  function. Instead, use something similar to this:
  @begin{pre}
(if document-is-new
    (progn
      ;; the user just created a new document
      (setf (gtk:file-chooser-current-folder-file chooser)
            default-file-for-saving)
      (setf (gtk:file-chooser-current-name chooser \"Untitled document\")))
      (progn
        ;; the user edited an existing document
        (setf (gtk:file-chooser-file chooser) existing-file)))
  @end{pre}
  @see-class{gtk:file-chooser}
  @see-class{g:file}
  @see-function{gtk:file-chooser-unselect-all}
  @see-function{gtk:file-chooser-select-filename}
  @see-function{gtk:file-chooser-current-folder-file}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-file)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_files
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_get_files" file-chooser-files)
    (g:slist-t (g:object g:file))
 #+liber-documentation
 "@version{#2023-3-14}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @begin{return}
    A list containing a @class{g:file} object for each selected file and
    subfolder in the current folder.
  @end{return}
  @begin{short}
    Lists all the selected files and subfolders in the current folder of
    the file chooser as a list of @class{g:file} objects.
  @end{short}
  This is an internal function, see the @fun{gtk:file-chooser-uris} function.
  @see-class{gtk:file-chooser}
  @see-class{g:file}
  @see-function{gtk:file-chooser-uris}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-files)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_get_preview_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_get_preview_file" file-chooser-preview-file)
    (g:object g:file)
 #+liber-documentation
 "@version{#2023-3-14}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @begin{return}
    The @class{g:file} object for the file to preview, or @code{nil} if no file
    is selected.
  @end{return}
  @begin{short}
    Gets the @class{g:file} object that should be previewed in a custom preview.
  @end{short}
  Internal function, see the @fun{gtk:file-chooser-preview-uri} function.
  @see-class{gtk:file-chooser}
  @see-class{g:file}
  @see-function{gtk:file-chooser-preview-uri}"
  (chooser (g:object file-chooser)))

(export 'file-chooser-preview-file)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_select_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_select_file" %file-chooser-select-file)
    :boolean
  (chooser (g:object file-chooser))
  (file (g:object g:file))
  (err :pointer))

(defun file-chooser-select-file (chooser file)
 #+liber-documentation
 "@version{#2024-11-20}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[file]{a @class{g:file} object to select}
  @begin{short}
    Selects the file referred to by @arg{file}.
  @end{short}
  An internal function. See the @fun{gtk:file-chooser-select-uri} function.
  @see-class{gtk:file-chooser}
  @see-class{g:file}
  @see-function{gtk:file-chooser-select-uri}"
  (glib:with-error (err)
    (%file-chooser-select-file chooser file err)))

(export 'file-chooser-select-file)

;;; ----------------------------------------------------------------------------
;;; gtk_file_chooser_unselect_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_chooser_unselect_file" file-chooser-unselect-file)
    :void
 #+liber-documentation
 "@version{#2023-3-14}
  @argument[chooser]{a @class{gtk:file-chooser} widget}
  @argument[file]{a @class{g:file} object}
  @begin{short}
    Unselects the file referred to by @arg{file}.
  @end{short}
  If the file is not in the current directory, does not exist, or is otherwise
  not currently selected, does nothing.
  @see-class{gtk:file-chooser}
  @see-class{g:file}
  @see-function{gtk:file-chooser-select-file}"
  (chooser (g:object file-chooser))
  (file (g:object g:file)))

(export 'file-chooser-unselect-file)

;;; --- End of file gtk3.file-chooser.lisp -------------------------------------
