;;; ----------------------------------------------------------------------------
;;; gtk3.print-operation.lisp
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
;;; GtkPrintOperation
;;;
;;;     High-level Printing API
;;;
;;; Types and Values
;;;
;;;     GtkPrintStatus
;;;     GtkPrintOperationAction
;;;     GtkPrintOperationResult
;;;     GtkPrintError
;;;
;;;     GTK_PRINT_ERROR
;;;
;;;     GtkPrintOperationPreview
;;;     GtkPrintOperation
;;;
;;; Functions
;;;
;;;     gtk_print_operation_new
;;;     gtk_print_operation_set_allow_async                Accessor
;;;     gtk_print_operation_get_error
;;;     gtk_print_operation_set_default_page_setup         Accessor
;;;     gtk_print_operation_get_default_page_setup         Accessor
;;;     gtk_print_operation_set_print_settings             Accessor
;;;     gtk_print_operation_get_print_settings             Accessor
;;;     gtk_print_operation_set_job_name                   Accessor
;;;     gtk_print_operation_set_n_pages                    Accessor
;;;     gtk_print_operation_get_n_pages_to_print           Accessor
;;;     gtk_print_operation_set_current_page               Accessor
;;;     gtk_print_operation_set_use_full_page              Accessor
;;;     gtk_print_operation_set_unit                       Accessor
;;;     gtk_print_operation_set_export_filename            Accessor
;;;     gtk_print_operation_set_show_progress              Accessor
;;;     gtk_print_operation_set_track_print_status         Accessor
;;;     gtk_print_operation_set_custom_tab_label           Accessor
;;;
;;;     gtk_print_operation_run
;;;     gtk_print_operation_cancel
;;;     gtk_print_operation_draw_page_finish
;;;     gtk_print_operation_set_defer_drawing
;;;     gtk_print_operation_get_status                     Accessor
;;;     gtk_print_operation_get_status_string              Accessor
;;;     gtk_print_operation_is_finished
;;;     gtk_print_operation_set_support_selection          Accessor
;;;     gtk_print_operation_get_support_selection          Accessor
;;;     gtk_print_operation_set_has_selection              Accessor
;;;     gtk_print_operation_get_has_selection              Accessor
;;;     gtk_print_operation_set_embed_page_setup           Accessor
;;;     gtk_print_operation_get_embed_page_setup           Accessor
;;;     gtk_print_run_page_setup_dialog
;;;
;;;     GtkPageSetupDoneFunc
;;;     gtk_print_run_page_setup_dialog_async
;;;
;;;     gtk_print_operation_preview_end_preview
;;;     gtk_print_operation_preview_is_selected
;;;     gtk_print_operation_preview_render_page
;;;
;;; Properties
;;;
;;;     allow-async
;;;     current-page
;;;     custom-tab-label
;;;     default-page-setup
;;;     embed-page-setup
;;;     export-filename
;;;     has-selection
;;;     job-name
;;;     n-pages
;;;     n-pages-to-print
;;;     print-settings
;;;     show-progress
;;;     status
;;;     status-string
;;;     support-selection
;;;     track-print-status
;;;     unit
;;;     use-full-page
;;;
;;; Signals
;;;
;;;     begin-print
;;;     create-custom-widget
;;;     custom-widget-apply
;;;     done
;;;     draw-page
;;;     end-print
;;;     paginate
;;;     preview
;;;     request-page-setup
;;;     status-changed
;;;     update-custom-widget
;;;     got-page-size
;;;     ready
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkPrintOperationPreview
;;;
;;;     GObject
;;;     ╰── GtkPrintOperation
;;;
;;; Implemented Interfaces
;;;
;;;     GtkPrintOperation implements GtkPrintOperationPreview.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPrintStatus
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkPrintStatus" print-status
  (:export t
   :type-initializer "gtk_print_status_get_type")
  (:initial 0)
  (:preparing 1)
  (:generating-data 2)
  (:sending-data 3)
  (:pending 4)
  (:pending-issue 5)
  (:printing 6)
  (:finished 7)
  (:finished-aborted 8))

#+liber-documentation
(setf (liber:alias-for-symbol 'print-status)
      "GEnum"
      (liber:symbol-documentation 'print-status)
 "@version{2024-3-22}
  @begin{declaration}
(gobject:define-genum \"GtkPrintStatus\" print-status
  (:export t
   :type-initializer \"gtk_print_status_get_type\")
  (:initial 0)
  (:preparing 1)
  (:generating-data 2)
  (:sending-data 3)
  (:pending 4)
  (:pending-issue 5)
  (:printing 6)
  (:finished 7)
  (:finished-aborted 8))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:initial]{The printing has not started yet. This status is set
        initially, and while the print dialog is shown.}
      @entry[:preparing]{This status is set while the @code{\"begin-print\"}
        signal is emitted and during pagination.}
      @entry[:generating-data]{This status is set while the pages are being
        rendered.}
      @entry[:sending-data]{The print job is being sent off to the printer.}
      @entry[:pending]{The print job has been sent to the printer, but is not
        printed for some reason, e.g. the printer may be stopped.}
      @entry[:pending-issue]{Some problem has occurred during printing,
        e.g. a paper jam.}
      @entry[:printing]{The printer is processing the print job.}
      @entry[:finished]{The printing has been completed successfully.}
      @entry[:finished-aborted]{The printing has been aborted.}
    @end{table}
  @end{values}
  @begin{short}
    The status gives a rough indication of the completion of a running print
    operation.
  @end{short}
  @see-class{gtk:print-operation}")

;;; ----------------------------------------------------------------------------
;;; GtkPrintOperationAction
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkPrintOperationAction" print-operation-action
  (:export t
   :type-initializer "gtk_print_operation_action_get_type")
  (:print-dialog 0)
  (:print 1)
  (:preview 2)
  (:export 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'print-operation-action)
      "GEnum"
      (liber:symbol-documentation 'print-operation-action)
 "@version{2024-3-22}
  @begin{declaration}
(gobject:define-genum \"GtkPrintOperationAction\" gtk:print-operation-action
  (:export t
   :type-initializer \"gtk_print_operation_action_get_type\")
  (:print-dialog 0)
  (:print 1)
  (:preview 2)
  (:export 3))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:print-dialog]{Show the print dialog.}
      @entry[:print]{Start to print without showing the print dialog, based on
        the current print settings.}
      @entry[:preview]{Show the print preview.}
      @entry[:export]{Export to a file. This requires the
        @slot[gtk:print-operation]{export-filename} property to be set.}
    @end{table}
  @end{values}
  @begin{short}
    The action parameter to the @fun{gtk:print-operation-run} function
    determines what action the print operation should perform.
  @end{short}
  @see-class{gtk:print-operation}
  @see-function{gtk:print-operation-run}
  @see-function{gtk:print-operation-export-filename}")

;;; ----------------------------------------------------------------------------
;;; GtkPrintOperationResult
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkPrintOperationResult" print-operation-result
  (:export t
   :type-initializer "gtk_print_operation_result_get_type")
  (:error 0)
  (:apply 1)
  (:cancel 2)
  (:in-progress 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'print-operation-result)
      "GEnum"
      (liber:symbol-documentation 'print-operation-result)
 "@version{2024-3-22}
  @begin{declaration}
(gobject:define-genum \"GtkPrintOperationResult\" print-operation-result
  (:export t
   :type-initializer \"gtk_print_operation_result_get_type\")
  (:error 0)
  (:apply 1)
  (:cancel 2)
  (:in-progress 3))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:error]{An error has occured.}
      @entry[:apply]{The print settings should be stored.}
      @entry[:cancel]{The print operation has been canceled, the print settings
        should not be stored.}
      @entry[:in-progress]{The print operation is not complete yet. This value
        will only be returned when running asynchronously.}
    @end{table}
  @end{values}
  @begin{short}
    A value of the @symbol{gtk:print-operation-result} enumeration is returned
    by the @fun{gtk:print-operation-run} function.
  @end{short}
  @see-class{gtk:print-operation}
  @see-function{gtk:print-operation-run}")

;;; ----------------------------------------------------------------------------
;;; GtkPrintError                                           not exported
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkPrintError" print-error
  (:export nil
   :type-initializer "gtk_print_error_get_type")
  (:general 0)
  (:internal-error 1)
  (:nomem 2)
  (:invalid-file 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'print-error)
      "GEnum"
      (liber:symbol-documentation 'print-error)
 "@version{2024-3-22}
  @begin{declaration}
(gobject:define-genum \"GtkPrintError\" print-error
  (:export t
   :type-initializer \"gtk_print_error_get_type\")
  (:general 0)
  (:internal-error 1)
  (:nomem 2)
  (:invalid-file 3))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:general]{An unspecified error occurred.}
      @entry[:internal-error]{An internal error occurred.}
      @entry[:nomem]{A memory allocation failed.}
      @entry[:invalid-file]{An error occurred while loading a page setup or
        paper size from a key file.}
    @end{table}
  @end{values}
  @begin{short}
    Error codes that identify various errors that can occur while using the
    GTK printing support.
  @end{short}
  @see-class{gtk:print-operation}")

;;; ----------------------------------------------------------------------------
;;; GTK_PRINT_ERROR
;;;
;;; #define GTK_PRINT_ERROR gtk_print_error_quark ()
;;;
;;; The error domain for GtkPrintError errors.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkPrintOperationPreview
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GtkPrintOperationPreview" print-operation-preview
  (:export t
   :type-initializer "gtk_print_operation_preview_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'print-operation-preview)
      "Interface"
      (documentation 'print-operation-preview 'type)
 "@version{2023-2-10}
  @begin{short}
    The @class{gtk:print-operation-preview} interface is implemented by the
    @class{gtk:print-operation} class.
  @end{short}
  By default the @class{gtk:print-operation} object uses an external
  application to do print preview. To implement a custom print preview, an
  application must connect to the @code{\"preview\"} signal. The
  @fun{gtk:print-operation-preview-render-page},
  @fun{gtk:print-operation-preview-end-preview} and
  @fun{gtk:print-operation-preview-is-selected} functions are useful when
  implementing a print preview.
  @begin[Signal Details]{dictionary}
    @subheading{The \"got-page-size\" signal}
      @begin{pre}
lambda (preview context setup)    :run-last
      @end{pre}
      The signal is emitted once for each page that gets rendered to the
      preview. A handler for this signal should update the context according to
      the @arg{setup} argument and set up a suitable Cairo context, using the
       @fun{gtk:print-context-set-cairo-context} function.
      @begin[code]{table}
        @entry[preview]{The @class{gtk:print-operation-preview} object on which
          the signal is emitted.}
        @entry[context]{The current @class{gtk:print-context} object.}
        @entry[setup]{The @class{gtk:page-setup} object for the current page.}
      @end{table}
    @subheading{The \"ready\" signal}
      @begin{pre}
lambda (preview context)    :run-last
      @end{pre}
      The signal gets emitted once per preview operation, before the first page
      is rendered. A handler for this signal can be used for setup tasks.
      @begin[code]{table}
        @entry[preview]{The @class{gtk:print-operation-preview} object on which
          the signal is emitted.}
        @entry[context]{The current @class{gtk:print-context} object.}
      @end{table}
  @end{dictionary}
  @see-class{gtk:print-operation}
  @see-function{gtk:print-operation-preview-render-page}
  @see-function{gtk:print-operation-preview-end-preview}
  @see-function{gtk:print-operation-preview-is-selected}")

;;; ----------------------------------------------------------------------------
;;; GtkPrintOperation
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkPrintOperation" print-operation
  (:superclass g:object
   :export t
   :interfaces ("GtkPrintOperationPreview")
   :type-initializer "gtk_print_operation_get_type")
  ((allow-async
    print-operation-allow-async
    "allow-async" "gboolean" t t)
   (current-page
    print-operation-current-page
    "current-page" "gint" t t)
   (custom-tab-label
    print-operation-custom-tab-label
    "custom-tab-label" "gchararray" t t)
   (default-page-setup
    print-operation-default-page-setup
    "default-page-setup" "GtkPageSetup" t t)
   (embed-page-setup
    print-operation-embed-page-setup
    "embed-page-setup" "gboolean" t t)
   (export-filename
    print-operation-export-filename
    "export-filename" "gchararray" t t)
   (has-selection
    print-operation-has-selection
    "has-selection" "gboolean" t t)
   (job-name
    print-operation-job-name
    "job-name" "gchararray" t t)
   (n-pages
    print-operation-n-pages
    "n-pages" "gint" t t)
   (n-pages-to-print
    print-operation-n-pages-to-print
    "n-pages-to-print" "gint" t nil)
   (print-settings
    print-operation-print-settings
    "print-settings" "GtkPrintSettings" t t)
   (show-progress
    print-operation-show-progress
    "show-progress" "gboolean" t t)
   (status
    print-operation-status
    "status" "GtkPrintStatus" t nil)
   (status-string
    print-operation-status-string
    "status-string" "gchararray" t nil)
   (support-selection
    print-operation-support-selection
    "support-selection" "gboolean" t t)
   (track-print-status
    print-operation-track-print-status
    "track-print-status" "gboolean" t t)
   (unit
    print-operation-unit
    "unit" "GtkUnit" t t)
   (use-full-page
    print-operation-use-full-page
    "use-full-page" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'print-operation 'type)
 "@version{2023-2-10}
  @begin{short}
    The @class{gtk:print-operation} class is the high-level, portable printing
    API.
  @end{short}
  It looks a bit different than other GTK dialogs such as the
  @class{gtk:file-chooser} widget, since some platforms do not expose enough
  infrastructure to implement a good print dialog. On such platforms,
  the @class{gtk:print-operation} class uses the native print dialog. On
  platforms which do not provide a native print dialog, GTK uses its own, see
  the @class{gtk:print-unix-dialog} widget.

  The typical way to use the high-level printing API is to create a
  @class{gtk:print-operation} object with the @fun{gtk:print-operation-new}
  function when the user selects to print. Then you set some properties on it,
  e.g. the page size, any @class{gtk:print-settings} settings from previous
  print operations, the number of pages, the current page, etc.

  Then you start the print operation by calling the
  @fun{gtk:print-operation-run} function. It will then show a dialog, let the
  user select a printer and options. When the user finished the dialog various
  signals will be emitted on the @class{gtk:print-operation} object, the main
  one being the @code{\"draw-page\"} signal, which you are supposed to catch
  and render the page on the provided @class{gtk:print-context} object using
  Cairo.

  By default the @class{gtk:print-operation} object uses an external application
  to do print preview. To implement a custom print preview, an application must
  connect to the @code{\"preview\"} signal. The
  @fun{gtk:print-operation-preview-render-page},
  @fun{gtk:print-operation-preview-end-preview} and
  @fun{gtk:print-operation-preview-is-selected} functions are useful when
  implementing a print preview.
  @begin[Examples]{dictionary}
    The high-level printing API.
    @begin{pre}
(defvar *print-settings* nil)

(defun do-print-operation (window)
  (let ((response nil)
        (print (gtk:print-operation-new)))
    ;; Connect signal handlers for the print operation
    (g:signal-connect print \"draw-page\" #'draw-page)
    (g:signal-connect print \"begin-print\" #'begin-print)
    ;; Restore the print settings
    (when *print-settings*
      (setf (gtk:print-operation-print-settings print) *print-settings*))
    ;; Perform the print operation
    (setf response (gtk:print-operation-run print :print-dialog window))
    ;; Check the response and save the print settings
    (when (eq :apply response)
      (setf *print-settings*
            (gtk:print-operation-print-settings print)))))
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"begin-print\" signal}
      @begin{pre}
lambda (operation context)    :run-last
      @end{pre}
      Emitted after the user has finished changing print settings in the dialog,
      before the actual rendering starts. A typical use for the
      @code{\"begin-print\"} signal is to use the parameters from the
      @class{gtk:print-context} object and paginate the document accordingly,
      and then set the number of pages with the
      @fun{gtk:print-operation-n-pages} function.
      @begin[code]{table}
        @entry[operation]{The @class{gtk:print-operation} object on which the
          signal was emitted.}
        @entry[context]{The @class{gtk:print-context} object for the current
          operation.}
      @end{table}
    @subheading{The \"create-custom-widget\" signal}
      @begin{pre}
lambda (operation)    :run-last
      @end{pre}
      Emitted when displaying the print dialog. If you return a widget in a
      handler for this signal it will be added to a custom tab in the print
      dialog. You typically return a container widget with multiple widgets in
      it. The print dialog owns the returned widget, and its lifetime is not
      controlled by the application. However, the widget is guaranteed to stay
      around until the @code{\"custom-widget-apply\"} signal is emitted on the
      operation. Then you can read out any information you need from the
      widgets.
      @begin[code]{table}
        @entry[operation]{The @class{gtk:print-operation} object on which the
          signal was emitted.}
        @entry[Returns]{A @class{gtk:widget} custom widget that gets embedded
          in the print dialog, or @code{nil}.}
      @end{table}
    @subheading{The \"custom-widget-apply\" signal}
      @begin{pre}
lambda (operation widget)    :run-last
      @end{pre}
      Emitted right before the @code{\"begin-print\"} signal if you added a
      custom widget in the @code{\"create-custom-widget\"} handler. When you get
      this signal you should read the information from the custom widgets, as
      the widgets are not guaraneed to be around at a later time.
      @begin[code]{table}
        @entry[operation]{The @class{gtk:print-operation} object on which the
          signal was emitted.}
        @entry[widget]{The @class{gtk:widget} custom widget added in a
          @code{\"create-custom-widget\"} handler.}
      @end{table}
    @subheading{The \"done\" signal}
      @begin{pre}
lambda (operation result)    :run-last
      @end{pre}
      Emitted when the print operation run has finished doing everything
      required for printing. The @arg{result} parameter gives you information
      about what happened during the run. If @arg{result} is @code{:error} then
      you can call the @code{gtk_print_operation_get_error ()} function for more
      information. If you enabled print status tracking then the
      @fun{gtk:print-operation-is-finished} function may still return @em{false}
      after the @code{\"done\"} signal was emitted.
      @begin[code]{table}
        @entry[operation]{The @class{gtk:print-operation} object on which the
          signal was emitted.}
        @entry[result]{The @symbol{gtk:print-operation-result} value with the
        result of the print operation.}
      @end{table}
    @subheading{The \"draw-page\" signal}
      @begin{pre}
lambda (operation context pagenum)    :run-last
      @end{pre}
      Emitted for every page that is printed. The signal handler must render the
      @arg{pagenum} page onto the Cairo context obtained from the @arg{context}
      parameter using the @fun{gtk:print-context-cairo-context} function. Use
      the @fun{gtk:print-operation-use-full-page} and
      @fun{gtk:print-operation-unit} functions before starting the print
      operation to set up the transformation of the Cairo context according to
      your needs.
      @begin{pre}
(defun draw-page (operation context pagenum)
  (declare (ignore operation pagenum))
  (let ((text-height 0)
        (cr (gtk:print-context-cairo-context context))
        (width (floor (gtk:print-context-get-width context)))
        (layout (gtk:print-context-create-pango-layout context)))
    ;; Print a grey colored header
    (cairo-rectangle cr 0 0 width *header-height*)
    (cairo-set-source-rgb cr 0.9 0.9 0.9)
    (cairo-fill cr)
    ;; Set the font and text to print
    (setf (pango-layout-font-description layout)
          (pango:font-description-from-string \"sans 14\"))
    (setf (pango-layout-text layout) \"Title\")
    (setf (pango-layout-width layout) (* width pango:+scale+))
    (setf (pango-layout-alignment layout) :center)
    ;; Get the height of the text
    (multiple-value-bind (width height)
        (pango-layout-size layout)
      (setf text-height (/ height pango:+scale+)))
    ;; Set color to black and center the text in header
    (cairo-set-source-rgb cr 0.0 0.0 0.0)
    (cairo-move-to cr 0 (floor (/ (- *header-height* text-height) 2)))
    (pango-cairo-show-layout cr layout)))
      @end{pre}
      @begin[code]{table}
        @entry[operation]{The @class{gtk:print-operation} object on which the
          signal was emitted.}
        @entry[context]{The @class{gtk:print-context} object for the current
          operation.}
        @entry[pagenum]{An integer with the 0-based number of the currently
          printed page.}
      @end{table}
    @subheading{The \"end-print\" signal}
      @begin{pre}
lambda (operation context)    :run-last
      @end{pre}
      Emitted after all pages have been rendered. A handler for this signal can
      clean up any resources that have been allocated in the \"begin-print\"
      handler.
      @begin[code]{table}
        @entry[operation]{The @class{gtk:print-operation} object on which the
          signal was emitted.}
        @entry[context]{The @class{gtk:print-context} object for the current
          operation.}
      @end{table}
    @subheading{The \"paginate\" signal}
      @begin{pre}
lambda (operation context)    :run-last
      @end{pre}
      Emitted after the @code{\"begin-print\"} signal, but before the actual
      rendering starts. It keeps getting emitted until a connected signal
      handler returns @em{true}. The @code{\"paginate\"} signal is intended to
      be used for paginating a document in small chunks, to avoid blocking the
      user interface for a long time. The signal handler should update the
      number of pages using the @fun{gtk:print-operation-n-pages} function, and
      return @em{true} if the document has been completely paginated. If you do
      not need to do pagination in chunks, you can simply do it all in the
      @code{\"begin-print\"} handler, and set the number of pages from there.
      @begin[code]{table}
        @entry[operation]{The @class{gtk:print-operation} object on which the
          signal was emitted.}
        @entry[context]{The @class{gtk:print-context} object for the current
          operation.}
        @entry[Returns]{@em{True} if pagination is complete.}
      @end{table}
    @subheading{The \"preview\" signal}
      @begin{pre}
lambda (operation preview context parent)    :run-last
      @end{pre}
      Gets emitted when a preview is requested from the native dialog. The
      default handler for this signal uses an external viewer application to
      preview. To implement a custom print preview, an application must return
      @em{true} from its handler for this signal. In order to use the provided
      context for the preview implementation, it must be given a suitable Cairo
      context with the @fun{gtk:print-context-set-cairo-context} function. The
      custom preview implementation can use the
      @fun{gtk:print-operation-preview-is-selected} and
      @fun{gtk:print-operation-preview-render-page} functions to find pages
      which are selected for print and render them. The preview must be finished
      by calling the @fun{gtk:print-operation-preview-end-preview} function,
      typically in response to the user clicking a Close button.
      @begin[code]{table}
        @entry[operation]{The @class{gtk:print-operation} object on which the
          signal was emitted.}
        @entry[preview]{The @class{gtk:print-operation-preview} object for the
          current operation.}
        @entry[context]{The @class{gtk:print-context} object that will be used.}
        @entry[parent]{The @class{gtk:window} widget to use as window parent,
          or @code{nil}.}
        @entry[Returns]{@em{True} if the listener wants to take over control of
          the preview.}
      @end{table}
    @subheading{The \"request-page-setup\" signal}
      @begin{pre}
lambda (operation context pagenum setup)    :run-last
      @end{pre}
      Emitted once for every page that is printed, to give the application a
      chance to modify the page setup. Any changes done to the page setup will
      be in force only for printing this page.
      @begin[code]{table}
        @entry[operation]{The @class{gtk:print-operation} object on which the
          signal was emitted.}
        @entry[context]{The @class{gtk:print-context} object for the current
          operation.}
        @entry[pagenum]{An integer with the 0-based number of the currently
          printed page.}
        @entry[setup]{The @class{gtk:page-setup} object.}
      @end{table}
    @subheading{The \"status-changed\" signal}
      @begin{pre}
lambda (operation)    :run-last
      @end{pre}
      Emitted at between the various phases of the print operation. See the
      @symbol{gtk:print-status} enumeration for the phases that are being
      discriminated. Use the @fun{gtk:print-operation-status} function to
      find out the current status.
      @begin[code]{table}
        @entry[operation]{The @class{gtk:print-operation} object on which the
          signal was emitted.}
      @end{table}
    @subheading{The \"update-custom-widget\" signal}
      @begin{pre}
lambda (operation widget setup settings)    :run-last
      @end{pre}
      Emitted after change of the selected printer. The actual page setup and
      print settings are passed to the custom widget, which can actualize
      itself according to this change.
      @begin[code]{table}
        @entry[operation]{The @class{gtk:print-operation} object on which the
          signal was emitted.}
        @entry[widget]{The @class{gtk:widget} custom widget added in the
          @code{\"create-custom-widget\"} handler.}
        @entry[setup]{Actual @class{gtk:page-setup} object.}
        @entry[settings]{Actual @class{gtk:print-settings} object.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:print-operation-new}
  @see-slot{gtk:print-operation-allow-async}
  @see-slot{gtk:print-operation-current-page}
  @see-slot{gtk:print-operation-custom-tab-label}
  @see-slot{gtk:print-operation-default-page-setup}
  @see-slot{gtk:print-operation-embed-page-setup}
  @see-slot{gtk:print-operation-export-filename}
  @see-slot{gtk:print-operation-has-selection}
  @see-slot{gtk:print-operation-job-name}
  @see-slot{gtk:print-operation-n-pages}
  @see-slot{gtk:print-operation-n-pages-to-print}
  @see-slot{gtk:print-operation-print-settings}
  @see-slot{gtk:print-operation-show-progress}
  @see-slot{gtk:print-operation-status}
  @see-slot{gtk:print-operation-status-string}
  @see-slot{gtk:print-operation-support-selection}
  @see-slot{gtk:print-operation-track-print-status}
  @see-slot{gtk:print-operation-unit}
  @see-slot{gtk:print-operation-use-full-page}
  @see-class{gtk:print-context}
  @see-class{gtk:print-settings}
  @see-class{gtk:print-operation-preview}
  @see-class{gtk:page-setup}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:print-operation-allow-async ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "allow-async"
                                               'print-operation) t)
 "The @code{allow-async} property of type @code{:boolean} (Read / Write) @br{}
  Determines whether the print operation may run asynchronously or not. Some
  systems do not support asynchronous printing, but those that do will return
  @code{:in-progress} as the status, and emit the @code{\"done\"} signal when
  the operation is actually done. The Windows port does not support asynchronous
  operation at all, this is unlikely to change. On other platforms, all actions
  except for @code{:export} support asynchronous operation. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'print-operation-allow-async)
      "Accessor"
      (documentation 'print-operation-allow-async 'function)
 "@version{2023-2-10}
  @syntax{(gtk:print-operation-allow-async object) => setting}
  @syntax{(setf (gtk:print-operation-allow-aysnc object) setting)}
  @argument[object]{a @class{gtk:print-operation} object}
  @argument[setting]{@em{true} to allow asynchronous operation}
  @begin{short}
    Accessor of the @slot[gtk:print-operation]{allow-async} slot of the
    @class{gtk:print-operation} class.
  @end{short}
  The @fun{(setf gtk:print-operation-allow-async)} function sets whether the
  @fun{gtk:print-operation-run} function may return before the print operation
  is completed. Note that some platforms may not allow asynchronous operation.
  @see-class{gtk:print-operation}
  @see-function{gtk:print-operation-run}")

;;; --- gtk:print-operation-current-page ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "current-page"
                                               'print-operation) t)
 "The @code{current-page} property of type @code{:int} (Read / Write) @br{}
  The current page in the document. If this is set before the
  @fun{gtk:print-operation-run} function, the user will be able to select to
  print only the current page. Note that this only makes sense for pre-paginated
  documents. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'print-operation-current-page)
      "Accessor"
      (documentation 'print-operation-current-page 'function)
 "@version{2023-2-10}
  @syntax{(gtk:print-operation-current-page object) => page}
  @syntax{(setf (gtk:print-operation-current-page object) page)}
  @argument[object]{a @class{gtk:print-operation} object}
  @argument[page]{an integer with the current page, 0-based}
  @begin{short}
    Accessor of the @slot[gtk:print-operation]{current-page} slot of the
    @class{gtk:print-operation} class.
  @end{short}
  The @setf{gtk:print-operation-current-page} function sets the current page.
  If this is called before the @fun{gtk:print-operation-run} function, the user
  will be able to select to print only the current page. Note that this only
  makes sense for pre-paginated documents.
  @see-class{gtk:print-operation}
  @see-function{gtk:print-operation-run}")

;;; --- gtk:print-operation-custom-tab-label -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "custom-tab-label"
                                               'print-operation) t)
 "The @code{custom-tab-label} property of type @code{:string} (Read / Write)
  @br{}
  Used as the label of the tab containing custom widgets. Note that this
  property may be ignored on some platforms. If this is @code{nil}, GTK uses
  a default label. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'print-operation-custom-tab-label)
      "Accessor"
      (documentation 'print-operation-custom-tab-label 'function)
 "@version{2023-2-10}
  @syntax{(gtk:print-operation-tab-label object) => label}
  @syntax{(setf (gtk:print-operation-tab-label object) label)}
  @argument[object]{a @class{gtk:print-operation} object}
  @argument[label]{a string with the label to use, or @code{nil} to use
    the default label}
  @begin{short}
    Accessor of the @slot[gtk:print-operation]{custom-tab-label} slot of the
    @class{gtk:print-operation} class.
  @end{short}
  The @setf{gtk:print-operation-custom-tab-label} function sets the label for
  the tab holding custom widgets.
  @see-class{gtk:print-operation}")

;;; --- gtk:print-operation-default-page-setup ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "default-page-setup"
                                               'print-operation) t)
 "The @code{default-page-setup} property of type @class{gtk:page-setup}
  (Read / Write) @br{}
  The page setup used by default. This page setup will be used by the
  @fun{gtk:print-operation-run} function, but it can be overridden on a per-page
  basis by connecting to the @code{\"request-page-setup\"} signal.")

#+liber-documentation
(setf (liber:alias-for-function 'print-operation-default-page-setup)
      "Accessor"
      (documentation 'print-operation-default-page-setup 'function)
 "@version{2023-2-10}
  @syntax{(gtk:print-operation-default-page-setup object) => setup}
  @syntax{(setf (gtk:print-operation-default-page-setup object) setup)}
  @argument[object]{a @class{gtk:print-operation} object}
  @argument[setup]{a @class{gtk:page-setup} object, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:print-operation]{default-page-setup} slot of the
    @class{gtk:print-operation} class.
  @end{short}
  The @fun{gtk:print-operation-default-page-setup} function returns the default
  page setup. The @setf{gtk:print-operation-current-page} function makes
  @arg{setup} the default page setup for the print operation. This page setup
  will be used by the @fun{gtk:print-operation-run} function, but it can be
  overridden on a per-page basis by connecting to the
  @code{\"request-page-setup\"} signal.
  @see-class{gtk:print-operation}
  @see-class{gtk:page-setup}
  @see-function{gtk:print-operation-run}")

;;; --- gtk:print-operation-embed-page-setup -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "embed-page-setup"
                                               'print-operation) t)
 "The @code{embed-page-setup} property of type @code{:boolean} (Read / Write)
  @br{}
  If @em{true}, the page size combo box and the orientation combo box are
  embedded into the page setup page. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'print-operation-embed-page-setup)
      "Accessor"
      (documentation 'print-operation-embed-page-setup 'function)
 "@version{2023-2-10}
  @syntax{(gtk:print-operation-embed-page-setup object) => embed}
  @syntax{(setf (gtk:print-operation-embed-page-setup object) embed)}
  @argument[object]{a @class{gtk:print-operation} object}
  @argument[embed]{@em{true} to embed page setup selection in the print dialog}
  @begin{short}
    Accessor of the @slot[gtk:print-operation]{embed-page-setup} slot of the
    @class{gtk:print-operation} class.
  @end{short}
  The @fun{gtk:print-operation-embed-page-setup} function returns whether page
  setup selection combos are embedded in the print dialog. The
  @setf{gtk:print-operation-embed-page-setup} function embed page size combo box
  and orientation combo box into the print dialog. Selected page setup is stored
  as default page setup in the @class{gtk:print-operation} object.
  @see-class{gtk:print-operation}")

;;; --- gtk:print-operation-export-filename ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "export-filename"
                                               'print-operation) t)
 "The @code{export-filename} property of type @code{:string} (Read / Write)@br{}
  The name of a file to generate instead of showing the print dialog. Currently,
  PDF is the only supported format. The intended use of this property is for
  implementing \"Export to PDF\" actions. \"Print to PDF\" support is
  independent of this and is done by letting the user pick the \"Print to PDF\"
  item from the list of printers in the print dialog. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'print-operation-export-filename)
      "Accessor"
      (documentation 'print-operation-export-filename 'function)
 "@version{2023-2-10}
  @syntax{(gtk:print-operation-export-filename object) => filename}
  @syntax{(setf (gtk:print-operation-export-filename object) filename)}
  @argument[object]{a @class{gtk:print-operation} object}
  @argument[filename]{a string with the filename for the exported file}
  @begin{short}
    Accessor of the @slot[gtk:print-operation]{export-filename} slot of the
    @class{gtk:print-operation} class.
  @end{short}
  The @setf{gtk:print-operation-export-filename} function sets up the
  @class{gtk:print-operation} object to generate a file instead of showing the
  print dialog. The indended use of this function is for implementing \"Export
  to PDF\" actions. Currently, PDF is the only supported format.

  \"Print to PDF\" support is independent of this and is done by letting the
  user pick the \"Print to PDF\" item from the list of printers in the print
  dialog.
  @see-class{gtk:print-operation}")

;;; --- gtk:print-operation-has-selection --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-selection"
                                               'print-operation) t)
 "The @code{has-selection} property of type @code{:boolean} (Read / Write) @br{}
  Determines whether there is a selection in your application. This can allow
  your application to print the selection. This is typically used to make a
  \"Selection\" button sensitive. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'print-operation-has-selection)
      "Accessor"
      (documentation 'print-operation-has-selection 'function)
 "@version{2023-2-10}
  @syntax{(gtk:print-operation-has-selection object object) => setting}
  @syntax{(setf (gtk:print-operation-has-selection object) setting)}
  @argument[object]{a @class{gtk:print-operation} object}
  @argument[setting]{@em{true} indicates that a selection exists}
  @begin{short}
    Accessor of the @slot[gtk:print-operation]{has-selection} slot of the
    @class{gtk:print-operation} class.
  @end{short}
  The @fun{gtk:print-operation-has-selection} function returns whether there is
  a selection. The @setf{gtk:print-operation-has-selection} function sets
  whether there is a selection to print. The application has to set the number
  of pages to which the selection will draw by the
  @fun{gtk:print-operation-n-pages} function in a callback of the
  @code{\"begin-print\"} signal.
  @see-class{gtk:print-operation}
  @see-function{gtk:print-operation-n-pages}")

;;; --- gtk:print-operation-job-name -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "job-name"
                                               'print-operation) t)
 "The @code{job-name} property of type @code{:string} (Read / Write) @br{}
  A string used to identify the job, e.g. in monitoring applications like
  eggcups. If you do not set a job name, GTK picks a default one by numbering
  successive print jobs. @br{}
  Default value: \"\" ")

#+liber-documentation
(setf (liber:alias-for-function 'print-operation-job-name)
      "Accessor"
      (documentation 'print-operation-job-name 'function)
 "@version{2023-2-10}
  @syntax{(gtk:print-operation-job-name object) => name}
  @syntax{(setf (gtk:print-operation-job-name object) name)}
  @argument[object]{a @class{gtk:print-operation} object}
  @argument[name]{a string that identifies the print job}
  @begin{short}
    Accessor of the @slot[gtk:print-operation]{job-name} slot of the
    @class{gtk:print-operation} class.
  @end{short}
  The @setf{gtk:print-operation-job-name} function sets the name of the print
  job. The name is used to identify the job, e.g. in monitoring applications
  like eggcups. If you do not set a job name, GTK picks a default one by
  numbering successive print jobs.
  @see-class{gtk:print-operation}")

;;; --- gtk:print-operation-n-pages --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "n-pages"
                                               'print-operation) t)
 "The @code{n-pages} property of type @code{:int} (Read / Write) @br{}
  The number of pages in the document. This must be set to a positive number
  before the rendering starts. It may be set in a @code{\"begin-print\"} signal
  handler. Note that the page numbers passed to the
  @code{\"request-page-setup\"} and @code{\"draw-page\"} signals are 0-based,
  i.e. if the user chooses to print all pages, the last @code{\"draw-page\"}
  signal will be for page @code{n-pages} - 1. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'print-operation-n-pages)
      "Accessor"
      (documentation 'print-operation-n-pages 'function)
 "@version{2023-2-10}
  @syntax{(gtk:print-operation-n-pages object) => n-pages}
  @syntax{(setf (gtk:print-operation-n-pages object) n-pages)}
  @argument[object]{a @class{gtk:print-operation} object}
  @argument[n-pages]{an integer with the number of pages}
  @begin{short}
    Accessor of the @slot[gtk:print-operation]{n-pages} slot of the
    @class{gtk:print-operation} class.
  @end{short}
  The @setf{gtk:print-operation-n-pages} function sets the number of pages in
  the document. This must be set to a positive number before the rendering
  starts. It may be set in a @code{\"begin-print\"} signal hander.

  Note that the page numbers passed to the @code{\"request-page-setup\"} and
  @code{\"draw-page\"} signals are 0-based, i.e. if the user chooses to print
  all pages, the last @code{\"draw-page\"} signal will be for page @arg{n-pages}
  - 1.
  @see-class{gtk:print-operation}")

;;; --- gtk:print-operation-n-pages-to-print -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "n-pages-to-print"
                                               'print-operation) t)
 "The @code{n-pages-to-print} property of type @code{:int} (Read) @br{}
  The number of pages that will be printed. Note that this value is set during
  print preparation phase @code{:preparing}, so this value should never be get
  before the data generation phase @code{:generating-data}. You can connect to
  the @code{\"status-changed\"} signal and call the
  @fun{gtk:print-operation-n-pages-to-print} function when print status is
  @code{:generating-data}. This is typically used to track the progress of
  print operation. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'print-operation-n-pages-to-print)
      "Accessor"
      (documentation 'print-operation-n-pages-to-print 'function)
 "@version{2023-2-10}
  @syntax{(gtk:print-operation-n-pages-to-print object) => n-pages}
  @argument[object]{a @class{gtk:print-operation} object}
  @argument[n-pages]{an integer with the number of pages to print}
  @begin{short}
    Accessor of the @code{n-pages-to-print} slot of the
    @class{gtk:print-operation} class.
  @end{short}
  The @fun{gtk:print-operation-n-pages} function returns the number of pages
  that will be printed.

  Note that this value is set during print preparation phase @code{:preparing},
  so this function should never be called before the data generation phase
  @code{:generating-data}. You can connect to the @code{\"status-changed\"}
  signal and call the @fun{gtk:print-operation-n-pages-to-print} function when
  print status is @code{:generating-data}. This is typically used to track the
  progress of print operation.
  @see-class{gtk:print-operation}")

;;; --- gtk:print-operation-print-settings -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "print-settings"
                                               'print-operation) t)
 "The @code{print-settings} property of type @class{gtk:print-settings}
  (Read / Write) @br{}
  The print settings used for initializing the dialog. Setting this property is
  typically used to re-establish print settings from a previous print operation,
  see the @fun{gtk:print-operation-run} function.")

#+liber-documentation
(setf (liber:alias-for-function 'print-operation-print-settings)
      "Accessor"
      (documentation 'print-operation-print-settings 'function)
 "@version{2023-2-10}
  @syntax{(gtk:print-operation-print-settings object) => settings}
  @syntax{(setf (gtk:print-operation-print-settings object) settings)}
  @argument[object]{a @class{gtk:print-operation} object}
  @argument[settings]{a @class{gtk:print-settings} object}
  @begin{short}
    Accessor of the @slot[gtk:print-operation]{print-settings} slot of the
    @class{gtk:print-operation} class.
  @end{short}
  The @fun{gtk:print-operation-print-settings} function returns the current
  print settings. The @setf{gtk:print-operation-print-settings} functon sets
  the print settings for the print operation. This is typically used to
  re-establish print settings from a previous print operation.

  Note that the return value is @code{nil} until either the
  @fun{gtk:print-operation-print-settings} function or the
  @fun{gtk:print-operation-run} function have been called.
  @see-class{gtk:print-operation}
  @see-function{gtk:print-operation-run}")

;;; --- gtk:print-operation-show-progress --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-progress"
                                               'print-operation) t)
 "The @code{show-progress} property of type @code{:boolean} (Read / Write) @br{}
  Determines whether to show a progress dialog during the print operation. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'print-operation-show-progress)
      "Accessor"
      (documentation 'print-operation-show-progress 'function)
 "@version{2023-2-10}
  @syntax{(gtk:print-operation-show-progress object) => show-progress}
  @syntax{(setf (gtk:print-operation-show-progress object) show-progress)}
  @argument[object]{a @class{gtk:print-operation} object}
  @argument[show-progress]{@em{true} to show a progress dialog}
  @begin{short}
    Accessor of the @slot[gtk:print-operation]{show-progress} of the
    @class{gtk:print-operation} class.
  @end{short}
  If @arg{show-progress} is @em{true}, the print operation will show a progress
  dialog during the print operation.
  @see-class{gtk:print-operation}")

;;; --- gtk:print-operation-status ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "status"
                                               'print-operation) t)
 "The @code{status} property of type @symbol{gtk:print-status} (Read) @br{}
  The status of the print operation. @br{}
  Default value: @code{:initial}")

#+liber-documentation
(setf (liber:alias-for-function 'print-operation-status)
      "Accessor"
      (documentation 'print-operation-status 'function)
 "@version{2023-2-10}
  @syntax{(gtk:print-operation-status object) => status}
  @argument[object]{a @class{gtk:print-operation} object}
  @argument[status]{a @symbol{gtk:print-status} value with the status of the
    print operation}
  @begin{short}
    Accessor of the @slot[gtk:print-operation]{status} slot of the
    @class{gtk:print-operation} class.
  @end{short}
  The @fun{gtk:print-operation-status} function returns the status of the print
  operation. Also see the @fun{gtk:print-operation-status-string} function.
  @see-class{gtk:print-operation}
  @see-symbol{gtk:print-status}
  @see-function{gtk:print-operation-status-string}")

;;; --- gtk:print-operation-status-string --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "status-string"
                                               'print-operation) t)
 "The @code{status-string} property of type @code{:string} (Read) @br{}
  A string representation of the status of the print operation. The string is
  translated and suitable for displaying the print status e.g. in a
  @class{gtk:statusbar} widget. See the @code{status} property for a status
  value that is suitable for programmatic use. @br{}
  Default value: \"\"")

#+liber-documentation
(setf (liber:alias-for-function 'print-operation-status-string)
      "Accessor"
      (documentation 'print-operation-status-string 'function)
 "@version{2023-2-10}
  @syntax{(gtk:print-operation-status-string object) => status-string}
  @argument[object]{a @class{gtk:print-operation} object}
  @argument[status-string]{a string representation of the status of the print
    operation}
  @begin{short}
    Accessor of the @slot[gtk:print-operation]{status-string} slot of the
    @class{gtk:print-operation} class.
  @end{short}
  The @fun{gtk:print-operation-status-string} function returns a string
  representation of the status of the print operation.

  The string is translated and suitable for displaying the print status e.g.
  in a @class{gtk:statusbar} widget. Use the @fun{gtk:print-operation-status}
  function to obtain a status value that is suitable for programmatic use.
  @see-class{gtk:print-operation}
  @see-function{gtk:print-operation-status}")

;;; --- gtk:print-operation-support-selection ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "support-selection"
                                               'print-operation) t)
 "The @code{support-selection} property of type @code{:boolean} (Read / Write)
  @br{}
  If @em{true}, the print operation will support print of selection. This
  allows the print dialog to show a \"Selection\" button. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'print-operation-support-selection)
      "Accessor"
      (documentation 'print-operation-support-selection 'function)
 "@version{2023-2-10}
  @syntax{(gtk:print-operation-status-support-selection object) => support-selection}
  @syntax{(setf (gtk:print-operation-support-selection object) support-selection)}
  @argument[object]{a @class{gtk:print-operation} object}
  @argument[support-selection]{@em{true} to support selection}
  @begin{short}
    Accessor of the @slot[gtk:print-operation]{support-selection} slot of the
    @class{gtk:print-operation} class.
  @end{short}
  The @fun{gtk:print-operation-support-selection} function returns whether the
  application supports print of selection. The
  @setf{gtk:print-operation-support-selection} function sets whether selection
  is supported by the print operation.
  @see-class{gtk:print-operation}")

;;; --- gtk:print-operation-track-print-status ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "track-print-status"
                                               'print-operation) t)
 "The @code{track-print-status} property of type @code{:boolean} (Read / Write)
  @br{}
  If @em{true}, the print operation will try to continue report on the status
  of the print job in the printer queues and printer. This can allow your
  application to show things like \"out of paper\" issues, and when the print
  job actually reaches the printer. However, this is often implemented using
  polling, and should not be enabled unless needed. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'print-operation-track-print-status)
      "Accessor"
      (documentation 'print-operation-track-print-status 'function)
 "@version{2023-2-10}
  @syntax{(gtk:print-operation-track-print-status object) => track-status}
  @syntax{(setf (gtk:print-operation-track-print-status object) track-status)}
  @argument[object]{a @class{gtk:print-operation} object}
  @argument[track-status]{@em{true} to track status after printing}
  @begin{short}
    Accessor of the @slot[gtk:print-operation]{track-print-status} slot of the
    @class{gtk:print-operation} class.
  @end{short}
  If @arg{track-status} is @em{true}, the print operation will try to continue
  report on the status of the print job in the printer queues and printer. This
  can allow your application to show things like \"out of paper\" issues, and
  when the print job actually reaches the printer.

  This function is often implemented using some form of polling, so it should
  not be enabled unless needed.
  @see-class{gtk:print-operation}")

;;; --- gtk:print-operation-unit -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "unit" 'print-operation) t)
 "The @code{unit} property of type @symbol{gtk:unit} (Read / Write) @br{}
  The transformation for the Cairo context obtained from the
  @class{gtk:print-context} object is set up in such a way that distances are
  measured in units of a value of the @symbol{gtk:unit} enumeration. @br{}
  Default value: @code{:pixel}")

#+liber-documentation
(setf (liber:alias-for-function 'print-operation-unit)
      "Accessor"
      (documentation 'print-operation-unit 'function)
 "@version{2023-2-10}
  @syntax{(gtk:print-operation-unit object) => unit}
  @syntax{(setf (gtk:print-operation-unit object) unit)}
  @argument[object]{a @class{gtk:print-operation} object}
  @argument[unit]{a @symbol{gtk:unit} value to use}
  @begin{short}
    Accessor of the @slot[gtk:print-operation]{unit} slot of the
    @class{gtk:print-operation} class.
  @end{short}
  The @setf{gtk:print-operation-unit} function sets up the transformation for
  the Cairo context obtained from a @class{gtk:print-context} object in such a
  way that distances are measured in units of a value of the @symbol{gtk:unit}
  enumeration.
  @see-class{gtk:print-operation}
  @see-class{gtk:print-context}
  @see-symbol{gtk:unit}")

;;; --- gtk:print-operation-use-full-page --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-full-page"
                                               'print-operation) t)
 "The @code{use-full-page} property of type @code{:boolean} (Read / Write) @br{}
  If @em{true}, the transformation for the Cairo context obtained from the
  @class{gtk:print-context} object puts the origin at the top left corner of the
  page, which may not be the top left corner of the sheet, depending on page
  orientation and the number of pages per sheet. Otherwise, the origin is at
  the top left corner of the imageable area, i.e. inside the margins. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'print-operation-use-full-page)
      "Accessor"
      (documentation 'print-operation-use-full-page 'function)
 "@version{2023-2-10}
  @syntax{(gtk:print-operation-use-full-page object) => full-page}
  @syntax{(setf (gtk:print-operation-use-full-page object) full-page)}
  @argument[object]{a @class{gtk:print-operation} object}
  @argument[full-page]{@em{true} to set up the @class{gtk:print-context} for
    the full page}
  @begin{short}
    Accessor of the @slot[gtk:print-operation]{use-full-page} of the
    @class{gtk:print-operation} class.
  @end{short}
  If @arg{full-page} is @em{true}, the transformation for the Cairo context
  obtained from the @class{gtk:print-context} object puts the origin at the top
  left corner of the page, which may not be the top left corner of the sheet,
  depending on page orientation and the number of pages per sheet. Otherwise,
  the origin is at the top left corner of the imageable area, i.e. inside the
  margins.
  @see-class{gtk:print-operation}
  @see-class{gtk:print-context}")

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_new
;;; ----------------------------------------------------------------------------

(declaim (inline print-operation-new))

(defun print-operation-new ()
 #+liber-documentation
 "@version{2023-2-10}
  @return{A new @class{gtk:print-operation} object.}
  @begin{short}
    Creates a new @class{gtk:print-operation} object.
  @end{short}
  @see-class{gtk:print-operation}"
  (make-instance 'print-operation))

(export 'print-operation-new)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_get_error ()
;;;
;;; void gtk_print_operation_get_error (GtkPrintOperation *op, GError **error);
;;;
;;; Call this when the result of a print operation is
;;; GTK_PRINT_OPERATION_RESULT_ERROR, either as returned by
;;; gtk_print_operation_run(), or in the "done" signal handler. The returned
;;; GError will contain more details on what went wrong.
;;;
;;; op :
;;;     a GtkPrintOperation
;;;
;;; error :
;;;     return location for the error
;;;
;;; Since 2.10
;;; ----------------------------------------------------------------------------

;; TODO: Do more work on reporting errors

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_run
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_operation_run" %print-operation-run)
    print-operation-result
  (operation (g:object print-operation))
  (action print-operation-action)
  (parent (g:object window))
  (err :pointer))

(defun print-operation-run (operation action parent)
 #+liber-documentation
 "@version{#2024-11-20}
  @argument[operation]{a @class{gtk:print-operation} object}
  @argument[action]{a @symbol{gtk:print-operation-action} value with the action
    to start}
  @argument[parent]{a @class{gtk:window} transient parent of the dialog}
  @begin{return}
    The result of the print operation. A return value of @code{:apply}
    indicates that the printing was completed successfully. In this case, it
    is a good idea to obtain the used print settings with the
    @fun{gtk:print-operation-print-settings} function and store them for reuse
    with the next print operation. A value of @code{:in-progress} means the
    operation is running asynchronously, and will emit the @code{\"done\"}
    signal when done.
  @end{return}
  @begin{short}
    Runs the print operation, by first letting the user modify print settings
    in the print dialog, and then print the document.
  @end{short}
  Normally this function does not return until the rendering of all pages is
  complete. You can connect to the @code{\"status-changed\"} signal on the print
  operation to obtain some information about the progress of the print
  operation. Furthermore, it may use a recursive main loop to show the print
  dialog.

  If you call the @fun{gtk:print-operation-allow-async} function the operation
  will run asynchronously if this is supported on the platform. The
  @code{\"done\"} signal will be emitted with the result of the operation when
  the it is done, i.e. when the dialog is canceled, or when the print succeeds
  or fails.

  Note that the @fun{gtk:print-operation-run} function can only be called once
  on a given @class{gtk:print-operation} object.
  @begin[Examples]{dictionary}
    @begin{pre}
(defun do-print-operation (window)
  (let ((response nil)
        (print (gtk:print-operation-new)))
    ;; Connect signal handlers for the print operation
    (g:signal-connect print \"draw-page\" #'draw-page)
    (g:signal-connect print \"begin-print\" #'begin-print)
    ;; Restore the print settings
    (when *print-settings*
      (setf (gtk:print-operation-print-settings print) *print-settings*))
    ;; Restore the page setup
    (when *page-setup*
      (setf (gtk:print-operation-page-setup print) *page-setup*))
    ;; Perform the print operation
    (setf response (gtk:print-operation-run print :print-dialog window))
    ;; Check the response and save the print settings
    (when (eq :apply response)
      (setf *print-settings* (gtk:print-operation-print-settings print)))))
    @end{pre}
  @end{dictionary}
  @see-class{gtk:print-operation}
  @see-symbol{gtk:print-operation-action}
  @see-function{gtk:print-operation-allow-async}"
  (glib:with-error (err)
    (%print-operation-run operation action parent err)))

(export 'print-operation-run)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_cancel
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_operation_cancel" print-operation-cancel) :void
 #+liber-documentation
 "@version{#2023-2-10}
  @argument[operation]{a @class{gtk:print-operation} object}
  @begin{short}
    Cancels a running print operation.
  @end{short}
  This function may be called from a @code{\"begin-print\"}, @code{\"paginate\"}
  or @code{\"draw-page\"} signal handler to stop the currently running print
  operation.
  @see-class{gtk:print-operation}"
  (operation (g:object print-operation)))

(export 'print-operation-cancel)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_draw_page_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_operation_draw_page_finish"
               print-operation-draw-page-finish) :void
 #+liber-documentation
 "@version{#2023-2-10}
  @argument[operation]{a @class{gtk:print-operation} object}
  @begin{short}
    Signalize that drawing of the particular page is complete.
  @end{short}
  The function is called after completion of page drawing, e.g. drawing in
  another thread. If the @fun{gtk:print-operation-set-defer-drawing} function
  was called before, then this function has to be called by the application.
  In another case it is called by the library itself.
  @see-class{gtk:print-operation}
  @see-function{gtk:print-operation-set-defer-drawing}"
  (operation (g:object print-operation)))

(export 'print-operation-draw-page-finish)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_set_defer_drawing
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_operation_set_defer_drawing"
               print-operation-set-defer-drawing) :void
 #+liber-documentation
 "@version{#2023-2-10}
  @argument[operation]{a @class{gtk:print-operation} object}
  @begin{short}
    Sets up the @class{gtk:print-operation} object to wait for calling of the
    @fun{gtk:print-operation-draw-page-finish} function from the application.
  @end{short}
  It can be used for drawing page in another thread. This function must be
  called in the callback of a @code{\"draw-page\"} signal.
  @see-class{gtk:print-operation}
  @see-function{gtk:print-operation-draw-page-finish}"
  (operation (g:object print-operation)))

(export 'print-operation-set-defer-drawing)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_is_finished
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_operation_is_finished" print-operation-is-finished)
    :boolean
 #+liber-documentation
 "@version{#2023-2-10}
  @argument[operation]{a @class{gtk:print-operation} object}
  @return{@em{True}, if the print operation is finished.}
  @begin{short}
    A convenience function to find out if the print operation is finished,
    either successfully @code{:finished} or unsuccessfully
    @code{:finished-aborted}.
  @end{short}
  @begin[Notes]{dictionary}
    When you enable print status tracking the print operation can be in a
    non-finished state even after the @code{\"done\"} signal has been called,
    as the operation status then tracks the print job status on the printer.
  @end{dictionary}
  @see-class{gtk:print-operation}"
  (operation (g:object print-operation)))

(export 'print-operation-is-finished)

;;; ----------------------------------------------------------------------------
;;; gtk_print_run_page_setup_dialog
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_run_page_setup_dialog" print-run-page-setup-dialog)
    (g:object page-setup)
 #+liber-documentation
 "@version{#2023-2-10}
  @argument[parent]{a @class{gtk:window} transient parent}
  @argument[setup]{an existing @class{gtk:page-setup} object}
  @argument[settings]{a @class{gtk:print-settings} object}
  @return{A new @class{gtk:page-setup} object.}
  @begin{short}
    Runs a page setup dialog, letting the user modify the values from
    @arg{setup}.
  @end{short}
  If the user cancels the dialog, the returned @class{gtk:page-setup} object is
  identical to the passed in @arg{setup}, otherwise it contains the
  modifications done in the dialog.

  Note that this function may use a recursive main loop to show the page setup
  dialog. See the @fun{gtk:print-run-page-setup-dialog-async} function if this
  is a problem.
  @see-class{gtk:print-operation}
  @see-class{gtk:page-setup}
  @see-class{gtk:print-settings}
  @see-function{gtk:print-run-page-setup-dialog-async}"
  (parent (g:object window))
  (setup (g:object page-setup))
  (settings (g:object print-settings)))

(export 'print-run-page-setup-dialog)

;;; ----------------------------------------------------------------------------
;;; GtkPageSetupDoneFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback page-setup-done-func :void
    ((setup (g:object page-setup))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func setup)
      (return () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'page-setup-done-func)
      "Callback"
      (liber:symbol-documentation 'page-setup-done-func)
 "@version{#2024-3-23}
  @syntax{lambda (setup)}
  @argument[setup]{a @class{gtk:page-setup} object that has been passed to the
    @fun{gtk:print-run-page-setup-dialog-async} function.}
  @begin{short}
    A callback function used by the @fun{gtk:print-run-page-setup-dialog-async}
    function.
  @end{short}
  This function will be called when the page setup dialog is dismissed.
  @see-function{gtk:print-run-page-setup-dialog-async}")

(export 'page-setup-done-func)

;;; ----------------------------------------------------------------------------
;;; gtk_print_run_page_setup_dialog_async
;;; ----------------------------------------------------------------------------

;; TODO: Can we set func to be NIL and pass a CFFI:NULL-POINTER for func!?

(cffi:defcfun ("gtk_print_run_page_setup_dialog_async"
               %print-run-page-setup-dialog-async) :void
  (parent (g:object window))
  (page-setup (g:object page-setup))
  (settings (g:object print-settings))
  (func :pointer)
  (data :pointer))

(defun print-run-page-setup-dialog-async (parent setup settings func)
 #+liber-documentation
 "@version{#2023-2-10}
  @argument[parent]{a @class{gtk:window} transient parent, or @code{nil}}
  @argument[setup]{an existing @class{gtk:page-setup}, or @code{nil}}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[func]{a @symbol{gtk:page-setup-done-func} callback function to call
    when the user saves the modified page setup}
  @begin{short}
    Runs a page setup dialog, letting the user modify the values from
    @arg{setup}.
  @end{short}
  In contrast to the @fun{gtk:print-run-page-setup-dialog} function, this
  function returns after showing the page setup dialog on platforms that support
  this, and calls @arg{func} from a signal handler for the @code{\"response\"}
  signal of the dialog.
  @see-class{gtk:print-operation}
  @see-class{gtk:window}
  @see-class{gtk:page-setup}
  @see-class{gtk:print-settings}"
  (glib:with-stable-pointer (ptr func)
    (%print-run-page-setup-dialog-async parent
                                        setup
                                        settings
                                        (cffi:callback page-setup-done-func)
                                        ptr)))

(export 'print-run-page-setup-dialog-async)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_preview_end_preview
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_operation_preview_end_preview"
               print-operation-preview-end-preview) :void
 #+liber-documentation
 "@version{#2023-2-10}
  @argument[preview]{a @class{gtk:print-operation-preview} object}
  @short{Ends a preview.}
  This function must be called to finish a custom print preview.
  @see-class{gtk:print-operation-preview}"
  (preview (g:object print-operation-preview)))

(export 'print-operation-preview-end-preview)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_preview_is_selected
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_operation_preview_is_selected"
               print-operation-preview-is-selected) :boolean
 #+liber-documentation
 "@version{#2023-2-10}
  @argument[preview]{a @class{gtk:print-operation-preview} object}
  @argument[page-nr]{an integer with the page number}
  @return{@em{True} if the page has been selected for printing.}
  @begin{short}
    Returns whether the given page is included in the set of pages that have
    been selected for printing.
  @end{short}
  @see-class{gtk:print-operation-preview}"
  (preview (g:object print-operation-preview))
  (page-nr :int))

(export 'print-operation-preview-is-selected)

;;; ----------------------------------------------------------------------------
;;; gtk_print_operation_preview_render_page
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_operation_preview_render_page"
               print-operation-preview-render-page) :void
 #+liber-documentation
 "@version{#2023-2-10}
  @argument[preview]{a @class{gtk:print-operation-preview} object}
  @argument[page-nr]{an integer with the number of the page to render}
  @begin{short}
    Renders a page to the preview, using the print context that was passed to
    the @code{\"preview\"} handler together with preview.
  @end{short}
  A custom print preview should use this function in its @code{\"expose\"}
  handler to render the currently selected page.

  Note that this function requires a suitable Cairo context to be associated
  with the print context.
  @see-class{gtk:print-operation-preview}
  @see-class{gtk:print-operation-preview-render-page}"
  (preview (g:object print-operation-preview))
  (page-nr :int))

(export 'print-operation-preview-render-page)

;;; --- End of file gtk3.print-operation.lisp ----------------------------------
