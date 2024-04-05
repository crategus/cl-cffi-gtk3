(defpackage :gtk3-widget-factory
  (:use :iterate :common-lisp)
  (:export #:gtk3-widget-factory))

(in-package #:gtk3-widget-factory)

(defvar *application* nil)
(defvar *application-window* nil)
(defvar *toplevel-stack* nil)
(defvar *current-page* 2)

(gobject:define-g-object-subclass "MyTextView" my-text-view
  (:superclass gtk:text-view
   :export t
   :interfaces ())
  nil)

(defun sys-path (filename)
  (let ((system-path (asdf:system-source-directory :gtk3-widget-factory)))
    (princ-to-string (merge-pathnames filename system-path))))

;;; ----------------------------------------------------------------------------

(defun mklist (obj)
  (if (listp obj)
      obj
      (list obj)))

(defun create-and-fill-tree-store (data &optional (model nil) (iter nil))
  (unless model
    (setf model (apply #'gtk:tree-store-new (mklist (first data))))
    (setf data (rest data)))
  (let ((parent iter))
    (dolist (entry (mklist data))
      (cond ((or (atom entry) (every #'atom entry)) ; entry is never an atom?
             (setf parent
                   (apply #'gtk:tree-store-set model
                                               (gtk:tree-store-append model
                                                                      iter)
                                               (mklist entry))))
            ((some #'listp entry)
             (create-and-fill-tree-store entry
                                         model
                                         parent)))))
  model)

(defun populate-model (store)
  (let ((model '(("Charlemagne" "742" "814")
                 (("Pepin the Short" "714" "768")
                  (("Charles Martel" "688" "741")
                   (("Pepin of Herstal" "635" "714")
                    (("Ansegisel" "602 of 610" "mudered before 679")
                     ("Begga" "615" "693"))
                    ("Alpaida"))
                   ("Rotrude")
                   (("Lièvin de Trèves")
                    (("Guérin")
                     ("Gunza"))
                   ("Willigarde de Bavière")))
                  ("Bertrade of Laon" "710" "783")
                  (("Caribert of Laon" "" "before 762")
                   (("Unknown")
                    ("Bertrade of Prüm" "ca. 670" "after 721"))
                   ("Gisele of Aquitaine")))
                 ("Attila the Hun" "ca. 390" "453"))))
    (create-and-fill-tree-store model store)))

;;; ----------------------------------------------------------------------------

;; Code to populate the flowbox for the selection dialog

#|
static void
populate_flowbox (GtkWidget *flowbox)
{
  const gchar *location;
  GDir *dir;
  GError *error = NULL;
  const gchar *name;
  gchar *filename;
  GFile *file;
  GInputStream *stream;
  BackgroundData *bd;
  GdkPixbuf *pixbuf;
  GtkWidget *child;

  if (GPOINTER_TO_UINT (g_object_get_data (G_OBJECT (flowbox), "populated")))
    return;

  g_object_set_data (G_OBJECT (flowbox), "populated", GUINT_TO_POINTER (1));

  pixbuf = gdk_pixbuf_new (GDK_COLORSPACE_RGB, FALSE, 8, 110, 70);
  gdk_pixbuf_fill (pixbuf, 0xffffffff);
  child = gtk_image_new_from_pixbuf (pixbuf);
  gtk_widget_show (child);
  gtk_flow_box_insert (GTK_FLOW_BOX (flowbox), child, -1);

  location = "/usr/share/backgrounds/gnome";
  dir = g_dir_open (location, 0, &error);
  if (error)
    {
      g_warning ("%s", error->message);
      g_error_free (error);
      return;
    }

  while ((name = g_dir_read_name (dir)) != NULL)
    {
      filename = g_build_filename (location, name, NULL);
      file = g_file_new_for_path (filename);
      stream = G_INPUT_STREAM (g_file_read (file, NULL, &error));
      if (error)
        {
          g_warning ("%s", error->message);
          g_clear_error (&error);
          g_free (filename);
        }
      else
        {
          bd = g_new (BackgroundData, 1);
          bd->flowbox = flowbox;
          bd->filename = filename;
          gdk_pixbuf_new_from_stream_at_scale_async (stream, 110, 110, TRUE, NULL,
                                                     background_loaded_cb, bd);
        }

      g_object_unref (file);
      g_object_unref (stream);
    }

  g_dir_close (dir);
}
|#

(defun populate-flowbox (flowbox)
  (format t "implement missing code to populate the flowbox ~a~%" flowbox))

;;; ----------------------------------------------------------------------------

;; Control the pulse mode of "progressbar3" and "entry1"

(let ((pulse-time 250)
      (pulse-entry-mode 0))

  ;; Function called by the timeout handler
  (defun pulse-it (widget)
    (let (pulse-id)
      ;; Pulse the widget which is an entry or a progress bar
      (if (eq 'gtk:entry (type-of widget))
          (gtk:entry-progress-pulse widget)
          (gtk:progress-bar-pulse widget))
      ;; Set a timeout handler and store a destroy notify handler on the
      ;; property list
      (setf pulse-id (g:timeout-add pulse-time
                                    (lambda () (pulse-it widget))))
      (g:object-set-data-full widget
                              "pulse-id"
                              (lambda ()
                                (g:source-remove pulse-id)))
    ;; Remove the source
    glib:+source-remove+))

  (defun pulse-update (adjustment widget)
    (let ((value (gtk:adjustment-value adjustment))
          (pulse-id (g:object-data widget "pulse-id")))
      (setf pulse-time (truncate (+ 50 (* 4 value))))
      (if (= 100 value)
          (setf (g:object-data widget "pulse-id") nil)
          (when (and (null pulse-id)
                     (or (eq 'gtk:progress-bar (type-of widget))
                         (and (eq 'gtk:entry (type-of widget))
                              (= 3 (mod pulse-entry-mode 3)))))
            (setf pulse-id
                  (g:timeout-add pulse-time
                                 (lambda () (pulse-it widget))))
            (g:object-set-data-full widget
                                    "pulse-id"
                                    (lambda ()
                                      (g:source-remove pulse-id)))))))

  (defun on-entry-icon-release (entry pos event)
    (declare (ignore event))
    (when (eq :secondary pos)
      (setf pulse-entry-mode (1+ pulse-entry-mode))
      (cond ((= 0 (mod pulse-entry-mode 3))
             (setf (g:object-data entry "pulse-id") nil)
             (setf (gtk:entry-progress-fraction entry) 0.0d0))
            ((= 1 (mod pulse-entry-mode 3))
             (setf (gtk:entry-progress-fraction entry) 0.25d0))
            (t
             (when (< (- pulse-time 50) 400)
               (setf (gtk:entry-progress-pulse-step entry) 0.1d0)
               (pulse-it entry)))))))

;;; ----------------------------------------------------------------------------

(defun change-theme-state (action state)
  (let ((settings (gtk:settings-default)))
    (setf (gtk:settings-gtk-application-prefer-dark-theme settings)
          (g:variant-boolean state))
    (setf (g:action-state action) state)))

(defun change-transition-state (action state)
  (setf (gtk:stack-transition-type *toplevel-stack*)
        (if (g:variant-boolean state)
            :slide-left-right
            :none))
  (setf (g:action-state action) state))

(defun activate-get-busy (action parameter)
  (declare (ignore action parameter))
  (g:application-mark-busy *application*)
  (let ((cursor (gdk:cursor-new-from-name
                    (gtk:widget-display *application-window*) "wait"))
        (window (gtk:widget-window *application-window*)))
    (setf (gdk:window-cursor window) cursor)
    (g:timeout-add 5000
                   (lambda ()
                     (format t "in timeout callback~%")
                     (setf (gtk:widget-sensitive *application-window*) t)
                     (setf (gdk:window-cursor window) nil)
                     (g:application-unmark-busy *application*)
                     glib:+source-remove+)))
    (setf (gtk:widget-sensitive *application-window*) nil))

(defun activate-search (action parameter)
  (declare (ignore action parameter))
  (when (= 2 *current-page*)
    (let ((searchbar (g:object-data *application-window* "searchbar")))
      (setf (gtk:search-bar-search-mode-enabled searchbar) t))))

(defun activate-delete (action parameter)
  (declare (ignore action parameter))
  (when (= 2 *current-page*)
    (gtk:widget-show (g:object-data *application-window* "infobar"))))

(defun activate-background (action parameter)
  (declare (ignore action parameter))
  (let ((dialog (g:object-data *application-window* "selection_dialog"))
        (flowbox (g:object-data *application-window* "selection_flowbox")))
    (gtk:widget-show dialog)
    (populate-flowbox flowbox)))

(defun activate-open (action parameter)
  (declare (ignore action parameter))
  (when (= 3 *current-page*)
    (gtk:button-clicked (g:object-data *application-window*
                                       "open_menubutton"))))

(defun activate-record (action parameter)
  (declare (ignore action parameter))
  (when (= 3 *current-page*)
    (gtk:button-clicked (g:object-data *application-window* "record_button"))))

(defun activate-lock (action parameter)
  (format t "   in ACTIVATE-LOCK for action ~a ~a~%" action parameter)
  (when (= 3 *current-page*)
    (gtk:button-clicked (g:object-data *application-window* "lockbutton"))))

(defun activate-about (action parameter)
  (declare (ignore action parameter))
  (gtk:show-about-dialog (gtk:application-active-window *application*)
                         :program-name "GTK Widget Factory"
                         :version (format nil "Running against GTK+ ~d.~d.~d"
                                          (gtk:major-version)
                                          (gtk:minor-version)
                                          (gtk:micro-version))
                         :copyright "© 2020 Dieter Kaiser"
                         :license-type :lgpl-2-1
                         :website "http://www.gtk.org"
                         :authors '("Dieter Kaiser")
                         :logo-icon-name "gtk3-widget-factory"
                         :title "About GTK Widget Factory"))

(defun activate-quit (action parameter)
  (format t "in action activate-quit: ~a, ~a~%" action parameter)
  ;; Destroy all windows of the application
  (dolist (window (gtk:application-windows *application*))
    (gtk:widget-destroy window)))

(defun activate-inspector (action parameter)
  (format t "in action activate-inspector: ~a, ~a~%" action parameter)
  (gtk:window-interactive-debugging t))

;;; ----------------------------------------------------------------------------

(defun increase-icon-size ())
(defun decrease-icon-size ())
(defun reset-icon-size ())

;;; ----------------------------------------------------------------------------

(defun activate (application)
  (let (;; Load UI file
        (builder (gtk:builder-new-from-file (sys-path "gtk3-widget-factory.ui")))
        ;; Create a provider
        (provider (gtk:css-provider-new))
        ;; Define action entries
        (entries `(("dark" nil nil "false" ,#'change-theme-state)
                   ("transition" nil nil "false" ,#'change-transition-state)
                   ("search" ,#'activate-search nil nil nil)
                   ("delete" ,#'activate-delete nil nil nil)
                   ("busy" ,#'activate-get-busy nil nil nil)
                   ("background" ,#'activate-background nil nil nil)
                   ("open" ,#'activate-open nil nil nil)
                   ("record" ,#'activate-record nil nil nil)
                   ("lock" ,#'activate-lock nil nil nil)))
        ;; Define accelerators
        (accels `(("app.about" "F1")
                  ("app.quit" "<Primary>q")
                  ("win.dark" "<Primary>d")
                  ("win.search" "<Primary>s")
                  ("win.delete" "Delete")
                  ("win.background" "<Primary>b")
                  ("win.open" "<Primary>o")
                  ("win.record" "<Primary>r")
                  ("win.lock" "<Primary>l"))))
    ;; Load CSS file and add CSS to the provider
    (gtk:css-provider-load-from-path provider
                                     (sys-path "gtk3-widget-factory.css"))
    (gtk:style-context-add-provider-for-screen (gdk:screen-default)
                                               provider
                                               gtk:+priority-application+)

    (setf *application-window*
          (gtk:builder-object builder "window"))

    ;; Connect signal "destroy" to the application window
    (g:signal-connect *application-window* "destroy"
                      (lambda (widget)
                        (declare (ignore widget))
                        ;; Quit the application
                        (g:application-quit application)))

    (setf (g:object-data *application-window* "toolbar")
          (gtk:builder-object builder "toolbar"))

    (setf (g:object-data *application-window* "searchbar")
          (gtk:builder-object builder "searchbar"))

    (let ((widget (gtk:builder-object builder "infobar")))
      (setf (g:object-data *application-window* "infobar") widget)
      (g:signal-connect widget "response"
                        (lambda (infobar id)
                          (format t "  in RESPONSE: ~a  ~a~%" infobar id)
                          (when (= id -7) ; for :close
                            (gtk:widget-hide infobar)))))

      ;; Add the application window to the application
      (gtk:application-add-window application *application-window*)
      ;; Add actions to the action map of the applicatin window
      (g:action-map-add-action-entries *application-window* entries)

      ;; Set accels for some actions
      (iter (for (name accel) in accels)
            (setf (gtk:application-accels-for-action application name) accel))

      ;; Save the toplevel stack in a global variable
      (setf *toplevel-stack*
            (gtk:builder-object builder "toplevel_stack"))
      (g:signal-connect *toplevel-stack* "notify::visible-child-name"
              (lambda (stack parameter)
                (declare (ignore parameter))
                (unless (gtk:widget-in-destruction stack)
                  (let ((name (gtk:stack-visible-child-name stack))
                        (window (gtk:widget-ancestor stack
                                                     "GtkApplicationWindow")))
                    (declare (ignore window))
#|
  g_object_set (gtk_application_window_get_help_overlay (GTK_APPLICATION_WINDOW (window)),
                "view-name", name,
                NULL);
|#
                    (cond ((string= "page1" name)
                           (setf *current-page* 1))
                          ((string= "page2" name)
                           (setf *current-page* 2))
                          ((string= "page3" name)
#|
      page = gtk_stack_get_visible_child (GTK_STACK (stack));
      set_needs_attention (GTK_WIDGET (page), FALSE);
|#
                           (setf *current-page* 3)
                           (let* ((stack *toplevel-stack*)
                                  (page (gtk:stack-visible-child stack)))
                             (setf (gtk:stack-child-needs-attention stack page)
                                   nil))
                           ))))))

    ;; Configure widgets on page1

    ;; Connect signal "icon-release" to "entry1" on page1
    (g:signal-connect (gtk:builder-object builder "entry1")
                      "icon-release"
                      #'on-entry-icon-release)

    (g:signal-connect (gtk:builder-object builder "scale3") "format-value"
                      (lambda (scale value)
                        (declare (ignore scale))
                        (format nil "~1$" value)))
    (g:signal-connect (gtk:builder-object builder "scale4") "format-value"
                      (lambda (scale value)
                        (declare (ignore scale value))
                        (format nil "")))

    ;; Connect entry1 and progressbar3 to adjustment1
    (let ((adjustment (gtk:builder-object builder "adjustment1"))
          (progressbar (gtk:builder-object builder "progressbar3"))
          (entry (gtk:builder-object builder "entry1")))
      (g:signal-connect adjustment "value-changed"
                        (lambda (adj)
                          (pulse-update adj progressbar)))
      (pulse-update adjustment progressbar)
      (g:signal-connect adjustment "value-changed"
                        (lambda (adj)
                          (pulse-update adj entry)))
      (pulse-update adjustment entry))

    (let ((pbar1 (gtk:builder-object builder "progressbar1"))
          (pbar2 (gtk:builder-object builder "progressbar2"))
          (adjustment (gtk:builder-object builder "adjustment3")))
      (g:signal-connect adjustment "value-changed"
              (lambda (adjustment)
                (let* ((value (gtk:adjustment-value adjustment))
                       (upper (gtk:adjustment-upper adjustment))
                       (lower (gtk:adjustment-lower adjustment))
                       (fraction (/ value (- upper lower))))
                  (setf (gtk:progress-bar-fraction pbar1) fraction)
                  (setf (gtk:progress-bar-fraction pbar2) fraction)))))

    ;; Configure widgets on page2

    ;; Configure a GtkRevealer and its widgets which opens when the value of
    ;; the GtkSpinButton is a multiple of 3
    (let ((page2reset (gtk:builder-object builder "page2reset"))
          (page2dismiss (gtk:builder-object builder "page2dismiss"))
          (page2note (gtk:builder-object builder "page2note"))
          (adjustment (gtk:builder-object builder "adjustment2")))

      (g:signal-connect page2reset "clicked"
              (lambda (button)
                (let ((revealer (gtk:widget-ancestor button "GtkRevealer")))
                  (setf (gtk:adjustment-value adjustment) 50.0)
                  (setf (gtk:revealer-reveal-child revealer) nil))))

      (g:signal-connect page2dismiss "clicked"
             (lambda (button)
               (let ((revealer (gtk:widget-ancestor button "GtkRevealer")))
                 (setf (gtk:revealer-reveal-child revealer) nil))))

      (g:signal-connect adjustment "value-changed"
              (lambda (adjustment)
                (let ((value (truncate (gtk:adjustment-value adjustment)))
                      (revealer (gtk:widget-ancestor page2note "GtkRevealer")))
                  (when (= 0 (mod value 3))
                    (setf (gtk:label-label page2note)
                          (format nil "~d is multiple of 3" value)))
                  (setf (gtk:revealer-reveal-child revealer)
                        (= 0 (mod value 3)))))))

    ;; Two signal handlers for the mic button on page2
    (g:signal-connect (gtk:builder-object builder "mic-button")
                      "value-changed"
                      (lambda (button value)
                        (declare (ignore value))
                        (gtk:widget-trigger-tooltip-query button)))
    (g:signal-connect (gtk:builder-object builder "mic-button") "query-tooltip"
        (lambda (button x y mode tooltip)
          (declare (ignore x y mode))
          (let ((adjustment (gtk:scale-button-adjustment button))
                (value (gtk:scale-button-value button))
                (epsilon 1.0e-10)
                str)
            (cond ((< value (+ (gtk:adjustment-lower adjustment) epsilon))
                   (setf str "Muted"))
                  ((>= value (- (gtk:adjustment-upper adjustment) epsilon))
                   (setf str "Full volume"))
                  (t
                   (let* ((upper (gtk:adjustment-upper adjustment))
                          (lower (gtk:adjustment-lower adjustment))
                          (percent (+ 0.5 (* 100 (/ value (- upper lower))))))
                     (setf str (format nil "~d %" (truncate percent))))))
            (gtk:tooltip-set-text tooltip str)
            t)))

      ;; Set text and an action on the statusbar
      (let ((statusbar (gtk:builder-object builder "statusbar")))
        (gtk:statusbar-push statusbar 0 "All systems are operating normally.")
        (g:action-map-add-action *application-window*
                                 (g:property-action-new "statusbar"
                                                        statusbar
                                                        "visible")))
      ;; Set an action on the toolbar
      (let ((toolbar (gtk:builder-object builder "toolbar")))
        (g:action-map-add-action *application-window*
                                 (g:property-action-new "toolbar"
                                                        toolbar
                                                        "visible")))

    ;; Button inform opens a dialog
    (let ((dialog (gtk:builder-object builder "info_dialog"))
          (button (gtk:builder-object builder "info_dialog_button")))
      (g:signal-connect dialog "response"
                        (lambda (dialog id)
                          (declare (ignore id))
                          (format t "in RESPONSE for ~a~%" dialog)
                          (gtk:widget-hide dialog)))
      (g:signal-connect button "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (format t "in CLICKED with ~a~%" dialog)
                          (gtk:widget-show dialog))))

    ;; Button Act opens a dialog
    (let ((dialog (gtk:builder-object builder "action_dialog"))
          (button (gtk:builder-object builder "action_dialog_button"))
          (actbutton (gtk:builder-object builder "act_action_dialog")))
      (g:signal-connect dialog "response"
                        (lambda (dialog response)
                          (declare (ignore response))
                          (gtk:widget-hide dialog)))
      (g:signal-connect button "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (gtk:widget-show dialog)))

      (g:signal-connect actbutton "clicked"
          (lambda (button)
            (format t " Action button clicked ~a~%" button)
            (g:timeout-add 1000
                           (lambda ()
                             (let* ((stack *toplevel-stack*)
                                    (page (gtk:stack-child-by-name stack
                                                                   "page3")))
                               (setf (gtk:stack-child-needs-attention stack
                                                                      page)
                                     t)
                               glib:+source-remove+)))
                )))

#|
  widget = (GtkWidget *)gtk_builder_get_object (builder, "act_action_dialog");
  g_signal_connect (widget, "clicked", G_CALLBACK (action_dialog_button_clicked), stack);
|#

    (let ((dialog (gtk:builder-object builder "preference_dialog"))
          (button (gtk:builder-object builder "preference_dialog_button"))
          (details (gtk:builder-object builder "details_entry"))
          (moredetails (gtk:builder-object builder "more_details_entry"))
          (scale (gtk:builder-object builder "level_scale"))
          (switch (gtk:builder-object builder "mode_switch"))
          (label (gtk:builder-object builder "error_label")))

      (setf (g:object-data dialog "level_scale") scale)
      (setf (g:object-data dialog "mode_switch") switch)
      (setf (g:object-data dialog "error_label") label)

      (g:signal-connect dialog "delete-event"
                        (lambda (widget event)
                          (declare (ignore event))
                          (gtk:widget-hide-on-delete widget)))

      (g:signal-connect dialog "response"
                        (lambda (dialog response)
                          (declare (ignore response))
                          (format t "in RESPONSE for ~a~%" dialog)
                          (gtk:widget-hide dialog)))

      (g:signal-connect button "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (format t "in CLICKED with ~a~%" dialog)
                          (gtk:widget-show dialog)))

      (g:signal-connect moredetails "notify::text"
              (lambda (entry pspec)
                (declare (ignore pspec))
                (format t "in NOTIFY::TEXT  ~a  ~a~%"
                          (length (gtk:entry-text entry))
                          (length (gtk:entry-text details)))
                (if (and (> (length (gtk:entry-text entry)) 50)
                         (= (length (gtk:entry-text details)) 0))
                    (progn
                      (setf (gtk:widget-tooltip-text entry)
                            "Must have details first.")
                      (gtk:style-context-add-class
                          (gtk:widget-style-context entry) "error"))
                    (progn
                      (setf (gtk:widget-tooltip-text entry) "")
                      (gtk:style-context-remove-class
                          (gtk:widget-style-context entry) "error")))))

      (g:signal-connect switch "state-set"
              (lambda (switch state)
                (let* ((dialog (gtk:widget-ancestor switch "GtkDialog"))
                       (scale (g:object-data dialog "level_scale"))
                       (label (g:object-data dialog "error_label")))
                  (format t "in STATE-SET with dialog ~a~%" dialog)
                  (format t "in STATE-SET with scale ~a~%" scale)
                  (format t "in STATE-SET with label ~a~%" label)

                  (if (or (not state)
                          (> (gtk:range-value scale) 50))
                      (progn
                        (gtk:widget-hide label)
                        (setf (gtk:switch-state switch) state))
                      (gtk:widget-show label)))
                  t))

      (g:signal-connect scale "value-changed"
              (lambda (scale)
                (format t "in VALUE-CHANGED~%")
                (let* ((dialog (gtk:widget-ancestor scale "GtkDialog"))
                       (switch (g:object-data dialog "mode_switch"))
                       (label (g:object-data dialog "error_label")))
                  (if (and (gtk:switch-active switch)
                           (not (gtk:switch-state switch))
                           (> (gtk:range-value scale) 50))
                      (progn
                        (gtk:widget-hide label)
                        (setf (gtk:switch-state switch) t))
                      (if (and (gtk:switch-state switch)
                               (<= (gtk:range-value scale) 50))
                          (setf (gtk:switch-state switch) nil))))))
)

#|
  dialog = (GtkWidget *)gtk_builder_get_object (builder, "selection_dialog");
  g_object_set_data (G_OBJECT (window), "selection_dialog", dialog);
  widget = (GtkWidget *)gtk_builder_get_object (builder, "text3");
  g_signal_connect (dialog, "response", G_CALLBACK (close_selection_dialog), widget);

  widget = (GtkWidget *)gtk_builder_get_object (builder, "selection_dialog_button");
  g_signal_connect (widget, "clicked", G_CALLBACK (show_dialog), dialog);

  widget2 = (GtkWidget *)gtk_builder_get_object (builder, "selection_flowbox");
  g_object_set_data (G_OBJECT (window), "selection_flowbox", widget2);
  g_signal_connect_swapped (widget, "clicked", G_CALLBACK (populate_flowbox), widget2);
|#

    (let ((dialog (gtk:builder-object builder "selection_dialog"))
          (widget1 (gtk:builder-object builder "text3"))
          (button (gtk:builder-object builder "selection_dialog_button"))
          (flowbox (gtk:builder-object builder "selection_flowbox")))
      (declare (ignore widget1))
      (setf (g:object-data *application-window* "selection_dialog") dialog)
      (setf (g:object-data *application-window* "selection_flowbox") flowbox)

      (g:signal-connect dialog "response"
                        (lambda (dialog id)
                          (declare (ignore id))
                          (gtk:widget-hide dialog)
                          ;; Here the rest of close_selection_dialog
                          ))
      (g:signal-connect button "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (gtk:widget-show dialog)))
      (g:signal-connect button "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (populate-flowbox flowbox)))
    )

    ;; Configure list box with 6 rows on left bottom
    (let ((listbox (gtk:builder-object builder "listbox"))
          (switch (gtk:builder-object builder "listboxrow1switch"))
          (row3 (gtk:builder-object builder "listboxrow3"))
          (row4 (gtk:builder-object builder "listboxrow4"))
          (image (gtk:builder-object builder "listboxrow3image"))
          (infodialog (gtk:builder-object builder "info_dialog"))
          (button (gtk:builder-object builder "listboxrow5button"))
          (actiondialog (gtk:builder-object builder "action_dialog")))

      (gtk:list-box-set-header-func listbox
              (lambda (row before)
                (when (and before
                           (not (gtk:list-box-row-header row)))
                (let ((separator (gtk:separator-new :horizontal)))
                  (gtk:widget-show separator)
                  (setf (gtk:list-box-row-header row) separator)))))
      (g:signal-connect listbox "row-activated"
              (lambda (listbox row)
                (declare (ignore listbox))
                (let ((image (g:object-data row "image"))
                      (dialog (g:object-data row "dialog")))
                  (when image
                    (if (> (gtk:widget-opacity image) 0)
                        (setf (gtk:widget-opacity image) 0)
                        (setf (gtk:widget-opacity image) 1)))
                  (when dialog
                    (gtk:window-present dialog)))))

      (g:signal-connect switch "notify::active"
              (lambda (switch pspec)
                (declare (ignore pspec))
                (format t "in notify::active for ~a~%" switch)
                (if (gtk:switch-active switch)
                    (setf (gtk:list-box-selection-mode listbox) :single)
                    (setf (gtk:list-box-selection-mode listbox) :none))
                (setf (gtk:list-box-activate-on-single-click listbox)
                      (not (gtk:switch-active switch)))))

      (setf (g:object-data row3 "image") image)
      (setf (g:object-data row4 "dialog") infodialog)

      (g:signal-connect button "clicked"
              (lambda (button)
                (declare (ignore button))
                (gtk:window-present actiondialog))))

    ;; Configure widgets on page3

    ;; Add/remove style class for record button on page3
    (g:signal-connect (gtk:builder-object builder "record_button") "toggled"
        (lambda (button)
          (let ((context (gtk:widget-style-context button)))
            (if (gtk:toggle-button-active button)
                (gtk:style-context-remove-class context "destructive-action")
                (gtk:style-context-add-class context "destructive-action")))))

    ;; Change sensitivity for buttons on page3
    (g:signal-connect (gtk:builder-object builder "page_combo") "changed"
        (lambda (combo)
          (let ((from (g:object-data combo "range_from_spin"))
                (to (g:object-data combo "range_to_spin"))
                (print (g:object-data combo "print_button")))
            (cond ((= 0 (gtk:combo-box-active combo))
                   (setf (gtk:widget-sensitive from) t)
                   (setf (gtk:widget-sensitive to) t)
                   (setf (gtk:widget-sensitive print) t))
                  ((= 1 (gtk:combo-box-active combo))
                   (setf (gtk:widget-sensitive from) nil)
                   (setf (gtk:widget-sensitive to) nil)
                   (setf (gtk:widget-sensitive print) t)
                   (setf (gtk:spin-button-value from) 1)
                   (setf (gtk:spin-button-value to) 99))
                  ((= 2 (gtk:combo-box-active combo))
                   (setf (gtk:widget-sensitive from) nil)
                   (setf (gtk:widget-sensitive to) nil)
                   (setf (gtk:widget-sensitive print) t)
                   (setf (gtk:spin-button-value from) 7)
                   (setf (gtk:spin-button-value to) 7))
                  ((= 4 (gtk:combo-box-active combo))
                   (setf (gtk:widget-sensitive from) nil)
                   (setf (gtk:widget-sensitive to) nil)
                   (setf (gtk:widget-sensitive print) nil))))))

    (g:signal-connect (gtk:builder-object builder "range_from_spin")
                      "value-changed"
        (lambda (from)
          (let* ((to (g:object-data from "range_to_spin"))
                 (value1 (gtk:spin-button-value-as-int from))
                 (value2 (gtk:spin-button-value-as-int to)))
            (when (> value1 value2)
              (setf (gtk:spin-button-value to) value1)))))

    (g:signal-connect (gtk:builder-object builder "range_to_spin")
                      "value-changed"
        (lambda (to)
          (let* ((from (g:object-data to "range_from_spin"))
                 (value1 (gtk:spin-button-value-as-int from))
                 (value2 (gtk:spin-button-value-as-int to)))
            (when (> value1 value2)
              (setf (gtk:spin-button-value from) value2)))))

    (g:signal-connect (gtk:builder-object builder "osd_frame")
                      "button-press-event"
        (lambda (widget event)
          (declare (ignore event))
          (let* ((osd (g:object-data widget "osd"))
                 (visible (gtk:widget-visible osd)))
            (setf (gtk:widget-visible osd) (not visible))
            gdk:+event-stop+)))

    ;; Set values on the property list for page combo on page3
    (let ((combo (gtk:builder-object builder "page_combo"))
          (from (gtk:builder-object builder "range_from_spin"))
          (to (gtk:builder-object builder "range_to_spin"))
          (print (gtk:builder-object builder "print_button")))

      (gtk:combo-box-set-row-separator-func combo
          (lambda (model iter)
            (let ((text (gtk:tree-model-value model iter 0)))
              (string= "-" text))))

      (setf (g:object-data combo "range_from_spin") from)
      (setf (g:object-data combo "range_to_spin") to)
      (setf (g:object-data combo "print_button") print)
      (setf (g:object-data from "range_to_spin") to)
      (setf (g:object-data to "range_from_spin") from))

    (let ((page (gtk:builder-object builder "closable_page_1")))
      (g:signal-connect (gtk:builder-object builder "closable_page_button_1")
                        "clicked"
          (lambda (button)
            (declare (ignore button))
            (gtk:widget-hide page)
            (g:timeout-add 2500
                           (lambda ()
                             (gtk:widget-show page)
                             glib:+source-remove+)))))

    (let ((page (gtk:builder-object builder "closable_page_2")))
      (g:signal-connect (gtk:builder-object builder "closable_page_button_2")
                        "clicked"
          (lambda (button)
            (declare (ignore button))
            (gtk:widget-hide page)
            (g:timeout-add 2500
                           (lambda ()
                             (gtk:widget-show page)
                             glib:+source-remove+)))))

    (let ((popover (gtk:builder-object builder "open_popover"))
          (entry (gtk:builder-object builder "open_popover_entry"))
          (button (gtk:builder-object builder "open_popover_button")))
      (setf (gtk:popover-default-widget popover) button)
      (g:signal-connect entry "notify::text"
              (lambda (entry pspec)
                (declare (ignore pspec))
                (setf (gtk:widget-sensitive button)
                      (> (length (gtk:entry-text entry)) 0))))
      (g:signal-connect button "clicked"
              (lambda (button)
                (declare (ignore button))
                (gtk:widget-hide popover)))

    (setf (g:object-data *application-window* "open_menubutton")
          (gtk:builder-object builder "open_menubutton"))
    (setf (g:object-data *application-window* "record_button")
          (gtk:builder-object builder "record_button")))

#|
    (g:signal-connect (gtk:builder-object builder "increase_button")
                      "clicked"
                      #'increase-icon-size)

    (g:signal-connect (gtk:builder-object builder "decrease_button")
                      "clicked"
                      #'decrease-icon-size)
    (g:signal-connect (gtk:builder-object builder "reset_button")
                      "clicked"
                      #'reset-icon-size)
|#

#|
  widget = (GtkWidget *)gtk_builder_get_object (builder, "charletree");
  populate_model ((GtkTreeStore *)gtk_tree_view_get_model (GTK_TREE_VIEW (widget)));
  gtk_tree_view_set_row_separator_func (GTK_TREE_VIEW (widget), row_separator_func, NULL, NULL);
  gtk_tree_view_expand_all (GTK_TREE_VIEW (widget));

  widget = GTK_WIDGET (gtk_builder_get_object (builder, "munsell"));
  widget2 = GTK_WIDGET (gtk_builder_get_object (builder, "cchooser"));
|#

    (let ((treeview (gtk:builder-object builder "charletree")))
      (populate-model (gtk:tree-view-model treeview))
    )

#|
  set_accel (GTK_APPLICATION (app), GTK_WIDGET (gtk_builder_get_object (builder, "quitmenuitem")));
  set_accel (GTK_APPLICATION (app), GTK_WIDGET (gtk_builder_get_object (builder, "deletemenuitem")));
  set_accel (GTK_APPLICATION (app), GTK_WIDGET (gtk_builder_get_object (builder, "searchmenuitem")));
  set_accel (GTK_APPLICATION (app), GTK_WIDGET (gtk_builder_get_object (builder, "darkmenuitem")));
  set_accel (GTK_APPLICATION (app), GTK_WIDGET (gtk_builder_get_object (builder, "aboutmenuitem")));
  set_accel (GTK_APPLICATION (app), GTK_WIDGET (gtk_builder_get_object (builder, "bgmenuitem")));

  widget2 = (GtkWidget *)gtk_builder_get_object (builder, "tooltextview");

  widget = (GtkWidget *)gtk_builder_get_object (builder, "toolbutton1");
  g_signal_connect (widget, "clicked", G_CALLBACK (handle_insert), widget2);
  widget = (GtkWidget *)gtk_builder_get_object (builder, "toolbutton2");
  g_signal_connect (widget, "clicked", G_CALLBACK (handle_insert), widget2);
  widget = (GtkWidget *)gtk_builder_get_object (builder, "toolbutton3");
  g_signal_connect (widget, "clicked", G_CALLBACK (handle_insert), widget2);
  widget = (GtkWidget *)gtk_builder_get_object (builder, "toolbutton4");
  g_signal_connect (widget, "clicked", G_CALLBACK (handle_insert), widget2);
  widget = (GtkWidget *)gtk_builder_get_object (builder, "cutbutton");
  g_signal_connect (widget, "clicked", G_CALLBACK (handle_cutcopypaste), widget2);
  g_signal_connect (gtk_text_view_get_buffer (GTK_TEXT_VIEW (widget2)), "notify::has-selection",
                    G_CALLBACK (textbuffer_notify_selection), widget);
  widget = (GtkWidget *)gtk_builder_get_object (builder, "copybutton");
  g_signal_connect (widget, "clicked", G_CALLBACK (handle_cutcopypaste), widget2);
  g_signal_connect (gtk_text_view_get_buffer (GTK_TEXT_VIEW (widget2)), "notify::has-selection",
                    G_CALLBACK (textbuffer_notify_selection), widget);
  widget = (GtkWidget *)gtk_builder_get_object (builder, "deletebutton");
  g_signal_connect (widget, "clicked", G_CALLBACK (handle_cutcopypaste), widget2);
  g_signal_connect (gtk_text_view_get_buffer (GTK_TEXT_VIEW (widget2)), "notify::has-selection",
                    G_CALLBACK (textbuffer_notify_selection), widget);
  widget = (GtkWidget *)gtk_builder_get_object (builder, "pastebutton");
  g_signal_connect (widget, "clicked", G_CALLBACK (handle_cutcopypaste), widget2);
  g_signal_connect_object (gtk_widget_get_clipboard (widget2, GDK_SELECTION_CLIPBOARD), "owner-change",
                           G_CALLBACK (clipboard_owner_change), widget, 0);
|#

    (let ((widget1 (gtk:builder-object builder "osd_frame"))
          (widget2 (gtk:builder-object builder "totem_like_osd")))
      (setf (g:object-data widget1 "osd") widget2))

#|
  widget = (GtkWidget *)gtk_builder_get_object (builder, "textview1");
  g_signal_connect (widget, "populate-popup",
                    G_CALLBACK (populate_popup), NULL);

  widget = (GtkWidget *)gtk_builder_get_object (builder, "open_popover");
  widget2 = (GtkWidget *)gtk_builder_get_object (builder, "open_popover_entry");
  widget3 = (GtkWidget *)gtk_builder_get_object (builder, "open_popover_button");
  gtk_popover_set_default_widget (GTK_POPOVER (widget), widget3);
  g_signal_connect (widget2, "notify::text", G_CALLBACK (open_popover_text_changed), widget3);
  g_signal_connect_swapped (widget3, "clicked", G_CALLBACK (gtk_widget_hide), widget);
  widget = (GtkWidget *)gtk_builder_get_object (builder, "open_menubutton");
  g_object_set_data (G_OBJECT (window), "open_menubutton", widget);
  widget = (GtkWidget *)gtk_builder_get_object (builder, "record_button");
  g_object_set_data (G_OBJECT (window), "record_button", widget);

  widget = (GtkWidget *)gtk_builder_get_object (builder, "lockbox");
  widget2 = (GtkWidget *)gtk_builder_get_object (builder, "lockbutton");
  g_object_set_data (G_OBJECT (window), "lockbutton", widget2);
|#

    (let ((widget (gtk:builder-object builder "lockbox"))
          (widget2 (gtk:builder-object builder "lockbutton")))
      (declare (ignore widget))
      (setf (g:object-data *application-window* "lockbutton") widget2))

#|
  permission = g_object_new (g_test_permission_get_type (), NULL);
  g_object_bind_property (permission, "allowed",
                          widget, "sensitive",
                          G_BINDING_SYNC_CREATE);
  action = g_action_map_lookup_action (G_ACTION_MAP (window), "open");
  g_object_bind_property (permission, "allowed",
                          action, "enabled",
                          G_BINDING_SYNC_CREATE);
  action = g_action_map_lookup_action (G_ACTION_MAP (window), "record");
  g_object_bind_property (permission, "allowed",
                          action, "enabled",
                          G_BINDING_SYNC_CREATE);
  gtk_lock_button_set_permission (GTK_LOCK_BUTTON (widget2), permission);
  g_object_unref (permission);

  widget = (GtkWidget *)gtk_builder_get_object (builder, "iconview1");
  widget2 = (GtkWidget *)gtk_builder_get_object (builder, "increase_button");
  g_object_set_data (G_OBJECT (widget), "increase_button", widget2);
  widget2 = (GtkWidget *)gtk_builder_get_object (builder, "decrease_button");
  g_object_set_data (G_OBJECT (widget), "decrease_button", widget2);

|#

      ;; Show the application window
      (gtk:widget-show-all *application-window*)))

(defun startup (application)
  (format t "in signal startup for ~a~%" application))

(defun shutdown (application)
  (format t "in signal shutdown for ~a~%" application)
  ;; Quit the main loop
  (gtk:leave-gtk-main))

;;; ----------------------------------------------------------------------------

(defun gtk3-widget-factory (&rest argv)
  (gtk:within-main-loop
    (let ((argv (cons "application-cmdline"
                      (if argv argv (uiop:command-line-arguments))))
          (entries `(("about" ,#'activate-about nil nil nil)
                     ("quit" ,#'activate-quit nil nil nil)
                     ("inspector" ,#'activate-inspector nil nil nil)
                     ("main" nil "s" "'steak'" nil)
                     ("wine" nil nil "false" nil)
                     ("beer" nil nil "false" nil)
                     ("water" nil nil "true" nil)
                     ("dessert" nil "s" "'bars'" nil)
                     ("pay" nil "s" nil nil)
                    )))
      ;; Create the application
      (setf *application*
            (gtk:application-new "com.crategus.gtk3-widget-factory" :non-unique))
      ;; Set actions for the application
      (g:action-map-add-action-entries *application* entries)
      ;; Set enabled to "false" for action "wine"
      (setf (g:action-enabled (g:action-map-lookup-action *application* "wine"))
            nil)
      ;; Add a local option and install a signal handler
      (g:application-add-main-option *application*
                                     "version"
                                     0
                                     0
                                     :none
                                     "Show program version"
                                     nil)
      (g:signal-connect *application* "handle-local-options"
                        (lambda (app options)
                          (declare (ignore app))
                          (let ((status -1))
                            (when (g:variant-dict-contains options "version")
                              (gtk:cl-cffi-gtk-build-info)
                              (setf status 0))
                            status)))
      ;; Connect signal "activate" to the applicaton
      (g:signal-connect *application* "activate" #'activate)
      ;; Connect signal "startup" to the applicaton
      (g:signal-connect *application* "startup" #'startup)
      ;; Connect signal "shutdown" to the applicaton
      (g:signal-connect *application* "shutdown" #'shutdown)
      ;; Run the application
      (g:application-run *application* argv))))
