(in-package :gtk-test)

(def-suite gtk-calendar :in gtk-suite)
(in-suite gtk-calendar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCalendarDisplayOptions

(test gtk-calendar-display-options
  ;; Check type
  (is (g:type-is-flags "GtkCalendarDisplayOptions"))
  ;; Check registered name
  (is (eq 'gtk:calendar-display-options
          (glib:symbol-for-gtype "GtkCalendarDisplayOptions")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCalendarDisplayOptions")
          (g:gtype (cffi:foreign-funcall "gtk_calendar_display_options_get_type"
                                         :size))))
  ;; Check names
  (is (equal '("GTK_CALENDAR_SHOW_HEADING" "GTK_CALENDAR_SHOW_DAY_NAMES"
               "GTK_CALENDAR_NO_MONTH_CHANGE" "GTK_CALENDAR_SHOW_WEEK_NUMBERS"
               "GTK_CALENDAR_SHOW_DETAILS")
             (glib-test:list-flags-item-names "GtkCalendarDisplayOptions")))
  ;; Check values
  (is (equal '(1 2 4 8 32)
             (glib-test:list-flags-item-values "GtkCalendarDisplayOptions")))
  ;; Check nick names
  (is (equal '("show-heading" "show-day-names" "no-month-change"
               "show-week-numbers" "show-details")
             (glib-test:list-flags-item-nicks "GtkCalendarDisplayOptions")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GtkCalendarDisplayOptions"
                                     GTK:CALENDAR-DISPLAY-OPTIONS
                       (:EXPORT T
                        :TYPE-INITIALIZER
                        "gtk_calendar_display_options_get_type")
                       (:SHOW-HEADING 1)
                       (:SHOW-DAY-NAMES 2)
                       (:NO-MONTH-CHANGE 4)
                       (:SHOW-WEEK-NUMBERS 8)
                       (:SHOW-DETAILS 32))
             (gobject:get-gtype-definition "GtkCalendarDisplayOptions"))))

;;;     GtkCalendar

(test gtk-calendar-class
  ;; Check type
  (is (g:type-is-object "GtkCalendar"))
  ;; Check registered name
  (is (eq 'gtk:calendar
          (glib:symbol-for-gtype "GtkCalendar")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCalendar")
          (g:gtype (cffi:foreign-funcall "gtk_calendar_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkCalendar")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkCalendar")))
  ;; Check interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (glib-test:list-interfaces "GtkCalendar")))
  ;; Check class properties
  (is (equal '("day" "detail-height-rows" "detail-width-chars" "month"
               "no-month-change" "show-day-names" "show-details" "show-heading"
               "show-week-numbers" "year")
             (glib-test:list-properties "GtkCalendar")))
  ;; Check style properties
  (is (equal '("horizontal-separation" "inner-border" "vertical-separation")
             (gtk-test:list-style-properties "GtkCalendar")))
  ;; Check signals
  (is (equal '("day-selected" "day-selected-double-click" "month-changed"
               "next-month" "next-year" "prev-month" "prev-year")
             (glib-test:list-signals "GtkCalendar")))
  ;; CSS information
  (is (string= "calendar"
               (gtk:widget-class-css-name "GtkCalendar")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCalendar" GTK:CALENDAR
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_calendar_get_type")
                       ((DAY CALENDAR-DAY "day" "gint" T T)
                        (DETAIL-HEIGHT-ROWS CALENDAR-DETAIL-HEIGHT-ROWS
                         "detail-height-rows" "gint" T T)
                        (DETAIL-WIDTH-CHARS CALENDAR-DETAIL-WIDTH-CHARS
                         "detail-width-chars" "gint" T T)
                        (MONTH CALENDAR-MONTH "month" "gint" T T)
                        (NO-MONTH-CHANGE CALENDAR-NO-MONTH-CHANGE
                         "no-month-change" "gboolean" T T)
                        (SHOW-DAY-NAMES CALENDAR-SHOW-DAY-NAMES
                         "show-day-names" "gboolean" T T)
                        (SHOW-DETAILS CALENDAR-SHOW-DETAILS
                         "show-details" "gboolean" T T)
                        (SHOW-HEADING CALENDAR-SHOW-HEADING
                         "show-heading" "gboolean" T T)
                        (SHOW-WEEK-NUMBERS CALENDAR-SHOW-WEEK-NUMBERS
                         "show-week-numbers" "gboolean" T T)
                        (YEAR CALENDAR-YEAR "year" "gint" T T)))
             (gobject:get-gtype-definition "GtkCalendar"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-calendar-properties
  (let ((calendar (make-instance 'gtk:calendar)))
    (multiple-value-bind
        (second minute hour day month year day-of-week dst-p tz)
        (get-decoded-time)
      (declare (ignore second minute hour day-of-week dst-p tz))
      (is (= day (gtk:calendar-day calendar)))
      (is (= 0 (gtk:calendar-detail-height-rows calendar)))
      (is (= 0 (gtk:calendar-detail-width-chars calendar)))
      (is (= (1- month) (gtk:calendar-month calendar)))
      (is-false (gtk:calendar-no-month-change calendar))
      (is-true (gtk:calendar-show-day-names calendar))
      (is-true (gtk:calendar-show-details calendar))
      (is-true (gtk:calendar-show-heading calendar))
      (is-false (gtk:calendar-show-week-numbers calendar))
      (is (= year (gtk:calendar-year calendar))))))

;;; --- Style Properties -------------------------------------------------------

;;;     horizontal-separation
;;;     inner-border
;;;     vertical-separation

(test gtk-calendar-properties
  (let ((calendar (make-instance 'gtk:calendar)))
    (is (= 4 (gtk:widget-style-property calendar "horizontal-separation")))
    (is-true (gtk:widget-style-property calendar "inner-border"))
    (is (= 4 (gtk:widget-style-property calendar "vertical-separation")))))

;;; --- Signals ----------------------------------------------------------------

;;;     day-selected
;;;     day-selected-double-click
;;;     month-changed
;;;     next-month
;;;     next-year
;;;     prev-month
;;;     prev-year

;;; --- Functions --------------------------------------------------------------

;;;     GtkCalendarDetailFunc

;;;     gtk_calendar_new

(test gtk-calendar-new
  (is (typep (gtk:calendar-new) 'gtk:calendar)))

;;;     gtk_calendar_select_month
;;;     gtk_calendar_select_day
;;;     gtk_calendar_mark_day
;;;     gtk_calendar_unmark_day
;;;     gtk_calendar_get_day_is_marked
;;;     gtk_calendar_clear_marks

;;;     gtk_calendar_get_display_options
;;;     gtk_calendar_set_display_options
;;;     gtk_calendar_get_date

;;;     gtk_calendar_set_detail_func

;;; 2024-9-22
