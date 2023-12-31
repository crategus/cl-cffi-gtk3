(in-package :gtk-test)

(def-suite gtk-calendar :in gtk-suite)
(in-suite gtk-calendar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCalendarDisplayOptions

(test gtk-calendar-display-options
  ;; Check the type
  (is (g:type-is-flags "GtkCalendarDisplayOptions"))
  ;; Check the registered name
  (is (eq 'gtk:calendar-display-options
          (glib:symbol-for-gtype "GtkCalendarDisplayOptions")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCalendarDisplayOptions")
          (g:gtype (cffi:foreign-funcall "gtk_calendar_display_options_get_type"
                                         :size))))
  ;; Check the names
  (is (equal '("GTK_CALENDAR_SHOW_HEADING" "GTK_CALENDAR_SHOW_DAY_NAMES"
               "GTK_CALENDAR_NO_MONTH_CHANGE" "GTK_CALENDAR_SHOW_WEEK_NUMBERS"
               "GTK_CALENDAR_SHOW_DETAILS")
             (list-flags-item-name "GtkCalendarDisplayOptions")))
  ;; Check the values
  (is (equal '(1 2 4 8 32)
             (list-flags-item-value "GtkCalendarDisplayOptions")))
  ;; Check the nick names
  (is (equal '("show-heading" "show-day-names" "no-month-change"
               "show-week-numbers" "show-details")
             (list-flags-item-nick "GtkCalendarDisplayOptions")))
  ;; Check the flags definition
  (is (equal '(GOBJECT:DEFINE-G-FLAGS "GtkCalendarDisplayOptions"
                                      GTK-CALENDAR-DISPLAY-OPTIONS
                                      (:EXPORT T
                                       :TYPE-INITIALIZER
                                       "gtk_calendar_display_options_get_type")
                                      (:SHOW-HEADING 1)
                                      (:SHOW-DAY-NAMES 2)
                                      (:NO-MONTH-CHANGE 4)
                                      (:SHOW-WEEK-NUMBERS 8)
                                      (:SHOW-DETAILS 32))
             (gobject:get-g-type-definition "GtkCalendarDisplayOptions"))))

;;;     GtkCalendar

(test gtk-calendar-class
  ;; Type check
  (is (g:type-is-object "GtkCalendar"))
  ;; Check the registered name
  (is (eq 'gtk:calendar
          (glib:symbol-for-gtype "GtkCalendar")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCalendar")
          (g:gtype (cffi:foreign-funcall "gtk_calendar_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkCalendar")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkCalendar")))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (list-interfaces "GtkCalendar")))
  ;; Check the class properties
  (is (equal '("day" "detail-height-rows" "detail-width-chars" "month"
               "no-month-change" "show-day-names" "show-details" "show-heading"
               "show-week-numbers" "year")
             (list-properties "GtkCalendar")))
  ;; Check the style properties
  (is (equal '("horizontal-separation" "inner-border" "vertical-separation")
             (list-style-properties "GtkCalendar")))
  ;; Check the signals
  (is (equal '("day-selected" "day-selected-double-click" "month-changed"
               "next-month" "next-year" "prev-month" "prev-year")
             (list-signals "GtkCalendar")))
  ;; CSS information
  (is (string= "calendar"
               (gtk:widget-class-css-name "GtkCalendar")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkCalendar" GTK-CALENDAR
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("AtkImplementorIface" "GtkBuildable")
                                :TYPE-INITIALIZER "gtk_calendar_get_type")
                               ((DAY GTK-CALENDAR-DAY "day" "gint" T T)
                                (DETAIL-HEIGHT-ROWS
                                 GTK-CALENDAR-DETAIL-HEIGHT-ROWS
                                 "detail-height-rows" "gint" T T)
                                (DETAIL-WIDTH-CHARS
                                 GTK-CALENDAR-DETAIL-WIDTH-CHARS
                                 "detail-width-chars" "gint" T T)
                                (MONTH GTK-CALENDAR-MONTH "month" "gint" T T)
                                (NO-MONTH-CHANGE GTK-CALENDAR-NO-MONTH-CHANGE
                                 "no-month-change" "gboolean" T T)
                                (SHOW-DAY-NAMES GTK-CALENDAR-SHOW-DAY-NAMES
                                 "show-day-names" "gboolean" T T)
                                (SHOW-DETAILS GTK-CALENDAR-SHOW-DETAILS
                                 "show-details" "gboolean" T T)
                                (SHOW-HEADING GTK-CALENDAR-SHOW-HEADING
                                 "show-heading" "gboolean" T T)
                                (SHOW-WEEK-NUMBERS
                                 GTK-CALENDAR-SHOW-WEEK-NUMBERS
                                 "show-week-numbers" "gboolean" T T)
                                (YEAR GTK-CALENDAR-YEAR "year" "gint" T T)))
             (gobject:get-g-type-definition "GtkCalendar"))))

;;; --- Properties -------------------------------------------------------------

;;;     day
;;;     detail-height-rows
;;;     detail-width-chars
;;;     month
;;;     no-month-change
;;;     show-day-names
;;;     show-details
;;;     show-heading
;;;     show-week-numbers
;;;     year

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

;;; 2023-12-30
