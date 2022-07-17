;;; calfw-blocks.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 null
;;
;; Author: null
;; Maintainer: null <null>
;; Created: July 06, 2022
;; Modified: July 06, 2022
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:


(defcustom calfw-blocks-earliest-visible-time '(8 0)
  "Earliest visible time as list (hours minutes)."
  :group 'calfw-blocks
  :type 'number)

(defcustom calfw-blocks-lines-per-hour 4
  "Number of lines per hour in a block."
  :group 'calfw-blocks
  :type 'number)

(defun calfw-blocks-view-week ()
    "Render weekly block calendar view.")

(defcustom calfw-blocks-colors
  '("#ef7969"
    "#49c029"
    "#ffcf00"
    "#7090ff"
    "#e07fff"
    "#70d3f0")
  "Colors to use for blocks. The default colors are
Modus Vivendi's colors for graphs.")

;; why does the title %t already include the start time???
(defun cfw:cp-dispatch-view-impl (view)
  "[internal] Return a view function which is corresponding to the view symbol.
VIEW is a symbol of the view type."
  (cond
   ((eq 'month     view)  'cfw:view-month)
   ((eq 'week      view)  'cfw:view-week)
   ((eq 'two-weeks view)  'cfw:view-two-weeks)
   ((eq 'day       view)  'cfw:view-day)
   ((eq 'block-week       view)  'calfw-blocks-view-block-week)
   (t (error "Not found such view : %s" view))))

(defun calfw-blocks-view-block-week-model (model)
  "[internal] Create a logical view model of weekly calendar.
This function collects and arranges contents.  This function does
not know how to display the contents in the destinations."
  (let* ((init-date (cfw:k 'init-date model))
         (begin-date (cfw:week-begin-date init-date))
         (end-date (cfw:week-end-date init-date)))
    (cfw:view-model-make-common-data-for-weeks model begin-date end-date)))

;; (cfw:view-week-model (cfw:model-abstract-new (cfw:date 1 1 2011) nil nil))

(defun calfw-blocks-view-block-week-calc-param (dest)
  "[internal] Calculate cell size from the reference size and
return an alist of rendering parameters."
  (let*
      ((win-width (cfw:dest-width dest))
       ;; title 2, toolbar 1, header 2, hline 2, footer 1, margin 2 => 10
       (win-height (max 15 (- (cfw:dest-height dest) 10)))
       (junctions-width (* (char-width cfw:fchar-junction) 8))
       (cell-width  (cfw:round-cell-width
                     (max 5 (/ (- win-width junctions-width) 7))))
       (cell-height (max 2 win-height))
       (total-width (+ (* cell-width cfw:week-days) junctions-width)))
    `((cell-width . ,cell-width)
      (cell-height . ,cell-height)
      (total-width . ,total-width)
      (columns . ,cfw:week-days))))

(defun calfw-blocks-view-week-calc-param (dest)
  "[internal] Calculate cell size from the reference size and
return an alist of rendering parameters."
  (let*
      ((time-width 5)
       (time-hline (make-string time-width ? ))
       (win-width (cfw:dest-width dest))
       ;; title 2, toolbar 1, header 2, hline 2, footer 1, margin 2 => 10
       (win-height (max 15 (- (cfw:dest-height dest) 10)))
       (junctions-width (* (char-width cfw:fchar-junction) 8))
       (cell-width  (cfw:round-cell-width
                     (max 5 (/ (- win-width junctions-width time-width) 7))))
       (cell-height (max 2 win-height))
       (total-width (+ time-width (* cell-width cfw:week-days) junctions-width)))
    `((cell-width . ,cell-width)
      (cell-height . ,cell-height)
      (total-width . ,total-width)
      (columns . ,cfw:week-days)
      (time-width . ,time-width)
      (time-hline . ,time-hline))))

;; (defun calfw-blocks-view-block-week-model (model)
;;   "[internal] Create a logical view model of weekly calendar.
;; This function collects and arranges contents.  This function does
;; not know how to display the contents in the destinations."
;;   (let* ((init-date (cfw:k 'init-date model))
;;          (begin-date (cfw:week-begin-date init-date))
;;          (end-date (cfw:week-end-date init-date)))
;; (cfw:model-create-updated-view-data
;;    model
;;    (cfw:view-model-make-common-data
;;     model begin-date end-date
;;     `((headers . ,(cfw:view-model-make-day-names-for-week)) ; a list of the index of day-of-week
;;       (weeks . ,(cfw:view-model-make-weeks ; a matrix of day-of-month, which corresponds to the index of `headers'
;;                  (cfw:week-begin-date begin-date)
;;                  (cfw:week-end-date   end-date))))))

;;     ))

(defun calfw-blocks-view-block-week (component)
  "[internal] Render weekly calendar view."
  (let* ((dest (cfw:component-dest component))
         (param (cfw:render-append-parts (calfw-blocks-view-week-calc-param dest)))
         (total-width (cfw:k 'total-width param))
         (time-width (cfw:k 'time-width param))
         (EOL (cfw:k 'eol param))
         (VL (cfw:k 'vl param))
         (time-hline (cfw:k 'time-hline param))
         (hline (concat time-hline (cfw:k 'hline param)))
         (cline (concat time-hline (cfw:k 'cline param)))
         (model (cfw:view-week-model (cfw:component-model component)))
         (begin-date (cfw:k 'begin-date model))
         (end-date (cfw:k 'end-date model)))
    ;; update model
    (setf (cfw:component-model component) model)
    ;; header
    (insert
     (cfw:rt
      (cfw:render-title-period begin-date end-date)
      'cfw:face-title)
     EOL (cfw:render-toolbar total-width 'week
                             'cfw:navi-previous-week-command
                             'cfw:navi-next-week-command)
     EOL hline)
    ;; time header
    ;; (insert cline)
    (insert (cfw:rt (cfw:render-right time-width "Time")
                           'default))
    ;; day names
    (cfw:render-day-of-week-names model param)
    (insert VL EOL cline)
    ;; contents
    (calfw-blocks-render-calendar-cells-block-weeks
     model param
     (lambda (date week-day hday)
       (cfw:rt (format "%s" (calendar-extract-day date))
               (if hday 'cfw:face-sunday
                 (cfw:render-get-week-face
                  week-day 'cfw:face-default-day)))))
    ;; footer
    (insert (cfw:render-footer total-width (cfw:model-get-contents-sources model)))))

(defun calfw-blocks-change-view-block-week ()
  "change-view-week"
  (interactive)
  (when (cfw:cp-get-component)
    (cfw:cp-set-view (cfw:cp-get-component) 'block-week)))

(defun calfw-blocks-render-calendar-cells-block-weeks (model param title-func)
  "[internal] Insert calendar cells for week based views."
  (loop for week in (cfw:k 'weeks model) do
        (calfw-blocks-render-calendar-cells-days model param title-func week
                                                 'calfw-blocks-render-content
                                                 t)))

(defun calfw-blocks-render-calendar-cells-days (model param title-func &optional
                                             days content-fun do-weeks)
  "[internal] Insert calendar cells for the linear views."
  (calfw-blocks-render-columns
   (loop with cell-width      = (cfw:k 'cell-width param)
         with days            = (or days (cfw:k 'days model))
         with content-fun     = (or content-fun
                                    'cfw:render-event-days-overview-content)
         with holidays        = (cfw:k 'holidays model)
         with annotations     = (cfw:k 'annotations model)
         with headers         = (cfw:k 'headers  model)
         with raw-periods-all = (calfw-blocks-render-periods-stacks model)
         with sorter          = (cfw:model-get-sorter model)

         for date in days ; days columns loop
         for count from 0 below (length days)
         for hday         = (car (cfw:contents-get date holidays))
         for week-day     = (nth count headers)
         for ant          = (cfw:rt (cfw:contents-get date annotations)
                                    'cfw:face-annotation)
         for raw-periods  = (cfw:contents-get date raw-periods-all)
         for raw-contents = (cfw:render-sort-contents
                             (funcall content-fun
                                      (cfw:model-get-contents-by-date date model))
                             sorter)
         for prs-contents = (cfw:render-rows-prop
                             (append (if do-weeks
                                         (calfw-blocks-render-periods
                                          date week-day raw-periods cell-width)
                                       (cfw:render-periods-days
                                        date raw-periods cell-width))
                                     (mapcar 'cfw:render-default-content-face
                                             raw-contents)))
         for num-label = (if prs-contents
                             (format "(%s)"
                                     (+ (length raw-contents)
                                        (length raw-periods))) "")
         for tday = (concat
                     " " ; margin
                     (funcall title-func date week-day hday)
                     (if num-label (concat " " num-label))
                     (if hday (concat " " (cfw:rt (substring hday 0)
                                                  'cfw:face-holiday))))
         collect
         (cons date (cons (cons tday ant) prs-contents)))
   param))

(defun calfw-blocks-render-periods-stacks (model)
  "Modified version of cfw:render-periods-stacks, where the last element of
period is a pair containing the start and end of time of each event.
[internal] Arrange the `periods' records of the model and
create period-stacks on the each days.
period-stack -> ((row-num . period) ... )"
  (let* (periods-each-days)
    (loop for (begin end event) in (cfw:k 'periods model)
          for content = (if (cfw:event-p event)
                            ;; (cfw:event-period-overview event)
                            (cfw:event-period-overview event)
                          event)
          for period = (list begin end content
                             (cfw:extract-text-props content 'face)
                             (if (cfw:event-p event) (calfw-blocks-get-time-interval event) nil))
          for row = (cfw:render-periods-get-min periods-each-days begin end)
          do
          (setq periods-each-days (cfw:render-periods-place
                                   periods-each-days row period)))
    periods-each-days))

(defun calfw-blocks-get-time-interval (event)
  "Return (start-time . end-time) of EVENT, a cfw:event struct.
start-time and end-time are both lists '(a b) where a is the hour,
b is the minute."
  (cons (cfw:event-start-time event)
        (cfw:event-end-time event)))

(defun calfw-blocks-render-content (lst)
  "[internal] Apply `cfw:event-overview' on `cfw:event's in `lst'."
  (mapcar (lambda (event)
            (if (cfw:event-p event)
                (progn
                  (propertize
                   (cfw:event-overview event)
                   'calfw-blocks-interval (calfw-blocks-get-time-interval event)))
                event))
          lst))

(defun calfw-blocks-render-periods (date week-day periods-stack cell-width)
  "[internal] This function translates PERIOD-STACK to display content on the DATE."
  (mapcar (lambda (p)
            (let ((content (nth 2 (cadr p)))
                  (props (nth 3 (cadr p)))
                  (interval (nth 4 (cadr p))))
              (propertize content
                          'face (cfw:render-get-face-period content 'cfw:face-periods)
                          'font-lock-face (cfw:render-get-face-period content 'cfw:face-periods)
                          'cfw:period t
                          'calfw-blocks-interval interval)))
          (seq-sort (lambda (a b) (< (car a) (car b)))
                    periods-stack)))

(defun calfw-blocks-format-time (t)
  (format "%02d:%02d" (car t) (cadr t)))

(defun calfw-blocks-time-column (time-width cell-height)
  (let* ((num-hours (floor (/ cell-height calfw-blocks-lines-per-hour)))
        (start-hour (car calfw-blocks-earliest-visible-time))
        (start-minute (cadr calfw-blocks-earliest-visible-time))
        (times-lst (mapcar (lambda (x) (list (+ x start-hour) start-minute))
                           (number-sequence 0 (1- num-hours)))))
    (print (mapcan (lambda (x) (append (list (calfw-blocks-format-time x))
                              (mapcar (lambda (x) (make-string time-width ? ))
                                      (number-sequence 0 (1- calfw-blocks-lines-per-hour)))))
            times-lst))
    )
  )

(defun calfw-blocks-render-columns (day-columns param)
  "[internal] This function concatenates each rows on the days into a string of a physical line.
DAY-COLUMNS is a list of columns. A column is a list of following form: (DATE (DAY-TITLE . ANNOTATION-TITLE) STRING STRING...)."
  (let* ((cell-width  (cfw:k 'cell-width  param))
        (cell-height (cfw:k 'cell-height param))
        (time-width (cfw:k 'time-width param))
        (EOL (cfw:k 'eol param))
        (VL (cfw:k 'vl param))
        (time-hline (cfw:k 'time-hline param))
        (hline (concat time-hline (cfw:k 'hline param)))
        (cline (concat time-hline (cfw:k 'cline param))))
    ;; day title
    ;; (push day-columns)
    ;; (insert
    ;;        VL (cfw:tp
    ;;                (cfw:render-default-content-face
    ;;                 (cfw:render-add-right cell-width "Time" "")
    ;;                 'cfw:face-day-title)
    ;;                'cfw:date date)
    ;;        ;; (cfw:render-left cell-width "")
    ;;        )
    (insert time-hline)
    (loop for day-rows in day-columns
          for date = (car day-rows)
          for (tday . ant) = (cadr day-rows)
          do
          (insert
           VL (if date
                  (cfw:tp
                   (cfw:render-default-content-face
                    (cfw:render-add-right cell-width tday ant)
                    'cfw:face-day-title)
                   'cfw:date date)
                (cfw:render-left cell-width ""))))
    (insert VL EOL)
    ;; (print cell-height) ;; 39
    ;; (print cell-width) ;; 17
    ;; day contents
    ;; (mapc (lambda (x) (insert (cfw:render-left time-width x) EOL))
    ;;       )
    (loop with breaked-day-columns =
          (loop for day-rows in day-columns
                for (date ants . lines) = day-rows
                collect
                (cons date (calfw-blocks-render-event-blocks
                            lines cell-width (1- cell-height))))
          with time-columns = (calfw-blocks-time-column time-width cell-height)
          for i from 1 below cell-height do
          (insert (cfw:render-left time-width (nth (1- i) time-columns)))
          (loop for day-rows in breaked-day-columns
                for date = (car day-rows)
                for row = (nth i day-rows)
                do
                (insert
                 VL (cfw:tp
                     (cfw:render-separator
                      (cfw:render-left cell-width (and row (format "%s" row))))
                     'cfw:date date)))
          (insert VL EOL))
    (insert cline)))


(defun calfw--concat-preserve-property (propstr1 str2)
  (apply 'propertize (concat propstr1 str2)
         (cfw:extract-text-props propstr1)))

(defun calfw--time-pair-to-decimal (p)
  (+ (car p) (/ (cadr p) 60.0)))
(defun calfw-blocks-render-event-blocks (lines cell-width cell-height)
  ""
  ;; filter lines based on text property calfw-blocks-interval
  ;; for those with start time, pad with spacing until it reaches the
  ;; right height. need variable for number of lines per hour.
  ;; then split the string
  (mapcan (lambda (line)
            (let ((interval (get-text-property 0 'calfw-blocks-interval line)))
              (if (and interval (car interval))
                  (let* (
                         (start (calfw--time-pair-to-decimal (car interval)))
                         (end (calfw--time-pair-to-decimal (cdr interval)))
                         (block-height (round (max 1 (* (- end start) 4))))
                         )
                    (cfw:render-line-breaker-simple (propertize (calfw--concat-preserve-property line
                                                                                                 "\n\n   \n")
                                                                'face  'isearch)
                                                    cell-width
                                                    20)
              )
              (list line))))
          lines))
  ;; (and lines
  ;;      (let ((num (/ cell-height (length lines))))
  ;;        (cond
  ;;         ((> 2 num) lines)
  ;;         (t
  ;;          (loop with total-rows = nil
  ;;                for line in lines
  ;;                for rows = (funcall cfw:render-line-breaker line cell-width num)
  ;;                do
  ;;                (when total-rows
  ;;                  (cfw:render-add-item-separator-sign total-rows))
  ;;                (setq total-rows (append total-rows rows))
  ;;                finally return total-rows)))))

;;; testing

;; (defun cfw:render-columns (day-columns param)
;;   "[internal] This function concatenates each rows on the days into a string of a physical line.
;; DAY-COLUMNS is a list of columns. A column is a list of following form: (DATE (DAY-TITLE . ANNOTATION-TITLE) STRING STRING...)."
;;   (setq test123 (nth 3 day-columns))
;;   (let ((cell-width  (cfw:k 'cell-width  param))
;;         (cell-height (cfw:k 'cell-height param))
;;         (EOL (cfw:k 'eol param)) (VL (cfw:k 'vl param))
;;         (hline (cfw:k 'hline param)) (cline (cfw:k 'cline param)))
;;     ;; day title
;;     ;; (print day-columns)
;;     (loop for day-rows in day-columns
;;           for date = (car day-rows)
;;           for (tday . ant) = (cadr day-rows)
;;           do
;;           (insert
;;            VL (if date
;;                   (cfw:tp
;;                    (cfw:render-default-content-face
;;                     (cfw:render-add-right cell-width tday ant)
;;                     'cfw:face-day-title)
;;                    'cfw:date date)
;;                 (cfw:render-left cell-width ""))))
;;     (insert VL EOL)
;;     ;; day contents
;;     (loop with breaked-day-columns =
;;           (loop for day-rows in day-columns
;;                 for (date ants . lines) = day-rows
;;                 collect
;;                 (cons date (cfw:render-break-lines
;;                             lines cell-width (1- cell-height))))
;;           ;; do
;;           ;; (setq test123 (nth 3 breaked-day-columns))
;;           for i from 1 below cell-height do
;;           (loop for day-rows in breaked-day-columns
;;                 for date = (car day-rows)
;;                 for row = (nth i day-rows)
;;                 do
;;                 (insert
;;                  VL (propertize (cfw:tp
;;                                  (cfw:render-separator
;;                                   (cfw:render-left cell-width (and row (format "%s" row))))
;;                                  'cfw:date date)
;;                                 'face 'italic)))
;;           (insert VL EOL))
;;     (insert cline)))


;; (setq cfw:render-line-breaker 'cfw:render-line-breaker-simple)

;; (defun cfw:render-calendar-cells-days (model param title-func &optional
;;                                              days content-fun do-weeks)
;;   "[internal] Insert calendar cells for the linear views."
;;   ;; (print (cfw: model))
;;   (calfw-blocks-render-columns
;;    (loop with cell-width      = (cfw:k 'cell-width param)
;;          with days            = (or days (cfw:k 'days model))
;;          with content-fun     = (or content-fun
;;                                     'cfw:render-event-days-overview-content)
;;          with holidays        = (cfw:k 'holidays model)
;;          with annotations     = (cfw:k 'annotations model)
;;          with headers         = (cfw:k 'headers  model)
;;          with raw-periods-all = (cfw:render-periods-stacks model)
;;          with sorter          = (cfw:model-get-sorter model)

;;          for date in days ; days columns loop
;;          for count from 0 below (length days)
;;          for hday         = (car (cfw:contents-get date holidays))
;;          for week-day     = (nth count headers)
;;          for ant          = (cfw:rt (cfw:contents-get date annotations)
;;                                     'cfw:face-annotation)
;;          for raw-periods  = (cfw:contents-get date raw-periods-all)
;;          do (setq test123 raw-periods)
;;          for raw-contents = (cfw:render-sort-contents
;;                              (funcall content-fun
;;                                       (cfw:model-get-contents-by-date date model))
;;                              sorter)
;;          for prs-contents = (cfw:render-rows-prop
;;                              (append (if do-weeks
;;                                          (cfw:render-periods
;;                                           date week-day raw-periods cell-width)
;;                                        (cfw:render-periods-days
;;                                         date raw-periods cell-width))
;;                                      (mapcar 'cfw:render-default-content-face
;;                                              raw-contents)))
;;          for num-label = (if prs-contents
;;                              (format "(%s)"
;;                                      (+ (length raw-contents)
;;                                         (length raw-periods))) "")
;;          for tday = (concat
;;                      " " ; margin
;;                      (funcall title-func date week-day hday)
;;                      (if num-label (concat " " num-label))
;;                      (if hday (concat " " (cfw:rt (substring hday 0)
;;                                                   'cfw:face-holiday))))
;;          ;; do (if prs-contents (print prs-contents))
;;          collect
;;          (cons date (cons (cons tday ant) prs-contents)))
;;    param))


;; (defun cfw:render-calendar-cells-weeks (model param title-func)
;;   "[internal] Insert calendar cells for week based views."
;;   (loop for week in (cfw:k 'weeks model) do
;;         (cfw:render-calendar-cells-days model param title-func week
;;                                         'cfw:render-event-overview-content
;;                                         t)))
;; (defun cfw:render-map-event-content (lst event-fun)
;;   "[internal] `lst' is a list of contents and `cfw:event's. Map over `lst',
;; where `event-fun' is applied if the element is a `cfw:event'."
;;   (if lst (print lst))
;;   (mapcar #'(lambda (evt)
;;               (if (cfw:event-p evt)
;;                   (funcall event-fun evt)
;;                 evt))
;;           lst))

;; (defun cfw:render-event-days-overview-content (lst)
;;   "[internal] Apply `cfw:event-days-overview' on `cfw:event's in `lst'."
;;   ;; (if lst (print lst))
;;   (cfw:render-map-event-content lst 'cfw:event-days-overview))

;; (defun cfw:render-event-overview-content (lst)
;;   "[internal] Apply `cfw:event-overview' on `cfw:event's in `lst'."
;;   ;; (if lst (print lst))
;;   (cfw:render-map-event-content lst 'cfw:event-overview)
;;   )

;; (defun cfw:render-event-days-overview-content2 (lst)
;;   "[internal] Apply `cfw:event-overview' on `cfw:event's in `lst'."
;;   ;; (if lst (print lst))
;;   ;; (print "hi")
;;   (cfw:render-map-event-content lst 'cfw:event-overview2)
;;   )


;; (defun cfw:event-overview2 (event)
;;   "Function that extracts the overview string from a`cfw:event'."
;;   (cfw:event-format event "%s %e %t")
;;   )

;; (defun cfw:event-period-overview2 (event)
;;   "Function that extracts the period overview string from a`cfw:event'."
;;   (cfw:event-format event "%t"))

;; (defun cfw:event-format (event format-string)
;;   "Format the `cfw:event' `event' according to `format-string'.
;; The following values are possible:
;; %t = title
;; %S = start date
;; %s = start time
;; %E = end date
;; %e = end time
;; %l = Location
;; %d = Description"
;;   (cfw:tp
;;    (format-spec
;;     format-string
;;     (mapcar #'(lambda (field)
;;                 `(,(car field) . ,(cfw:event-format-field
;;                                    event (cadr field) (caddr field))))
;;             '((?t title       cfw:event-format-field-string)
;;               (?S start-date  cfw:event-format-field-date)
;;               (?s start-time  cfw:event-format-field-time)
;;               (?E end-date    cfw:event-format-field-date)
;;               (?e end-time    cfw:event-format-field-time)
;;               (?l location    cfw:event-format-field-string)
;;               (?d description cfw:event-format-field-string))))
;;    'cfw:source (cfw:event-source event)))


(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-file-source "Test" "~/code/emacs/calfw-blocks/calfw-blocks-test.org" "Green") ; orgmode source
    )
   :view 'block-week)
  )


;; (defun cfw:render-periods-place (periods-each-days row period)
;;   "[internal] Assign PERIOD content to the ROW-th row on the days of the period,
;; and append the result to periods-each-days."
;;   (loop for d in (cfw:enumerate-days (car period) (cadr period))
;;         for periods-stack = (cfw:contents-get-internal d periods-each-days)
;;         if periods-stack
;;         do (setcdr periods-stack (append (cdr periods-stack)
;;                                          (list (list row period))))
;;         else
;;         do (push (cons d (list (list row period))) periods-each-days))
;;   periods-each-days)

;; (defun cfw:render-periods-stacks (model)
;;   "[internal] Arrange the `periods' records of the model and
;; create period-stacks on the each days.
;; period-stack -> ((row-num . period) ... )"
;;   (let* (periods-each-days)
;;     (loop for (begin end event) in (cfw:k 'periods model) ;;begin, end are dates e.g. (7 6 2022)
;;           for content = (if (cfw:event-p event)
;;                             (cfw:event-period-overview2 event)
;;                           event)
;;           for period = (list begin end content
;;                              (cfw:extract-text-props content 'face))
;;           for row = (cfw:render-periods-get-min periods-each-days begin end)
;;           do
;;           (setq periods-each-days (cfw:render-periods-place
;;                                    periods-each-days row period)))
;;     periods-each-days))


;; cfw:org-create-file-source
(provide 'calfw-blocks)
;;; calfw-blocks.el ends here
