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
  :type 'list)

(defcustom calfw-blocks-lines-per-hour 4
  "Number of lines per hour in a block."
  :group 'calfw-blocks
  :type 'number)

(defcustom calfw-blocks-default-event-length 1
  "Length in hours of events with same start and end time.
Also used for events with a start time and no end time."
  :group 'calfw-blocks
  :type 'number)


(defcustom calfw-blocks-show-time-grid t
  "Whether to show horizontal lines for each hour."
  :group 'calfw-blocks
  :type 'boolean)

(defcustom calfw-blocks-time-grid-lines-on-top t
  "Whether time grid lines should cut through vertical lines."
  :group 'calfw-blocks
  :type 'boolean)

(defcustom calfw-blocks-colors-list
  '("#ef7969"
    "#49c029"
    "#7090ff"
    "#e07fff"
    "#70d3f0"
    "#ffcf00")
  "Colors to use for blocks. The default colors are
Modus Vivendi's colors for graphs."
  :group 'calfw-blocks
  :type 'list)



(defun calfw-blocks-create-faces ()
  (let ((faces ())
        newface)
    (dolist (color calfw-blocks-colors-list)
      (setq newface (make-face
                (intern (concat "calfw-blocks-" color))))
            (set-face-background newface color)
            (set-face-foreground newface "black")
            (push newface faces))
    (reverse faces)))

(defvar calfw-blocks-faces-list
  (calfw-blocks-create-faces)
  "")


(defun calfw-blocks-view-week ()
    "Render weekly block calendar view.")

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
   ((eq 'block-day       view)  'calfw-blocks-view-block-day)
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
       ;; (cell-height (max 2 win-height))
       (cell-height (* calfw-blocks-lines-per-hour 24))
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
(defun calfw-blocks-view-block-day (component)
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
    ;; remove overlays for today's region
    ;; (let ((new-ols '()))
    ;;   (dolist (o (cfw:dest-today-ol dest))
    ;;     (if (eq (overlay-get o 'face) 'cfw:face-today-title)
    ;;         (push o new-ols)))
    ;;   )

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
    ;; remove overlays for today's region
    ;; (let ((new-ols '()))
    ;;   (dolist (o (cfw:dest-today-ol dest))
    ;;     (if (eq (overlay-get o 'face) 'cfw:face-today-title)
    ;;         (push o new-ols)))
    ;;   )

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
    (advice-add 'cfw:dest-ol-today-set :override 'calfw-blocks-dest-ol-today-set)
    (cfw:cp-set-view (cfw:cp-get-component) 'block-week
    (advice-remove 'cfw:dest-ol-today-set 'calfw-blocks-dest-ol-today-set))))

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
                                       (calfw-blocks-render-periods-days
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


(defun calfw-blocks-render-periods-days (date periods-stack cell-width)
  "[internal] Insert period texts.
Modified to not truncate events. TODO"
  (when periods-stack
    (let ((stack (sort (copy-sequence periods-stack)
                       (lambda (a b) (< (car a) (car b))))))
      (loop for (row (begin end content)) in stack
            for beginp = (equal date begin)
            for endp = (equal date end)
            for width = (- cell-width 2)
            for title = (cfw:render-truncate
                         (concat
                          (cfw:strtime begin) " - "
                          (cfw:strtime end) " : "
                          content) width t)
            collect
            (if content
                (cfw:rt
                 (concat
                  (if beginp "(" " ")
                  (cfw:render-left width title ?-)
                  (if endp ")" " "))
                 (cfw:render-get-face-period content 'cfw:face-periods))
              "")))))


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
        (times-lst (mapcar (lambda (x) (list (mod (+ x start-hour) 24) start-minute))
                           (number-sequence 0 (1- num-hours)))))
    (mapcan (lambda (x) (append (list (calfw-blocks-format-time x))
                       (mapcar (lambda (x) (make-string time-width ? ))
                               (number-sequence 0 (- calfw-blocks-lines-per-hour 2)))))
     times-lst)))

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
        (cline (concat time-hline (cfw:k 'cline param)))
        (earliest-date (caar day-columns)))
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
    (loop with breaked-all-day-columns =
        (loop for day-rows in day-columns
                        for (date ants . lines) = day-rows
                        collect
                        (cons date (calfw-blocks-render-all-day-events
                                lines cell-width (1- cell-height))))
        with breaked-all-day-columns-padded =
        (calfw-blocks-pad-whitespace breaked-all-day-columns)
        with all-day-columns-height = (seq-max (mapcar 'length breaked-all-day-columns))
        for i from 1 below all-day-columns-height do
          (insert (cfw:render-left time-width ""))
        (loop for day-rows in breaked-all-day-columns-padded
                for date = (car day-rows)
                for row = (nth i day-rows)
                do
                (insert
                 (if (and calfw-blocks-show-time-grid
                          calfw-blocks-time-grid-lines-on-top
                          (= (mod (1- i) calfw-blocks-lines-per-hour) 0)
                          (string= row (make-string cell-width ?-))
                          (not (eq date earliest-date)))
                     ?-
                   VL) ;;broken
                 (cfw:tp
                     (cfw:render-separator
                      (cfw:render-left cell-width (and row (format "%s" row))))
                     'cfw:date date)))
          (insert VL EOL))

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
                 (if (and calfw-blocks-show-time-grid
                          calfw-blocks-time-grid-lines-on-top
                          (= (mod (1- i) calfw-blocks-lines-per-hour) 0)
                          (string= row (make-string cell-width ?-))
                          (not (eq date earliest-date)))
                     ?-
                   VL) ;;broken
                 (cfw:tp
                     (cfw:render-separator
                      (cfw:render-left cell-width (and row (format "%s" row))))
                     'cfw:date date)))
          (insert VL EOL))
    (insert cline)))

(defun calfw-blocks-pad-whitespace (columns)
  (let ((max-len (apply 'max (mapcar 'length columns)))
        (new-columns '()))
    (dolist (c columns (nreverse new-columns))
      (push (append c (make-list (- max-len (length c)) "")) new-columns)
    )))

(defun calfw-blocks--concat-preserve-property (propstr1 str2)
  (apply 'propertize (concat propstr1 str2)
         (cfw:extract-text-props propstr1)))


(defun calfw-blocks--interval-member? (elem a)
  "Return t iff ELEM is within interval A."
  (and (< elem (cadr a)) (>= elem (car a))))

(defun calfw-blocks--interval-intersect? (a b)
  "Return t iff intervals A and B intersect.
Return nil otherwise. Ain interval [a1, a2) is represented as a
list '(a1 a2)."
  (or (calfw-blocks--interval-member? (car a) b)
      (calfw-blocks--interval-member? (car b) a)))

(defun calfw-blocks--interval-intersection (a b)
  "Compute intersection of intervals A and B.
An interval [a1, a2) is represented as a list '(a1 a2).
Return nil if intersection is empty."
  (if (calfw-blocks--interval-intersect? a b)
      (let ((start (max (car a) (car b)))
            (end (min (cadr a) (cadr b))))
        (list start end))))

(defun calfw-blocks--interval-subtract-many (a lst)
  "Return interval(s) resulting from removing intervals in LST from A.
Assume that all intervals in lst are disjoint and subsets of A."
  (let* ((lst (seq-sort (lambda (x y) (< (car x) (car y))) lst))
        (endpoints (append (list (car a))
                          (cfw:flatten lst)
                          (list (cadr a))
                          ))
        (intervals nil))
    (while endpoints
      (let ((start (pop endpoints))
            (end (pop endpoints)))
        (if (< start end)
            (push (list start end) intervals))))
    (reverse intervals)))

(defun calfw-blocks--interval-distribute (lst n)
  "Return N intervals of approx equal size, whose union is the union of LST."
  (let* ((lst (seq-sort (lambda (x y) (> (- (cadr x) (car x))
                                         (- (cadr y) (car y)))) lst))
         (lst-lengths (mapcar (lambda (x) (- (cadr x) (car x))) lst))
         (avg-len (/ (seq-reduce '+ lst-lengths 0) (float n)))
         (intervals nil))
    (while (and lst (< (length intervals) n))
      (let* ((l (pop lst))
             (l-len (float (- (cadr l) (car l))))
             (num-intervals
              (if (not lst)
                  (- n (length intervals))
                  (min (round (/ l-len avg-len))
                                 (- n (length intervals)))))
             (l-inner-len (/ l-len num-intervals)))
        ;; (print num-intervals)
        (dolist (i (number-sequence 0 (1- num-intervals)))
          (let (
                (start
                 (+ (car l)
                    (if (= i 0)
                        (floor (* i l-inner-len))
                      (round (* i l-inner-len)))))
                (end
                 (+ (car l)
                    (if (= i (1- num-intervals))
                        (ceiling (* (1+ i) l-inner-len))
                      (round (* (1+ i) l-inner-len)))))
                )
            (push (list start end) intervals)
            ))
          )
      )
(seq-sort (lambda (a b) (< (car a) (car b))) intervals)
))

(defun calfw-blocks--get-intersection-groups (lines-lst)
  (let ((groups '())
        (prev-line-indices '()))
    (dotimes (i (length lines-lst))
      (let* ((i-interval (nth 1 (nth i lines-lst)))
            (i-intersects-indices '())
            (in-prev-group nil)
            (i-group (cons i-interval (list i))))
        ;; (print i-interval)
        (dolist (g groups)
          (when (and (calfw-blocks--interval-intersect? (car g) i-interval)
                     (not (member i (cdr g))))
            (dolist (elem (cdr g)) (push elem i-intersects-indices))
            (setcdr g (reverse (cons i (cdr g))))))
        (dolist (j prev-line-indices)
          (let ((j-interval (nth 1 (nth j lines-lst))))
            (when (and (not (member j i-intersects-indices))
                (calfw-blocks--interval-intersect? i-interval j-interval))
                    (setcdr i-group (reverse (cons j (cdr i-group))))
                    (setcar i-group (calfw-blocks--interval-intersection (car i-group) j-interval))
                    )))
        (if (not (and i-intersects-indices
                      (= 1 (length (cdr i-group)))))
            (push i-group groups))
        )
      (push i prev-line-indices)
      )
    (seq-sort (lambda (a b)
                (let ((a-size (length (cdr a)))
                      (b-size (length (cdr b))))
                  (if (= a-size b-size)
                      (< (caar a) (caar b))
                    (> a-size b-size))))
                 groups)
    )
  )

;; (defun calfw-blocks--get-block-positions (lines cell-width)
;;   (let* ((lines-lst (mapcar (lambda (x) (list x (calfw-blocks--get-block-vertical-position x))) lines))
;;          (lines-pos (make-list (length lines-lst) nil))
;;          (groups (calfw-blocks--get-intersection-groups lines-lst))
;;          )
;;     (if lines-lst (print (mapcar (lambda (x) (length x)) lines-lst)))
;;     (dolist (g groups)
;;       (let* (
;;              (taken-intervals (seq-filter (lambda (x) (and (nth x lines-pos)
;;                                                            (calfw-blocks--interval-intersect? (nth 1 (nth x lines-lst)) (car g))))
;;                                           (number-sequence 0 (1- (length lines-lst)))))
;;              (lines-left-in-group (- (length (cdr g)) (length taken-intervals)))
;;              (remaining-intervals (calfw-blocks--interval-subtract-many `(0 ,cell-width) taken-intervals))
;;              (distributed-intervals (calfw-blocks--interval-distribute remaining-intervals lines-left-in-group))
;;              )
;;         (dolist (x (cdr g))
;;           (when (not (nth x lines-pos))
;;             (let ((full-line  (pop distributed-intervals))
;;                   (curr-line (nthcdr x lines-pos)))
;;               (setcar (nthcdr x lines-pos) full-line)
;;               ))
;;             )
;;         ))
;;       ;; (if lines-lst (print lines-lst))
;;       nil
;;     ))

(defun calfw-blocks--get-block-positions (lines cell-width)
  (let* ((lines-lst (mapcar (lambda (x) (list x (calfw-blocks--get-block-vertical-position x))) lines))
         (groups (calfw-blocks--get-intersection-groups lines-lst))
         (new-lines-lst nil)
         (added-indices nil)
         )
    ;; (if lines-lst (print (mapcar (lambda (x) (length x)) lines-lst)))
    (dolist (g groups)
      (let* (
             (taken-intervals (seq-map (lambda (x) (nth 2 (cdr x)))
                                       (seq-filter (lambda (x)  (and (member (car x) added-indices)
                                                            (calfw-blocks--interval-intersect? (nth 1 (cdr x)) (car g))
                                                            )
                                                     )
                                          ;; (number-sequence 0 (1- (length lines-lst)))
                                          (seq-map-indexed (lambda (elt idx) (cons idx elt)) new-lines-lst)
                                          )))
             (lines-left-in-group (- (length (cdr g)) (length taken-intervals)))
             (remaining-intervals (calfw-blocks--interval-subtract-many `(0 ,cell-width) taken-intervals))
             ;; if there is not enough space, reduce lines-left-in-group; and manually add a +k block to new-lines-lst
             ;; the total remaining interval length is (reduce '+ (mapcar (lambda (x) (- (cadr x) (car x))) remaining-intervals))
             ;; do a check to see if dividng this by lines-left-in-group, is at least the minimum viable block width
             ;; if not, you need to remove n, such that lines-left-in-group - n + 1 is ok, where the last block is going to be width 1 or 2
             (distributed-intervals (reverse (calfw-blocks--interval-distribute remaining-intervals lines-left-in-group)))
             )
        (print taken-intervals)
        ;; (print distributed-intervals)
        (dolist (x (cdr g))
          (when (not (member x added-indices))
            ;; (print (length (car (nthcdr x lines-lst))))
            (let* ((new-line (append (nth x lines-lst) (list (pop distributed-intervals)))))
              (push new-line new-lines-lst)
              ;; (setq added-indices '(1 2))
              ;; (print added-indices)
              (push x added-indices)
              )
            ))
        ))
    new-lines-lst
    ;; (print new-lines-lst)
      ;; (if lines-lst (print lines-lst))
      ;; (print new-lines-lst)
      ;; (seq-sort (lambda (a b) ))
    ))


(defun calfw-blocks-round-start-time (time)
  (floor time))
(defun calfw-blocks-round-end-time (time)
  (ceiling time))

(defun calfw-blocks-hours-per-line ()
  (/ 1.0 calfw-blocks-lines-per-hour))

(defun calfw-blocks--get-block-vertical-position (p)
  "[inclusive, exclusive)"
  (let* ((float-interval (calfw-blocks--get-float-time-interval p))
        (start-time (calfw-blocks--time-pair-to-float calfw-blocks-earliest-visible-time))
        (minutes-per-line (/ 60 calfw-blocks-lines-per-hour))
        (interval-start (car float-interval))
        (interval-end (if (= interval-start (cadr float-interval)) (+ calfw-blocks-default-event-length interval-start) (cadr float-interval))))
  (list (calfw-blocks-round-start-time (* calfw-blocks-lines-per-hour (- interval-start start-time)))
  (calfw-blocks-round-end-time (* calfw-blocks-lines-per-hour (- interval-end start-time))))))


(defun calfw-blocks--time-pair-to-float (p)
  (+ (car p) (/ (cadr p) 60.0)))

(defun calfw-blocks--get-float-time-interval (line)
  (let* ((interval (get-text-property 0 'calfw-blocks-interval line))
         (start (calfw-blocks--time-pair-to-float (car interval)))
         (end (calfw-blocks--time-pair-to-float (cdr interval))))
    (list start end)))

(defun calfw-blocks-generalized-substring (s start end)
  (cond ((<= end (length s)) (substring s start end))
        ((< start (length s)) (concat (substring s start (length s))
                                      (make-string (- (- end start) (- (length s) start)) ? )))
        (t (make-string (- end start) ? ))))

(defun calfw-blocks-split-single-block (block cell-width face)
  ;; does substring preserve properties?
  (let* ((block-string (car block))
        (block-vertical-pos (cadr block))
        (block-horizontal-pos (caddr block))
        (block-width (- (cadr block-horizontal-pos) (car block-horizontal-pos)))
        (block-height (- (cadr block-vertical-pos) (car block-vertical-pos)))
        (end-of-cell (= (cadr block-horizontal-pos) cell-width))
        (is-beginning-of-cell (= (car block-horizontal-pos) 0))
        (block-width-adjusted (if is-beginning-of-cell block-width (1- block-width)))
        (rendered-block '()))
    (dolist (i (number-sequence 0 (- block-height 1)))
      (push (list (+ (car block-vertical-pos) i)
                  (propertize (concat
                               (when (not is-beginning-of-cell) "|")
                               (calfw-blocks-generalized-substring block-string (* i block-width-adjusted) (* (1+ i) block-width-adjusted))
                               ;; (when (not end-of-cell) "|")
                               )
                              'face
                              face
                              ))
            rendered-block)
      )
;; (push (list (+ (car block-vertical-pos) (- block-height 1))
;;                   (propertize (concat
;;                                (when (not is-beginning-of-cell) "|")
;;                                (make-string block-width-adjusted ?_)
;;                                ;; (when (not end-of-cell) "|")
;;                                )
;;                               'face
;;                               face
;;                               ))
;;             rendered-block)
    ;; (push (list (1- (cdr block-vertical-pos))
    ;;             (propertize (concat (make-string block-width-adjusted ?-)
    ;;                                 (when (not end-of-cell) "+")) 'face face)) rendered-block)
    (reverse rendered-block)))
(setq block-positions `(("test1" (1 . 4) (0 . 20))
                            ("test2" (5 . 7) (0 . 20))
                            ("test3" (9 . 10) (0 . 20))
                            ))

;; (calfw-blocks-split-single-block (car block-positions) 20)

(setq calfw-blocks-block-faces
      '(ffap isearch))

(defun calfw-blocks-zip-with-faces (blocks)
  (let ((blocks-with-faces '()))
    (dotimes (i (length blocks))
      (push (cons (nth i blocks)
                  (nth (mod i (length calfw-blocks-faces-list)) calfw-blocks-faces-list))
            blocks-with-faces))
    (reverse blocks-with-faces)))

(defun calfw-blocks-render-all-day-events (lines cell-width cell-height)
  (let ((all-day-lines (seq-filter (lambda (line) (not (car (get-text-property 0 'calfw-blocks-interval
                                                                       line))))
                                     lines))
        (cfw:render-line-breaker 'cfw:render-line-breaker-simple))
        (cfw:render-break-lines all-day-lines cell-width cell-height)))

(defun calfw-blocks-render-event-blocks (lines cell-width cell-height)
  ""
  (let* ((interval-lines (seq-filter (lambda (line) (car (get-text-property 0 'calfw-blocks-interval
                                                                       line)))
                                     lines))
         (all-day-lines (seq-filter (lambda (line) (not (car (get-text-property 0 'calfw-blocks-interval
                                                                       line))))
                                     lines))
         (block-positions (calfw-blocks--get-block-positions interval-lines cell-width))
         ;; (block-positions `(("test1" (1 . 4) (0 . ,cell-width))
         ;;                    ("test2" (5 . 7) (0 . ,cell-width))
         ;;                    ("test3" (15 . 20) (0 . 4))
         ;;                    ("test3" (15 . 20) (4 . 10))
         ;;                    ))
         (split-blocks (seq-sort (lambda (a b) (< (car a) (car b)))
                                 (mapcan (lambda (bf) (calfw-blocks-split-single-block (car bf) cell-width (cdr bf)))
                                  (calfw-blocks-zip-with-faces block-positions))))
         (rendered-lines '()))
    (dolist (i (number-sequence 0 (1- cell-height)))
    ;; sort rendered blocks by car, iterate through lines starting at 0 going to cell height
    ;; whenever there is an empty line, add spaces and new line
    ;; concatenate anythings on the same line
      (if (or (not split-blocks) (< i (caar split-blocks)))
          (if (and calfw-blocks-show-time-grid
                   (= (mod i calfw-blocks-lines-per-hour) 0))
          (push (make-string cell-width ?-) rendered-lines)
          (push (make-string cell-width ? ) rendered-lines))
        (let ((current-line '()))
          (while (and split-blocks (= i (caar split-blocks)))
            (push (cadr (pop split-blocks)) current-line)
            )
          (push (string-join (reverse current-line) "") rendered-lines)
          )
        )
      )
    ;; (append (reverse rendered-lines) (cfw:render-break-lines all-day-lines cell-width cell-height)
    ;;  )
    (reverse rendered-lines)
    )
  )
  ;; filter lines based on text property calfw-blocks-interval
  ;; for those with start time, pad with spacing until it reaches the
  ;; right height. need variable for number of lines per hour.
  ;; then split the string

  ;; (mapcan (lambda (line)
  ;;           (let ((interval (get-text-property 0 'calfw-blocks-interval line)))
  ;;             (if (and interval (car interval))
  ;;                 (let* (
  ;;                        (start (calfw--time-pair-to-decimal (car interval)))
  ;;                        (end (calfw--time-pair-to-decimal (cdr interval)))
  ;;                        (block-height (round (max 1 (* (- end start) 4))))
  ;;                        )
  ;;                   (cfw:render-line-breaker-simple (propertize (calfw--concat-preserve-property line
  ;;                                                                                                "\n\n   \n")
  ;;                                                               'face  'isearch)
  ;;                                                   cell-width
  ;;                                                   20)
  ;;             )
  ;;             (list line))))
  ;;         lines))
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

(defun my/org-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-file-source "Todos" "~/pensieve-beta/tasks/todos.org" "Green") ; orgmode source
    )
   :view 'block-week)
  )

(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-file-source "Test" "~/code/emacs/calfw-blocks/calfw-blocks-test.org" "Green") ; orgmode source
    )
   :view 'block-week)
  )
(require 'calfw)
(defmacro cfw:dest-with-region (dest &rest body)
    (let (($dest (gensym)))
      `(let ((,$dest ,dest))
         (with-current-buffer (cfw:dest-buffer ,$dest)
           (save-restriction
             (narrow-to-region
              (cfw:dest-point-min ,$dest) (cfw:dest-point-max ,$dest))
             ,@body)))))
(defun calfw-blocks-dest-ol-today-set (dest)
  "[internal] Put a highlight face on today."
  (let ((ols))
               (cfw:dest-with-region dest
                                     (cfw:find-all-by-date
                                      dest (calendar-current-date)
                                      (lambda (begin end)
                                        (let ((overlay (make-overlay begin end)))
                                          (if (eq 'cfw:face-day-title
                                                  (get-text-property begin 'face))
                                              (overlay-put overlay 'face
                                                       'cfw:face-today-title))
                                          (push overlay ols)))))
               (setf (cfw:dest-today-ol dest) ols)))

(defun cfw:cp-update (component)
  "[internal] Clear and re-draw the component content."
  (let* ((buf (cfw:cp-get-buffer component))
         (dest (cfw:component-dest component)))
    (with-current-buffer buf
      (cfw:dest-before-update dest)
      (cfw:dest-ol-selection-clear dest)
      (cfw:dest-ol-today-clear dest)
      (let ((buffer-read-only nil))
        (cfw:dest-with-region dest
                              (cfw:dest-clear dest)
                              (funcall (cfw:cp-dispatch-view-impl
                                        (cfw:component-view component))
                                       component)))
      (if (eq (cfw:component-view component) 'block-week)
          (calfw-blocks-dest-ol-today-set dest)
      (cfw:dest-ol-today-set dest))
      (cfw:cp-set-selected-date
       component (cfw:component-selected component))
      (cfw:dest-after-update dest)
      (cfw:cp-fire-update-hooks component))))

;; (advice-remove 'cfw:dest-ol-today-set 'calfw-blocks-dest-ol-today-set)

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
