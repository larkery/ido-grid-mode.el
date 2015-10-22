;;; ido-grid-mode.el --- Display ido-prospects in the minibuffer in a grid. -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2015  Tom Hinton

;; Author: Tom Hinton
;; Maintainer: Tom Hinton <t@larkery.com>
;; Version: 1.0.0
;; Keywords: convenience
;; URL: https://github.com/larkery/ido-grid-mode.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Makes ido-mode display prospects in a grid. The mechanism is based
;; on ido-vertical-mode, but it is sufficiently different that I
;; reimplemented it. The purpose is to look a bit like zsh style
;; completion lists.  Most of the behaviour can be customized, in the
;; ido-grid-mode group.  relevant variables are
;; `ido-grid-mode-min-rows', `ido-grid-mode-max-rows',
;; `ido-grid-mode-keys', `ido-grid-mode-start-collapsed'.  If you want
;; ido-grid-mode to sometimes be more horizontal or more vertical, you
;; can let `ido-grid-mode-max-rows' or `ido-grid-mode-max-columns'
;; around the call you are interested in (or in advice around an
;; existing command).

;;; Code:

(eval-when-compile (require 'cl))

;;; The following four variables and the first three comments are lifted
;;; directly from `ido.el'; they are defined here to avoid compile-log
;;; warnings. See `ido.el' for more information.

;; Non-nil if we should add [confirm] to prompt
(defvar ido-show-confirm-message)

;; Remember if current directory is non-readable (so we cannot do completion).
(defvar ido-directory-nonreadable)

;; Remember if current directory is 'huge' (so we don't want to do completion).
(defvar ido-directory-too-big)

(defvar ido-report-no-match t
  "Report [No Match] when no completions matches ido-text.")

(defvar ido-matches nil
  "List of files currently matching `ido-text'.")

(defvar ido-incomplete-regexp nil
  "Non-nil if an incomplete regexp is entered.")

;; this is defined dynamically by ido
(eval-when-compile
  (defvar ido-cur-list))

;; custom settings

(defgroup ido-grid-mode nil
  "Displays ido prospects in a grid in the minibuffer."
  :group 'ido)

(defcustom ido-grid-mode-max-columns nil
  "The maximum number of columns - nil means no maximum."
  :type '(choice 'integer (const :tag "Unlimited" nil))
  :group 'ido-grid-mode)

(defcustom ido-grid-mode-max-rows 5
  "The maximum number of rows."
  :type 'integer
  :group 'ido-grid-mode)

(defcustom ido-grid-mode-min-rows 5
  "The minimum number of rows."
  :type 'integer
  :group 'ido-grid-mode)

(defcustom ido-grid-mode-order 'columns
  "The order to put things in the grid."
  :type '(choice (const :tag "Row-wise (row 1, then row 2, ...)" rows)
                (const :tag "Column-wise (column 1, then column 2, ...)" columns))
  :group 'ido-grid-mode)

(defcustom ido-grid-mode-jank-rows 1000
  "Only this many rows will be considered when packing the grid.
If this is a low number, the column widths will change more when scrolling."
  :type 'integer
  :group 'ido-grid-mode)

(defcustom ido-grid-mode-padding "    "
  "The padding text to put between columns - this can contain characters like | if you like."
  :type 'string
  :group 'ido-grid-mode)

(defcustom ido-grid-mode-first-line '(" [" ido-grid-mode-count "]")
  "How to generate the top line of input.
This can be a list of symbols; function symbols will be
evaluated.  The function `ido-grid-mode-count' displays a count
of visible and matching items.  `ido-grid-mode-long-count'
displays more detail about this."
  :type '(repeat (choice function symbol string))
  :group 'ido-grid-mode)

(defcustom ido-grid-mode-exact-match-prefix ">> "
  "A string to put before an exact match."
  :type 'string
  :group 'ido-grid-mode)

(defcustom ido-grid-mode-prefix "-> "
  "A string to put at the start of the first row when there isn't an exact match."
  :type 'string
  :group 'ido-grid-mode)

(defface ido-grid-mode-match
  '((t (:underline t)))
  "The face used to mark up matching groups when showing a regular expression."
  :group 'ido-grid-mode)

(defface ido-grid-mode-prefix
  '((t (:inherit shadow)))
  "The face used to display the common match prefix."
  :group 'ido-grid-mode)

(defcustom ido-grid-mode-always-show-min-rows t
  "Whether to expand the minibuffer to be `ido-grid-mode-min-rows' under all circumstances (like when there is a single match, or an error in the input)."
  :group 'ido-grid-mode
  :type 'boolean)

(defcustom ido-grid-mode-keys '(tab backtab up down left right C-n C-p)
  "Which keys to reconfigure in the minibuffer.

Tab and backtab will move to the next/prev thing, arrow keys will
move around in the grid, and C-n, C-p will scroll the grid in
pages."
  :group 'ido-grid-mode
  :type '(set (const tab) (const backtab) (const up) (const down) (const left) (const right) (const C-n) (const C-p)))

(defcustom ido-grid-mode-advise-perm '(ido-exit-minibuffer)
  "Functions which will want to see the right thing at the head of the ido list."
  :type 'hook
  :options '(ido-exit-minibuffer)
  :group 'ido-grid-mode)

(defcustom ido-grid-mode-advise-temp '(ido-kill-buffer-at-head ido-delete-file-at-head)
  "Functions which will refer to `ido-matches', but will return to ido later.
If you've added stuff to ido which operates on the current match, pop it in this list."
  :type 'hook
  :options '(ido-kill-buffer-at-head ido-delete-file-at-head)
  :group 'ido-grid-mode)

(defcustom ido-grid-mode-start-collapsed nil
  "If t, ido-grid-mode shows one line when it starts, and displays the grid when you press tab.

Note that this depends on `ido-grid-mode-keys' having tab
enabled; if it is not, bind something to `igm-tab' to un-collapse."
  :type 'boolean
  :group 'ido-grid-mode)

;; vars
(defvar igm-rows 0)
(defvar igm-columns 0)
(defvar igm-count 0)
(defvar igm-offset 0)
(defvar igm-prefix nil)

(defvar igm-collapsed nil)

(defun igm-row-major ()
  "Is the grid row major?"
  (eq 'rows ido-grid-mode-order))
(defun igm-column-major ()
  "Is the grid column major?"
  (not (igm-row-major)))

(defvar ign-invocation-cache
  (make-hash-table :test 'equal :weakness 'key))

(defun igm-mapcar (fn stuff)
  "Map FN over some STUFF, storing the result in a weak cache."
  (let* ((key (cons fn stuff))
         (existing (gethash key ign-invocation-cache)))
    (or existing (puthash key (mapcar fn stuff)
                          ign-invocation-cache))))

(defmacro igm-debug (s)
  ;; `(with-current-buffer
  ;;      (get-buffer-create "ido-grid-debug")
  ;;    (end-of-buffer)
  ;;    (insert ,s)
  ;;    (insert "\n"))
  )

;; functions to compute how many columns to use

(defun igm-columns (lengths max-width)
  "Packing items of LENGTHS into MAX-WIDTH, what columns are needed?.
The items will be placed into columns row-wise, so the first row
will contain the first k items, and so on.  The result is a
vector of column widths, or nil if even 1 column is too many.
Refers to `ido-grid-mode-order' to decide whether to try and fill
rows or columns."
  (let* ((padding (length ido-grid-mode-padding))
         (lower 1)
         (item-count (length lengths))
         (jank (> item-count ido-grid-mode-jank-rows))
         (upper (min
                 (or ido-grid-mode-max-columns max-width)
                 (if (igm-row-major)
                    (1+ (/ max-width (apply #'min lengths)))
                  (+ 2 (/ item-count ido-grid-mode-min-rows)))))
         lower-solution)

    (while (< lower upper)
      (let* ((middle (+ lower (/ (- upper lower) 2)))
             (rows (max ido-grid-mode-min-rows
                    (min ido-grid-mode-max-rows
                     (/ (+ (- middle 1) item-count) middle))))

             (spare-width (- max-width (* padding (- middle 1))))
             (total-width 0)
             (column 0)
             (row 0)
             (widths (make-vector middle 0)))
        (igm-debug (format "try %dx%d %s" rows middle widths))
        ;; try and pack the items
        (let ((overflow
               (catch 'stop
                 (dolist (length lengths)
                   ;; if we have reached the jank point, stop
                   (when (and (igm-row-major)
                              jank
                              (>= row ido-grid-mode-max-rows))
                     (igm-debug (format "looking too far vertically at %dx%d %s" row column widths))
                     (throw 'stop nil))

                   (when (and (igm-column-major)
                              (>= column middle))
                     (throw 'stop nil))

                   ;; see if this grows the current column

                   (let ((w (aref widths column)))
                     (when (> length w)
                       (igm-debug (format "%dx%d expands %d to %d" row column w length))
                       (incf total-width (- length w))
                       (aset widths column length)))

                   ;; bump counters
                   (if (igm-row-major)
                       (progn (setq column (% (1+ column) middle))
                              (when (zerop column) (incf row)))
                     (progn (setq row (% (1+ row) rows))
                            (when (zerop row) (incf column))))

                   ;; die if overflow
                   (when (> total-width spare-width)
                     (igm-debug "doesn't fit horizontally")
                     (throw 'stop t))))))

          ;; move bound in search
          (if overflow
              (setf upper middle)
            (progn (setf lower middle
                         lower-solution widths)
                   (if (= (1+ lower) upper)
                       (setf upper lower))))
          )))
    (igm-debug (format "solution: %s" lower-solution))
    lower-solution))

;; functions to layout text in a grid of known dimensions.

(defun igm-pad (string current-length desired-length)
  "Given a STRING of CURRENT-LENGTH, pad it to the DESIRED-LENGTH with spaces, if it is shorter."
  (let ((delta (- desired-length current-length)))
    (cond ((zerop delta) string)
          ((> delta 0) (concat string (make-list delta 32)))
          (t string))))

(defun igm-copy-name (item)
  "Copy the `ido-name' of ITEM into a new string."
  (substring (ido-name item) 0))

(defun igm-string-width (s)
  "The displayed width of S in the minibuffer, excluding invisible text."
  (let* ((base-length (length s))
         (onset (text-property-any 0 base-length 'invisible t s)))
    (if onset
        (let ((base-width (string-width s))
              (offset 0))

          (while onset
            (setf offset (text-property-any onset base-length 'invisible nil s))
            (decf base-width (string-width (substring s onset offset)))
            (setf onset (text-property-any offset base-length 'invisible t s)))

          base-width)
      (string-width s))))

(cl-defun igm-gen-grid (items
                        &key
                        name
                        decorate
                        max-width)
  "Generate string which lays out the given ITEMS to fit in MAX-WIDTH. Also refers to `ido-grid-min-rows' and `ido-grid-max-rows', etc.
NAME will be used to turn ITEMS into strings, and the DECORATE to fontify them based on their location and name.
Modifies `igm-rows', `igm-columns', `igm-count' and sometimes `igm-offset' as a side-effect, sorry."
  (let* ((row-padding (make-list (length ido-grid-mode-prefix) 32))
         (names (igm-mapcar name items))
         (lengths (igm-mapcar #'igm-string-width names))
         (padded-width (- max-width (length row-padding)))
         (col-widths (or (igm-columns lengths padded-width)
                         (make-vector 1 padded-width)))
         (col-count (length col-widths))
         (row-count (max ido-grid-mode-min-rows
                         (min ido-grid-mode-max-rows
                              (/ (+ (length names) (- col-count 1)) col-count))))
         (grid-size (* row-count col-count))
         (col 0)
         (row 0)
         (index 0)
         all-rows)

    (add-face-text-property 0 (length ido-grid-mode-prefix)
                            'minibuffer-prompt
                            nil ido-grid-mode-prefix)

    (setf igm-rows    row-count
          igm-columns col-count)

    (setf igm-count (min (* igm-rows igm-columns)
                         (length items)))

    (setf igm-offset (max 0 (min igm-offset (- igm-count 1))))

    (if (igm-row-major)
        ;; this is the row-major code, which is easy
        (while (and names (< row row-count))
          (push
           (if (zerop col)
               (if (zerop row) ido-grid-mode-prefix row-padding)
             ido-grid-mode-padding)
           all-rows)

          (push (igm-pad (funcall decorate (pop names) (pop items) row col index)
                         (pop lengths)
                         (aref col-widths col)) all-rows)

          (incf index)
          (incf col)

          (when (= col col-count)
            (setf col 0)
            (incf row)
            (if (< row row-count) (push "\n" all-rows))))

      ;; column major:
      (let ((row-lists (make-vector row-count nil)))
        (while (and names (< index grid-size))
          (setf row (% index row-count)
                col (/ index row-count))

          (push
           (if (zerop col)
               (if (zerop row) ido-grid-mode-prefix row-padding)
             ido-grid-mode-padding)
           (elt row-lists (- row-count (1+ row))))

          (push (igm-pad (funcall decorate (pop names) (pop items)
                                  row col index)
                         (pop lengths)
                         (aref col-widths col))
                (elt row-lists (- row-count (1+ row))))

          (incf index))

        (dotimes (i (- (length row-lists) 1))
          (push "\n" (aref row-lists (- (length row-lists) 1 i))))

        ;; now we have row-lists
        ;; each one is a row, and we want to put them
        ;; all into all-rows, with "\n" as well
        ;; each row is backwards
        (setf all-rows (apply #'nconc (append row-lists nil)))
        ))

    (list (apply #'concat (nreverse all-rows))
          row-count
          col-count)))

;; functions to produce the whole text
;; not going to respect ido-use-faces

(defun igm-gen-first-line ()
  "Generate the first line suffix text using `ido-grid-mode-first-line' hook."
  (concat igm-prefix
          (mapconcat (lambda (x)
                       (cond
                        ((functionp x) (or (funcall x) ""))
                        ((symbolp x) (format "%s" (or (eval x) "")))
                        (t (format "%s" x))))
                     ido-grid-mode-first-line
                     "")))

(defun igm-no-matches ()
  "If `ido-matches' is emtpy, produce a helpful string about it."
  (unless ido-matches
    (cond (ido-show-confirm-message  " [Confirm]")
          (ido-directory-nonreadable " [Not readable]")
          (ido-directory-too-big     " [Too big]")
          (ido-report-no-match       " [No match]")
          (t ""))
    ))

(defun igm-incomplete-regexp ()
  "If `ido-incomplete-regexp', return the first match coloured using the relevant face."
  (when ido-incomplete-regexp
    (concat " "
            (let ((name (igm-copy-name (car ido-matches))))
              (add-face-text-property 0 (length name) 'ido-incomplete-regexp nil name)
              name))))

(defun igm-exact-match ()
  "If there is a single match, return just that."
  (when (not (cdr ido-matches))
    (add-face-text-property 0 (length ido-grid-mode-exact-match-prefix)
                            'minibuffer-prompt
                            nil ido-grid-mode-exact-match-prefix)
    (concat
     (igm-gen-first-line) "\n"
     ido-grid-mode-exact-match-prefix
     (let ((name (igm-copy-name (car ido-matches))))
       (add-face-text-property
        0 (length name)
        'ido-only-match nil name)
       name))))

(defun igm-highlight-matches (re s)
  "Highlight matching groups for RE in S.
Given a regex RE and string S, add `ido-vertical-match-face' to
all substrings of S which match groups in RE.  If there are no
groups, add the face to all of S."
  (when (string-match re s)
    (ignore-errors
      ;; try and match each group in case it's a regex with groups
      (let ((group 1))
        (while (match-beginning group)
          (add-face-text-property (match-beginning group)
                                  (match-end group)
                                  'ido-grid-mode-match
                                  nil s)
          (incf group))
        ;; it's not a regex with groups, so just mark the whole match region.
        (when (= 1 group)
          (add-face-text-property (match-beginning 0)
                                  (match-end 0)
                                  'ido-grid-mode-match
                                  nil s)
          )))))

(defun igm-merged-indicator (item)
  "Generate the merged indicator string for ITEM."
  (if (and (consp item)
           (sequencep (cdr item))
           (> (length (cdr item)) 1))
      (let ((mi (substring ido-merged-indicator 0)))
        (add-face-text-property 0 (length mi) 'ido-indicator nil mi)
        mi)
    ""))

(defun igm-grid (name)
  "Draw the grid for input NAME."
  (let* ((decoration-regexp (if ido-enable-regexp name (regexp-quote name)))
         (max-width (- (window-body-width (minibuffer-window)) 1))
         (decorator (lambda (name item row column offset)
                      (concat
                       (let ((name (substring name 0))
                             (l (length name)))
                         ;; copy the name so we can add faces to it
                         (when (and (/= offset igm-offset) ; directories get ido-subdir
                                    (ido-final-slash name))
                           (add-face-text-property 0 l 'ido-subdir nil name))
                         ;; selected item gets special highlight
                         (when (= offset igm-offset)
                           (add-face-text-property 0 l 'ido-first-match nil name))
                         (igm-highlight-matches decoration-regexp name)
                         name)
                       (igm-merged-indicator item))
                      ))
         (generated-grid (igm-gen-grid
                          ido-matches
                          :name #'ido-name
                          :decorate decorator
                          :max-width max-width))
         (first-line (igm-gen-first-line)))

    (concat first-line "\n" (nth 0 generated-grid))))

(defun igm-pad-missing-rows (s)
  "Pad out S to at least `ido-grid-mode-min-rows'."
  (if ido-grid-mode-always-show-min-rows
      (let ((rows 0))
        (dotimes (i (length s))
          (when (= (aref s i) 10)
            (incf rows)))
        (if (< rows ido-grid-mode-min-rows)
            (apply #'concat (cons s (make-list (- ido-grid-mode-min-rows rows) "\n")))
          s))
    s))

(defun igm-completions (name)
  "Generate the prospect grid for input NAME."
  (setf igm-rows 1
        igm-columns 1
        igm-count 1)

  (let ((igm-prefix
         (and (stringp ido-common-match-string)
              (> (length ido-common-match-string) (length name))
              (substring ido-common-match-string (length name)))))
    (when igm-prefix
      (add-face-text-property 0 (length igm-prefix) 'ido-grid-mode-prefix nil igm-prefix))

    (let ((ido-grid-mode-max-rows    (if igm-collapsed 1 ido-grid-mode-max-rows))
          (ido-grid-mode-min-rows    (if igm-collapsed 1 ido-grid-mode-min-rows))
          (ido-grid-mode-order   (if igm-collapsed 'rows ido-grid-mode-order))
          (ido-grid-mode-jank-rows (if igm-collapsed 0 ido-grid-mode-jank-rows))
          (ido-grid-mode-always-show-min-rows (if igm-collapsed nil ido-grid-mode-always-show-min-rows)))

      (igm-pad-missing-rows
       (or (igm-no-matches)
           (igm-incomplete-regexp)
           (igm-exact-match)
           (igm-grid name)
           )))))

(defun ido-grid-mode-long-count ()
  "For use in `ido-grid-mode-first-line.
Produces a string like '10/20, 8 not shown'
to say that there are 20 candidates, of
which 10 match, and 8 are off-screen."
  (let ((count (length ido-matches))
        (cand (length ido-cur-list))
        (vis igm-count))
    (if (< vis count)
        (format "%d/%d, %d not shown" count cand (- count vis))
      (format "%d/%d" count cand))))

(defun ido-grid-mode-count ()
  "For use in `ido-grid-mode-first-line'.
Counts matches, and tells you how many you can see in the grid."
  (let ((count (length ido-matches)))
    (if (> count igm-count)
        (format "%d/%d" igm-count count)
      (number-to-string count))))

;; movement in grid keys
(defun igm-move (dr dc)
  "Move `igm-offset' by DR rows and DC cols."
  (let* ((nrows igm-rows)
         (ncols igm-columns)

         (row   (if (igm-row-major) (/ igm-offset igm-rows) (% igm-offset igm-rows)))
         (col   (if (igm-row-major) (% igm-offset igm-rows) (/ igm-offset igm-rows))))

    (incf row dr)
    (incf col dc)

    (unless (or (and (= col 0)
                     (= row -1))
                (and (= row nrows)
                     (= (1+ col) ncols)))
      (while (< row 0)
        (decf col)
        (incf row nrows))

      (while (>= row nrows)
        (incf col)
        (decf row nrows))

      (while (< col 0)
        (decf row)
        (incf col ncols))

      (while (>= col ncols)
        (incf row)
        (decf col ncols)))

     (setf igm-offset
           (if (igm-row-major)
               (+ col (* row ncols))
             (+ row (* col nrows))))

     ;; check whether we went out of bounds
     (cond ((or (< row 0) (< col 0))  ; this is the case where we went upwards or left from the top
            (igm-previous-page))

           ((or (= row nrows) (= col ncols) ; this is the case where we have scrolled down
                (>= igm-offset igm-count))
            (igm-next-page))

           ;; catchall - if we are out of bounds in any way, just reset
           ((not (< -1 igm-offset igm-count))
            (setf igm-offset 0)))
     ))



(defun igm-left ()
  "Move left in the grid."
  (interactive)
  (igm-move 0 -1))

(defun igm-right ()
  "Move right in the grid."
  (interactive)
  (igm-move 0 1))

(defun igm-up ()
  "Move up in the grid."
  (interactive)
  (igm-move -1 0))

(defun igm-down ()
  "Move down in the grid."
  (interactive)
  (igm-move 1 0))

(defun igm-previous ()
  "Move up or left in the grid."
  (interactive)
  (if (igm-row-major)
      (call-interactively #'igm-left)
    (call-interactively #'igm-up)))

(defun igm-next ()
  "Move down or right in the grid."
  (interactive)
  (if (igm-row-major)
      (call-interactively #'igm-right)
    (call-interactively #'igm-down)))

(defun igm-tab ()
  "Move to the next thing in the grid, or show the grid."
  (interactive)
  (if (and igm-collapsed (< igm-count (length ido-matches)))
      (setf igm-collapsed nil)
    (call-interactively #'igm-next)))

(defun igm-previous-page ()
  "Page up in the grid."
  (interactive)
  (when (and ido-matches
             (< igm-count (length ido-matches)))
    ;; this bit is not efficient, but I don't think people will scroll up much
    ;; rather than working it out properly, we just loop backwards and redo the
    ;; layout until we can't see the item we started on
    (let ((shift 0))
      (while (<= shift igm-count)
        (ido-prev-match)
        (incf shift)
        (igm-completions ""))
      ;; now we go forwards again, so that the previous first item is the new last item
      (ido-next-match)
      ;; and do the layout one more time, so that `ido-vertical--visible-count' is right
      (igm-completions "")))
  (setf igm-offset (- igm-count 1)))

(defun igm-next-page ()
  "Page down in the grid."
  (interactive)
  (when (and ido-matches
             (< igm-count (length ido-matches)))
    ;; this is basically what ido-next-match does, but times N
    ;; it doesn't seem to work in all cases, but it works here.
    (let ((next (nth igm-count ido-matches)))
      (setq ido-cur-list (ido-chop ido-cur-list next)))
    (setq ido-rescan t)
    (setq ido-rotate t))
  (setf igm-offset 0))

;; glue to ido

(defvar igm-old-max-mini-window-height nil)

(defun igm-advise-match-temporary (o &rest args)
  "Advice for things which use `ido-matches' temporarily."
  (let ((ido-matches (nthcdr igm-offset ido-matches))
        (ido-offset 0))
    (apply o args)))

(defun igm-advise-match-permanent (o &rest args)
  "Advice for things which use `ido-matches' permanently"
  (dotimes (n igm-offset) (ido-next-match))
  (setf igm-offset 0)
  (setf max-mini-window-height (or igm-old-max-mini-window-height max-mini-window-height)
        igm-old-max-mini-window-height 0)
  (apply o args))

(defun igm-advise-functions ()
  "Add advice to functions which need it."
  (dolist (fn ido-grid-mode-advise-perm)
    (advice-add fn :around #'igm-advise-match-permanent))
  (dolist (fn ido-grid-mode-advise-temp)
    (advice-add fn :around #'igm-advise-match-temporary)))

(defun igm-unadvise-functions ()
  "Remove added advice."
  (dolist (fn ido-grid-mode-advise-perm)
    (advice-remove fn #'igm-advise-match-permanent))
  (dolist (fn ido-grid-mode-advise-temp)
    (advice-remove fn #'igm-advise-match-temporary)))

(defun igm-ido-setup ()
  "Setup key bindings, etc."
  (setf igm-offset 0)
  (setf igm-collapsed ido-grid-mode-start-collapsed)
  (setf igm-old-max-mini-window-height max-mini-window-height
        max-mini-window-height (max max-mini-window-height
                                    (1+ ido-grid-mode-max-rows)))

  (dolist (k ido-grid-mode-keys)

    (pcase k
      (`tab (setq ido-cannot-complete-command 'igm-tab))
      (`backtab (define-key ido-completion-map (kbd "<backtab>") #'igm-previous))
      (`left    (define-key ido-completion-map (kbd "<left>")    #'igm-left))
      (`right   (define-key ido-completion-map (kbd "<right>")   #'igm-right))
      (`up      (define-key ido-completion-map (kbd "<up>")      #'igm-up))
      (`down    (define-key ido-completion-map (kbd "<down>")    #'igm-down))
      (`C-n     (define-key ido-completion-map (kbd "C-n")       #'igm-next-page))
      (`C-p     (define-key ido-completion-map (kbd "C-p")       #'igm-previous-page))
      )))

;; this could be done with advice - is advice better?
;; I guess this is like advice which definitely ends up at the bottom?
(defvar igm-old-completions nil)
(defvar igm-old-cannot-complete-command nil)

(defun igm-enable ()
  "Turn on ido-grid-mode."
  (setq igm-old-completions (symbol-function 'ido-completions))
  (setq igm-old-cannot-complete-command ido-cannot-complete-command)
  (fset 'ido-completions #'igm-completions)
  (add-hook 'ido-setup-hook #'igm-ido-setup)
  (igm-advise-functions))

(defun igm-disable ()
  "Turn off ido-grid-mode."
  (fset 'ido-completions igm-old-completions)
  (setq ido-cannot-complete-command igm-old-cannot-complete-command)
  (remove-hook 'ido-setup-hook #'igm-ido-setup)
  (igm-unadvise-functions))

;;;###autoload
(define-minor-mode ido-grid-mode
  "Makes ido-mode display candidates in a grid."
  :global t
  :group 'ido-grid-mode
  (if ido-grid-mode
      (igm-enable)
    (igm-disable)))

(provide 'ido-grid-mode)

;;; ido-grid-mode.el ends here
