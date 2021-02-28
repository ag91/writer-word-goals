;;; wwg.el --- Writer word goals  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Andrea

;; Author: Andrea andrea-dev@hotmail.com>
;; Version: 0.0.1
;; Package-Version: 20210209
;; Package-Requires: ((emacs "25.1"))
;; Keywords: wp
;; Homepage: https://github.com/ag91/writer-word-goals

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>

;;; Commentary:

;; A word number countdown for your writing goals.
;;
;; This mode helps you staying focused on the number of words you
;; setup for your goals. The more you write the closer you get to your
;; self-set goal for this session.
;;
;; See documentation on https://github.com/ag91/writer-word-goals

;;; Code:

(defgroup wwg nil
  "Options specific to wwg."
  :tag "wwg"
  :group 'wwg)

(defvar wwg-active-timer-alist nil "Alist of (buffer . timer) bindings to cleanup achieved targets.")

(defcustom wwg-monitor-period 15 "How many seconds before checking if a writer has reached the target number of words. Defaults to a minute." :group 'wwg :type 'string)

(defvar wwg-monitor-function 'wwg-check-count-and-beep-with-message-if-finished "The function to monitor the target was reached in buffer. It takes two arguments: a number (target) and the buffer. It should return any value when it finds the target satisfied for cleanup purposes.")

(defun wwg-choose-message (remaining-words)
  "Produce a message according to the delta between TARGET-COUNT and REMAINING-WORDS."
  (cond
   ((< remaining-words 50) "Fabulous, so close!")
   ((< remaining-words 200) "You are quite done, awesome!")
   ((< remaining-words 500) "Okay, doing good effort there!")
   ((< remaining-words 800) "Not bad!")
   ('otherwise "Okay!")))

(defun wwg-check-count-and-beep-with-message-if-finished (target-count buffer)
  "Beep if TARGET-COUNT was reached in BUFFER."
  (let* ((total-so-far (with-current-buffer buffer (count-words (point-min) (point-max))))
         (remaining-words (- target-count total-so-far)))
    (if (<= remaining-words 0)
        (progn
          (beep)
          (message
           "Well done! You wrote %s words, and %s extra words of what you planned!!"
           target-count
           (abs remaining-words))
          'finished)
      (progn
        (message
         "%s %s words left."
         (wwg-choose-message remaining-words)
         remaining-words)
        nil))))

(defun wwg-run-monitor (target-number buffer)
  "Call `wwg-monitor-function' with TARGET-NUMBER and BUFFER and cleanup timer if completed."
  (when (and
         (eq buffer (current-buffer))
         (funcall wwg-monitor-function target-number buffer))
    (cancel-timer (car (alist-get buffer wwg-active-timer-alist)))))

(defun wwg-monitor-word-count-for-buffer (target-number buffer)
  "Monitor every `wwg-monitor-period' seconds if the writer reached the TARGET-NUMBER in BUFFER."
  (add-to-list
   'wwg-active-timer-alist
   (list
    buffer
    (run-with-timer
     wwg-monitor-period
     wwg-monitor-period
     `(lambda () (wwg-run-monitor ,target-number ,buffer))))))

(defun wwg-set-goal-current-buffer (number-of-words)
  "Monitor when you achieve the target NUMBER-OF-WORDS."
  (interactive "nHow many words do you want to write for this session?")
  (let ((buffer (current-buffer))
        (words-already-there (count-words (point-min) (point-max))))
    (wwg-monitor-word-count-for-buffer (+ number-of-words words-already-there) buffer)))

(defun wwg-set-1k-goal-current-buffer ()
  "Monitor when you achieve the target 1k words."
  (interactive)
  (wwg-set-goal-current-buffer 1000))


;; BEGIN editing

(defface wwg-red-face
  '((((class grayscale)
      (background light)) (:background "DimGray"))
    (((class grayscale)
      (background dark))  (:background "LightGray"))
    (((class color)
      (background light)) (:foreground "Black" :background "OrangeRed")) ; TODO changing only for mine for now
    (((class color)
      (background dark))  (:foreground "Black" :background "DarkOrange1")))
  "Face used to highlight current line.")

(defface wwg-yellow-face
  '((((class grayscale)
      (background light)) (:background "DimGray"))
    (((class grayscale)
      (background dark))  (:background "LightGray"))
    (((class color)
      (background light)) (:foreground "Black" :background "LightYellow")) ; TODO changing only for mine for now
    (((class color)
      (background dark))  (:foreground "Black" :background "DarkOrange1")))
  "Face used to highlight current line.")

(defface wwg-green-face
  '((((class grayscale)
      (background light)) (:background "DimGray"))
    (((class grayscale)
      (background dark))  (:background "LightGray"))
    (((class color)
      (background light)) (:foreground "Black" :background "LightGreen")) ; TODO changing only for mine for now
    (((class color)
      (background dark))  (:foreground "Black" :background "DarkOrange1")))
  "Face used to highlight current line.")

(defun wwg-pick-color-face (score)
  "Pick a face according to SCORE."
  (cond
   ((<= score 50) 'wwg-red-face)
   ((<= score 85) 'wwg-yellow-face)
   ('otherwise 'wwg-green-face)))

(defun wwg-highlight-paragraphs ()
  "Highlight paragraphs according to easy score."
  (save-excursion
    (goto-char (point-min))
    (let (last-visited)
      (while (and (bounds-of-thing-at-point 'paragraph) (not (eq last-visited (point-max))))
        (let* ((bounds (bounds-of-thing-at-point 'paragraph))
               (begin (car bounds))
               (end (cdr bounds))
               (score (writegood-calculate-reading-ease begin end))
               (color-face (wwg-pick-color-face score)))
          (ignore-errors (wwg-unhighlight-region begin))
          (wwg-highlight-region begin end color-face)
          (setq last-visited (point))
          (forward-paragraph))))))

(defun wwg-unhighlight-paragraphs ()
  "Remove paragraph highlighting."
  (save-excursion
    (let (last-visited)
      (while (and (bounds-of-thing-at-point 'paragraph) (not (eq last-visited (point-max))))
        (let ((bounds (bounds-of-thing-at-point 'paragraph)))
          (wwg-unhighlight-region (car bounds))
          (setq last-visited (point))
          (forward-paragraph))))))

(defun wwg-highlight-sentences ()
  "Highlight sentences according to easy score."
  (save-excursion
    (goto-char (point-min))
    (while (bounds-of-thing-at-point 'sentence)
      (let* ((bounds (bounds-of-thing-at-point 'sentence))
             (begin (car bounds))
             (end (cdr bounds))
             (score (writegood-calculate-reading-ease begin end))
             (color-face (wwg-pick-color-face score)))
        (ignore-errors (wwg-unhighlight-region begin))
        (wwg-highlight-region begin end color-face)
        (forward-sentence)))))

(defun wwg-unhighlight-sentences ()
  "Remove sentence highlighting."
  (save-excursion
    (while (bounds-of-thing-at-point 'sentence)
      (let ((bounds (bounds-of-thing-at-point 'sentence)))
        (wwg-unhighlight-region (car bounds))
        (forward-sentence)))))

(defun wwg-highlight-region (begin end &optional color-face)
  "Highlight region between BEGIN and END in green, unless COLOR-FACE."
  (let ((overlay (make-overlay begin end)))
    (overlay-put overlay 'category 'wwg-overlay) ;; add my category to the overlay
    (overlay-put overlay 'face (or color-face 'wwg-green-face))))

(defun wwg-unhighlight-region (point &optional color-face)
  "Delete any overlay at POINT. Optionally only those with COLOR-FACE."
  (--> (overlays-at point)
    (--filter (eq (overlay-get it 'category) 'wwg-overlay) it)
    (--each it (delete-overlay it))))

(defvar wwg-editing-highlighting-atom 'sentence "What to highlight for editing.")

(defun wwg-highlight-for-editing ()
  "Highlight complexity for editing."
  (interactive)
  (cond
   ((eq wwg-editing-highlighting-atom 'sentence) (wwg-highlight-sentences))
   ((eq wwg-editing-highlighting-atom 'paragraph) (wwg-highlight-paragraphs))))

(defun wwg-unhighlight-for-editing ()
  "Unhighlight complexity for editing."
  (interactive)
  (cond
   ((eq wwg-editing-highlighting-atom 'sentence) (wwg-unhighlight-sentences))
   ((eq wwg-editing-highlighting-atom 'paragraph) (wwg-unhighlight-paragraphs))))

(defun wwg-toggle-highlighting-atom ()
  "Toggle `wwg-toggle-highlighting-atom' for highlighting style between sentences and paragraphs."
  (interactive)
  (cond
   ((eq wwg-editing-highlighting-atom 'sentence)
    (progn
      (setq wwg-editing-highlighting-atom 'paragraph)
      (wwg-unhighlight-sentences)
      (wwg-highlight-for-editing)))
   ((eq wwg-editing-highlighting-atom 'paragraph)
    (progn
      (setq wwg-editing-highlighting-atom 'sentence)
      (wwg-unhighlight-paragraphs)
      (wwg-highlight-for-editing)))))

(defun wwg-toggle-highlighting ()
  "Toggle editing highlighting."
  (interactive)
  (if (ignore-errors (eq 'wwg-overlay (overlay-get (car (overlays-at (point-min))) 'category)))
      (wwg-unhighlight-for-editing)
    (wwg-highlight-for-editing)))

(defun wwg-editing-highlighting-hook-fn ()
  "Hook function after saving to use during an editing session to find more easily editing targets."
  (wwg-unhighlight-for-editing)
  (wwg-highlight-for-editing))

(defun wwg-calculate-readability-buffer (buffer)
  "Calculate readability of BUFFER."
  (with-current-buffer buffer
    (writegood-calculate-reading-ease (point-min) (point-max))))

(defun wwg-editing-goal-diff (editing-goal buffer)
  "Check how far away we are from EDITING-GOAL in BUFFER."
  (- editing-goal (wwg-calculate-readability-buffer buffer)))

(defun wwg-editing-goal-reached-p (editing-goal buffer)
  "Check if EDITING-GOAL is reached in BUFFER."
  (<= (wwg-editing-goal-diff editing-goal buffer) 0))

(defun wwg-check-readibility-and-beep-with-message-if-finished (target-count buffer)
  "Beep if TARGET-COUNT was reached in BUFFER."
  (if (wwg-editing-goal-reached-p target-count buffer)
      (progn
        (beep)
        (wwg-unhighlight-for-editing)
        (remove-hook 'after-save-hook 'wwg-editing-highlighting-hook-fn 't)
        (message
         "Well done! You increased readability of %.2f%%, now it is at %.2f%%!!"
         target-count
         (wwg-calculate-readability-buffer buffer)))
    (progn
      (message
       "Okay! Just %.2f%% readability more."
       (wwg-editing-goal-diff target-count buffer))
      nil)))

;; TODO use only built-ins for this mode:  no s.el nor dash.el
(defun wwg-set-editing-goal (percentage)
  "Set an editing goal by defining by which PERCENTAGE you want to increase readibility."
  (interactive
   (list (--> (completing-read "Choose readability increase: " (list "Easy: 5%" "Medium: 20%" "Hard: 50%") nil 't nil nil "Easy: 5%")
           (substring it (- (length it) 3) (- (length it) 1))
           s-trim
           string-to-number)))
  (let* ((buffer (current-buffer))
         (current-readibility (wwg-calculate-readability-buffer buffer))
         )
    (add-hook 'after-save-hook 'wwg-editing-highlighting-hook-fn nil 't)
    (wwg-highlight-for-editing)
    (wwg-monitor-word-count-for-buffer (+ current-readibility percentage) buffer 'wwg-check-readibility-and-beep-with-message-if-finished))) ;; TODO there is an issue here because I can set unreachable goals?


(defun wwg-run-monitor (target-number buffer &optional fn)
  "Call `wwg-monitor-function' with TARGET-NUMBER and BUFFER and cleanup timer if completed."
  (when (and
         (eq buffer (current-buffer))
         (funcall (or fn wwg-monitor-function) target-number buffer))
    (cancel-timer (car (alist-get buffer wwg-active-timer-alist)))))

(defun wwg-monitor-word-count-for-buffer (target-number buffer &optional fn)
  "Monitor every `wwg-monitor-period' seconds if the writer reached the TARGET-NUMBER in BUFFER."
  (add-to-list
   'wwg-active-timer-alist
   (list
    buffer
    (run-with-timer
     wwg-monitor-period
     wwg-monitor-period
     `(lambda () (wwg-run-monitor ,target-number ,buffer ',fn))))))
;; END editing


(provide 'wwg)
;;; wwg.el ends here

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\?[ \t]+1.%02y%02m%02d\\?\n"
;; End:
