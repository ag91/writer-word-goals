;;; wwg.el --- writer word goals

;; Copyright (C) 2021 Andrea

;; Author: Andrea andrea-dev@hotmail.com>
;; Version: 0.0.1
;; Package-Version: 20210209
;; Keywords: writing

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

(defvar wwg/active-timer-alist nil "Alist of (buffer . timer) bindings to cleanup achieved targets.")

(defcustom wwg/monitor-period 15 "How many seconds before checking if a writer has reached the target number of words. Defaults to a minute." :group 'wwg)

(defvar wwg/monitor-function 'wwg/check-count-and-beep-with-message-if-finished "The function to monitor the target was reached in buffer. It takes two arguments: a number (target) and the buffer. It should return any value when it finds the target satisfied for cleanup purposes.")

(defun wwg/choose-message (remaining-words)
  "Produce a message according to the delta between TARGET-COUNT and REMAINING-WORDS."
  (cond
   ((< remaining-words 50) "Fabulous, so close!")
   ((< remaining-words 200) "You are quite done, awesome!")
   ((< remaining-words 500) "Okay, doing good effort there!")
   ((< remaining-words 800) "Not bad!")
   ('otherwise "Okay!")))

(defun wwg/check-count-and-beep-with-message-if-finished (target-count buffer)
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
         (wwg/choose-message remaining-words)
         remaining-words)
        nil))))

(defun wwg/run-monitor (target-number buffer)
  "Call `wwg/monitor-function' with TARGET-NUMBER and BUFFER and cleanup timer if completed."
  (when (and
         (eq buffer (current-buffer))
         (funcall wwg/monitor-function target-number buffer))
    (cancel-timer (car (alist-get buffer wwg/active-timer-alist)))))

(defun wwg/monitor-word-count-for-buffer (target-number buffer)
  "Monitor every `wwg/monitor-period' seconds if the writer reached the TARGET-NUMBER in BUFFER."
  (add-to-list
   'wwg/active-timer-alist
   (list
    buffer
    (run-with-timer
     wwg/monitor-period
     wwg/monitor-period
     `(lambda () (wwg/run-monitor ,target-number ,buffer))))))

(defun wwg/set-goal-current-buffer (number-of-words)
  "Monitor when you achieve the target NUMBER-OF-WORDS."
  (interactive "nHow many words do you want to write for this session?")
  (let ((buffer (current-buffer))
        (words-already-there (count-words (point-min) (point-max))))
    (wwg/monitor-word-count-for-buffer (+ number-of-words words-already-there) buffer)))

(defun wwg/set-1k-goal-current-buffer ()
  "Monitor when you achieve the target 1k words."
  (interactive)
  (wwg/set-goal-current-buffer 1000))


(provide 'wwg)
;;; wwg ends here

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\?[ \t]+1.%02y%02m%02d\\?\n"
;; End:
