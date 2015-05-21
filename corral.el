;;; corral.el --- Incrementally wrap delimiters around s-expressions

;; Copyright (C) 2015 Kevin Liu
;; Author: Kevin Liu <nivekuil@gmail.com>
;; Created: 16 May 2015
;; Homepage: http://github.com/nivekuil/corral
;; Version: 0.1.8

;; This file is not part of GNU Emacs.

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

;; This package contains functions that incrementally wrap (or "corral")
;; s-expressions with delimiters, such as parentheses and brackets.
;; After calling one of the interactive commands, repeated calls will shift
;; the corral instead of inserting new delimiters, expanding the amount of
;; text contained within the delimiters.

;;; Code:

(defcustom corral-preserve-point nil
  "Preserve the position of the point instead of following a delimiter."
  :type 'boolean
  :group 'corral)

(defvar corral-syntax-entries nil
  "Syntax rules to apply when coralling text.
An example usage to have # and * be counted as symbols:
\(setq corral-syntax-entries '((?# \"_\")
                               (?* \"_\")))

You can also use 'add-to-list', like this:
\(add-to-list 'corral-syntax-entries '(?# \"_\"))")

(defvar corral--virtual-point 0
  "Virtual point position to use for shifting, when preserving the real point.")

(defun corral-wrap-backward (open close)
  "Wrap OPEN and CLOSE delimiters around sexp, leaving point at OPEN."
  (when (and (string-match-p "\\w" (char-to-string (char-after)))
             (string-match-p "\\W" (char-to-string (char-before))))
    (forward-char))
  (backward-sexp)
  (save-excursion
    (forward-sexp) (insert close))
  (insert open)
  (backward-char))

(defun corral-wrap-forward (open close)
  "Wrap OPEN and CLOSE around sexp, leaving point at CLOSE."
  (when (and (string-match-p "\\w" (char-to-string (char-before)))
             (string-match-p "\\W" (char-to-string (char-after))))
    (forward-char))
  (forward-sexp)
  (save-excursion
    (backward-sexp) (insert open))
  (insert close))

(defun corral-shift-backward (open close)
  "Shift OPEN delimiter backward one sexp.  CLOSE is not moved."
  (cond
   ((eq (char-before) open)
    (backward-char) (corral-shift-backward open close))
   ((eq (char-after) open)
    (save-excursion (backward-sexp))    ; Handle scan error
    (delete-char 1) (backward-sexp) (insert open) (backward-char))
   (t (backward-sexp) (corral-shift-backward open close))))

(defun corral-shift-forward (open close)
  "Without moving OPEN, shift CLOSE delimiter forward one sexp."
  (cond
   ((eq (char-after) close)
    (forward-char) (corral-shift-forward open close))
   ((eq (char-before) close)
    (save-excursion (forward-sexp))     ; Handle scan error
    (delete-char -1) (forward-sexp) (insert close))
   (t (forward-sexp) (corral-shift-forward open close))))

(defun corral-command-backward (open close backward forward)
  "Handle command with OPEN and CLOSE from commands BACKWARD and FORWARD."
  (save-excursion
    (let ((temp-syntax-table
           (make-syntax-table (syntax-table)))) ;Inherit current syntax table
      ;; Loop through corral syntax entries and apply them temporarily
      (cl-loop for entry in corral-syntax-entries collect
               (apply (lambda (x y)
                        (modify-syntax-entry x y temp-syntax-table)) entry))
      (with-syntax-table temp-syntax-table
        (if (or (eq last-command forward)
                (eq last-command backward))
            (progn (goto-char corral--virtual-point)
                   (corral-shift-backward open close))
          (corral-wrap-backward open close)))
      (setq corral--virtual-point (point))))
    (unless corral-preserve-point
      (goto-char corral--virtual-point)))

(defun corral-command-forward (open close backward forward)
  "Handle command with OPEN and CLOSE from commands BACKWARD and FORWARD."
  (save-excursion
    (let ((temp-syntax-table
           (make-syntax-table (syntax-table)))) ;Inherit current syntax table
      ;; Loop through corral syntax entries and apply them temporarily
      (cl-loop for entry in corral-syntax-entries collect
               (apply (lambda (x y)
                        (modify-syntax-entry x y temp-syntax-table)) entry))
      (with-syntax-table temp-syntax-table
        (if (or (eq last-command forward)
                (eq last-command backward))
            (progn (goto-char corral--virtual-point)
                   (corral-shift-forward open close))
          (corral-wrap-forward open close)))
      (setq corral--virtual-point (point))))
    (unless corral-preserve-point
      (goto-char corral--virtual-point)))


;;;###autoload
(defun corral-parentheses-backward ()
  "Wrap parentheses around sexp, moving point to the closing parentheses."
  (interactive)
  (corral-command-backward ?( ?)
                          'corral-parentheses-backward
                          'corral-parentheses-forward))

;;;###autoload
(defun corral-parentheses-forward ()
  "Wrap parentheses around sexp, moving point to the closing parentheses."
  (interactive)
  (corral-command-forward ?( ?)
                          'corral-parentheses-backward
                          'corral-parentheses-forward))

;;;###autoload
(defun corral-brackets-backward ()
  "Wrap brackets around sexp, moving point to the opening bracket."
  (interactive)
  (corral-command-backward ?( ?)
                          'corral-brackets-backward
                          'corral-brackets-forward))

;;;###autoload
(defun corral-brackets-forward ()
  "Wrap brackets around sexp, moving point to the closing bracket."
  (interactive)
  (corral-command-forward ?( ?)
                          'corral-brackets-backward
                          'corral-brackets-forward))

;;;###autoload
(defun corral-braces-backward ()
  "Wrap brackets around sexp, moving point to the closing bracket."
  (interactive)
  (corral-command-backward ?( ?)
                           'corral-brackets-backward
                           'corral-brackets-forward))

;;;###autoload
(defun corral-braces-forward ()
  "Wrap brackets around sexp, moving point to the closing bracket."
  (interactive)
  (corral-command-backward ?{ ?}
                           'corral-braces-backward
                           'corral-braces-forward))

;;;###autoload
(defun corral-double-quotes-forward ()
  "Wrap double quotes around sexp, moving point to the closing double quote."
  (interactive)
  (corral-command-forward ?\" ?\"
                           'corral-double-quotes-backward
                           'corral-double-quotes-forward))

;;;###autoload
(defun corral-double-quotes-backward ()
  "Wrap double quotes around sexp, moving point to the opening double quote."
  (interactive)
  (corral-command-backward ?\" ?\"
                           'corral-double-quotes-backward
                           'corral-double-quotes-forward))

(provide 'corral)

;;; corral.el ends here
