;;; caser.el --- Change text casing from camelCase to dash-case to snake_case -*- lexical-binding: t; -*-

;; Version: 0.1
;; Homepage: https://hg.sr.ht/~zck/caser.el

;; Package-Requires: ((emacs "24.1"))

;; Copyright 2023 Zachary Kanfer <zkanfer@gmail.com>

;; This program is free software: you can redistribute it and/or modify
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

;;; Code:

(defun caser/camelcase-dwim (arg)
  "Camelcase words in the region, if active; if not, camelcase word at point.

This converts it from dash-case or snake_case to camelCase.

If the region is active, this function calls `caser/camelcase-region'.
Otherwise, it calls `caser/camelcase-word', with prefix argument passed to it
to camelcase ARG words."
  (interactive "*p")
  (if (use-region-p)
      (caser/camelcase-region (region-beginning) (region-end))
    (caser/camelcase-word arg)))
(defalias 'caser-camelcase-dwim #'caser/camelcase-dwim)

(defun caser/camelcase-region (region-beginning region-end)
  "Camelcase the region between REGION-BEGINNING and REGION-END.

  This converts it from snake_case or dash-case to camelCase.

  After returning, point is at the end of the region."
  (interactive "*r")

  (downcase-region region-beginning region-end)

  (let ((end-marker (make-marker)))
    (move-marker end-marker region-end)

    (goto-char region-beginning)

    (while (re-search-forward (rx (or "-" "_"))
                              (marker-position end-marker)
                              t)
      (replace-match "")
      (unless (eolp)
        (caser/upcase-char)))

    (goto-char (marker-position end-marker))))
(defalias 'caser-camelcase-region #'caser/camelcase-region)

(defun caser/upcase-char ()
  "Upcase the char at point."
  (upcase-region (point)
                 (1+ (point))))
(defalias 'caser-upcase-char #'caser/upcase-char)

(defun caser//move-to-beginning-of-word ()
  "Move to the beginning of the word point is in."
  (while (looking-back (rx (one-or-more (or word
                                            "-"
                                            "_")))
                       (point-min)
                       t)
    (goto-char (match-beginning 0))))

(defun caser//move-to-end-of-word ()
  "Move to the beginning of the word point is in."
  (when (looking-at (rx (one-or-more (or word
                                         "-"
                                         "_"))))
    (goto-char (match-end 0))))

(defun caser//forward-word (number-of-words)
  "Move forward NUMBER-OF-WORDS words, defaulting to 1.

This differs from `forward-word' in that the only separators it
cares about are whitespace."
  (interactive "P")
  (if (> number-of-words 0)
      (progn (looking-at (rx-to-string
                          `(repeat 1 ,number-of-words
                                   (seq (zero-or-more space)
                                        (one-or-more (not space))))))
             (goto-char (match-end 0)))
    (when (looking-back (rx-to-string
                         `(repeat ,(- number-of-words)
                                  (seq (or space string-start)
                                       (one-or-more (not space))
                                       (zero-or-more space))))
                        (point-min)
                        t)
      (goto-char (match-beginning 0))
      (when (looking-at (rx (one-or-more space)))
        (goto-char (match-end 0))))))
(defalias 'caser--forward-word #'caser//forward-word)

(defun caser//word-helper (words case-function)
  "Call CASE-FUNCTION on WORDS words."
  (cond ((and (> words 0)
              (looking-at (rx (or word
                                  "-"
                                  "_"))))
         (caser//move-to-beginning-of-word))
        ((looking-back (rx (or word
                               "-"
                               "_"))
                       (1- (point)))
         (caser//move-to-end-of-word)))
  (let* ((initial-bound (point))
         (other-bound (progn (caser//forward-word words) (point)))
         (starting-point (min initial-bound other-bound))
         (ending-point (max initial-bound other-bound))
         (marker (make-marker)))
    (move-marker marker other-bound)
    (funcall case-function starting-point ending-point)
    (goto-char (marker-position marker))))

(defun caser/camelcase-word (words)
  "Camelcase WORDS words forward from point."
  (caser//word-helper words #'caser/camelcase-region))
(defalias 'caser-camelcase-word #'caser/camelcase-word)

(defun caser/snakecase-dwim (arg)
  "Snakecase words in the region, if active; if not, snakecase word at point.

This converts it from camelCase or dash-case to snake_case.

If the region is active, this function calls `caser/snakecase-region'.
Otherwise, it calls `caser/snakecase-word', with prefix argument passed to it
to snakecase ARG words."
  (interactive "*p")
  (if (use-region-p)
      (caser/snakecase-region (region-beginning) (region-end))
    (caser/snakecase-word arg)))
(defalias 'caser-snakecase-dwim #'caser/snakecase-dwim)

(defun caser/snakecase-region (region-beginning region-end)
  "Snakecase the region between REGION-BEGINNING and REGION-END.

  This converts it from camelCase or dash-case to snake_case."
  (interactive "*r")
  (goto-char region-beginning)
  (let ((end-marker (make-marker))
        (case-fold-search nil))
    (move-marker end-marker region-end)

    ;;We want insertions before the marker, not after.
    ;;This prevents the marker being not at the end of a word we've already snakecased,
    ;;if the word ends with capital letters. (e.g., myIP)
    (set-marker-insertion-type end-marker t)

    (goto-char region-beginning)

    (while (re-search-forward (rx (or (seq (group lower) ;;camelCase is groups 1 & 2
                                           (group upper))
                                      (seq (group (one-or-more "-")) ;;dash-case is groups 3 & 4
                                           (group word))))
                              (marker-position end-marker)
                              t)
      (let ((matched-camelcase (match-string 1)))
        (if matched-camelcase
            (progn (replace-match (concat "_"
                                          (downcase (match-string 2)))
                                  t nil nil 2)
                   (when (looking-at (rx upper))
                     ;;there is more than one uppercase letter in a row, so we're looking at an acronym.
                     (while (looking-at (rx upper))
                       (downcase-region (point)
                                        (1+ (point)))
                       (forward-char 1))
                     (unless (= (point)
                                (marker-position end-marker))
                       (backward-char 1)
                       (insert "_"))))
          ;;dashcase
          (replace-match "_" nil nil nil 3))))
    (goto-char (marker-position end-marker))))
(defalias 'caser-snakecase-region #'caser/snakecase-region)

(defun caser/snakecase-word (&optional words)
  "Snakecase WORDS words forward from point."
  (caser//word-helper words #'caser/snakecase-region))
(defalias 'caser-snakecase-word #'caser/snakecase-word)

(defun caser/dashcase-word (&optional words)
  "Dashcase WORDS words forward from point."
  (caser//word-helper words #'caser/dashcase-region))
(defalias 'caser-dashcase-word #'caser/dashcase-word)

(defun caser/dashcase-region (region-beginning region-end)
  "Dashcase the region between REGION-BEGINNING and REGION-END.

  This converts it from camelCase or snake_case to dash-case."
  (interactive "*r")
  (goto-char region-beginning)
  (let ((end-marker (make-marker))
        (case-fold-search nil))
    (move-marker end-marker region-end)

    ;;We want insertions before the marker, not after.
    ;;This prevents the marker being not at the end of a word we've already snakecased,
    ;;if the word ends with capital letters. (e.g., myIP)
    (set-marker-insertion-type end-marker t)

    (goto-char region-beginning)

    (while (re-search-forward (rx (or (seq (group lower) ;;camelCase is groups 1 & 2
                                           (group upper))
                                      (seq (group (one-or-more "_")) ;;snake_case is groups 3 & 4
                                           (group word))))
                              (marker-position end-marker)
                              t)
      (let ((matched-camelcase (match-string 1)))
        (if matched-camelcase
            (progn (replace-match (concat "-"
                                          (downcase (match-string 2)))
                                  t nil nil 2)
                   (when (looking-at (rx upper))
                     ;;there is more than one uppercase letter in a row, so we're looking at an acronym.
                     (while (looking-at (rx upper))
                       (downcase-region (point)
                                        (1+ (point)))
                       (forward-char 1))
                     (unless (= (point)
                                (marker-position end-marker))
                       (backward-char 1)
                       (insert "-"))))
          ;;dashcase
          (replace-match "-" nil nil nil 3))))
    (goto-char (marker-position end-marker))))
(defalias 'caser-dashcase-region #'caser/dashcase-region)

(defun caser/dashcase-dwim (arg)
  "Dashcase words in the region, if active; if not, dashcase word at point.

This converts it from camelCase or snake_case to dash-case.

If the region is active, this function calls `caser/dashcase-region'.
Otherwise, it calls `caser/dashcase-word', with prefix argument passed to it
to dashcase ARG words."
  (interactive "*p")
  (if (use-region-p)
      (caser/dashcase-region (region-beginning) (region-end))
    (caser/dashcase-word arg)))
(defalias 'caser-dashcase-dwim #'caser/dashcase-dwim)

;;suggested.
;; (bind-key "M-C" #'caser/camelcase-dwim)
;; (bind-key "M-S" #'caser/snakecase-dwim)
;; (bind-key "M-D" #'caser/dashcase-dwim)

(provide 'caser)
;;; caser.el ends here
