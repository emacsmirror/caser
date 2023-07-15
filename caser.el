(defun camelcase-dwim (arg)
  "Camelcase words in the region, if active; if not, camelcase word at point.
  If the region is active, this function calls `camelcase-region'.
  Otherwise, it calls `camelcase-word', with prefix argument passed to it
  to camelcase ARG words."
  (interactive "*p")
  (if (use-region-p)
      (camelcase-region (region-beginning) (region-end))
    (progn (camelcase-word arg)

           ;;this handles ending in the middle of a snake_cased word.
           (while (looking-at "_+\\w")
             (camelcase-word 1))

           ;;drop underlines at the end of the word
           (while (looking-at "_")
             (delete-char 1)))))

;;how does this work on caps?
(defun camelcase-region (region-beginning region-end)
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
        (upcase-char)))

    (goto-char (marker-position end-marker))))

(defun downcase-char ()
  "Downcase the char at point."
  (interactive)
  (downcase-region (point)
                   (1+ (point))))

(defun upcase-char ()
  "Upcase the char at point."
  (interactive)
  (upcase-region (point)
                 (1+ (point))))

(defun caser/forward-word (number-of-words)
  (interactive "P")
  (if (> number-of-words 0)
      (progn (looking-at (rx-to-string
                          `(repeat 1 ,number-of-words
                                   (seq (zero-or-more space)
                                        (one-or-more (not space))))))
             (goto-char (match-end 0)))
    (dotimes (n (- number-of-words))
      (when (looking-back (rx (seq (one-or-more (not space))
                                   (zero-or-more space)))
                          (point-min)
                          t)
        (goto-char (match-beginning 0))))))

(defun camelcase-word (&optional words)
  "Camelcase WORDS words forward from point."
  (interactive "p")
  (let ((starting-point (point))
        (ending-point (progn (looking-at (rx-to-string
                                          `(repeat 0 ,(or words 1)
                                                   (seq (zero-or-more blank)
                                                        ,case//quoted-word-regexp))))
                             (match-end 0))))
    (goto-char ending-point)
    (camelcase-region starting-point ending-point)))

(defun snakecase-dwim (arg)
  "Snakecase words in the region, if active; if not, snakecase word at point.
  If the region is active, this function calls `snakecase-region'.
  Otherwise, it calls `snakecase-word', with prefix argument passed to it
  to snakecase ARG words."
  (interactive "*p")
  (if (use-region-p)
      (snakecase-region (region-beginning) (region-end))
    (snakecase-word arg)))

(defun snakecase-region (region-beginning region-end)
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

(defun snakecase-word (words)
  "Snakecase WORDS words forward from point."
  (interactive "p")
  (snakecase-region (point)
                    (progn (caser/forward-word words)
                           (point))))

(defun dashcase-word (words)
  "Dashcase WORDS words forward from point."
  (interactive "p")
  (dashcase-region (point)
                   (progn (caser/forward-word words)
                          (point))))

(defun dashcase-region (region-beginning region-end)
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

(defun dashcase-dwim (arg)
  "Dashcase words in the region, if active; if not, dashcase word at point.
  If the region is active, this function calls `dashcase-region'.
  Otherwise, it calls `dashcase-word', with prefix argument passed to it
  to dashcase ARG words."
  (interactive "*p")
  (if (use-region-p)
      (dashcase-region (region-beginning) (region-end))
    (dashcase-word arg)))

;;suggested.
;; (bind-key "M-C" #'camelcase-dwim)
;; (bind-key "M-S" #'snakecase-dwim)
;; (bind-key "M-D" #'dashcase-dwim)
