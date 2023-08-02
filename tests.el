(require 'caser)

(defmacro caser//on-temp-buffer (string &rest body)
  "Insert STRING into a temp buffer, then run BODY on the temp buffer.

Point starts at the beginning of the buffer, or where a pipe character
occurs.  To insert an actual pipe, include two pipes.

After running BODY, the entire buffer is returned as a string."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (insert ,string)
     (caser//on-temp-buffer//preprocess-buffer)

     ,@body
     (buffer-string)))

(defmacro caser//on-temp-buffer-point (string &rest body)
  "Insert STRING into a temp buffer, then run BODY on the temp buffer.

Point starts at the beginning of the buffer, or where a pipe character
occurs.  To insert an actual pipe, include two pipes.

After running BODY, the entire buffer is returned as a string.  In
this returned string, point is indicated by a pipe character.  Pipe
characters in the string are replaced with a double pipe."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (insert ,string)
     (caser//on-temp-buffer//preprocess-buffer)

     ,@body

     (caser//on-temp-buffer//postprocess-buffer-for-point)
     (buffer-string)))

(defmacro caser//on-temp-buffer-region (string &rest body)
  "Insert STRING into a temp buffer, run BODY, and return the region.

Point starts at the beginning of the buffer, or where a pipe character
occurs.  To insert an actual pipe, include two pipes."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (insert ,string)
     (caser//on-temp-buffer//preprocess-buffer)

     ,@body
     (buffer-substring (region-beginning) (region-end))))


(defun caser//on-temp-buffer//preprocess-buffer ()
  "Preprocess the current buffer for caser//on-temp-buffer.

This should be run before running any body code for caser//on-temp-buffer

To do this:

1. Move point to the location of a single pipe by itself.
2. Replace all escaped pipe characters (\\|) with a single pipe."
  (goto-char (point-min))
  (let ((point-to-start-with (point-min)))
    (while (re-search-forward (rx (or "|" "\\")) nil t)
      (let ((string-matched (match-string 0)))
        (delete-char -1)
        (when (equal "|"
                     string-matched)
          (setf point-to-start-with (point))))
      (unless (eobp)
        (forward-char 1)))
    (goto-char point-to-start-with)))

(defun caser//on-temp-buffer//postprocess-buffer-for-point ()
  "Process the current buffer so it indicates where point was.

This is for use after running body, for caser//on-temp-buffer-point.

To do this:

1. Place a backslash before each pipe character.
2. Insert a single pipe character where point was when this function
was called."
  (let ((point-to-return (point)))
         (goto-char (point-min))
         (while (search-forward "|" nil t)
           (when (< (point) point-to-return)
             ;;we're going to insert a character before point-to-return, so increase it by one.
             (setf point-to-return (1+ point-to-return)))
           (backward-char 1)
           (insert "\\")
           (forward-char))
         (goto-char point-to-return)
         (insert "|")))

;;camelcase tests
;;region tests
(ert-deftest camelcase-region/no-change ()
  (should (equal "hi|"
                 (caser//on-temp-buffer-point
                   "hi"
                   (caser/camelcase-region 1 3)))))

(ert-deftest camelcase-region/from-snakecase/one-word ()
  (should (equal "hiMom|"
                 (caser//on-temp-buffer-point
                   "hi_mom"
                   (caser/camelcase-region 1 7)))))

(ert-deftest camelcase-region/from-dashcase/one-word ()
  (should (equal "hiMom|"
                 (caser//on-temp-buffer-point
                   "hi-mom"
                   (caser/camelcase-region 1 7)))))

;;word tests
(ert-deftest camelcase-word/from-snakecase ()
  (should (equal "abCd| ef_gh"
                 (caser//on-temp-buffer-point
                   "|ab_cd ef_gh"
                   (caser/camelcase-word 1)))))

(ert-deftest camelcase-word/from-dashcase ()
  (should (equal "abCd| ef-gh"
                 (caser//on-temp-buffer-point
                   "|ab-cd ef-gh"
                   (caser/camelcase-word 1)))))

(ert-deftest camelcase-word/called-twice ()
  (should (equal "hiMom andOther| stuff_here"
                 (caser//on-temp-buffer-point
                   "hi_mom and_other stuff_here"
                   (caser/camelcase-word 2)))))

(ert-deftest camelcase-word/snakecase-and-lispcase ()
  (should (equal "hiMom andOther| stuff_here"
                 (caser//on-temp-buffer-point
                   "hi_mom and-other stuff_here"
                   (caser/camelcase-word 2)))))


;;dwim tests
(ert-deftest camelcase-dwim/single-word-doesnt-change ()
  (should (equal "hi"
                 (caser//on-temp-buffer
                   "hi"
                   (caser/camelcase-dwim 1)))))

(ert-deftest camelcase-dwim/from-snakecase ()
  (should (equal "hiMom|"
                 (caser//on-temp-buffer-point
                   "hi_mom"
                   (caser/camelcase-dwim 1)))))

(ert-deftest camelcase-dwim/from-dashcase ()
  (should (equal "hiMom|"
                 (caser//on-temp-buffer-point
                   "hi-mom"
                   (caser/camelcase-dwim 1)))))

(ert-deftest camelcase-dwim/snakecase-two-arg ()
  (should (equal "hiMom andOther| stuff_here"
                 (caser//on-temp-buffer-point
                   "hi_mom and_other stuff_here"
                   (caser/camelcase-dwim 2)))))

(ert-deftest camelcase-dwim/snakecase-three-arg-complicated-words ()
  (should (equal "hiThere thisIsWayFun youGuys| and_more"
                 (caser//on-temp-buffer-point
                   "|hi_there this_is_way_fun you_guys and_more"
                   (caser/camelcase-dwim 3)))))

;;snakecase_tests
;;region tests
(ert-deftest snakecase-region/no-change ()
  (should (equal "hi|"
                 (caser//on-temp-buffer-point
                   "hi"
                   (caser/snakecase-region 1 3)))))

(ert-deftest snakecase-region/from-camelcase/one-word ()
  (should (equal "hi_mom|"
                 (caser//on-temp-buffer-point
                   "hiMom"
                   (caser/snakecase-region 1 7)))))

(ert-deftest snakecase-region/from-dashcase/one-word ()
  (should (equal "hi_mom|"
                 (caser//on-temp-buffer-point
                   "hi-mom"
                   (caser/snakecase-region 1 7)))))

(ert-deftest snakecase-region/from-camelcase/multiple-caps/middle-of-word ()
  (should (equal "my_ip_address|"
                 (caser//on-temp-buffer-point
                   "myIPAddress"
                   (caser/snakecase-region (point-min) (point-max))))))

(ert-deftest snakecase-region/from-camelcase/multiple-caps/end-of-word ()
  (should (equal "my_ip|"
                 (caser//on-temp-buffer-point
                   "myIP"
                   (caser/snakecase-region (point-min) (point-max))))))

;;word tests
(ert-deftest snakecase-word/from-camelcase ()
  (should (equal "ab_cd| efGh"
                 (caser//on-temp-buffer-point
                   "|abCd efGh"
                   (caser/snakecase-word 1)))))

(ert-deftest snakecase-word/from-dashcase ()
  (should (equal "ab_cd| ef-gh"
                 (caser//on-temp-buffer-point
                   "|ab-cd ef-gh"
                   (caser/snakecase-word 1)))))

(ert-deftest snakecase-word/called-twice ()
  (should (equal "hi_mom and_other| stuffHere"
                 (caser//on-temp-buffer-point
                   "hiMom andOther stuffHere"
                   (caser/snakecase-word 2)))))

(ert-deftest snakecase-word/camelcase-and-dashcase ()
  (should (equal "hi_mom and_other| stuff_here"
                 (caser//on-temp-buffer-point
                   "hiMom and-other stuff_here"
                   (caser/snakecase-word 2)))))

;;dwim tests
(ert-deftest snakecase-dwim/single-word-doesnt-change ()
  (should (equal "hi"
                 (caser//on-temp-buffer
                   "hi"
                   (caser/snakecase-dwim 1)))))

(ert-deftest snakecase-dwim/from-camelcase ()
  (should (equal "hi_mom|"
                 (caser//on-temp-buffer-point
                   "hiMom"
                   (caser/snakecase-dwim 1)))))

(ert-deftest snakecase-dwim/from-dashcase ()
  (should (equal "hi_mom|"
                 (caser//on-temp-buffer-point
                   "hi-mom"
                   (caser/snakecase-dwim 1)))))

(ert-deftest snakecase-dwim/camelcase-two-arg ()
  (should (equal "hi_mom and_other| stuffHere"
                 (caser//on-temp-buffer-point
                   "hiMom andOther stuffHere"
                   (caser/snakecase-dwim 2)))))
;;dashcase-tests
;; region tests
(ert-deftest dashcase-region/no-change ()
  (should (equal "hi|"
                 (caser//on-temp-buffer-point
                   "hi"
                   (caser/dashcase-region 1 3)))))

(ert-deftest dashcase-region/from-camelcase/one-word ()
  (should (equal "hi-mom|"
                 (caser//on-temp-buffer-point
                   "hiMom"
                   (caser/dashcase-region 1 7)))))

(ert-deftest dashcase-region/from-snakecase/one-word ()
  (should (equal "hi-mom|"
                 (caser//on-temp-buffer-point
                   "hi-mom"
                   (caser/dashcase-region 1 7)))))

(ert-deftest dashcase-region/from-camelcase/multiple-caps/middle-of-word ()
  (should (equal "my-ip-address|"
                 (caser//on-temp-buffer-point
                   "myIPAddress"
                   (caser/dashcase-region (point-min) (point-max))))))

(ert-deftest dashcase-region/from-camelcase/multiple-caps/end-of-word ()
  (should (equal "my-ip|"
                 (caser//on-temp-buffer-point
                   "myIP"
                   (caser/dashcase-region (point-min) (point-max))))))

;; word tests
(ert-deftest dashcase-word/from-camelcase ()
  (should (equal "ab-cd| efGh"
                 (caser//on-temp-buffer-point
                   "|abCd efGh"
                   (caser/dashcase-word 1)))))

(ert-deftest dashcase-word/from-snakecase ()
  (should (equal "hi-mom| and_other stuff_here"
                 (caser//on-temp-buffer-point
                   "hi_mom and_other stuff_here"
                   (caser/dashcase-word 1)))))

(ert-deftest dashcase-word/called-twice ()
  (should (equal "hi-mom and-other| stuffHere"
                 (caser//on-temp-buffer-point
                   "hiMom andOther stuffHere"
                   (caser/dashcase-word 2)))))

(ert-deftest dashcase-word/camelcase-and-snakecase ()
  (should (equal "hi-mom and-other| stuff_here"
                 (caser//on-temp-buffer-point
                   "hiMom and_other stuff_here"
                   (caser/dashcase-word 2)))))


;; dwim tests
(ert-deftest dashcase-dwim/single-word-doesnt-change ()
  (should (equal "hi"
                 (caser//on-temp-buffer
                   "hi"
                   (caser/dashcase-dwim 1)))))

(ert-deftest dashcase-dwim/from-camelcase ()
  (should (equal "hi-mom|"
                 (caser//on-temp-buffer-point
                   "hiMom"
                   (caser/dashcase-dwim 1)))))

(ert-deftest dashcase-dwim/from-snakecase ()
  (should (equal "hi-mom|"
                 (caser//on-temp-buffer-point
                   "hi_mom"
                   (caser/dashcase-dwim 1)))))

(ert-deftest dashcase-dwim/camelcase-two-arg ()
  (should (equal "hi-mom and-other| stuffHere"
                 (caser//on-temp-buffer-point
                   "hiMom andOther stuffHere"
                   (caser/dashcase-dwim 2)))))

;;forward-word
(ert-deftest forward-word/all-lowercase ()
  (should (equal "hi| there friends"
                 (caser//on-temp-buffer-point
                   "|hi there friends"
                   (caser//forward-word 1)))))

(ert-deftest forward-word/starting-end-of-line ()
  (should (equal "hi there
you| all"
                 (caser//on-temp-buffer-point
                   "hi there|
you all"
                   (caser//forward-word 1)))))

(ert-deftest forward-word/multiple-newlines ()
  (should (equal "hi there



you| all"
                 (caser//on-temp-buffer-point
                   "hi there|



you all"
                   (caser//forward-word 1)))))

(ert-deftest forward-word/backward/multiple-newlines ()
  (should (equal "hi |there



you all"
                 (caser//on-temp-buffer-point
                   "hi there



|you all"
                   (caser//forward-word -1)))))

(ert-deftest forward-word/ending-end-of-line ()
  (should (equal "hi there|
you all"
                 (caser//on-temp-buffer-point
                   "hi| there
you all"
                   (caser//forward-word 1)))))

(ert-deftest forward-word/backwards/starting-end-of-line ()
  (should (equal "hi |there
you all"
                 (caser//on-temp-buffer-point
                   "hi there|
you all"
                   (caser//forward-word -1)))))

(ert-deftest forward-word/backwards/ending-start-of-line ()
  (should (equal "hi there
|you all"
                 (caser//on-temp-buffer-point
                   "hi there
you| all"
                   (caser//forward-word -1)))))

(ert-deftest forward-word/end-of-buffer ()
  (should (equal "hi there|"
                 (caser//on-temp-buffer-point
                   "hi| there"
                   (caser//forward-word 1)))))

(ert-deftest forward-word/backwards/start-of-buffer ()
  (should (equal "|hi there"
                 (caser//on-temp-buffer-point
                   "hi| there"
                   (caser//forward-word -1)))))

(ert-deftest forward-word/through-line-end ()
  (should (equal "hi there
you| all"
                 (caser//on-temp-buffer-point
                   "hi| there
you all"
                   (caser//forward-word 2)))))

(ert-deftest forward-word/camelcase ()
  (should (equal "hiThere| friends"
                 (caser//on-temp-buffer-point
                   "|hiThere friends"
                   (caser//forward-word 1)))))

(ert-deftest forward-word/dashcase ()
  (should (equal "hi-there| friends-who"
                 (caser//on-temp-buffer-point
                   "|hi-there friends-who"
                   (caser//forward-word 1)))))

(ert-deftest forward-word/snakecase ()
  (should (equal "hi_there| friends_who"
                 (caser//on-temp-buffer-point
                   "|hi_there friends_who"
                   (caser//forward-word 1)))))

(ert-deftest forward-word/camelcase/backwards ()
  (should (equal "hiThere |friendsWho"
                 (caser//on-temp-buffer-point
                   "hiThere friendsWho|"
                   (caser//forward-word -1)))))

(ert-deftest forward-word/dashcase/backwards ()
  (should (equal "hi-there |friends-who"
                 (caser//on-temp-buffer-point
                   "hi-there friends-who|"
                   (caser//forward-word -1)))))

(ert-deftest forward-word/snakecase/backwards ()
  (should (equal "hi_there |friends_who"
                 (caser//on-temp-buffer-point
                   "hi_there friends_who|"
                   (caser//forward-word -1)))))

(ert-deftest forward-word/one-back-starting-before-space/camelcase ()
  (should (equal "hi |thereYou friends"
                 (caser//on-temp-buffer-point
                   "hi thereYou| friends"
                   (caser//forward-word -1)))))

(ert-deftest forward-word/one-back-starting-before-space/snakecase ()
  (should (equal "hi |there_you friends"
                 (caser//on-temp-buffer-point
                   "hi there_you| friends"
                   (caser//forward-word -1)))))

(ert-deftest forward-word/one-back-starting-before-space/dashcase ()
  (should (equal "hi |there-you friends"
                 (caser//on-temp-buffer-point
                   "hi there-you| friends"
                   (caser//forward-word -1)))))

(ert-deftest forward-word/one-back-starting-after-space/camelcase ()
  (should (equal "hi |thereYou friends"
                 (caser//on-temp-buffer-point
                   "hi thereYou |friends"
                   (caser//forward-word -1)))))

(ert-deftest forward-word/one-back-starting-after-space/snakecase ()
  (should (equal "hi |there_you friends"
                 (caser//on-temp-buffer-point
                   "hi there_you |friends"
                   (caser//forward-word -1)))))

(ert-deftest forward-word/one-back-starting-after-space/dashcase ()
  (should (equal "hi |there-you friends"
                 (caser//on-temp-buffer-point
                   "hi there-you |friends"
                   (caser//forward-word -1)))))

(ert-deftest forward-word/one-back-starting-before-space/no-case ()
  (should (equal "hi there |you friends"
                 (caser//on-temp-buffer-point
                   "hi there you| friends"
                   (caser//forward-word -1)))))

(ert-deftest forward-word/one-back-starting-after-space/no-case ()
  (should (equal "hi there |you friends"
                 (caser//on-temp-buffer-point
                   "hi there you |friends"
                   (caser//forward-word -1)))))

(ert-deftest forward-word/two-back-starting-before-space/no-case ()
  (should (equal "hi |there you friends"
                 (caser//on-temp-buffer-point
                   "hi there you| friends"
                   (caser//forward-word -2)))))

(ert-deftest forward-word/two-back-starting-after-space/no-case ()
  (should (equal "hi |there you friends"
                 (caser//on-temp-buffer-point
                   "hi there you |friends"
                   (caser//forward-word -2)))))

(ert-deftest forward-word/two-back-starting-after-space/camelcase ()
  (should (equal "I say |hiThere allYou friends"
                 (caser//on-temp-buffer-point
                   "I say hiThere allYou |friends"
                   (caser//forward-word -2)))))

(ert-deftest forward-word/two-back-starting-after-space/dashcase ()
  (should (equal "I say |hi-there all-you friends"
                 (caser//on-temp-buffer-point
                   "I say hi-there all-you |friends"
                   (caser//forward-word -2)))))

(ert-deftest forward-word/two-back-starting-after-space/snakecase ()
  (should (equal "I say |hi_there all_you friends"
                 (caser//on-temp-buffer-point
                   "I say hi_there all_you |friends"
                   (caser//forward-word -2)))))

(ert-deftest forward-word/two-back-starting-before-space ()
  (should (equal "hi |thEre all-you friends"
                 (caser//on-temp-buffer-point
                   "hi thEre all-you| friends"
                   (caser//forward-word -2)))))

(ert-deftest forward-word/two-back-starting-after-space ()
  (should (equal "hi |thEre all-you friends"
                 (caser//on-temp-buffer-point
                   "hi thEre all-you |friends"
                   (caser//forward-word -2)))))

(ert-deftest forward-word/two-lowercase ()
  (should (equal "hi there| friends"
                 (caser//on-temp-buffer-point
                   "|hi there friends"
                   (caser//forward-word 2)))))


(ert-deftest forward-word/two-camelcase ()
  (should (equal "hiThere friendsWho| areHere"
                 (caser//on-temp-buffer-point
                   "|hiThere friendsWho areHere"
                   (caser//forward-word 2)))))

(ert-deftest forward-word/two-camelcase ()
  (should (equal "hi-there friends-who| are-here"
                 (caser//on-temp-buffer-point
                   "|hi-there friends-who are-here"
                   (caser//forward-word 2)))))

(ert-deftest forward-word/one-of-each ()
  (should (equal "hi-there friendsWho are_here| but not really"
                 (caser//on-temp-buffer-point
                   "|hi-there friendsWho are_here but not really"
                   (caser//forward-word 3)))))

(ert-deftest forward-word/through-special-characters ()
  (should (equal "hi +-_=there| friends"
                 (caser//on-temp-buffer-point
                   "|hi +-_=there friends"
                   (caser//forward-word 2)))))
