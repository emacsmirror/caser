;;camelcase tests
;;region tests
(ert-deftest camelcase-region/no-change ()
  (should (equal "hi|"
                 (on-temp-buffer-point
                   "hi"
                   (camelcase-region 1 3)))))

(ert-deftest camelcase-region/from-snakecase/one-word ()
  (should (equal "hiMom|"
                 (on-temp-buffer-point
                   "hi_mom"
                   (camelcase-region 1 7)))))

(ert-deftest camelcase-region/from-dashcase/one-word ()
  (should (equal "hiMom|"
                 (on-temp-buffer-point
                   "hi-mom"
                   (camelcase-region 1 7)))))

;;word tests
(ert-deftest camelcase-word/from-snakecase ()
  (should (equal "abCd| ef_gh"
                 (on-temp-buffer-point
                   "|ab_cd ef_gh"
                   (camelcase-word 1)))))

(ert-deftest camelcase-word/from-dashcase ()
  (should (equal "abCd| ef-gh"
                 (on-temp-buffer-point
                   "|ab-cd ef-gh"
                   (camelcase-word 1)))))

(ert-deftest camelcase-word/called-twice ()
  (should (equal "hiMom andOther| stuff_here"
                 (on-temp-buffer-point
                   "hi_mom and_other stuff_here"
                   (camelcase-word 2)))))

(ert-deftest camelcase-word/snakecase-and-lispcase ()
  (should (equal "hiMom andOther| stuff_here"
                 (on-temp-buffer-point
                   "hi_mom and-other stuff_here"
                   (camelcase-word 2)))))


;;dwim tests
(ert-deftest camelcase-dwim/single-word-doesnt-change ()
  (should (equal "hi"
                 (on-temp-buffer
                   "hi"
                   (camelcase-dwim 1)))))

(ert-deftest camelcase-dwim/from-snakecase ()
  (should (equal "hiMom|"
                 (on-temp-buffer-point
                   "hi_mom"
                   (camelcase-dwim 1)))))

(ert-deftest camelcase-dwim/from-dashcase ()
  (should (equal "hiMom|"
                 (on-temp-buffer-point
                   "hi-mom"
                   (camelcase-dwim 1)))))

(ert-deftest camelcase-dwim/snakecase-two-arg ()
  (should (equal "hiMom andOther| stuff_here"
                 (on-temp-buffer-point
                   "hi_mom and_other stuff_here"
                   (camelcase-dwim 2)))))

(ert-deftest camelcase-dwim/snakecase-three-arg-complicated-words ()
  (should (equal "hiThere thisIsWayFun youGuys| and_more"
                 (on-temp-buffer-point
                   "|hi_there this_is_way_fun you_guys and_more"
                   (camelcase-dwim 3)))))

;;snakecase_tests
;;region tests
(ert-deftest snakecase-region/no-change ()
  (should (equal "hi|"
                 (on-temp-buffer-point
                   "hi"
                   (snakecase-region 1 3)))))

(ert-deftest snakecase-region/from-camelcase/one-word ()
  (should (equal "hi_mom|"
                 (on-temp-buffer-point
                   "hiMom"
                   (snakecase-region 1 7)))))

(ert-deftest snakecase-region/from-dashcase/one-word ()
  (should (equal "hi_mom|"
                 (on-temp-buffer-point
                   "hi-mom"
                   (snakecase-region 1 7)))))

(ert-deftest snakecase-region/from-camelcase/multiple-caps/middle-of-word ()
  (should (equal "my_ip_address|"
                 (on-temp-buffer-point
                   "myIPAddress"
                   (snakecase-region (point-min) (point-max))))))

(ert-deftest snakecase-region/from-camelcase/multiple-caps/end-of-word ()
  (should (equal "my_ip|"
                 (on-temp-buffer-point
                   "myIP"
                   (snakecase-region (point-min) (point-max))))))

;;word tests
(ert-deftest snakecase-word/from-camelcase ()
  (should (equal "ab_cd| efGh"
                 (on-temp-buffer-point
                   "|abCd efGh"
                   (snakecase-word 1)))))

(ert-deftest snakecase-word/from-dashcase ()
  (should (equal "ab_cd| ef-gh"
                 (on-temp-buffer-point
                   "|ab-cd ef-gh"
                   (snakecase-word 1)))))

(ert-deftest snakecase-word/called-twice ()
  (should (equal "hi_mom and_other| stuffHere"
                 (on-temp-buffer-point
                   "hiMom andOther stuffHere"
                   (snakecase-word 2)))))

(ert-deftest snakecase-word/camelcase-and-dashcase ()
  (should (equal "hi_mom and_other| stuff_here"
                 (on-temp-buffer-point
                   "hiMom and-other stuff_here"
                   (snakecase-word 2)))))

;;dwim tests
(ert-deftest snakecase-dwim/single-word-doesnt-change ()
  (should (equal "hi"
                 (on-temp-buffer
                   "hi"
                   (snakecase-dwim 1)))))

(ert-deftest snakecase-dwim/from-camelcase ()
  (should (equal "hi_mom|"
                 (on-temp-buffer-point
                   "hiMom"
                   (snakecase-dwim 1)))))

(ert-deftest snakecase-dwim/from-dashcase ()
  (should (equal "hi_mom|"
                 (on-temp-buffer-point
                   "hi-mom"
                   (snakecase-dwim 1)))))

(ert-deftest snakecase-dwim/camelcase-two-arg ()
  (should (equal "hi_mom and_other| stuffHere"
                 (on-temp-buffer-point
                   "hiMom andOther stuffHere"
                   (snakecase-dwim 2)))))
;;dashcase-tests
;; region tests
(ert-deftest dashcase-region/no-change ()
  (should (equal "hi|"
                 (on-temp-buffer-point
                   "hi"
                   (dashcase-region 1 3)))))

(ert-deftest dashcase-region/from-camelcase/one-word ()
  (should (equal "hi-mom|"
                 (on-temp-buffer-point
                   "hiMom"
                   (dashcase-region 1 7)))))

(ert-deftest dashcase-region/from-snakecase/one-word ()
  (should (equal "hi-mom|"
                 (on-temp-buffer-point
                   "hi-mom"
                   (dashcase-region 1 7)))))

(ert-deftest dashcase-region/from-camelcase/multiple-caps/middle-of-word ()
  (should (equal "my-ip-address|"
                 (on-temp-buffer-point
                   "myIPAddress"
                   (dashcase-region (point-min) (point-max))))))

(ert-deftest dashcase-region/from-camelcase/multiple-caps/end-of-word ()
  (should (equal "my-ip|"
                 (on-temp-buffer-point
                   "myIP"
                   (dashcase-region (point-min) (point-max))))))

;; word tests
(ert-deftest dashcase-word/from-camelcase ()
  (should (equal "ab-cd| efGh"
                 (on-temp-buffer-point
                   "|abCd efGh"
                   (dashcase-word 1)))))

(ert-deftest dashcase-word/from-snakecase ()
  (should (equal "hi-mom| and_other stuff_here"
                 (on-temp-buffer-point
                   "hi_mom and_other stuff_here"
                   (dashcase-word 1)))))

(ert-deftest dashcase-word/called-twice ()
  (should (equal "hi-mom and-other| stuffHere"
                 (on-temp-buffer-point
                   "hiMom andOther stuffHere"
                   (dashcase-word 2)))))

(ert-deftest dashcase-word/camelcase-and-snakecase ()
  (should (equal "hi-mom and-other| stuff_here"
                 (on-temp-buffer-point
                   "hiMom and_other stuff_here"
                   (dashcase-word 2)))))


;; dwim tests
(ert-deftest dashcase-dwim/single-word-doesnt-change ()
  (should (equal "hi"
                 (on-temp-buffer
                   "hi"
                   (dashcase-dwim 1)))))

(ert-deftest dashcase-dwim/from-camelcase ()
  (should (equal "hi-mom|"
                 (on-temp-buffer-point
                   "hiMom"
                   (dashcase-dwim 1)))))

(ert-deftest dashcase-dwim/from-snakecase ()
  (should (equal "hi-mom|"
                 (on-temp-buffer-point
                   "hi_mom"
                   (dashcase-dwim 1)))))

(ert-deftest dashcase-dwim/camelcase-two-arg ()
  (should (equal "hi-mom and-other| stuffHere"
                 (on-temp-buffer-point
                   "hiMom andOther stuffHere"
                   (dashcase-dwim 2)))))

;;forward-word
(ert-deftest forward-word/all-lowercase ()
  (should (equal "hi| there friends"
                 (on-temp-buffer-point
                   "|hi there friends"
                   (caser/forward-word 1)))))

(ert-deftest forward-word/starting-end-of-line ()
  (should (equal "hi there
you| all"
                 (on-temp-buffer-point
                   "hi there|
you all"
                   (caser/forward-word 1)))))

(ert-deftest forward-word/multiple-newlines ()
  (should (equal "hi there



you| all"
                 (on-temp-buffer-point
                   "hi there|



you all"
                   (caser/forward-word 1)))))

(ert-deftest forward-word/backward/multiple-newlines ()
  (should (equal "hi |there



you all"
                 (on-temp-buffer-point
                   "hi there



|you all"
                   (caser/forward-word -1)))))

(ert-deftest forward-word/ending-end-of-line ()
  (should (equal "hi there|
you all"
                 (on-temp-buffer-point
                   "hi| there
you all"
                   (caser/forward-word 1)))))

(ert-deftest forward-word/backwards/starting-end-of-line ()
  (should (equal "hi |there
you all"
                 (on-temp-buffer-point
                   "hi there|
you all"
                   (caser/forward-word -1)))))

(ert-deftest forward-word/backwards/ending-start-of-line ()
  (should (equal "hi there
|you all"
                 (on-temp-buffer-point
                   "hi there
you| all"
                   (caser/forward-word -1)))))

(ert-deftest forward-word/end-of-buffer ()
  (should (equal "hi there|"
                 (on-temp-buffer-point
                   "hi| there"
                   (caser/forward-word 1)))))

(ert-deftest forward-word/backwards/start-of-buffer ()
  (should (equal "|hi there"
                 (on-temp-buffer-point
                   "hi| there"
                   (caser/forward-word -1)))))

(ert-deftest forward-word/through-line-end ()
  (should (equal "hi there
you| all"
                 (on-temp-buffer-point
                   "hi| there
you all"
                   (caser/forward-word 2)))))

(ert-deftest forward-word/camelcase ()
  (should (equal "hiThere| friends"
                 (on-temp-buffer-point
                   "|hiThere friends"
                   (caser/forward-word 1)))))

(ert-deftest forward-word/dashcase ()
  (should (equal "hi-there| friends-who"
                 (on-temp-buffer-point
                   "|hi-there friends-who"
                   (caser/forward-word 1)))))

(ert-deftest forward-word/snakecase ()
  (should (equal "hi_there| friends_who"
                 (on-temp-buffer-point
                   "|hi_there friends_who"
                   (caser/forward-word 1)))))

(ert-deftest forward-word/camelcase/backwards ()
  (should (equal "hiThere |friendsWho"
                 (on-temp-buffer-point
                   "hiThere friendsWho|"
                   (caser/forward-word -1)))))

(ert-deftest forward-word/dashcase/backwards ()
  (should (equal "hi-there |friends-who"
                 (on-temp-buffer-point
                   "hi-there friends-who|"
                   (caser/forward-word -1)))))

(ert-deftest forward-word/snakecase/backwards ()
  (should (equal "hi_there |friends_who"
                 (on-temp-buffer-point
                   "hi_there friends_who|"
                   (caser/forward-word -1)))))

(ert-deftest forward-word/one-back-starting-before-space/camelcase ()
  (should (equal "hi |thereYou friends"
                 (on-temp-buffer-point
                   "hi thereYou| friends"
                   (caser/forward-word -1)))))

(ert-deftest forward-word/one-back-starting-before-space/snakecase ()
  (should (equal "hi |there_you friends"
                 (on-temp-buffer-point
                   "hi there_you| friends"
                   (caser/forward-word -1)))))

(ert-deftest forward-word/one-back-starting-before-space/dashcase ()
  (should (equal "hi |there-you friends"
                 (on-temp-buffer-point
                   "hi there-you| friends"
                   (caser/forward-word -1)))))

(ert-deftest forward-word/one-back-starting-after-space/camelcase ()
  (should (equal "hi |thereYou friends"
                 (on-temp-buffer-point
                   "hi thereYou |friends"
                   (caser/forward-word -1)))))

(ert-deftest forward-word/one-back-starting-after-space/snakecase ()
  (should (equal "hi |there_you friends"
                 (on-temp-buffer-point
                   "hi there_you |friends"
                   (caser/forward-word -1)))))

(ert-deftest forward-word/one-back-starting-after-space/dashcase ()
  (should (equal "hi |there-you friends"
                 (on-temp-buffer-point
                   "hi there-you |friends"
                   (caser/forward-word -1)))))

(ert-deftest forward-word/one-back-starting-before-space/no-case ()
  (should (equal "hi there |you friends"
                 (on-temp-buffer-point
                   "hi there you| friends"
                   (caser/forward-word -1)))))

(ert-deftest forward-word/one-back-starting-after-space/no-case ()
  (should (equal "hi there |you friends"
                 (on-temp-buffer-point
                   "hi there you |friends"
                   (caser/forward-word -1)))))

(ert-deftest forward-word/two-back-starting-before-space/no-case ()
  (should (equal "hi |there you friends"
                 (on-temp-buffer-point
                   "hi there you| friends"
                   (caser/forward-word -2)))))

(ert-deftest forward-word/two-back-starting-after-space/no-case ()
  (should (equal "hi |there you friends"
                 (on-temp-buffer-point
                   "hi there you |friends"
                   (caser/forward-word -2)))))

(ert-deftest forward-word/two-back-starting-after-space/camelcase ()
  (should (equal "I say |hiThere allYou friends"
                 (on-temp-buffer-point
                   "I say hiThere allYou |friends"
                   (caser/forward-word -2)))))

(ert-deftest forward-word/two-back-starting-after-space/dashcase ()
  (should (equal "I say |hi-there all-you friends"
                 (on-temp-buffer-point
                   "I say hi-there all-you |friends"
                   (caser/forward-word -2)))))

(ert-deftest forward-word/two-back-starting-after-space/snakecase ()
  (should (equal "I say |hi_there all_you friends"
                 (on-temp-buffer-point
                   "I say hi_there all_you |friends"
                   (caser/forward-word -2)))))

(ert-deftest forward-word/two-back-starting-before-space ()
  (should (equal "hi |thEre all-you friends"
                 (on-temp-buffer-point
                   "hi thEre all-you| friends"
                   (caser/forward-word -2)))))

(ert-deftest forward-word/two-back-starting-after-space ()
  (should (equal "hi |thEre all-you friends"
                 (on-temp-buffer-point
                   "hi thEre all-you |friends"
                   (caser/forward-word -2)))))

(ert-deftest forward-word/two-lowercase ()
  (should (equal "hi there| friends"
                 (on-temp-buffer-point
                   "|hi there friends"
                   (caser/forward-word 2)))))


(ert-deftest forward-word/two-camelcase ()
  (should (equal "hiThere friendsWho| areHere"
                 (on-temp-buffer-point
                   "|hiThere friendsWho areHere"
                   (caser/forward-word 2)))))

(ert-deftest forward-word/two-camelcase ()
  (should (equal "hi-there friends-who| are-here"
                 (on-temp-buffer-point
                   "|hi-there friends-who are-here"
                   (caser/forward-word 2)))))

(ert-deftest forward-word/one-of-each ()
  (should (equal "hi-there friendsWho are_here| but not really"
                 (on-temp-buffer-point
                   "|hi-there friendsWho are_here but not really"
                   (caser/forward-word 3)))))

(ert-deftest forward-word/through-special-characters ()
  (should (equal "hi +-_=there| friends"
                 (on-temp-buffer-point
                   "|hi +-_=there friends"
                   (caser/forward-word 2)))))
