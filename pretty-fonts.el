(require 'dash)
(provide 'pretty-fonts)

;;;###autoload
(defun pretty-fonts-set-fontsets (CODE-FONT-ALIST)
  "Utility to associate many unicode points with specified fonts."
  (--each CODE-FONT-ALIST
    (-let (((font . codes) it))
      (--each codes
        (set-fontset-font t `(,it . ,it) font)))))

;;;###autoload
(defun pretty-fonts--add-kwds (FONT-LOCK-ALIST)
  "Exploits `font-lock-add-keywords' to apply regex-unicode replacements."
  (font-lock-add-keywords
   nil (--map (-let (((rgx uni-point) it))
               `(,rgx (0 (progn
                           (compose-region
                            (match-beginning 1) (match-end 1)
                            ,(concat "\t" (list uni-point)))
                           nil))))
             FONT-LOCK-ALIST)))

;;;###autoload
(defmacro pretty-fonts-set-kwds (FONT-LOCK-HOOKS-ALIST)
  "Set regex-unicode replacements to many modes."
  `(--each ,FONT-LOCK-HOOKS-ALIST
     (-let (((font-locks . mode-hooks) it))
       (--each mode-hooks
         (add-hook it (-partial 'pretty-fonts--add-kwds
                                (symbol-value font-locks)))))))

;;; Fira Font
(defconst pretty-fonts-fira-font
  '(
    ;; OPERATORS
    ;; Pipes
    ("\\(<|\\)" #Xe14d)
    ("\\(|>\\)" #Xe135)
    ("\\(<|>\\)" #Xe14e)
    ("\\(<>\\)" #Xe15b)

    ;; Brackets
    ("\\(<\\*\\)"  #Xe14b)
    ("\\(\\*>\\)"  #Xe104)
    ("\\(<\\*>\\)" #Xe14c)
    ("\\(<\\$\\)"  #Xe14f)
    ("\\(\\$>\\)"  #Xe137)
    ("\\(<\\$>\\)" #Xe150)
    ("\\(<\\+\\)"  #Xe155)
    ("\\(\\+>\\)"  #Xe13a)
    ("\\(<\\+>\\)" #Xe156)

    ;; Equality
    ("\\(/=\\)" #Xe12c)
    ("\\(!=\\)" #Xe10e)
    ("\\(!==\\)" #Xe10f)
    ("\\(=/=\\)" #Xe143)
    ("\\(/==\\)" #Xe12d)
    ("\\(==\\)" #Xe13c)
    ("\\(===\\)" #Xe13d)

    ;; Equality Special
    ("\\(\\^=\\)" #Xe136)
    ("\\(|=\\)"   #Xe134)
    ("\\(||=\\)"   #Xe133)
    ("\\(~=\\)"   #Xe166)
    ("\\(:=\\)"   #Xe10c)
    ("\\(=:=\\)"   #Xe13b)

    ;; Comparisons
    ("\\(<=\\)" #Xe141)
    ("\\(>=\\)" #Xe145)

    ;; Shifts
    ("\\(>>\\)" #Xe147)
    ("\\(<<\\)" #Xe15c)
    ("\\(>>>\\)" #Xe14a)
    ("\\(<<<\\)" #Xe15f)

    ;; Dots
    ("\\(\\.-\\)"     #Xe122)
    ("\\(\\.=\\)"     #Xe123)
    ("\\(\\.\\.<\\)" #Xe125)

    ;; Hashes
    ("\\( #{\\)"    #Xe119)
    ("\\( #(\\)"    #Xe11e)
    ("\\( #_\\)"    #Xe120)
    ("\\( #_(\\)"    #Xe121)
    ("\\(#\\?\\)" #Xe11f)
    ("\\(#\\[\\)" #Xe11a)

    ;; 2+3-Repeats
    ("\\({-\\)" #Xe108)
    ("\\(-}\\)" #Xe110)

    ;; ARROWS
    ;; Direct
    ("\\(->\\)" #Xe114)
    ("\\(<-\\)" #Xe152)
    ("\\(=>\\)" #Xe13f)
    ("\\(-->\\)" #Xe113)
    ("\\(->>\\)" #Xe115)
    ("\\(==>\\)" #Xe13e)
    ("\\(=>>\\)" #Xe140)
    ("\\(<--\\)" #Xe153)
    ("\\(<<-\\)" #Xe15d)
    ("\\(<->\\)" #Xe154)
    ("\\(<=>\\)" #Xe159)
    ("\\(<==\\)" #Xe158)
    ("\\(<<=\\)" #Xe15e)

    ;; Branches
    ("\\(>-\\)" #Xe144)
    ("\\(-<\\)" #Xe116)
    ("\\(-<<\\)" #Xe117)
    ("\\(>>-\\)" #Xe148)
    ("\\(=<<\\)" #Xe142)
    ("\\(>>=\\)" #Xe149)
    ("\\(>=>\\)" #Xe146)
    ("\\(<=<\\)" #Xe15a)

    ;; Squiggly
    ("\\(<~\\)"  #Xe160)
    ("\\(~>\\)"  #Xe167)
    ("\\(-~\\)"  #Xe118)
    ("\\(~-\\)"  #Xe165)
    ("\\(~~\\)"   #Xe168)
    ("\\(~~>\\)" #Xe169)
    ("\\(<~~\\)"  #Xe161)

    ;; Math
    (" \\(\\.\\) " #X2218)
    ("\\(forall\\)"     #X2200)
    ("\\(\\\\\\)"  #X3bb)

    ;; MISC
    ("\\(www\\)"   #Xe100)
    ("\\(<!--\\)"   #Xe151)
    ("\\(~@\\)"   #Xe164)
    ("\\(\\?=\\)" #Xe127))
  "Fira font ligatures and their regexes")

(set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
