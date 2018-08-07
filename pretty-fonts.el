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
    ("\\(<|\\)"     #Xe14d)
    ("\\(<>\\)"     #Xe15b)
    ("\\(<|>\\)"     #Xe14e)
    ("[^<]\\(|>\\)" #Xe135)

    ;; Brackets
    ("\\(<\\*>\\)" #Xe14c)
    ("\\(<\\*\\)"  #Xe14b)
    ("\\(\\*>\\)"  #Xe104)
    ("\\(<\\$>\\)" #Xe150)
    ("\\(<\\$\\)"  #Xe14f)
    ("\\(\\$>\\)"  #Xe137)
    ("\\(<\\+>\\)" #Xe156)
    ("\\(<\\+\\)"  #Xe155)
    ("\\(\\+>\\)"  #Xe13a)

    ;; Equality
    ("\\(!=\\)"         #Xe10e)
    ("\\(!==\\)"         #Xe10f)
    ("\\(=/=\\)"         #Xe143)
    ("[^=]\\(/=\\)"      #Xe12c)
    ("\\(/==\\)"         #Xe12d)
    ("\\(===\\)"         #Xe13d)
    ("[^!/=]\\(==\\)[^=]" #Xe13c)

    ;; Equality Special
    ("\\(||=\\)"     #Xe133)
    ("[^|]\\(|=\\)" #Xe134)
    ("\\(~=\\)"     #Xe166)
    ("\\(\\^=\\)"   #Xe136)
    ("\\(=:=\\)"     #Xe13b)

    ;; Comparisons
    ("\\(<==\\)"        #Xe158)
    ("\\(<<=\\)"        #Xe15e)
    ("[^<]\\(<=\\)[^=]" #Xe141)
    ("\\(>=\\)"        #Xe145)

    ;; Shifts
    ("[^-=]\\(>>\\)"    #Xe147)
    ("\\(>>>\\)"         #Xe14a)
    ("[^-=]\\(<<\\)[^=]" #Xe15c)
    ("\\(<<<\\)"         #Xe15f)

    ;; Dots
    ("\\(\\.-\\)"     #Xe122)
    ("\\(\\.=\\)"     #Xe123)
    ("\\(\\.\\.<\\)" #Xe125)

    ;; Hashes
    ("\\( #{\\)"    #Xe119)
    ("\\( #(\\)"    #Xe11e)
    ("\\( #_\\)"    #Xe120)
    ("\\( #_(\\)"    #Xe121)
    ("\\( #\\?\\)" #Xe11f)
    ("\\( #\\[\\)" #Xe11a)

    ;; 2+3-Repeats
    ("\\({-\\)" #Xe108)
    ("\\(-}\\)" #Xe110)

    ;; ARROWS
    ;; Direct
    ("[^-]\\(->\\)" #Xe114)
    ("[^=]\\(=>\\)"  #Xe13f)
    ("\\(<-\\)"     #Xe152)
    ("\\(-->\\)"     #Xe113)
    ("\\(->>\\)"     #Xe115)
    ("\\(==>\\)"     #Xe13e)
    ("\\(=>>\\)"     #Xe140)
    ("\\(<--\\)"     #Xe153)
    ("\\(<<-\\)"     #Xe15d)
    ("\\(<->\\)"     #Xe154)
    ("\\(<=>\\)"     #Xe159)

    ;; Branches
    ("\\(-<\\)" #Xe116)
    ("\\(-<<\\)" #Xe117)
    ("\\(>-\\)" #Xe144)
    ("\\(>>-\\)" #Xe148)
    ("\\(=<<\\)" #Xe142)
    ("\\(>>=\\)" #Xe149)
    ("\\(>=>\\)" #Xe146)
    ("\\(<=<\\)" #Xe15a)

    ;; Squiggly
    ("\\(<~\\)"  #Xe160)
    ("\\(<~~\\)"  #Xe161)
    ("\\(~>\\)"  #Xe167)
    ("\\(~~>\\)" #Xe169)
    ("\\(-~\\)"  #Xe118)
    ("\\(~-\\)"  #Xe165)

    ;; Math
    (" \\(\\.\\) " #X2218)
    ("\\(forall\\)"     #X2200)
    ("\\(\\\\\\)"  #X3bb)

    ;; MISC
    ("\\(www\\)"     #Xe100)
    ("\\(<!--\\)"     #Xe151)
    ("\\(~@\\)"     #Xe164)
    ("[^<]\\(~~\\)" #Xe168)
    ("\\(\\?=\\)"   #Xe127)
    ("[^=]\\(:=\\)"  #Xe10c))
  "Fira font ligatures and their regexes")

(set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
