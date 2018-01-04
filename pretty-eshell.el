(require 'dash)
(require 's)

(defmacro with-face (STR &rest PROPS)
  "Return STR propertized with PROPS."
  `(propertize ,STR 'face (list ,@PROPS)))

(defmacro esh-section (NAME ICON FORM &rest PROPS)
  "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
  `(setq ,NAME
         (lambda () (when ,FORM
                 (-> ,ICON
                    (concat esh-section-delim ,FORM)
                    (with-face ,@PROPS))))))

(defun esh-acc (acc x)
  "Accumulator for evaluating and concatenating esh-sections."
  (--if-let (funcall x)
      (if (s-blank? acc)
          it
        (concat acc esh-sep it))
    acc))

(defun esh-prompt-func ()
  "Build `eshell-prompt-function'"
  (concat esh-header
          (-reduce-from 'esh-acc "" eshell-funcs)
          "\n"
          eshell-prompt-string))

(esh-section esh-dir
             "\xf114"  ; (faicon folder)
             (abbreviate-file-name (eshell/pwd))
             '(:foreground "yellow" :bold ultra-bold :underline t))

(esh-section esh-git
             "\xf09b"  ; (git icon)
             (magit-get-current-branch)
             '(:foreground "cornflower blue"))

(esh-section esh-clock
             "\xf253"  ; (clock icon)
             (format-time-string "%H:%M" (current-time))
             '(:foreground "forest green"))

;; Separator between esh-sections
(setq esh-sep " | ")  ; or " | "

;; Separator between an esh-section icon and form
(setq esh-section-delim " ")

;; Eshell prompt header
(setq esh-header "\n")  ; or "\n┌─"

;; Eshell prompt regexp and string. Unless you are varying the prompt by eg.
;; your login, these can be the same.
(setq eshell-prompt-regexp "\n\xf178 ")   ; or "└─> "
(setq eshell-prompt-string "\n\xf178 ")   ; or "└─> "

;; Choose which eshell-funcs to enable
(setq eshell-funcs (list esh-dir esh-git esh-clock))

;; Enable the new eshell prompt
(setq eshell-prompt-function 'esh-prompt-func)
