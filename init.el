(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation nil
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(python
     graphviz
     docker
     haskell
     (haskell :variables haskell-enable-hindent-style "chris-done")
     auto-completion (haskell :variables
                              haskell-completion-backend 'intero)
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 0.2
                      auto-completion-private-snippets-directory nil)
     c-c++
     html
     javascript
     helm
     emacs-lisp
     git
     markdown
     org
     ansible
     clojure
     csharp
     scala
     pdf-tools
     (erc :variables
          erc-nick "huss"
          erc-prompt-for-password nil
          erc-prompt-for-nickserv-password nil
          erc-highlight-notice '("JOIN" "QUIT")
          erc-hide-list '("PART" "QUIT")
          erc-autojoin-channels-alist '(("freenode.net" "#haskell")))
     treemacs
     theming
     xkcd
     gnus
     (shell :variables
            shell-default-shell 'eshell
            shell-default-position 'bottom
            shell-default-height 30)
     )
   dotspacemacs-additional-packages '(
                                      all-the-icons
                                      ;; spaceline-all-the-icons
                                      centered-window-mode
                                      dracula-theme
                                      pretty-mode
                                      dash
                                      groovy-mode
                                      legalese
                                      processing-mode
                                      )
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(vi-tilde-fringe)
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Function that will be called before loading packages etc..."
  (init/vars)
  (init/proxy))

(defun dotspacemacs/user-init ()
  "Avoid custom-vars to be set in init.el file"
  (setq custom-file "~/.spacemacs.d/custom.el")
  (if (not (file-exists-p custom-file))
      (write-region "" nil custom-file)
    (load-file custom-file)))

(defun dotspacemacs/user-config ()
  "Custom user configuration, doing all the displaying stuff after package are loaded."
  (user-config/email)
  (user-config/pretty)
  (user-config/editing)
  (user-config/icons)
  (user-config/legalese)
  (user-config/csharp)
  (user-config/java)
  (user-config/layout)
  (user-config/magit))

(defun init/vars ()
  "General variable configurations."
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'random
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-colorize-cursor-according-to-state nil
   dotspacemacs-default-font '("Consolas"
                               :size 17
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Olympe"
   dotspacemacs-display-default-layout t
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.6
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling nil
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server t
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil
   dotspacemacs-themes '(dracula)
   ))

(defun init/proxy ()
  "Load the proxy configuration if defined."
  (when (file-exists-p "~/.spacemacs.d/proxy.el")
    (load-file "~/.spacemacs.d/proxy.el")))

(defun user-config/icons ())

(defun user-config/csharp ()
  (setq-default omnisharp-server-executable-path "~/omnisharp/run"))

(defun user-config/java ()
  (setq eclim-eclipse-dirs '("~/eclipse")
        eclim-executable "~/eclipse/eclim"
        eclimd-executable "~/eclipse/eclimd"
        eclimd-wait-for-process t))

(defun user-config/magit ()
  (load-file "~/.spacemacs.d/magit-gerrit.el"))

(defun user-config/layout ()
  (when (string= "*scratch*" (buffer-name))
    (spacemacs/switch-to-scratch-buffer))
  (golden-ratio-mode)
  (setq golden-ratio-exclude-modes '(eshell-mode)))

(defun user-config/email ()
  (add-hook 'gnus-message-setup-hook 'mml-secure-message-sign-pgpmime)
  (setq message-signature t)
  (setq mm-verify-option 'always)
  (setq gnus-always-read-dribble-file t)
  (setq epa-file-cache-passphrase-for-symmetric-encryption t)
  (setq shr-color-visible-luminance-min 80)
  (setq user-full-name "Hussein Ait-Lahcen"
        user-mail-address "hussein.aitlahcen@gmail.com")
  (setq gnus-secondary-select-methods
        '(
          (nnimap "gmail"
                  (nnimap-address
                   "imap.gmail.com")
                  (nnimap-server-port 993)
                  (nnimap-stream ssl))
          ))
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'ssl
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 465)
  (setq gnus-message-archive-method '(nnimap "imap.gmail.com")
        gnus-message-archive-group "[Gmail]/Sent Mail")
  (setq nnml-directory "~/gmail")
  (setq message-directory "~/gmail"))

(defun user-config/editing ()
  ;; Line numbers
  (when (not (version<= emacs-version "26"))
    (setq display-line-numbers-type 'absolute)
    (custom-set-faces '(line-number ((t (:foreground "dim gray")))))
    (custom-set-faces '(line-number-current-line ((t (:foreground "white")))))
    (add-hook 'prog-mode-hook 'display-line-numbers-mode)
    (add-hook 'yaml-mode-hook 'display-line-numbers-mode))

  ;; Auto complete disabled in eshell
  (spacemacs|disable-company eshell-mode)
  (spacemacs|disable-company erc-mode)

  ;; Cursor
  (global-evil-mc-mode t)
  (blink-cursor-mode t)
  (setq evil-insert-state-cursor '((bar . 4) "white")
        evil-normal-state-cursor '(box "white"))

  ;; Org-mode
  (setq org-default-notes-file (concat org-directory "/notes.org"))

  ;; Bindings
  (setq evil-escape-key-sequence "dk")
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

  ;; Display
  (global-whitespace-mode t)
  (setq whitespace-global-modes '(not erc-mode))
  (setq whitespace-line-column 500)
  (setq whitespace-display-mappings
        '((newline-mark 10 [182 10])
          (space-mark 32 [183] [46])))
  (set-face-foreground 'whitespace-newline "#505050")
  (set-face-foreground 'whitespace-tab "#505050")
  (set-face-background 'whitespace-tab 'nil)
  (set-face-foreground 'whitespace-space"#454545")
  (set-face-background 'whitespace-space 'nil))

(defun user-config/pretty ()
  (load-file "~/.spacemacs.d/pretty-fonts.el")
  (load-file "~/.spacemacs.d/pretty-eshell.el")
  (load-file "~/.spacemacs.d/pretty-magit.el")
  (setq powerline-default-separator nil)
  (pretty-fonts-set-kwds
   '((pretty-fonts-fira-font prog-mode-hook org-mode-hook)))
  (pretty-fonts-set-fontsets
   '(("fontawesome"
      #xf07c #xf0c9 #xf0c4 #xf0cb #xf017 #xf101)
     ("all-the-icons"
      #xe907 #xe928)
     ("github-octicons"
      #xf091 #xf059 #xf076 #xf075 #xe192  #xf016)
     ("material icons"
      #xe871 #xe918 #xe3e7
      #xe3d0 #xe3d1 #xe3d2 #xe3d4)
     ("Symbola"
      #x1d54a #x2a02 #x2205 #x27fb #x27fc #x2299 #x1d54b #x1d53d
      #x1d539 #x1d507 #x1d517))))

(defun user-config/legalese ()
  (setq legalese-default-copyright "Hussein Ait-Lahcen")
  (setq legalese-default-author "Hussein Ait-Lahcen <hussein.aitlahcen@gmail.com>"))
