(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation nil
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers '(typescript
                                       python
                                       ocaml
                                       yaml
                                       purescript
                                       themes-megapack
                                       c-c++
                                       html
                                       nixos
                                       javascript
                                       helm
                                       emacs-lisp
                                       git
                                       markdown
                                       pdf
                                       neotree
                                       theming
                                       gnus
                                       slack
                                       ocaml
                                       agda
                                       floobits
                                       (typescript :variables typescript-indent-level 2)
                                       (multiple-cursors :variables mutliple-cursors-backend 'evil-mc)
                                       (rust :variables rust-backend 'lsp)
                                       (lsp :variables lsp-rust-server 'rust-analyzer)
                                       (haskell :variables haskell-completion-backend 'lsp)
                                       (lsp :variables lsp-use-lsp-ui t)
                                       (shell :variables
                                              shell-default-shell 'eshell
                                              shell-default-position 'bottom
                                              shell-default-height 40))
   dotspacemacs-additional-packages '((dhall-mode
                                       :location (recipe
                                                  :repo "psibi/dhall-mode"
                                                  :fetcher github
                                                  :files ("dhall-mode.el")))
                                      (michelson-mode :location (recipe :fetcher url
                                                                        :url "https://gitlab.com/tezos/tezos/-/raw/master/emacs/michelson-mode.el"))
                                      pandoc-mode
                                      all-the-icons
                                      eglot
                                      (helm-ls-git :location "/home/hussein/github/helm-ls-git")
                                      legalese)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(vi-tilde-fringe
                                    spaceline)
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Function that will be called before loading packages etc..."
  (init/vars)
  (init/proxy))

(defun dotspacemacs/user-init ()
  "Avoid custom-vars to be set in init.el file"
  (push '("melpa-stable" . "stable.melpa.org/packages/") configuration-layer-elpa-archives)
  (setq custom-file "~/.spacemacs.d/custom.el")
  (if (not (file-exists-p custom-file))
      (write-region "" nil custom-file)
    (load-file custom-file)))

(defun dotspacemacs/user-config ()
  "Custom user configuration, doing all the displaying stuff after package are loaded."
  (user-config/email)
  (user-config/pretty)
  (user-config/icons)
  (user-config/editing)
  (user-config/legalese)
  (user-config/layout))

(defun init/vars ()
  "General variable configurations."
  (setq-default
   dotspacemacs-elpa-https nil
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner '998
   dotspacemacs-startup-lists '((recents . 10)
                                (projects . 10))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-colorize-cursor-according-to-state nil
   dotspacemacs-default-font '("Fira Code"
                               :size 18)
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
   dotspacemacs-mode-line-theme 'vanilla
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
   dotspacemacs-themes '(dichromacy)))

(defun init/proxy ()
  "Load the proxy configuration if defined."
  (when (file-exists-p "~/.spacemacs.d/proxy.el")
    (load-file "~/.spacemacs.d/proxy.el")))

(defun user-config/icons ()
  (setq neo-theme 'icons))

(defun user-config/layout ()
  (setq projectile-mode-line "Projectile")
  (custom-set-faces
    '(helm-grep-file ((t (:foreground "DarkGray" :underline t))))
    '(helm-grep-finish ((t (:foreground "dim gray"))))
    '(helm-grep-lineno ((t (:foreground "LightCoral"))))
    '(helm-grep-match ((t (:background "beige" :foreground "black"))))
    '(helm-match ((t (:background "beige" :foreground "dim gray")))))
  (setq evil-insert-state-cursor '((bar . 4) "red")
        evil-normal-state-cursor '(box "blue")
        evil-replace-state-cursor '(hollow "black")
        evil-visual-state-cursor '(box "black")
        evil-iedit-state-cursor '(box "deep pink")))

(defun user-config/email ()
  (add-hook 'gnus-message-setup-hook 'mml-secure-message-sign-pgpmime)
  (setq gnus-message-replysign t
        message-signature t
        mm-text-html-renderer 'shr
        mm-verify-option 'always
        gnus-always-read-dribble-file t
        epa-file-cache-passphrase-for-symmetric-encryption t
        shr-color-visible-luminance-min 80
        user-full-name "Hussein Ait-Lahcen"
        user-mail-address "hussein.aitlahcen@gmail.com"
        gnus-secondary-select-methods '((nnimap "gmail"
                                                (nnimap-address "imap.gmail.com")
                                                (nnimap-server-port 993)
              (nnimap-stream ssl)))
        message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'ssl
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 465
        nnml-directory "~/gmail"
        message-directory "~/gmail"))

(defun user-config/editing ()
  (setq gc-cons-threshold 10000000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-log-io nil)

  (setq adaptive-wrap-prefix-mode f)

  (setq redisplay-dont-pause t)

  (setq compilation-scroll-output t)

  ;; Pandoc mode for markdown
  (add-hook 'markdown-mode-hook 'pandoc-mode)

  ;; Avoid conflicting M-k/j with I3
  (with-eval-after-load 'git-rebase
    (define-key git-rebase-mode-map (kbd "K") 'git-rebase-move-line-up)
    (define-key git-rebase-mode-map (kbd "J") 'git-rebase-move-line-down))

  ;; Golden ratio for the current window
  (evil-leader/set-key "gr" 'golden-ratio)

  (setq display-line-numbers-type 'absolute)
  (custom-set-faces
   '(line-number ((t (:background "white" :foreground "gray"))))
   '(line-number-current-line ((t (:background "beige" :foreground "black")))))
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'conf-mode-hook 'display-line-numbers-mode)
  (add-hook 'text-mode-hook 'display-line-numbers-mode)
  (add-hook 'org-mode-hook 'display-line-numbers-modeadd-hook 'yaml-mode-hook 'display-line-numbers-mode)

  ;; Cursor
  (blink-cursor-mode t)

  ;; Bindings
  (setq evil-escape-key-sequence "dk")
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line))

(defun user-config/pretty ()
  (load-file "~/.spacemacs.d/pretty-fonts.el")
  (pretty-fonts-set-kwds
   '((pretty-fonts-fira-font prog-mode-hook org-mode-hook))))

(defun user-config/legalese ()
  (setq legalese-default-copyright "Hussein Ait-Lahcen"
        legalese-default-author "Hussein Ait-Lahcen <hussein.aitlahcen@gmail.com>"))
