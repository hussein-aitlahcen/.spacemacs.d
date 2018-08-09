(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation nil
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers '(twitter
                                       purescript
				                               agda
                                       themes-megapack
                                       idris
                                       octave
                                       python
                                       docker
                                       haskell
                                       c-c++
                                       html
                                       nixos
                                       javascript
                                       helm
                                       emacs-lisp
                                       git
                                       dash
                                       markdown
                                       org
                                       ansible
                                       csharp
                                       scala
                                       rust
                                       pdf
                                       neotree
                                       theming
                                       xkcd
                                       gnus
                                       (auto-completion :variables
                                                        auto-completion-return-key-behavior 'complete
                                                        auto-completion-tab-key-behavior 'cycle
                                                        auto-completion-complete-with-key-sequence nil
                                                        auto-completion-complete-with-key-sequence-delay 0.1
                                                        auto-completion-idle-delay 0.2
                                                        auto-completion-private-snippets-directory nil)
                                       (haskell :variables
                                                haskell-enable-hindent-style "fundamental"
                                                haskell-enable-hindent t
                                                haskell-completion-backend 'dante)
                                       (shell :variables
                                              shell-default-shell 'eshell
                                              shell-default-position 'bottom
                                              shell-default-height 40))
   dotspacemacs-additional-packages '(pandoc-mode
                                      dhall-mode
                                      all-the-icons
                                      groovy-mode
                                      legalese
                                      processing-mode)
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
  (push '(ensime . "melpa-stable") package-pinned-packages)
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
  (user-config/layout)
  (user-config/magit))

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
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-colorize-cursor-according-to-state nil
   dotspacemacs-default-font '("DejaVue Sans Mono"
                               :size 15)
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

(defun user-config/magit ()
  (load-file "~/.spacemacs.d/magit-gerrit.el"))

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
        evil-visual-state-cursor '(box "orange")
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
  ;; Pandoc mode for markdown
  (add-hook 'markdown-mode-hook 'pandoc-mode)

  ;; Avoid conflicting M-k/j with I3
  (evil-define-key 'git-rebase-mode 'git-rebase-mode-map
    "K" 'git-rebase-move-line-up
    "J" 'git-rebase-move-line-down)

  ;; Golden ratio for the current window
  (evil-leader/set-key "gr" 'golden-ratio)

  ;; Haskell dante / Purescript psc
  (evil-leader/set-key-for-mode 'haskell-mode
    "x" 'xref-find-definitions
    "a" 'dante-type-at
    "z" 'dante-info)
  (evil-leader/set-key-for-mode 'purescript-mode
    "a" 'psc-ide-show-type)

  (add-hook 'dante-mode-hook '(lambda() (flycheck-add-next-checker
                                         'haskell-dante
                                         '(warning . haskell-hlint))))
  (add-hook 'dante-mode-hook 'flycheck-mode)

  ;; Line numbers
  (when (version<= "26.0.50" emacs-version )
    (setq display-line-numbers-type 'absolute)
    (custom-set-faces
     '(line-number ((t (:foreground "black"))))
     '(line-number-current-line ((t (:background "dim gray" :foreground "white")))))
    (custom-set-faces '(line-number-current-line ((t (:background "beige" :foreground "black")))))
    (add-hook 'prog-mode-hook 'display-line-numbers-mode)
    (add-hook 'conf-mode-hook 'display-line-numbers-mode)
    (add-hook 'text-mode-hook 'display-line-numbers-mode)
    (add-hook 'org-mode-hook 'display-line-numbers-mode)
    (add-hook 'yaml-mode-hook 'display-line-numbers-mode))

  ;; Cursor
  (global-evil-mc-mode t)
  (blink-cursor-mode t)

  ;; Org-mode
  (setq org-agenda-files (mapcar (lambda (d) (concat org-directory d))
                                 '("/general.org"
                                   "/work.org"
                                   "/personnal.org"
                                   "/school.org"))
        org-default-notes-file (concat org-directory "/general.org"))

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

(defun user-config/nixify-command (command)
  "Sandbox a command inside nix-shell if required"
  (let* ((nix-file "shell.nix")
         (nix-file-directory (locate-dominating-file default-directory nix-file))
         (sandbox-script (make-temp-file "emacs-sandbox-"))
         (sandbox-script-content
          (if nix-file-directory
              (let ((nix-shell-path (expand-file-name nix-file nix-file-directory)))
                (string-join (list "exec" "nix-shell" nix-shell-path "--command" "\"" command "\"") " "))
            (string-join (list "exec" command) " "))))
    (write-region (string-join (list "#!/usr/bin/env bash" sandbox-script-content) "\n") nil sandbox-script)
    (shell-command (string-join (list "chmod u+x" sandbox-script) " "))
    sandbox-script))
