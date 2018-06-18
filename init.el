(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation nil
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers '(twitter
                                       purescript
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
                                       (scala :variables
                                              scala-auto-start-ensime t
                                              scala-auto-insert-asterisk-in-comments t
                                              scala-enable-eldoc t)
                                       rust
                                       pdf-tools
                                       neotree
                                       theming
                                       xkcd
                                       gnus
                                       lsp
                                       (haskell :variables haskell-enable-hindent-style "fundamental"
                                                haskell-completion-backend 'ghci
                                                haskell-process-type 'stack-ghci)
                                       (auto-completion :variables
                                                        auto-completion-return-key-behavior 'complete
                                                        auto-completion-tab-key-behavior 'cycle
                                                        auto-completion-complete-with-key-sequence nil
                                                        auto-completion-complete-with-key-sequence-delay 0.1
                                                        auto-completion-private-snippets-directory nil)
                                       (erc :variables
                                            erc-nick "huxx"
                                            erc-prompt-for-password nil
                                            erc-prompt-for-nickserv-password nil
                                            erc-highlight-notice '("JOIN" "QUIT")
                                            erc-hide-list '("PART" "QUIT")
                                            erc-autojoin-channels-alist '(("freenode.net" "#haskell" "#nixos")))
                                       (shell :variables
                                              shell-default-shell 'eshell
                                              shell-default-position 'bottom
                                              shell-default-height 40))
   dotspacemacs-additional-packages '(magithub
                                      pandoc-mode
                                      all-the-icons
                                      dracula-theme
                                      pretty-mode
                                      groovy-mode
                                      legalese
                                      processing-mode
                                      (lsp-haskell :location (recipe :fetcher github :repo "emacs-lsp/lsp-haskell")))
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
  (user-config/magithub)
  (user-config/email)
  (user-config/pretty)
  (user-config/icons)
  (user-config/editing)
  (user-config/legalese)
  (user-config/csharp)
  (user-config/java)
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
   dotspacemacs-themes '(dracula)))

(defun init/proxy ()
  "Load the proxy configuration if defined."
  (when (file-exists-p "~/.spacemacs.d/proxy.el")
    (load-file "~/.spacemacs.d/proxy.el")))

(defun user-config/magithub ()
  (use-package magithub
    :after magit
    :config (magithub-feature-autoinject t)))

(defun user-config/icons ()
  (setq neo-theme 'icons))

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
  (set-face-background 'mode-line "slate blue")
  (set-face-foreground 'mode-line "yellow")
  (set-face-background 'mode-line-inactive "black")
  (set-face-foreground 'mode-line-inactive "white")
  (setq projectile-mode-line "Projectile")
  (setq evil-insert-state-cursor '((bar . 4) "white")
        evil-normal-state-cursor '(box "white")
        evil-replace-state-cursor '(hollow "white")
        evil-visual-state-cursor '(box "red")
        evil-iedit-state-cursor '(box "gold")
        evil-lisp-state-cursor '(box "deep pink")))

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

  ;; Haskell hie
  (require 'lsp-haskell)
  (add-hook 'haskell-mode-hook  (lambda ()
                                  ;; from https://github.com/michalrus/dotfiles/blob/bdc726eb8847a9f70275587001d37fb489a9b059/dotfiles/emacs/.emacs.d/init.d/080-proglang-haskell.el#L32-L34
                                  (user-config/sandbox-nix "hie --lsp -d -l /tmp/hie.log")
                                  ;; If there’s a 'hie.sh' defined locally by a project
                                  ;; (e.g. to run HIE in a nix-shell), use it…
                                  ;; (let (hie-directory (locate-dominating-file default-directory "hie.sh"))
                                  ;;   (when hie-directory
                                  ;;     (setq-local lsp-haskell-process-path-hie (expand-file-name "hie.sh" hie-directory))))
                                  ;; … and only then setup the LSP.
                                  (lsp-haskell-enable)))
  (add-hook 'haskell-mode-hook 'flycheck-mode)

  ;; Line numbers
  (when (not (version< emacs-version "26"))
    (setq display-line-numbers-type 'absolute)
    (custom-set-faces '(line-number ((t (:foreground "dim gray")))))
    (custom-set-faces '(line-number-current-line ((t (:background "gray12" :foreground "white")))))
    (add-hook 'prog-mode-hook 'display-line-numbers-mode)
    (add-hook 'org-mode-hook 'display-line-numbers-mode)
    (add-hook 'yaml-mode-hook 'display-line-numbers-mode))

  ;; Auto complete disabled in eshell
  (spacemacs|disable-company eshell-mode)
  (spacemacs|disable-company erc-mode)

  ;; Cursor
  (global-evil-mc-mode t)
  (blink-cursor-mode t)

  ;; Org-mode
  (setq org-agenda-files (mapcar (lambda (d) (concat org-directory d)) '("/general.org" "/work.org" "/personnal.org" "/school.org"))
        org-default-notes-file (concat org-directory "/general.org"))

  ;; Bindings
  (setq evil-escape-key-sequence "dk")
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line))

(defun user-config/pretty ()
  (load-file "~/.spacemacs.d/pretty-fonts.el")
  (load-file "~/.spacemacs.d/pretty-eshell.el")
  (load-file "~/.spacemacs.d/pretty-magit.el")
  (require 'pretty-mode)
  (add-hook 'haskell-mode-hook 'turn-on-pretty-mode)
  (add-hook 'purescript-mode-hook 'turn-on-pretty-mode)
  (pretty-deactivate-groups '(:equality :ordering :ordering-double :ordering-triple :arrows :arrows-twoheaded :punctuation :logic :nil))
  (pretty-activate-groups
   '(:arithmetic-nary :undefined :sqrt :greek :sets :quantifiers))
  (pretty-fonts-set-kwds
   '((pretty-fonts-fira-font prog-mode-hook org-mode-hook))))

(defun user-config/legalese ()
  (setq legalese-default-copyright "Hussein Ait-Lahcen"
        legalese-default-author "Hussein Ait-Lahcen <hussein.aitlahcen@gmail.com>"))

(defun user-config/sandbox-nix (command)
  "Sandbox a command inside nix-shell if required"
  (let ((nix-file "shell.nix"))
    (let ((nix-path "NIX_PATH=\"nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos/nixpkgs\""))
      (let ((nix-file-directory (locate-dominating-file default-directory nix-file)))
        (when nix-file-directory
          (let (nix-shell-path (expand-file-name nix-file nix-file-directory))
            string-join '(nix-path "nix-shell -f" nix-shell-path "--command" "\"" command "\"") " "))))))
