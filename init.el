(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation nil
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(graphviz
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
     erc
     treemacs
     theming
     xkcd
     (shell :variables
            shell-default-shell 'eshell
            shell-default-position 'bottom
            shell-default-height 30)
     )
   dotspacemacs-additional-packages '(
                                      all-the-icons
                                      spaceline-all-the-icons
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
  ;; (setq theming-modifications
  ;;       '((dracula
  ;;          (default :background "#212121"))))
  (dotspacemacs/init/vars)
  (dotspacemacs/init/proxy))

(defun dotspacemacs/user-init ()
  "Avoid custom-vars to be set in init.el file"
  (setq-default dotspacemacs-persistent-server t)
  (setq-default centered-window-mode t)
  (setq-default spacemacs-show-trailing-whitespace t)
  (setq custom-file "~/.spacemacs.d/custom.el")
  (if (not (file-exists-p custom-file))
      (write-region "" nil custom-file)
    (load-file custom-file)))

(defun dotspacemacs/user-config ()
  "Custom user configuration, doing all the displaying stuff after package are loaded."
  (dotspacemacs/user-config/pretty)
  (dotspacemacs/user-config/layout)
  (dotspacemacs/user-config/editing)
  (dotspacemacs/user-config/icons)
  (dotspacemacs/user-config/legalese)
  (dotspacemacs/user-config/csharp)
  (dotspacemacs/user-config/java)
  (dotspacemacs/user-config/magit))

(defun dotspacemacs/init/vars ()
  "General variable configurations."
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-colorize-cursor-according-to-state nil
   dotspacemacs-default-font '("Consolas"
                               :size 16
                               :weight normal
                               :width normal
                               :powerline-scale 1.4)
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
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling nil
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil
   dotspacemacs-themes '(dracula t)
   ))

(defun dotspacemacs/init/proxy ()
  "Load the proxy configuration if defined."
  (if (file-exists-p "~/.spacemacs.d/proxy.el")
      (load-file "~/.spacemacs.d/proxy.el")))

(defun dotspacemacs/user-config/icons ()
  (spaceline-all-the-icons-theme)
  (spaceline-all-the-icons--setup-anzu)
  (spaceline-all-the-icons--setup-git-ahead)
  (spaceline-all-the-icons--setup-neotree)
  (setq neo-theme 'icons))

(defun dotspacemacs/user-config/csharp ()
  (setq-default omnisharp-server-executable-path "~/omnisharp/run"))

(defun dotspacemacs/user-config/java ()
  (setq eclim-eclipse-dirs '("~/eclipse")
        eclim-executable "~/eclipse/eclim"
        eclimd-executable "~/eclipse/eclimd"
        eclimd-wait-for-process t))

(defun dotspacemacs/user-config/magit ()
  (load-file "~/.spacemacs.d/magit-gerrit.el"))

(defun dotspacemacs/user-config/layout ()
  ;; (require 'golden-ratio)
  ;; (golden-ratio-mode 1)
  )

(defun dotspacemacs/user-config/editing ()
  ;; Waiting emacs26
  ;; (global-display-line-numbers-mode t)
  (setq evil-insert-state-cursor '((bar . 4) "white")
        evil-normal-state-cursor '(box "white"))
  (global-evil-mc-mode t)
  (setq evil-escape-key-sequence "dk")
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)(global-whitespace-mode t)
  (setq whitespace-line-column 500)
  (setq whitespace-display-mappings
        '((newline-mark 10 [182 10])
          (space-mark 32 [183] [46])))
  (set-face-foreground 'whitespace-newline "#505050")
  (set-face-foreground 'whitespace-tab "#505050")
  (set-face-background 'whitespace-tab 'nil)
  (set-face-foreground 'whitespace-space"#454545")
  (set-face-background 'whitespace-space 'nil))

(defun dotspacemacs/user-config/pretty ()
  (load-file "~/.spacemacs.d/pretty-fonts.el")
  (load-file "~/.spacemacs.d/pretty-eshell.el")
  (load-file "~/.spacemacs.d/pretty-magit.el")
  (global-pretty-mode t)
  (pretty-deactivate-groups
   '(:equality :ordering :ordering-double :ordering-triple
               :arrows :arrows-twoheaded :punctuation
               :logic :sets
               :sub-and-superscripts :greek :arithmetic-nary))
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

(defun dotspacemacs/user-config/legalese ()
  (setq legalese-default-copyright "Hussein Ait-Lahcen")
  (setq legalese-default-author "Hussein Ait-Lahcen <hussein.aitlahcen@gmail.com>"))
