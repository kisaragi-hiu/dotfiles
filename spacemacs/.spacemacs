;; -*- mode: emacs-lisp -*-
;; vim: filetype=lisp
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.
;; (kbd "SPC f e R") to reload

(defun dotspacemacs/layers ()
  "Layers. No other code should live here."
  (setq-default
   dotspacemacs-distribution 'spacemacs ; spacemacs-base, spacemacs
   dotspacemacs-enable-lazy-installation 'unused ; all, unused, nil
   dotspacemacs-ask-for-lazy-installation t ; ask before lazy install
   dotspacemacs-configuration-layer-path '() ; extra layer paths
   dotspacemacs-configuration-layers ; layers to load
   `(
     ;; == editor ==
     helm
     auto-completion
     git
     version-control
     syntax-checking
     emoji
     ;; == languages ==
     racket
     emacs-lisp
     vimscript
     shell-scripts
     markdown
     pollen
     ;; == apps ==
     (wakatime :variables
               wakatime-api-key
               ,(with-temp-buffer
                  (insert-file-contents "~/.wakatime.cfg")
                  (second
                   (split-string
                    (first (remove-if-not
                            (lambda (x) (string-match "api_key" x))
                            (split-string (buffer-string) "\n")))
                    "=")))
               wakatime-cli-path "/usr/bin/wakatime")
     (ranger :variables
             ranger-show-preview t)
     org
     (shell :variables
            shell-default-shell 'eshell
            shell-default-height 30
            shell-default-position 'bottom))

   ;; config should be in `dotspacemacs/user-config' or make a layer.
   dotspacemacs-additional-packages '(f s dash parinfer)
   dotspacemacs-frozen-packages '() ; frozen package list
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(evil-search-highlight-persist)
   ;; used-only, used-but-keep-unused: no uninstall, all: everything local
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Spacemacs init. Called before layers config."
  (setq-default
   evil-move-beyond-eol t ; evil /is/ part of spacemacs core, right?
   dotspacemacs-elpa-https t ; use https?
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t ; check for updates at startup thru GitHub?
   dotspacemacs-elpa-subdirectory nil ; (string) install packages under this if non-nil
   dotspacemacs-editing-style 'vim ; vim, emacs, or hybrid (emacs in insert state).
   dotspacemacs-verbose-loading nil ; output loading progress in `*Messages*'?
   ;; official, (id(integer)), random, or (path(string)).
   dotspacemacs-startup-banner 'official
   ;; `(list-type . list-size)`. Disabled if nil.
   ;; list-type := `recents' `bookmarks' `projects' `agenda' `todos'
   ;; list-size: number or nil (fallback to `spcaemacs-buffer-startup-lists-length)
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive nil ; should home be responsive?
   dotspacemacs-scratch-mode 'text-mode ; scratch buffer default major mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list.
   dotspacemacs-themes '(material
                         material-light
                         spacemacs-dark
                         spacemacs-light)
   dotspacemacs-colorize-cursor-according-to-state t ; sync cursor with state color
   ;; Default font, or prioritized list of fonts.
   dotspacemacs-default-font '("Hack"
                               :size 16
                               :weight normal
                               :width normal
                               :powerline-scale 1.5)
   dotspacemacs-leader-key "SPC" ; The leader key
   dotspacemacs-emacs-command-key "SPC" ; (<leader> what) for M-x?
   dotspacemacs-ex-command-key ":" ; ":"q, ":"edit, etc.
   dotspacemacs-emacs-leader-key "M-m" ; <leader> in insert or emacs states
   ;; Major-mode-specific prefix. "<leader> m" does the same. `nil` disables.
   dotspacemacs-major-mode-leader-key "," ; Let me call this <mleader>.
   dotspacemacs-major-mode-emacs-leader-key "C-M-m" ; <mleader> in insert or emacs states
   dotspacemacs-distinguish-gui-tab nil ; treat C-i, TAB, C-m, RET as different keys? (GUI only)
   dotspacemacs-remap-Y-to-y$ t ; bind Y to y$ ?
   dotspacemacs-retain-visual-state-on-shift t ; should < > in visual stay in visual?
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil ; inverse :s/from/to/"g" 's meaning.
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil ; display default layout name in mode line?
   dotspacemacs-auto-resume-layouts nil ; reopen last auto saved layouts upon start?
   dotspacemacs-large-file-size 10 ; prompt to open files > this (in MB) literally
   ;; auto save location. original, cache, nil
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize t ; should helm try to mimimize space used?
   dotspacemacs-helm-no-header nil ; no header when there's only one source
   dotspacemacs-helm-position 'bottom ; bottom, top, left, right
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state t
   dotspacemacs-which-key-delay 0.4 ; which-key delay in seconds
   dotspacemacs-which-key-position 'bottom ; right, bottom, right-then-bottom
   dotspacemacs-loading-progress-bar nil ; show loading progress bar
   dotspacemacs-fullscreen-at-startup nil ; emacs 24.4+
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   dotspacemacs-maximized-at-startup t ; emacs 24.4+

   ;; Per-frame. 100 = opache. toggle with `toggle-transparency'.
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90

   dotspacemacs-show-transient-state-title t ; show transient state titles?
   dotspacemacs-show-transient-state-color-guide t ; show transient state color guide?
   dotspacemacs-mode-line-unicode-symbols t ; use unicode symbols in mode line?
   ;; smooth scrolling means point pushes view instead of being recentered at borders.
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers '(:relative t :disabled-for-modes dired-mode doc-view-mode org-mode pdf-view-mode)
   dotspacemacs-folding-method 'evil ; available: evil, origami
   dotspacemacs-smartparens-strict-mode nil ; use smartparens strict mode?
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil))
   

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (bind-key "C-," #'parinfer-toggle-mode evil-insert-state-map)
  (setq parinfer-extensions
        '(defaults       ; should be included.
           pretty-parens  ; different paren styles for different modes.
           evil           ; If you use Evil.
           ;; lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
           paredit        ; Introduce some paredit commands.
           smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
           smart-yank))   ; Yank behavior depend on mode.
  (add-hook 'lisp-mode-hook #'parinfer-mode)
  (add-hook 'clojure-mode-hook #'parinfer-mode)
  (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
  (add-hook 'common-lisp-mode-hook #'parinfer-mode)
  (add-hook 'scheme-mode-hook #'parinfer-mode)
  (add-hook 'racket-mode-hook #'parinfer-mode)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(package-selected-packages
   (quote
    (parinfer vimrc-mode ranger insert-shebang fish-mode emoji-cheat-sheet-plus dactyl-mode company-shell company-emoji wakatime-mode xterm-color smeargle shell-pop racket-mode faceup orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download multi-term mmm-mode markdown-toc markdown-mode magit-gitflow htmlize helm-gitignore helm-company helm-c-yasnippet gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md fuzzy flycheck-pos-tip pos-tip flycheck evil-magit magit magit-popup git-commit ghub with-editor eshell-z eshell-prompt-extras esh-help diff-hl company-statistics company auto-yasnippet yasnippet ac-ispell auto-complete ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump diminish define-word column-enforce-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#263238")))))
