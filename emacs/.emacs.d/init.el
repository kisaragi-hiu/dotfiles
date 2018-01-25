;; Install straight.el
(defun straight-init ()
  (let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
        (bootstrap-version 2))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))
(straight-init)

;; File library
(straight-use-package 'f)
(require 'f)

;; save backups and autosaves to system temp dir
(setq backup-directory-alist
      (list (cons ".*" temporary-file-directory)))
(setq auto-save-file-name-transforms
      (list (list ".*" temporary-file-directory t)))

;; Editing
;; (electric-pair-mode 1)
(straight-use-package 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode)

(delete-selection-mode 1)
(setq tab-always-indent 'complete)

;; Symbolic link to Git-controlled source file; follow link? (YES PLEASE)
(setq vc-follow-symlinks t)

(straight-use-package 'parinfer)
(global-set-key (kbd "C-,") 'parinfer-toggle-mode)
(setq parinfer-extensions
      '(defaults
         pretty-parens
         evil
         smart-tab
         smart-yank))
(add-hook 'racket-mode-hook #'parinfer-mode)
(add-hook 'lisp-mode-hook #'parinfer-mode)
(add-hook 'emacs-lisp-mode-hook #'parinfer-mode)

;; (fold-this) folds active region, C-g or Enter unfolds
(straight-use-package 'fold-this)
(require 'fold-this)

;; run moccur-grep
;; then "C-c C-i" / "C-x C-q" to start editing
;; "C-c C-f" to "commit", "C-c C-k" to abort, "C-c C-r to clear changes"
(straight-use-package 'color-moccur)
(straight-use-package 'moccur-edit)
(require 'moccur-edit)

;; Good'ol EmacsWiki
;; Indent with spaces by default
;; indent-tabs-mode is buffer local
(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))
(setq-default indent-tabs-mode nil)

(defun kisaragi/setup-keys ()
  (straight-use-package 'evil)
  (straight-use-package 'evil-surround)
  (straight-use-package 'evil-commentary)
  (global-evil-surround-mode)
  (evil-commentary-mode)

  (straight-use-package 'evil-numbers)
  (global-set-key (kbd "C-=") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C--") 'evil-numbers/dec-at-pt)

  ;; evil text objects
  (straight-use-package '(evil-textobj-line :type git :host github
                                            :repo "syohex/evil-textobj-line"))
  (setq evil-move-beyond-eol t)
  (evil-mode 1)
  (straight-use-package 'evil-easymotion)
  (evilem-default-keybindings "SPC")

  ;; Keybinds
  (straight-use-package 'hydra)
  (defhydra hydra-eval ()
    "l → last sexp, b → buffer, r → region, d → defun\n"
    ("l" eval-last-sexp)
    ("b" eval-buffer)
    ("r" eval-region)
    ("d" eval-defun))

  (defhydra hydra-buffer ()
    "l → buffer list, d → :bd, n → :bn, p → :bp\n"
    ("l" ivy-switch-buffer)
    ("d" evil-delete-buffer)
    ("n" evil-next-buffer)
    ("p" evil-prev-buffer))

  (defhydra hydra-magit ()
    "magit operations"
    ("s" magit-status)
    ("↑" magit-push-current)
    ("↓" magit-pull))

  (defhydra hydra-help ()
    "help"
    ("f" counsel-describe-function)
    ("v" counsel-describe-variable)
    ("b" counsel-descbinds))

  (defhydra hydra-zoom ()
    "zoom"
    ; up and down → in and out
    ("k" text-scale-increase "in")
    ("j" text-scale-decrease "out"))

  (defhydra hydra-racket ()
    "racket stuff"
    ("d" racket-describe "describe")
    ("x" racket-trim-requires "trim requires")
    ("b" racket-base-requires "base requires")
    ("t" racket-tidy-requires "tidy requires")
    ("m" racket-visit-module "visit module")
    ("v" racket-visit-definition "visit definition"))

  (straight-use-package 'evil-leader)
  (require 'evil-leader)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "e" 'hydra-eval/body
    "z" 'hydra-zoom/body
    "b" 'hydra-buffer/body
    "g" 'hydra-magit/body
    "l" 'evil-next-buffer
    "h" 'evil-prev-buffer
    "p" 'parinfer-toggle-mode
    "x" 'execute-extended-command)
  (evil-leader/set-key-for-mode 'org-mode "c" 'org-toggle-checkbox)
  (evil-leader/set-key-for-mode 'racket-mode
    "r" 'hydra-racket/body)
  (global-evil-leader-mode))
(kisaragi/setup-keys)

;; Auto completion
(straight-use-package 'company)
(add-hook 'after-init-hook 'global-company-mode)
(require 'company)
(add-to-list 'company-backends 'company-files)

(straight-use-package 'company-shell)
(add-to-list 'company-backends '(company-shell company-shell-env))

(straight-use-package 'company-flx)
(company-flx-mode +1)
;; (global-company-mode 1)

;; Syntax checking
(straight-use-package 'flycheck)

;; Fuzzy find thing
(straight-use-package 'ivy)
(setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
(ivy-mode 1)

;; Languages
(defun kisaragi/setup-languages ()
  (straight-use-package 'racket-mode)
  (straight-use-package 'pollen-mode)
  (require 'pollen-mode)
  (straight-use-package '(scribble-mode :type git :host github
                                        :repo "emacs-pe/scribble-mode"))

  (straight-use-package 'markdown-mode)
  (straight-use-package 'fish-mode)

  (straight-use-package 'elm-mode)
  (setq elm-format-on-save t)

  (straight-use-package 'vimrc-mode)
  (add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))

  (straight-use-package 'web-mode))
(kisaragi/setup-languages)

;; UI
(defun kisaragi/setup-ui ()
  (setq initial-scratch-message nil) ; leave scratch empty
  ;; disable toolbar, menubar, scrollbar
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (when (functionp 'scroll-bar-mode)
    (scroll-bar-mode -1))

  ;; line numbers
  (straight-use-package 'linum-relative)
  (setq linum-relative-current-symbol "")
  (linum-relative-global-mode 1)
  (global-linum-mode 1)
  (add-hook 'doc-view-mode
            (lambda ()
              (linum-relative-off)
              (linum-mode -1)))
  ;; color
  (straight-use-package 'monokai-theme)
  (straight-use-package 'material-theme)
  (straight-use-package 'dracula-theme)
  (setq kisaragi/theme (if (display-graphic-p) 'material 'monokai))
  (load-theme kisaragi/theme t)
  (add-hook 'after-make-frame-functions
            (lambda () (load-theme kisaragi/theme t)))

  (straight-use-package 'telephone-line)
  (require 'telephone-line-config)
  (telephone-line-evil-config)

  (setq-default show-trailing-whitespace t) ; highlight trailing whitespace

  (global-hl-line-mode 1) ; highlight current line

  ;; show matching parens
  (show-paren-mode 1)
  (straight-use-package 'rainbow-delimiters)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'pollen-mode-hook #'rainbow-delimiters-mode))
(kisaragi/setup-ui)

(defun kisaragi/setup-fonts ()
  (set-default-font (font-spec :name "Overpass Mono" :size 20))
  (if (functionp 'set-fontset-font)
      (set-fontset-font "fontset-default" 'unicode
                        (font-spec :name "Noto Sans Mono CJK TC" :size 18))))
(kisaragi/setup-fonts)

;; Apps
(straight-use-package 'magit)
(straight-use-package 'evil-magit)
(straight-use-package '(git-undo :type git :host github
                                 :repo "jwiegley/git-undo-el"))
(require 'magit)
(require 'evil-magit)

(straight-use-package 'evil-org)
(require 'evil-org)
(require 'evil-org-agenda)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(evil-org-agenda-set-keys)

(straight-use-package 'helpful)
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

(straight-use-package 'ranger) ; ranger emulation for dired
;; run ranger to start it
(ranger-override-dired-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8e0c6a96a17a5b45979c31265821053aff9beea9fb5ac5e41130e0c27a89214e" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" default)))
 '(inhibit-startup-screen t))
(custom-set-faces)
