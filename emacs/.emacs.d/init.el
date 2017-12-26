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

;; Editing
;; (electric-pair-mode 1)
(straight-use-package 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode)

(delete-selection-mode 1)
(setq tab-always-indent 'complete)

(straight-use-package 'parinfer)
(global-set-key (kbd "C-,") 'parinfer-toggle-mode)
(setq parinfer-extensions
      '(defaults
	 pretty-parens
	 evil
	 smart-tab
	 smart-yank))
(add-hook 'racket-mode-hook #'parinfer-mode)

;; run moccur-grep
;; then "C-c C-i" / "C-x C-q" to start editing
;; "C-c C-f" to "commit", "C-c C-k" to abort, "C-c C-r to clear changes"
(straight-use-package 'color-moccur)
(straight-use-package 'moccur-edit)
(require 'moccur-edit)

;; Good'ol EmacsWiki
;; Indent with spaces by default
(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))
(setq indent-tabs-mode nil)
(infer-indentation-style)

(straight-use-package 'evil-leader)
(require 'evil-leader)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "x" 'execute-extended-command
  "el" 'eval-last-sexp
  "eb" 'eval-buffer
  "ed" 'eval-defun
  "hf" 'counsel-describe-function
  "hv" 'counsel-describe-variable
  "hb" 'counsel-descbinds)
(global-evil-leader-mode)

(straight-use-package 'evil)

(straight-use-package 'evil-surround)
(global-evil-surround-mode)

(straight-use-package 'evil-commentary)
(evil-commentary-mode)

(straight-use-package 'evil-numbers)
(global-set-key (kbd "C-=") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C--") 'evil-numbers/dec-at-pt)

(straight-use-package '(evil-textobj-line :type git :host github
                                          :repo "syohex/evil-textobj-line"))
(setq evil-move-beyond-eol t)
(evil-mode 1)

(straight-use-package 'evil-easymotion)
(evilem-default-keybindings "SPC")

(straight-use-package 'linum-relative)
(setq linum-relative-current-symbol "")
(linum-relative-global-mode 1)
(global-linum-mode 1)

;; Keybinds
(global-set-key (kbd "M-l") 'evil-next-buffer)
(global-set-key (kbd "M-h") 'evil-prev-buffer)

(straight-use-package 'hydra)
(defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out"))

;; Auto completion
(straight-use-package 'company)
(add-hook 'after-init-hook 'global-company-mode)
(require 'company)

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

  (straight-use-package 'markdown-mode)
  (straight-use-package 'fish-mode)

  (straight-use-package 'elm-mode)
  (setq elm-format-on-save t)

  (straight-use-package 'vimrc-mode)
  (add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))

  (straight-use-package 'web-mode))
(kisaragi/setup-languages)

(straight-use-package 'web-mode)
(evil-define-key 'normal web-mode-map
  (kbd "<key-chord> za") #'web-mode-fold-or-unfold)
(evil-define-key 'normal web-mode-map
  (kbd "<SPC>r") #'web-mode-element-rename)

;; UI
(defun kisaragi/setup-ui ()
  (setq initial-scratch-message nil) ; leave scratch empty
  ;; disable toolbar, menubar, scrollbar
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (if (functionp 'scroll-bar-mode)
      (scroll-bar-mode -1))

  ;; highlight current line
  (global-hl-line-mode 1)
  (global-whitespace-newline-mode 1)

  ;; show matching parens
  (show-paren-mode 1)
  (straight-use-package 'rainbow-delimiters)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'pollen-mode-hook #'rainbow-delimiters-mode)

  ;; spaceline
  (straight-use-package 'spaceline)
  (require 'spaceline-config)
  (setq-default spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (setq powerline-text-scale-factor 1.2)
  (spaceline-spacemacs-theme))
(kisaragi/setup-ui)

(defun kisaragi/setup-fonts ()
  (set-default-font (font-spec :name "Overpass Mono" :size 20))
  (if (functionp 'set-fontset-font)
      (set-fontset-font "fontset-default" 'unicode
			(font-spec :name "Noto Sans Mono CJK TC" :size 18))))
(kisaragi/setup-fonts)

(defun kisaragi/setup-colorscheme ()
  (straight-use-package 'monokai-theme)
  (straight-use-package 'material-theme)
  (load-theme 'material t))
(kisaragi/setup-colorscheme)

;; Apps
(straight-use-package 'magit)
(require 'magit)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
