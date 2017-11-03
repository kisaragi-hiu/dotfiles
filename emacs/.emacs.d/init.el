(let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
      (bootstrap-version 2))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(unless (require 'use-package nil 'noerror)
  (straight-use-package 'use-package))

(setq use-package-always-ensure t)

;; Editing
(electric-pair-mode 1)
(delete-selection-mode 1)
(setq tab-always-indent 'complete)

(use-package evil
  :init
  ;; evil-leader should be enabled before evil loads
  (use-package evil-leader
    :config
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "<SPC>" 'execute-extended-command)
    (global-evil-leader-mode))
  :config
  (use-package evil-surround
    :config
    (global-evil-surround-mode))
  (use-package evil-commentary
    :config
    (evil-commentary-mode))
  (use-package evil-numbers
    :config
    (global-set-key (kbd "C-=") 'evil-numbers/inc-at-pt)
    (global-set-key (kbd "C--") 'evil-numbers/dec-at-pt))
  (setq evil-move-beyond-eol t)
  (evil-mode 1))

(use-package linum-relative
  :config
  (linum-relative-global-mode 1)
  (global-linum-mode 1))

;; Keybinds
(global-set-key (kbd "C-l") 'evil-next-buffer)
(global-set-key (kbd "C-h") 'evil-prev-buffer)

;; Auto completion
(use-package company
  :config
  (global-company-mode 1))

;; Syntax checking
(use-package flycheck
  :commands flycheck-mode)

;; Fuzzy find thing
(use-package ivy
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (use-package flx)
  (ivy-mode 1))

;; Languages
(use-package racket-mode)
(use-package pollen-mode)
(use-package markdown-mode)
(use-package fish-mode)

;; UI
(tool-bar-mode -1)
(menu-bar-mode -1)
(if (functionp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(global-hl-line-mode 1)
(show-paren-mode 1)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package spaceline
  :config
  (setq-default spaceline-highlight-face-func 'spaceline-highlight-face-evil-state))
(use-package spaceline-config :ensure spaceline
  :config
  (setq powerline-text-scale-factor 1.2)
  (spaceline-emacs-theme))

(if (functionp 'set-fontset-font)
    (set-fontset-font "fontset-default" 'unicode
		      (font-spec :name "Noto Sans Mono CJK TC")))

(use-package monokai-theme)

(use-package seoul256-theme
  :init
  (setq seoul256-background 235)
  :config
  (load-theme 'seoul256 t))

;; Apps
(use-package magit)
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
