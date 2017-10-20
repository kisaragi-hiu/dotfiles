(tool-bar-mode -1)
(menu-bar-mode -1)
(electric-pair-mode 1)

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

(straight-use-package 'use-package)
(setq use-package-always-ensure t)

(use-package evil
  :init
  ; evil-leader should be enabled before evil loads
  (use-package evil-leader
    :config
    (evil-leader/set-leader (kbd "<SPC>"))
    (evil-leader/set-key
      "<SPC>" 'execute-extended-command)
    (global-evil-leader-mode))
  :config
  (use-package evil-surround
    :config
    (global-evil-surround-mode))
  (setq evil-move-beyond-eol t)
  (evil-mode 1))

(use-package company
  :config
  (company-mode 1))
(use-package ivy
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (use-package flx)
  (ivy-mode 1))

(use-package monokai-theme
  :config
  (load-theme 'monokai t))
