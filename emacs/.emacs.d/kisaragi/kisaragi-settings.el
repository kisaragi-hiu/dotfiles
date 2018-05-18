;;; kisaragi-settings.el --- settings
;;; Commentary:
;;; Settings after packages have been loaded.
;;; Code:
(setq inhibit-startup-screen t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(when (functionp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(defun kisaragi/set-font ()
  "Set up my font preferences."
  (set-frame-font (font-spec :name "Sarasa Term TC" :size 20)))
(kisaragi/set-font)

;; this needs to be set before evil starts

;; start modes
(smartparens-global-mode)
(global-evil-surround-mode)
(evil-commentary-mode)
;; (global-evil-leader-mode)
(evil-mode 1)
(with-eval-after-load 'evil
  (evil-collection-init))
(company-flx-mode)
(ivy-mode)

(linum-relative-global-mode)
(global-linum-mode)

(require 'telephone-line-config)
(telephone-line-evil-config)

(which-key-mode)

;; auto-mode-alist
(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; hooks
(add-hook 'doc-view-mode
    (lambda ()
      (linum-relative-off)
      (linum-mode -1)))

(add-hook 'after-make-frame-functions #'kisaragi/set-font)

(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'racket-mode-hook #'parinfer-mode)
(add-hook 'lisp-mode-hook #'parinfer-mode)
(add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
(add-hook 'xah-elisp-mode-hook #'parinfer-mode)
(add-hook 'after-init-hook #'global-company-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'pollen-mode-hook #'rainbow-delimiters-mode)
(add-hook 'org-mode-hook #'evil-org-mode)
(add-hook 'prog-mode-hook #'flycheck-mode)

;; save backups and autosaves to system temp dir
(setq backup-directory-alist
      (list (cons ".*" temporary-file-directory)))
(setq auto-save-file-name-transforms
      (list (list ".*" temporary-file-directory t)))

(delete-selection-mode 1)
(setq tab-always-indent 'complete)
;; Follow link into vc'd files
(setq vc-follow-symlinks t)

(setq parinfer-extensions
      '(defaults
         pretty-parens
         evil
         smart-tab
         smart-yank))
(setq evil-move-beyond-eol t)

(add-to-list 'company-backends 'company-files)
(add-to-list 'company-backends '(company-shell company-shell-env))
(add-to-list 'company-backends '(company-jedi company-files))

(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))

(setq initial-scratch-message nil)
(setq linum-relative-current-symbol "")

(defun kisaragi/set-theme ()
  "Set up my theme preferences."
  (load-theme
    (if (display-graphic-p)
      'material
      'monokai)
    t))
(kisaragi/set-theme)
(add-hook 'after-make-frame-functions
          #'kisaragi/set-theme)
(setq-default show-trailing-whitespace t)
(setq whitespace-style
      '(face tabs newline space-mark tab-mark newline-mark)
      whitespace-display-mappings
      ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
      '(((newline-mark 10 [172 10]) ; 172: not sign (U00A7)
         (tab-mark 9 [9655 9] [92 9]))))

;; should mode enabling be in settings or packages?
(global-whitespace-mode 1)
(global-hl-line-mode 1)
(show-paren-mode 1)
(add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-1 80)))


(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(evil-org-agenda-set-keys)
(setq org-todo-keywords '((sequence "TODO" "NEXT" "DONE")))
(setq org-todo-keyword-faces
      '(("TODO" . org-todo) ("NEXT" . org-todo) ("DONE" . org-done)))
(setq org-src-fontify-natively t) ;; highlight SRC blocks
(ranger-override-dired-mode t)

(setq-default indent-tabs-mode nil)

(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

(setq scroll-conservatively 150
      scroll-margin 10
      scroll-step 1)
; (setq warning-minimum-level :emergency)
(setq warning-minimum-level :warning)
;;; kisaragi-settings.el ends here
