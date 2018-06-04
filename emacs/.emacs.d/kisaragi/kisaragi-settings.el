;;; kisaragi-settings.el --- settings
;;; Commentary:
;;; Settings after packages have been loaded.
;;; Code:

;; UI Base
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(when (functionp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(defun kisaragi/set-font ()
  "Set up my font preferences."
  (set-frame-font (find-font (font-spec :name "Sarasa Term TC" :size 20))))
(kisaragi/set-font)

(defun kisaragi/set-theme ()
  "Set up my theme preferences."
  (load-theme
    (if (display-graphic-p)
      'material
      'monokai)
    t))
(kisaragi/set-theme)

(when (s-prefix? "26" emacs-version)
  (setq-default display-line-numbers 'relative))

;; auto-mode-alist
(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.l\\'" . picolisp-mode))

(add-to-list 'company-backends 'company-files)
(add-to-list 'company-backends '(company-shell company-shell-env))
(add-to-list 'company-backends '(company-jedi company-files))

;; hooks
(when (not (s-prefix? "26" emacs-version))
  (add-hook 'doc-view-mode
            (lambda ()
              (linum-relative-off)
              (linum-mode -1))))

(add-hook 'after-make-frame-functions #'kisaragi/set-font)
(add-hook 'after-make-frame-functions #'kisaragi/set-theme)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-1 80)))
(add-hook 'racket-mode-hook #'parinfer-mode)
(add-hook 'lisp-mode-hook #'parinfer-mode)
(add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
(add-hook 'emacs-lisp-mode-hook (lambda () (interactive) (column-marker-1 120)))

(add-hook 'after-init-hook #'global-company-mode)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 20 1024 1024))))

(add-hook 'pollen-mode-hook #'rainbow-delimiters-mode)
(add-hook 'org-mode-hook #'evil-org-mode)
(add-hook 'prog-mode-hook #'flycheck-mode)

;; emacs behavior
;; save backups and autosaves to system temp dir
(setq backup-directory-alist
      (list (cons ".*" temporary-file-directory))
      auto-save-file-name-transforms
      (list (list ".*" temporary-file-directory t)))

(when (getenv "ANDROID_ROOT")
  (setq picolisp-documentation-directory
        (f-join (getenv "PREFIX")
                "lib/picolisp/doc/")))

(setq which-key-idle-delay 0.4
      which-key-idle-secondary-delay 0.1)

(setq vc-follow-symlinks t)
(setq warning-minimum-level :emergency)

(setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
(setq initial-scratch-message nil)
(when (featurep 'linum-relative)
  (setq linum-relative-current-symbol ""))
(setq evil-move-beyond-eol t)
(setq-default indent-tabs-mode nil)

;; editing
(delete-selection-mode 1)
(setq tab-always-indent 'complete)

(setq parinfer-extensions
      '(defaults
         pretty-parens
         evil
         smart-tab
         smart-yank))

;; UI
(setq-default show-trailing-whitespace t)
(setq whitespace-style
      '(face tabs newline space-mark tab-mark newline-mark)
      whitespace-display-mappings
      ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
      '(((newline-mark 10 [172 10]) ; 172: not sign (U00A7)
         (tab-mark 9 [9655 9] [92 9]))))

(setq scroll-conservatively 150
      scroll-margin 10
      scroll-step 1)

(setq evil-emacs-state-cursor '("red" box)
      evil-normal-state-cursor '("green" box)
      evil-visual-state-cursor '("orange" box)
      evil-insert-state-cursor '("red" bar)
      evil-replace-state-cursor '("red" bar)
      evil-operator-state-cursor '("red" hollow))

;; should mode enabling be in settings or packages?
(global-whitespace-mode 1)
(global-hl-line-mode 1)
(show-paren-mode 1)

(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(evil-org-agenda-set-keys)
(setq org-todo-keywords '((sequence "TODO" "NEXT" "DONE"))
      org-todo-keyword-faces
      '(("TODO" . org-todo) ("NEXT" . org-todo) ("DONE" . org-done)))
(setq org-src-fontify-natively t) ;; highlight SRC blocks

;;; kisaragi-settings.el ends here
