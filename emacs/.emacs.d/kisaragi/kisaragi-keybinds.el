;;; kisaragi-keybinds.el --- Key bindings
;;; Commentary:
;;; All the keybindings, in one file.
;;; I'm trying to utilize general.el.
;;; Code:
(defconst kisaragi/primary-leader "SPC" "My primary leader key.")
(defconst kisaragi/mode-leader "," "My leader key for mode specific bindings.")

(general-define-key
 "C-=" 'evil-numbers/inc-at-pt
 "C--" 'evil-numbers/dec-at-pt
 "C-h f" 'helpful-callable
 "C-h v" 'helpful-variable
 "C-h k" 'helpful-key)

(general-define-key
 :keymaps 'prog-mode-map
 "C-<tab>" 'evil-toggle-fold)

(general-define-key
 :state 'normal
 "C-x v f" 'vimish-fold
 "C-x v v" 'vimish-fold-delete)

;; global normal state leader maps
(general-define-key
 :prefix kisaragi/primary-leader
 :states 'normal
 kisaragi/primary-leader '(:keymap evilem-map :wk "easymotion")
 "s" 'dired-sidebar-toggle-sidebar
 "x" 'execute-extended-command
 "p" 'projectile-command-map
 "b" 'ivy-switch-buffer
 "h" 'evil-prev-buffer
 "l" 'evil-next-buffer
 "c" 'powerthesaurus-lookup-word)

(general-define-key
 :prefix (key-description (list kisaragi/primary-leader "g"))
 :states 'normal
 "" '(:ignore t :wk "magit")
 "s" 'magit-status
 "↓" 'magit-pull
 "↑" 'magit-push)

(general-define-key
 :prefix (key-description (list kisaragi/primary-leader "f"))
 :states 'normal
 "" '(:ignore t :which-key "file")
 "f" 'find-file
 "p" 'find-file-at-point
 "e" '(:ignore t :which-key "open common files")
 "ed" `(,(lambda () (interactive) (find-file (f-join user-emacs-directory "init.el")))
        :which-key "init.el")
 "es" `(,(lambda () (interactive) (find-file (f-join user-emacs-directory "kisaragi" "kisaragi-settings.el")))
        :which-key "settings.el")
 "ep" `(,(lambda () (interactive) (find-file (f-join user-emacs-directory "kisaragi" "kisaragi-packages.el")))
        :which-key "packages.el")
 "ek" `(,(lambda () (interactive) (find-file (f-join user-emacs-directory "kisaragi" "kisaragi-keybinds.el")))
         :which-key "keybinds.el")
 "en" `(,(lambda () (interactive) (find-file (if (getenv "ANDROID_ROOT")
                                                 (f-join "/sdcard" "0.git" "notes" "notes.org")
                                                 (f-join (getenv "HOME") "git" "notes" "notes.org"))))
         :which-key "notes.org"))

(general-define-key
 :prefix kisaragi/mode-leader
 :states 'normal
 :keymaps 'org-mode-map
 "c" 'org-toggle-checkbox)

;; mode specific maps

(general-define-key
 :prefix kisaragi/mode-leader
 :states '(normal visual)
 :keymaps '(emacs-lisp-mode-map racket-mode-map clojure-mode-map lisp-mode-map)
 "p" 'parinfer-toggle-mode)

(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 "TAB" 'org-cycle)

(general-define-key
 :prefix kisaragi/mode-leader
 :states '(normal visual)
 :keymaps 'emacs-lisp-mode-map
 "e" '(:ignore t :which-key "eval")
 "eb" 'eval-buffer
 "ed" 'eval-defun
 "er" 'eval-region
 "el" 'eval-last-sexp)

(general-define-key
 :prefix kisaragi/mode-leader
 :states '(normal visual)
 :keymaps 'racket-mode-map
 "d" 'racket-doc
 "e" '(:ignore t :which-key "expand")
 "ea" 'racket-expand-again
 "el" 'racket-expand-last-sexp
 "er" 'racket-expand-region
 "ex" 'racket-expand-definition
 "f" 'racket-describe
 "o" 'racket-open-require-path
 "r" '(:ignore t :which-key "repl")
 "re" 'racket-repl
 "rl" 'racket-send-last-sexp
 "rr" 'racket-send-region
 "t" 'racket-test)

;;; kisaragi-keybinds.el ends here
