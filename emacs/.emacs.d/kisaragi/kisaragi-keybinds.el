;;; kisaragi-keybinds.el --- Key bindings
;;; Commentary:
;;; All the keybindings, in one file.
;;; I'm trying to utilize general.el.
;;; Code:
(defconst kisaragi/primary-leader "SPC" "My primary leader key.")

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

(general-define-key
 :prefix kisaragi/primary-leader
 :states 'normal
 kisaragi/primary-leader 'execute-extended-command
 "x" 'execute-extended-command
 "b" 'ivy-switch-buffer
 "h" 'evil-prev-buffer
 "l" 'evil-next-buffer)

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
 "e" '(:ignore t :which-key "edit config")
 "ed" `(,(lambda () (interactive) (find-file (f-join user-emacs-directory "init.el")))
        :which-key "init.el")
 "es" `(,(lambda () (interactive) (find-file (f-join user-emacs-directory "kisaragi" "kisaragi-settings.el")))
        :which-key "settings.el")
 "ep" `(,(lambda () (interactive) (find-file (f-join user-emacs-directory "kisaragi" "kisaragi-packages.el")))
        :which-key "packages.el")
 "ek" `(,(lambda () (interactive) (find-file (f-join user-emacs-directory "kisaragi" "kisaragi-keybinds.el")))
         :which-key "keybinds.el"))

(general-define-key
 :prefix kisaragi/primary-leader
 :states 'normal
 :keymaps 'org-mode-map
 "c" 'org-toggle-checkbox)

(general-define-key
 :prefix kisaragi/primary-leader
 :states '(normal visual)
 :keymaps '(xah-elisp-mode-map emacs-lisp-mode-map)
 "p" 'parinfer-toggle-mode
 "eb" 'eval-buffer
 "ed" 'eval-defun
 "er" 'eval-region
 "el" 'eval-last-sexp)

(evilem-default-keybindings ",")
;;; kisaragi-keybinds.el ends here
