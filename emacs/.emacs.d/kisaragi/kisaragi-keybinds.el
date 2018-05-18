;;; kisaragi-keybinds.el --- Key bindings
;;; Commentary:
;;; All the keybindings, in one file.
;;; I'm trying to utilize general.el.
;;; Code:
(general-define-key
 "C-," 'parinfer-toggle-mode
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

(defconst kisaragi/primary-leader "SPC" "My primary leader key.")
(general-create-definer kisaragi/primary-leader-def
  :prefix kisaragi/primary-leader
  :states 'normal)

(kisaragi/primary-leader-def
 "b" 'ivy-switch-buffer
 "f" 'ffap
 "h" 'evil-prev-buffer
 "l" 'evil-next-buffer
 "gs" 'magit-status
 "g↓" 'magit-pull
 "g↑" 'magit-push
 "p" 'parinfer-toggle-mode
 "SPC" 'execute-extended-command
 "x" 'execute-extended-command)

(kisaragi/primary-leader-def
 :keymaps 'org-mode-map
 "c" 'org-toggle-checkbox)

(kisaragi/primary-leader-def
 :keymaps '(xah-elisp-mode-map emacs-lisp-mode-map)
 "eb" 'eval-buffer
 "ed" 'eval-defun
 "er" 'eval-region
 "el" 'eval-last-sexp)

(evilem-default-keybindings ",")
;;; kisaragi-keybinds.el ends here
