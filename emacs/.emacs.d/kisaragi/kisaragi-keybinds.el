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
 "g" 'magit-status
 "p" 'parinfer-toggle-mode
 "x" 'execute-extended-command)

(kisaragi/primary-leader-def
 :keymaps 'org-mode-map
 "c" 'org-toggle-checkbox)

(evilem-default-keybindings ",")
;;; kisaragi-keybinds.el ends here
