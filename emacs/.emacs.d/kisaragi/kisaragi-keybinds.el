;; kisaragi-keybinds.el: Key bindings
;; TODO: switch to using general.el
(global-set-key (kbd "C-,") 'parinfer-toggle-mode)

(evil-define-key 'normal global-map (kbd "C-x v f") #'vimish-fold)
(evil-define-key 'normal global-map (kbd "C-x v v") #'vimish-fold-delete)
(define-key prog-mode-map (kbd "C-<tab>") #'evil-toggle-fold)
(global-set-key (kbd "C-=") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C--") 'evil-numbers/dec-at-pt)
(evilem-default-keybindings "SPC")
(evil-leader/set-leader ",")
(evil-leader/set-key
  "l" 'evil-next-buffer
  "h" 'evil-prev-buffer
  "b" 'ivy-switch-buffer
  "p" 'parinfer-toggle-mode
  "x" 'execute-extended-command)
(evil-leader/set-key-for-mode 'org-mode
  "c" 'org-toggle-checkbox)
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
