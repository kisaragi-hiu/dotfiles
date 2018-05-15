(add-to-list 'load-path
             user-emacs-directory)
(load "kisaragi-configure-pm")

(load "kisaragi-packages")
(load "kisaragi-settings")
(load "kisaragi-keybinds")
(load "kisaragi-mode-hooks")

;; from https://github.com/jhamrick/emacs
;; Put auto 'custom' changes in a separate file (this is stuff like
;; custom-set-faces and custom-set-variables)
(load 
 (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
 'noerror)
