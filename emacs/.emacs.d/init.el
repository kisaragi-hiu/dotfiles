(add-to-list 'load-path
             (expand-file-name "kisaragi/" user-emacs-directory))
(load "kisaragi-configure-pm")

(load "kisaragi-packages")
(load "kisaragi-settings")
(load "kisaragi-keybinds")
;; (load "kisaragi-mode-hooks")
