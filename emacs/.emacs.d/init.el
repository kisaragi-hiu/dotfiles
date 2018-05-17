;;; init.el --- Kisaragi Hiu's Emacs config
;;; Commentary:
;;; I'm roughly following the config model of VSCode.
;;; Code:
(add-to-list 'load-path
             (expand-file-name "kisaragi/" user-emacs-directory))
(load "kisaragi-configure-pm")

(load "kisaragi-packages")
(load "kisaragi-settings")
(load "kisaragi-keybinds")

(server-start)
;;; init.el ends here
