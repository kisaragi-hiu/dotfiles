;;; init.el --- Kisaragi Hiu's Emacs config
;;; Commentary:
;;; I'm roughly following the config model of VSCode.
;;; Code:
(add-to-list 'load-path
             (expand-file-name "kisaragi/" user-emacs-directory))
(load "kisaragi-configure-pm")

(load "kisaragi-packages.el")
(load "kisaragi-settings.el")
(load "kisaragi-keybinds.el")

(server-start)
;;; init.el ends here
