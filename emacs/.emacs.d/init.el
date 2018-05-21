;;; init.el --- Kisaragi Hiu's Emacs config
;;; Commentary:
;;; I'm roughly following the config model of VSCode.
;;; Code:
(add-to-list 'load-path
             (expand-file-name "kisaragi/" user-emacs-directory))

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'after-init-hook
           (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(load "kisaragi-configure-pm.el")

(load "kisaragi-packages.el")
(load "kisaragi-settings.el")
(load "kisaragi-keybinds.el")

(server-start)
;;; init.el ends here
