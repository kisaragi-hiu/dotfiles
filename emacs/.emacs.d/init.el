;; -*- lexical-binding: t -*-

;;; init.el --- Kisaragi Hiu's Emacs config
;;; Commentary:
;;; I'm roughly following the config model of VSCode.
;;; Code:
(add-to-list 'load-path (expand-file-name "kisaragi" user-emacs-directory))

(setq gc-cons-threshold (* 128 1024 1024))

(load "kisaragi-configure-pm.el")

(load "kisaragi-packages.el")
(load "kisaragi-settings.el")
(load "kisaragi-keybinds.el")

(server-start)
;;; init.el ends here
