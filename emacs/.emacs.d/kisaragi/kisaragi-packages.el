;;; kisaragi-packages.el --- package installations
;;; Commentary:
;;; Package installation, mode initialization
;;; Hooks go in settings.el
;;; Code:

;; Libraries
(use-package f)
(use-package s)
(use-package fn)
(straight-use-package
 '(xmlgen :type git :host github
          :repo "philjackson/xmlgen"))

;; Emacs features
(use-package color-moccur)
(use-package fzf)
(use-package ag)
(use-package helpful)
(use-package ivy
  :config
  (ivy-mode))
(use-package moccur-edit)
(use-package which-key
  :config
  (which-key-mode))
(use-package projectile
  :config
  (use-package counsel-projectile)
  (projectile-global-mode))
(use-package editorconfig
  :config
  (editorconfig-mode))

;; Apps
(use-package dired-sidebar)
(use-package magit)
(use-package suggest)

;; Editing
;; (use-package smartparens)
(electric-pair-mode)
(use-package parinfer)
(use-package vimish-fold)
(use-package fold-this)
(straight-use-package
 '(git-undo :type git :host github
            :repo "jwiegley/git-undo-el"))
(use-package yasnippet
  :config
  (use-package yasnippet-snippets)
  (use-package ivy-yasnippet)
  (yas-reload-all))

;; Evil
(use-package evil
  :init
  (setq evil-want-integration nil)
  (use-package evil-surround
    :config
    (global-evil-surround-mode))
  (use-package evil-commentary
    :config
    (evil-commentary-mode))

  :config
  (use-package evil-collection
    :config
    (evil-collection-init))
  (evil-mode)
  (use-package evil-numbers)
  (straight-use-package
   '(evil-textobj-line
     :type git :host github
     :repo "syohex/evil-textobj-line"))
  (use-package evil-org
    :config
    (require 'evil-org-agenda))
  (use-package evil-magit)
  (use-package evil-easymotion))

(use-package general)

;; UI
(straight-use-package
 '(minimal-fringes :type git :host github
                   :repo "SpecialBomb/emacs-minimal-fringes"))
(use-package monokai-theme)
(use-package material-theme)
(use-package dracula-theme)
(use-package telephone-line
  :config
  (telephone-line-evil-config))
(use-package rainbow-delimiters)
(when (not (s-prefix? "26" emacs-version))
  (use-package linum-relative
    :config
    (linum-relative-global-mode)
    (global-linum-mode)))
; (display-line-numbers-mode))
(use-package column-marker)

;; Autocomplete, Syntax
(use-package company
  :config
  (use-package company-shell)
  (use-package company-jedi)
  (use-package company-flx)
  (company-flx-mode))
(use-package flycheck)

;; Languages
;; (use-package xah-elisp-mode)
(use-package picolisp-mode)
(use-package racket-mode
  :config
  (use-package pollen-mode)
  (use-package scribble-mode
    :straight
    (scribble-mode
      :type git :host github
      :repo "emacs-pe/scribble-mode")))
(use-package markdown-mode)
(use-package fish-mode)
(use-package vimrc-mode)
(use-package yaml-mode)
(use-package web-mode)
(use-package clojure-mode
  :config
  (use-package cider))
(when (executable-find "chuck")
  (use-package chuck-mode
    :straight
    (chuck-mode :type git :host github
                :repo "emacsattic/chuck-mode")))

;;; kisaragi-packages.el ends here
