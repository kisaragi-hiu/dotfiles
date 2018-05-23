;;; kisaragi-packages.el --- package installations
;;; Commentary:
;;; This file contains package installation statements, and maybe variables
;;; that need to be set before a package loads.
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
(use-package helpful)
(use-package ivy)
(use-package moccur-edit)
(use-package which-key)

;; Apps
(use-package ranger)
(use-package magit)
(use-package suggest)

;; Editing
(use-package smartparens)
(use-package parinfer)
(use-package vimish-fold)
(use-package fold-this)
(straight-use-package
 '(git-undo :type git :host github
            :repo "jwiegley/git-undo-el"))
(use-package yasnippet)
(use-package yasnippet-snippets)
(use-package ivy-yasnippet)

;; Evil
(use-package evil-surround)
(use-package evil-commentary)
(use-package evil
  :init
  (setq evil-want-integration nil))
(use-package evil-collection)
(use-package evil-numbers)
(straight-use-package
 '(evil-textobj-line
   :type git :host github
   :repo "syohex/evil-textobj-line"))
(use-package evil-easymotion)
(use-package evil-org
  :config
  (require 'evil-org-agenda))
(use-package general)
(use-package evil-magit)

;; UI
(straight-use-package
 '(minimal-fringes :type git :host github
                   :repo "SpecialBomb/emacs-minimal-fringes"))
(use-package monokai-theme)
(use-package material-theme)
(use-package dracula-theme)
(use-package telephone-line)
(use-package rainbow-delimiters)
(use-package linum-relative)
(use-package column-marker)

;; Autocomplete, Syntax
(use-package company)
(use-package company-shell)
(use-package company-jedi)
(use-package company-flx)
(use-package flycheck)

;; Languages
;; (use-package xah-elisp-mode)
(use-package racket-mode)
(use-package pollen-mode)
(use-package scribble-mode
  :straight
  (scribble-mode
    :type git :host github
    :repo "emacs-pe/scribble-mode"))
(use-package markdown-mode)
(use-package fish-mode)
(use-package vimrc-mode)
(use-package yaml-mode)
(use-package web-mode)

;;; kisaragi-packages.el ends here
