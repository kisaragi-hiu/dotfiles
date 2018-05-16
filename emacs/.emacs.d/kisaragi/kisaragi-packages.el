;; kisaragi-packages.el: package installations
(use-package f)
(use-package fn)
(use-package suggest)
(use-package smartparens)
(use-package parinfer)
(use-package vimish-fold)
(use-package fold-this)
(use-package color-moccur)
(use-package moccur-edit)
(use-package evil-surround)
(use-package evil-commentary)
(use-package evil-leader)
(use-package evil)
(use-package evil-collection)
(use-package evil-numbers)
(use-package evil-textobj-line
  :straight
  (evil-textobj-line
    :type git :host github
    :repo "syohex/evil-textobj-line"))
(use-package evil-easymotion)
;; (use-paclage general)
;; (use-package hydra)

(use-package company)
(use-package company-shell)
(use-package company-flx)
(use-package flycheck)
(use-package ivy)

(use-package xah-elisp-mode)
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
(use-package linum-relative)

(use-package monokai-theme)
(use-package material-theme)
(use-package dracula-theme)

(use-package telephone-line)
(use-package rainbow-delimiters)
(use-package column-marker)
(use-package magit)
(use-package evil-magit)
(straight-use-package
 '(git-undo :type git :host github
            :repo "jwiegley/git-undo-el"))
(use-package evil-org
  :config
  (require 'evil-org-agenda))
(use-package helpful)
(use-package ranger)
(use-package fzf)

;; xmlgenexp->xml
;; where xexp looks like '(tag ([attr "attr-val"]) (nested-tag) "tag-val"),
;; "xmlgenexp" looks like '(tag :attr "attr-val" (nested-tag) "tag-val").
;; -> <tag attr="attr-val"><nested-tag/>tag-val</tag>
(straight-use-package
 '(xmlgen :type git :host github
    :repo "philjackson/xmlgen"))

