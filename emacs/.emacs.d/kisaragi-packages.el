;; kisaragi-packages.el:
;; installation and enabling of packages
(use-package f)
(use-package suggest)
(use-package smartparens
  :config
  (require smartparens-config)
  (smartparens-global-mode))
(use-package parinfer)
(use-package vimish-fold)
(use-package fold-this)
(use-package color-moccur)
(use-package moccur-edit)
(use-package evil-surround
  :config
  (global-evil-surround-mode))
(use-package evil-commentary
  :config
  (evil-commentary-mode))
(use-package evil-leader
  :config 
  (global-evil-leader-mode))
(use-package evil
  :config
  (evil-mode 1))
(use-package evil-numbers)
(use-package evil-textobj-line
  :straight
  (evil-textobj-line
    :type git :host github
    :repo "syohex/evil-textobj-line"))
(use-package evil-easymotion)
(use-package hydra)

(use-package company)
(use-package company-shell)
(use-package company-flx
  :config
  (company-flx-mode +1))
(use-package flycheck)
(use-package ivy
  :config
  (ivy-mode 1))

(use-package racket-mode)
(use-package pollen-mode)
(use-package scribble-mode
  :straight
  (scribble-mode
    :type git :host github
    :repo "emacs-pe/scribble-mode"))
(use-package markdown-mode)
(use-package fish-mode)
(use-package vimrc-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode)))
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))
(use-package web-mode)
(use-package linum-relative
  :config
  (linum-relative-global-mode 1)
  (global-linum-mode 1)
  (add-hook 'doc-view-mode
            (lambda ()
              (linum-relative-off)
              (linum-mode -1))))
(use-package monokai-theme)
(use-package material-theme)
(use-package dracula-theme)
(use-package telephone-line
  :config
  (require 'telephone-line-config)
  (telephone-line-evil-config))
(use-package rainbow-delimiters)
(use-package column-marker)
(use-package magit)
(use-package evil-magit)
(use-package git-undo
  :straight
  (git-undo :type git :host github
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
(straight-use-package '(xmlgen :type git :host github
                               :repo "philjackson/xmlgen"))

