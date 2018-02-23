(setq pollen-packages
  '(
    company
    (company-pollen :toggle (configuration-layer/package-usedp 'company))
    pollen-mode
    ))

(defun pollen/post-init-company ()
  (spacemacs|add-company-hook pollen-mode))

(defun pollen/init-company-pollen ()
  (use-package company-pollen
    :init
    (progn
      (push 'company-pollen-backend company-backends-pollen-mode))))

(defun pollen/init-pollen-mode ()
  (use-package pollen-mode
    :config
    (progn
      ;; smartparens configuration
      (with-eval-after-load 'smartparens
        (add-to-list 'sp--lisp-modes 'pollen-mode)
        (when (fboundp 'sp-local-pair)
          (sp-local-pair 'pollen-mode "'" nil :actions nil)
          (sp-local-pair 'pollen-mode "`" nil :actions nil)))

      (defun spacemacs/pollen-server-start ()
        "Call `pollen-server-start'."
        (interactive)
        (pollen-server-start))

      (dolist (prefix '(("ms" . "server")))
        (spacemacs/declare-prefix-for-mode 'pollen-mode (car prefix) (cdr prefix)))

      (spacemacs/set-leader-keys-for-major-mode 'pollen-mode
        ;; server
        "sb" 'pollen-server-start
        "sp" 'pollen-server-stop))))
