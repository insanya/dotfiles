;;; early-init.el --- -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration bootstrap

;;; Code:

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode 0)

(provide 'early-init)
;;; early-init.el ends here
