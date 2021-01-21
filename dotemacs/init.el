;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration bootstrap

;;; Code:

;;(setq debug-on-error t)

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)

(setq package-check-signature nil)

(org-babel-load-file (expand-file-name "README.org" user-emacs-directory))
(put 'dired-find-alternate-file 'disabled nil)

(provide 'init)
;;; init.el ends here
