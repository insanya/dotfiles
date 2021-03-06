;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration bootstrap

;;; Code:

(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)
(setq use-package-always-defer t)

;; (setq package-check-signature nil)

(org-babel-load-file (expand-file-name "README.org" user-emacs-directory))

(provide 'init)
;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)
