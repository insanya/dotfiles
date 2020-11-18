;; Set custom file.
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Make all commands of the “package” module present.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

;; use-package configuration.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; Always ensures packages are in the system.
(setq use-package-always-ensure t)
(setq use-package-always-defer t)

;; Automatic package update.
(use-package auto-package-update
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

;; Load Secrets
(load (concat user-emacs-directory "secrets.el"))

;; Disable Package Signature Check
;;(setq package-check-signature nil)

;; Obsolete warnings
(setq byte-compile-warnings '(not obsolete))

;; Tangle configuration
(org-babel-load-file (expand-file-name "README.org" user-emacs-directory))
