;; ========== Packaging ==========

;; add package repositories
(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

;; install packages if they haven't been installed yet
(defun c/require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (c/require-package package min-version t)))))

(setq c/elpa-packages '(
                        flymake
                        flymake-cursor
                        flymake-jshint
                        flymake-php
                        flymake-python-pyflakes
                        magit
                        maxframe
                        multi-web-mode
                        php-mode
                        python
                        sml-mode
                        solarized-theme
                        ))
(dolist (package c/elpa-packages)
  (c/require-package package))


;; ========== Appearance ==========

;; disable menu bar and tool bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; set font and font size
(set-face-attribute 'default nil :font "courier" :height 160)

;; maximize frame
(require 'maxframe)
(maximize-frame)


;; ========== Key Remapping ==========

;;; I prefer cmd key for meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;; ========== Indentation ==========

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; for C based modes
(setq c-basic-offset 4)


;; ========== Place Backup Files in Specific Directory ==========

;; Enable backup files.
(setq make-backup-files t)

;; Enable versioning with default values (keep five last versions, I think!)
(setq version-control t)

;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))


;; ========== Version control with magit ==========

;; magit configurations
(require 'magit)
(define-key global-map (kbd "C-c mm") 'magit-status)


;; ========== Python ==========

;; use fgallina's python mode
(require 'python)
(add-hook 'python-mode-hook 'flymake-mode)


;; ========== Scala ==========

(add-to-list 'load-path "~/.emacs.d/scala-mode2/")
(require 'scala-mode)


;; ========== Flymake ==========

;; Flymake python configurations
(defun c/flymake-python-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-intemp))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "pycheckers"  (list local-file))))
(eval-after-load 'flymake
  '(progn
     (require 'flymake-cursor)
     (setq flymake-run-in-place nil)  ;; I want my copies in the system temp dir.
     (add-to-list 'flymake-allowed-file-name-masks
                  (list "\\.py\\'" 'c/flymake-python-init))))

;; Flymake php configuration
(require 'flymake-php)
(add-hook 'php-mode-hook 'flymake-php-load)


;; ========== Web developmetn setup ==========

;; load mulit-web-mode
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)


;; ======== I18n ========

(setq utf-translate-cjk-mode nil)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
