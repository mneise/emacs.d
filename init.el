(defun c/set-exec-path-from-shell-PATH ()
  (let ((path-from-shell 
         (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(c/set-exec-path-from-shell-PATH)

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
                        auto-complete
                        expand-region
                        flymake
                        flymake-cursor
                        flymake-jshint
                        flymake-php
                        flymake-python-pyflakes
                        helm
                        helm-git
                        helm-gtags
                        helm-mercurial-queue
                        magit
                        maxframe
                        multi-web-mode
                        multiple-cursors
                        php-mode
                        python
                        scala-mode2
                        sml-mode
                        solarized-theme
                        ))
(dolist (package c/elpa-packages)
  (c/require-package package))


;; ========== Appearance ==========

;; disable menu bar and tool bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; turn on column number mode
(column-number-mode 1)

;; set font and font size
(set-face-attribute 'default nil :font "Consolas" :height 160)

;; ;; maximize frame
;; (require 'maxframe)
;; (maximize-frame)

;; use color-theme-sanityinc-solarized
(add-to-list 'load-path "~/.emacs.d/color-theme-sanityinc-solarized")
(require 'color-theme-sanityinc-solarized)

;; graphically indicate the location of the fill column by drawing a thin line
(require 'fill-column-indicator)
(setq fci-rule-width 2)

;; Show fill column line when php mode is enables
(add-hook 'php-mode-hook 'fci-mode)


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

;; Make backups by copying
(setq backup-by-copying t)

;; Enable versioning with default values
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Save all backup file in this directory.
(setq backup-directory-alist (quote (("." . "~/.emacs.d/backups/"))))


;; ========== Version control with magit ==========

;; magit configurations
(require 'magit)
(define-key global-map (kbd "C-c mm") 'magit-status)


;; ========== Version control with monky ==========

(add-to-list 'load-path "~/.emacs.d/monky")
(require 'monky)
(setq monky-process-type 'cmdserver)
(define-key global-map (kbd "C-c mh") 'monky-status)


;; ========== Python ==========

;; use fgallina's python mode
(require 'python)
(add-hook 'python-mode-hook 'flymake-mode)

;; ========== PHP ==========

(require 'php-mode)

(defun c/php-mode-initialization () 
  (gtags-mode 1))

(add-hook 'php-mode-hook 'c/php-mode-initialization)

;; ========== Autocomplete ==========

(require 'auto-complete-config)
(ac-config-default)

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


;; ========== GNU Global ==========

(add-to-list 'load-path "~/.emacs.d/gtags/")
(require 'gtags)

;;Update gtags database to synchronize the changes in source code (https://github.com/fgeller/emacs.d/blob/master/init.org)

(defun c/gtags-update-single (filename gtags-root) "Update GNU Global database in GTAGS-ROOT for changes in file named FILENAME."
  (interactive) 
  (start-process "update-gtags" "update-gtags" "bash" "-c" 
                  (concat "cd " gtags-root " ; gtags -i --single-update " filename )))

(defun c/gtags-update-current-file () "Updates a GNU Global database based on the definitions in the current file."
  (interactive) 
  (let* ((gtags-root (gtags-get-rootpath)) 
         (filename (buffer-file-name (current-buffer)))) 
    (c/gtags-update-single filename gtags-root) 
    (message "Gtags updated for %s" filename)))

(defun c/gtags-update-hook () "Optionally updates the GNU Global database incrementally, if applicable."
  (when (and (boundp 'gtags-mode) gtags-mode) 
    (when (gtags-get-rootpath) (c/gtags-update-current-file))))

(defun c/initialize-gtags-mode () 
  (add-hook 'after-save-hook 'c/gtags-update-hook)) 

(add-hook 'gtags-mode-hook 'c/initialize-gtags-mode)

(define-key gtags-mode-map (kbd "C-h ;") 'helm-gtags-find-tag)
(define-key gtags-mode-map (kbd "C-h ,") 'helm-gtags-find-rtag)

;; ========== Web developmetn setup ==========

;; load mulit-web-mode
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)


;; ========== Helm ==========

(require 'helm-config)
(helm-mode 1)

(require 'helm-git)
(require 'helm-ls-hg)

(eval-after-load 'helm '(eval-after-load 'gtags '(progn (require 'helm-gtags))))

(define-key global-map (kbd "C-;") 'c/helm-jump)

(setq helm-ff-auto-update-initial-value nil)

(defun c/helm-jump ()
  (interactive)
  (helm-other-buffer
   '(
     helm-c-source-buffers-list
     helm-c-source-git-files
     helm-c-source-hg-list-files
     helm-c-source-recentf
     helm-c-source-buffer-not-found
     )
   "*c/helm-jump*"))

(eval-after-load 'helm-git
  '(progn
     (defadvice helm-c-git-files (around check-git-repo-p)
       (when (and (boundp 'default-directory)
                  (magit-get-top-dir default-directory))
         ad-do-it))
     (ad-activate 'helm-c-git-files)))

(eval-after-load 'helm-ls-hg
  '(progn
     (defadvice helm-hg-list-files (around check-hg-repo-p)
       (when (helm-hg-root)
         ad-do-it))
     (ad-activate 'helm-hg-list-files)))

;; ========== Multiple Cursors ==========

;; https://github.com/magnars/multiple-cursors.el

(require 'multiple-cursors)

(global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)

;; ========== Expand Region ==========

;; git://github.com/magnars/expand-region.el.git

(require 'expand-region)

(global-set-key (kbd "C-c >") 'er/expand-region)
(global-set-key (kbd "C-c <") 'er/contract-region)

;; ========== Parantheses ==========

(require 'electric)

;; ========== I18n ==========

(setq utf-translate-cjk-mode nil)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
