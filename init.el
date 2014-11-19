(require 'cl)

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
                        4clojure
                        ac-cider
                        ag
                        auto-complete
                        cider
                        clojure-mode
                        color-theme-sanityinc-solarized
                        expand-region
                        fill-column-indicator
                        go-mode
                        helm
                        helm-ls-git
                        js2-mode
                        magit
                        multiple-cursors
                        python
                        scala-mode2
                        web-mode
                        yasnippet
                        ))
(dolist (package c/elpa-packages)
  (c/require-package package))


;; ========== Appearance ==========

;; disable menu bar and tool bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; turn on column number mode
(column-number-mode 1)

;; set font and font size
(set-face-attribute 'default nil :font "Consolas" :height 140)

;; ;; maximize frame
(toggle-frame-maximized)

;; use steve purcell's version of the solarized theme
(require 'color-theme-sanityinc-solarized)
(load-theme 'sanityinc-solarized-light t)

;; graphically indicate the location of the fill column by drawing a thin line
(require 'fill-column-indicator)
(setq fci-rule-width 2)
(setq fci-rule-color "#eee8d5")
(setq fill-column 80)

;; always add new line at the end
(setq require-final-newline t)


;; ========== Parantheses ==========

(require 'electric)
(electric-pair-mode 1)

(require 'paren)
(set-face-attribute 'show-paren-match nil :background "#d3d3d3")
(show-paren-mode 1)


;; ========== Recent files ==========

(setq recentf-exclude (quote ("tmp"))
      recentf-max-saved-items 300
      recentf-save-file "~/.emacs.d/recentf"
      recentf-mode t)


;; ========== Key Remapping ==========

;; I prefer cmd key for meta
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


;; ========== JavaScript ==========

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; use tern
(add-to-list 'load-path "~/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

(add-hook 'js2-mode-hook 'fci-mode)
(add-hook 'js2-mode-hook
          (lambda ()
            (set-fill-column 80)))

;; ========== Clojure ==========

(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook 'fci-mode)
(add-hook 'clojure-mode-hook
          (lambda ()
            (set-fill-column 80)))

;; cider
(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; ac-cider
(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))

;; Poping-up contextual documentation
(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-cider-popup-doc))


;; ========== Autocomplete ==========

(require 'auto-complete-config)
(ac-config-default)


;; ========== Web development setup ==========

;; load web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

(setq web-mode-markup-indent-offset 4)

;; don't override key mapping for helm
(define-key web-mode-map (kbd "C-;") nil)


;; ========== Helm ==========

(require 'helm-config)
(require 'helm-ls-git)

(helm-mode 1)

(define-key global-map (kbd "C-;") 'c/helm-jump)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)

(defun c/helm-jump ()
  (interactive)
  (helm :sources `(helm-source-buffers-list
                   ,(helm-make-source "Git files" 'helm-ls-git-source)
                   helm-source-recentf
                   helm-source-buffer-not-found)))

(setq helm-idle-delay 0.01)
(setq helm-input-idle-delay 0.01)


;; ========== Multiple Cursors ==========

(require 'multiple-cursors)
(global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)


;; ========== Expand Region ==========

(require 'expand-region)
(global-set-key (kbd "C-c e w") 'er/expand-region)
(global-set-key (kbd "C-c e s") 'er/contract-region)


;; ========== I18n ==========

(setq utf-translate-cjk-mode nil)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;; ========== Silver Searcher ==========

(require 'ag)
(global-set-key (kbd "C-c a g") 'ag-project)


;; ========== Org ==========

(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook
          (lambda ()
            (set-fill-column 80)))
(add-hook 'org-mode-hook 'auto-fill-mode)


;; ========== Check Spelling ==========

(global-set-key (kbd "C-c s w") 'ispell-word)

;; don't override key mapping for helm
(require 'flyspell)
(define-key flyspell-mode-map (kbd "C-;") nil)
