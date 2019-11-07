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
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))

  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)

  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("marmalade" . "http://marmalade-repo.org/packages/")
;;                          ("melpa" . "http://melpa.milkbox.net/packages/")))

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
                        ag
                        wgrep
                        wgrep-ag
                        auto-complete
                        cider
                        clojure-mode
                        color-theme-sanityinc-solarized
                        company
                        expand-region
                        fill-column-indicator
                        fingers
                        go-mode
                        helm
                        helm-ls-git
                        highlight-thing
                        js2-mode
                        magit
                        markdown-mode
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

;; show line numbers
(global-linum-mode 1)

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
(setq magit-last-seen-setup-instructions "1.4.0")


;; ========== JavaScript ==========

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; use tern
;; (add-to-list 'load-path "~/tern/emacs/")
;; (autoload 'tern-mode "tern.el" nil t)
;; (add-hook 'js2-mode-hook (lambda () (tern-mode t)))

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
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)


;; inf-clojure

(defun figwheel-repl ()
  (interactive)
  (inf-clojure "lein figwheel"))

(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)

;; ========== Autocomplete ==========

;; (require 'auto-complete-config)
;; (ac-config-default)


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
(require 'helm-for-files)

(helm-mode 1)

(define-key global-map (kbd "C-;") 'helm-for-files)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)

(unless helm-source-ls-git
  (setq helm-source-ls-git
        (helm-make-source "Git files" 'helm-ls-git-source
          :fuzzy-match helm-ls-git-fuzzy-match)))

(custom-set-variables '(helm-for-files-preferred-list '(helm-source-buffers-list
                                                        helm-source-recentf
                                                        helm-source-ls-git)))

;; (defun c/helm-jump ()
;;   (interactive)

;;   (helm :sources `(helm-source-buffers-list
;;                    helm-source-recentf
;;                    helm-source-ls-git
;;                    helm-source-buffer-not-found)))

(setq helm-idle-delay 0.01)
(setq helm-input-idle-delay 0.01)


;; ========== Multiple Cursors ==========

(require 'multiple-cursors)
(global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)


;; ========== Expand Region ==========

(require 'expand-region)
(global-set-key (kbd "C-c e r") 'er/expand-region)

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
(setq helm-ag-use-agignore t)
(with-eval-after-load 'ag
(add-to-list 'ag-ignore-list "style.css" t)  )

(global-set-key (kbd "C-c a g") 'ag-project)


;; ========== Org ==========

(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook
          (lambda ()
            (set-fill-column 80)))
(add-hook 'org-mode-hook 'auto-fill-mode)
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
(setq org-element-use-cache nil)


;; ========== Check Spelling ==========

(global-set-key (kbd "C-c s w") 'ispell-word)

;; don't override key mapping for helm
(require 'flyspell)
(define-key flyspell-mode-map (kbd "C-;") nil)


;; ========== Use fgeller's Fingers Mode ==========
;; https://github.com/fgeller/fingers.el

(require 'fingers)
(require 'fingers-qwerty)

(setq fingers-region-specifiers fingers-qwerty-region-specifiers)
(setq fingers-keyboard-layout-mapper 'fingers-workman-to-qwerty)
(fingers-reset-bindings)

(define-key fingers-mode-x-map (kbd "g") 'magit-status)
(define-key fingers-mode-x-map (kbd "f") 'helm-find-files)
(define-key fingers-mode-map (kbd "m") 'ag-project)

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "fj" 'global-fingers-mode)

(defun fingers-mode-visual-toggle ()
  (let ((faces-to-toggle '(mode-line mode-line-inactive))
        (enabled-color "light gray")
        (disabled-color "#d6ebd6"))
    (cond (fingers-mode
           (mapcar (lambda (face) (set-face-background face enabled-color))
                   faces-to-toggle))
          (t
           (mapcar (lambda (face) (set-face-background face disabled-color))
                   faces-to-toggle)))))

(add-hook 'fingers-mode-hook 'fingers-mode-visual-toggle)


;; ========== Use fgeller's Highlight-thing.el ==========
;; https://github.com/fgeller/highlight-thing.el

(require 'highlight-thing)
(global-highlight-thing-mode)


(add-hook 'before-save-hook #'gofmt-before-save)

;; ========== Markdown ==========

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; ========== C++ ==========

(require 'clang-format)
(global-set-key (kbd "C-c i") 'clang-format-region)
(global-set-key (kbd "C-c u") 'clang-format-buffer)

(setq clang-format-style-option "llvm")

;; ========== exec-path-from-shell ==========

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; ========== Projectile ==========

(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

;; ========== flycheck-joker ==========

(require 'flycheck-joker)

;; ========== cucumber.el ==========

(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

(global-set-key (kbd "C-u") 'undo)
