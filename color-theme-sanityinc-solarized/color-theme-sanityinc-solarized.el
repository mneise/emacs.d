;;; color-theme-sanityinc-solarized.el --- A version of Ethan Schoonover's Solarized themes

;; Copyright (C) 2011 Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: themes
;; X-URL: http://github.com/purcell/color-theme-sanityinc-solarized
;; URL: http://github.com/purcell/color-theme-sanityinc-solarized
;; Version: {{VERSION}}

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Here are two slightly subdued color themes that are easy on the eyes
;; and cover a reasonably complete set of faces.

;; In Emacs versions without built-in theme support, ie. < 24,
;; color-theme.el is required.

;; Use:

;;   M-x color-theme-solarized-light

;;   M-x color-theme-solarized-dark
;;
;;; Credit:

;; Genius colour selection by Ethan Schoonover:
;; http://ethanschoonover.com/solarized

;; Some faces borrowed from Greg Pfeil's emacs theme:
;; https://github.com/sellout/solarized/blob/master/emacs-color-theme-solarized/color-theme-solarized.el

;;; Code:

(require 'cl)

(defmacro color-theme-sanityinc-solarized--with-colors (mode &rest body)
  "Execute `BODY' in a scope with variables bound to the various solarized colors.

`MODE' should be set to either 'light or 'dark."
  ;; These are the Generic RGB equivalents of the "official" sRGB hex values
  `(let* ((base03  "#042028")            ; (0.0159 0.1265 0.1597)
          (base02  "#0a2832")            ; (0.0394 0.1601 0.1983)
          (base01  "#465a61")            ; (0.2767 0.3567 0.3830)
          (base00  "#52676f")            ; (0.3244 0.4072 0.4385)
          (base0   "#708183")            ; (0.4406 0.5096 0.5169)
          (base1   "#81908f")            ; (0.5060 0.5649 0.5636)
          (base2   "#e9e2cb")            ; (0.9161 0.8900 0.7978)
          (base3   "#fcf4dc")            ; (0.9894 0.9579 0.8641)
          (yellow  "#a57705")            ; (0.6475 0.4675 0.0235)
          (orange  "#bd3612")            ; (0.7418 0.2133 0.0735)
          (red     "#c60007")            ; (0.7770 0.0000 0.0290)
          (magenta "#c61b6e")            ; (0.7774 0.1080 0.4352)
          (violet  "#5859b7")            ; (0.3479 0.3514 0.7179)
          (blue    "#2075c7")            ; (0.1275 0.4627 0.7823)
          (cyan    "#259185")            ; (0.1468 0.5708 0.5250)
          (green   "#728a05")            ; (0.4498 0.5412 0.0202)
          (foregrounds (list base1 base0 base00 base01))
          (backgrounds (list base03 base02))
          (contrast-backgrounds (list base3 base2)))
     (when (eq 'light ,mode)
       (rotatef backgrounds contrast-backgrounds)
       (setq foregrounds (reverse foregrounds)))
     (let ((background (nth 0 backgrounds))
           (alt-background (nth 1 backgrounds))
           (strong (nth 0 foregrounds))
           (normal (nth 1 foregrounds))
           (faint (nth 2 foregrounds))
           (faintest (nth 3 foregrounds))
           (contrast-background (nth 1 contrast-backgrounds))
           (class '((class color) (min-colors 89))))
       ,@body)))

(defmacro color-theme-sanityinc-solarized--face-specs ()
  "Return a backquote which defines a list of face specs.

It expects to be evaluated in a scope in which the various color
names to which it refers are bound."
  (quote
   `(;; Standard font lock faces
     (default ((,class (:foreground ,normal :background ,background))))
     (bold ((,class (:weight bold))))
     (bold-italic ((,class (:slant italic :weight bold))))
     (underline ((,class (:underline t))))
     (italic ((,class (:slant italic))))
     (font-lock-builtin-face ((,class (:foreground ,violet))))
     (font-lock-comment-delimiter-face ((,class (:foreground ,faintest :slant italic))))
     (font-lock-comment-face ((,class (:foreground ,faint :slant italic))))
     (font-lock-constant-face ((,class (:foreground ,violet))))
     (font-lock-doc-face ((,class (:foreground ,faintest :slant italic))))
     (font-lock-doc-string-face ((,class (:foreground ,yellow))))
     (font-lock-function-name-face ((,class (:foreground ,blue))))
     (font-lock-keyword-face ((,class (:foreground ,green))))
     (font-lock-negation-char-face ((,class (:foreground ,green))))
     (font-lock-preprocessor-face ((,class (:foreground ,magenta))))
     (font-lock-regexp-grouping-backslash ((,class (:foreground ,magenta))))
     (font-lock-regexp-grouping-construct ((,class (:foreground ,violet))))
     (font-lock-string-face ((,class (:foreground ,cyan))))
     (font-lock-type-face ((,class (:foreground ,yellow))))
     (font-lock-variable-name-face ((,class (:foreground ,yellow))))
     (font-lock-warning-face ((,class (:foreground ,orange))))
     (shadow ((,class (:foreground ,(fourth foregrounds)))))
     (success ((,class (:foreground ,green))))
     (error ((,class (:foreground ,red))))
     (warning ((,class (:foreground ,orange :weight normal))))
     (success ((,class (:foreground ,green))))

     ;; Flymake
     (flymake-warnline ((,class (:foreground ,yellow :background nil :underline nil))))
     (flymake-errline ((,class (:foreground ,red :background nil :underline nil))))

     ;; ENSIME
     (ensime-warnline-highlight ((,class (:foreground ,yellow :background nil :underline nil))))
     (ensime-errline-highlight ((,class (:foreground ,red :background nil :underline nil))))

     ;; Flyspell
     (flyspell-duplicate ((,class (:foreground ,yellow :background nil :underline nil :weight normal))))
     (flyspell-incorrect ((,class (:foreground ,red :background nil :underline nil :weight normal))))

     ;; mark-multiple
     (mm/mirror-face ((,class (:background ,alt-background))))
     (mm/master-face ((,class (:background ,alt-background))))

     ;; fic-mode
     (fic-face ((,class (:background nil :foreground ,magenta :weight normal))))

     ;; helm
     (helm-action ((,class (:underline nil))))
     (helm-selection ((,class (:foreground ,blue :background ,alt-background :underline nil))))
     (helm-candidate-number ((,class (:foreground ,faintest :background nil :underline nil))))
     (helm-source-header ((,class (:foreground ,blue :background nil :underline nil :height 1.0 :weight bold :family "Consolas"))))
     (helm-visible-mark ((,class (:foreground ,red :background nil :underline nil))))
     (helm-ff-directory ((,class (:foreground ,yellow :background nil :underline nil))))
     (helm-ff-executable ((,class (:foreground ,green :background nil :underline nil))))
     (helm-ff-symlink ((,class (:foreground ,magenta :background nil :underline nil))))
     (helm-ff-prefix ((,class (:foreground ,red :background nil :underline nil))))
     (helm-M-x-key ((,class (:foreground ,green :underline nil))))
     (helm-grep-file ((,class (:foreground ,green :underline nil))))
     (helm-grep-lineno ((,class (:foreground ,yellow :underline nil))))
     (helm-grep-running ((,class (:foreground ,red :underline nil))))
     (helm-grep-finish ((,class (:foreground ,green :underline nil))))
     (helm-buffer-saved-out ((,class (:foreground ,orange :underline nil :background nil))))
     (helm-moccur-buffer ((,class (:foreground ,green :underline nil))))

     ;; bookmark+
     (bmkp-heading ((,class (:foreground ,blue :background nil))))
     (bmkp-X-mark ((,class (:foreground ,red :background nil))))
     (bmkp-D-mark ((,class (:foreground ,red :background nil))))
     (bmkp-*-mark ((,class (:foreground ,magenta :background nil))))
     (bmkp-local-file-without-region ((,class (:foreground ,green :background nil))))
     (bmkp-local-file-with-region ((,class (:foreground ,green :background nil))))
     (bmkp-light-fringe-autonamed ((,class (:foreground ,blue :background nil))))
     (bmkp-light-fringe-non-autonamed ((,class (:foreground ,yellow :background nil))))

     ;; icicles
     (icicle-Completions-instruction-1 ((,class (:foreground ,blue))))
     (icicle-Completions-instruction-2 ((,class (:foreground ,orange))))
     (icicle-candidate-part ((,class (:background ,blue :foreground ,faintest))))
     (icicle-common-match-highlight-Completions ((,class (:foreground ,yellow))))
     (icicle-complete-input ((,class (:foreground ,green))))
     (icicle-completion ((,class (:foreground ,red))))
     (icicle-current-candidate-highlight ((,class (:foreground ,blue :background ,alt-background))))
     (icicle-extra-candidate ((,class (:foreground ,faintest :background ,cyan))))
     (icicle-historical-candidate ((,class (:foreground ,blue))))
     (icicle-input-completion-fail ((,class (:foreground ,normal :background ,violet))))
     (icicle-input-completion-fail-lax ((,class (:foreground ,normal :background ,magenta))))
     (icicle-match-highlight-Completions ((,class (:foreground ,red))))
     (icicle-match-highlight-minibuffer ((,class (:foreground ,faint))))
     (icicle-mode-line-help ((,class (:foreground ,blue))))
     (icicle-msg-emphasis ((,class (:foreground ,cyan))))
     (icicle-multi-command-completion ((,class (:foreground ,red :background ,alt-background))))
     (icicle-mustmatch-completion ((,class (:foreground ,blue :weight bold))))
     (icicle-proxy-candidate ((,class (:background: ,alt-background))))
     (icicle-saved-candidate ((,class (:foreground ,strong))))
     (icicle-search-context-level-1 ((,class (:foreground ,faint :background nil))))
     (icicle-search-context-level-2 ((,class (:foreground ,faint :background nil))))
     (icicle-search-context-level-3 ((,class (:foreground ,faint :background nil))))
     (icicle-search-context-level-4 ((,class (:foreground ,faint :background nil))))
     (icicle-search-context-level-5 ((,class (:foreground ,faint :background nil))))
     (icicle-search-context-level-6 ((,class (:foreground ,faint :background nil))))
     (icicle-search-context-level-7 ((,class (:foreground ,faint :background nil))))
     (icicle-search-context-level-8 ((,class (:foreground ,faint :background nil))))
     (icicle-search-current-input ((,class (:foreground ,normal :background ,green))))
     (icicle-search-main-regexp-current ((,class (:foreground ,normal :background ,yellow))))
     (icicle-search-main-regexp-others ((,class (:foreground ,normal :background ,yellow))))
     (icicle-special-candidate ((,class (:foreground ,normal :background ,green))))
     (icicle-whitespace-highlight ((,class (:foreground ,normal :background ,red))))

     ;; eshell
     (eshell-prompt ((,class (:background nil :foreground ,blue :underline nil :weight normal))))
     (eshell-ls-directory ((,class (:background nil :foreground ,blue :underline nil :weight normal))))
     (eshell-ls-symlink ((,class (:background nil :foreground ,magenta :underline nil :weight normal))))
     (eshell-ls-backup ((,class (:background nil :foreground ,faint :underline nil))))
     (eshell-ls-archive ((,class (:background nil :foreground ,yellow :underline nil :weight normal))))
     (eshell-ls-special ((,class (:background nil :foreground ,magenta :underline nil :weight normal))))
     (eshell-ls-executable ((,class (:background nil :foreground ,green :underline nil :weight normal))))
     (eshell-ls-product ((,class (:background nil :foreground ,faintest :underline nil :weight normal))))

     ;; auto-complete
     (ac-candidate-face ((,class (:background ,alt-background :foreground ,normal :underline nil))))
     (ac-selection-face ((,class (:background ,blue :foreground ,background))))
     (ac-completion-face ((,class (:foreground ,faintest :underline nil))))

     ;; company-mode
     (company-tooltip ((,class (:background ,alt-background :foreground ,normal :underline nil))))
     (company-tooltip-selection ((,class (:background ,alt-background :foreground nil :underline nil))))
     (company-tooltip-common ((,class (:background nil :foreground ,blue :underline nil))))
     (company-tooltip-common-selection ((,class (:background nil :foreground ,orange :underline nil))))
     (company-preview ((,class (:background nil :foreground ,faintest :underline nil))))
     (company-preview-common ((,class (:background nil :foreground ,orange :underline nil))))

     (popup-tip-face ((,class (:background ,alt-background :foreground ,normal))))

     ;; Clojure errors
     (clojure-test-failure-face ((,class (:background nil :inherit flymake-warnline))))
     (clojure-test-error-face ((,class (:background nil :inherit flymake-errline))))
     (clojure-test-success-face ((,class (:background nil :foreground nil :underline ,green))))

     ;; For Brian Carper's extended clojure syntax table
     (clojure-keyword ((,class (:foreground ,yellow))))
     (clojure-parens ((,class (:foreground ,strong))))
     (clojure-braces ((,class (:foreground ,green))))
     (clojure-brackets ((,class (:foreground ,yellow))))
     (clojure-double-quote ((,class (:foreground ,cyan :background nil))))
     (clojure-special ((,class (:foreground ,blue))))
     (clojure-java-call ((,class (:foreground ,magenta))))

     ;; Rainbow-delimiters
     (rainbow-delimiters-depth-1-face ((,class (:foreground ,normal))))
     (rainbow-delimiters-depth-2-face ((,class (:foreground ,cyan))))
     (rainbow-delimiters-depth-3-face ((,class (:foreground ,yellow))))
     (rainbow-delimiters-depth-4-face ((,class (:foreground ,green))))
     (rainbow-delimiters-depth-5-face ((,class (:foreground ,blue))))
     (rainbow-delimiters-depth-6-face ((,class (:foreground ,normal))))
     (rainbow-delimiters-depth-7-face ((,class (:foreground ,cyan))))
     (rainbow-delimiters-depth-8-face ((,class (:foreground ,yellow))))
     (rainbow-delimiters-depth-9-face ((,class (:foreground ,green))))
     (rainbow-delimiters-unmatched-face ((,class (:foreground ,red))))

     ;; MMM-mode
     (mmm-code-submode-face ((,class (:background ,alt-background))))
     (mmm-comment-submode-face ((,class (:inherit font-lock-comment-face))))
     (mmm-output-submode-face ((,class (:background ,alt-background))))

     ;; Search
     (match ((,class (:foreground ,blue :background ,background :inverse-video t))))
     (isearch ((,class (:foreground ,blue :background ,background :inverse-video nil))))
     (isearch-lazy-highlight-face ((,class (:foreground ,blue :background nil :weight bold :inverse-video nil))))
     (isearch-fail ((,class (:background ,background :inherit font-lock-warning-face :inverse-video t))))

     ;; IDO
     (ido-subdir ((,class (:foreground ,faintest))))
     (ido-first-match ((,class (:foreground ,green))))
     (ido-only-match ((,class (:foreground ,cyan))))

     ;; Emacs interface
     (cursor ((,class (:background ,blue))))
     (fringe ((,class (:background ,background))))
     (border ((,class (:background ,alt-background))))
     (border-glyph ((,class (nil))))
     (highlight ((,class (:inverse-video nil :background ,alt-background))))
     (gui-element ((,class (:background ,alt-background :foreground ,normal))))
     (mode-line ((,class (:foreground ,faintest :background ,normal :weight normal :box nil))))
     (mode-line-buffer-id ((,class (:foreground ,base2 :background nil :weight normal))))
     (mode-line-inactive ((,class (:inherit mode-line :foreground ,faintest :background ,normal :weight normal :box nil))))
     (mode-line-emphasis ((,class (:foreground ,strong))))
     (mode-line-highlight ((,class (:foreground ,magenta :box nil :weight bold))))
     (minibuffer-prompt ((,class (:foreground ,blue))))
     (region ((,class (:background ,contrast-background))))
     (secondary-selection ((,class (:background ,alt-background))))

     (header-line ((,class (:background ,alt-background :foreground nil :inherit nil))))
     (trailing-whitespace ((,class (:background ,red :underline nil))))

     ;; Whitespace mode
     (whitespace-line ((t (:background nil :foreground ,faintest))))
     (whitespace-indentation ((t (:background nil :foreground ,faintest))))
     (whitespace-newline ((t (:background nil :foreground ,faintest))))
     (whitespace-space ((t (:background nil :foreground ,faintest))))
     (whitespace-space-after-tab ((t (:background nil :foreground ,faintest))))
     (whitespace-space-before-tab ((t (:background nil :foreground ,faintest))))
     (whitespace-tab ((t (:background nil :foreground ,faintest))))
     (whitespace-trailing ((t (:background ,red :foreground ,faintest))))

     ;; Parenthesis matching (built-in)
     (show-paren-match ((,class (:background ,contrast-background))))
     (show-paren-mismatch ((,class (:background ,magenta))))

     ;; Parenthesis matching (mic-paren)
     (paren-face-match ((,class (:inherit show-paren-match))))
     (paren-face-mismatch ((,class (:inherit show-paren-mismatch))))
     (paren-face-no-match ((,class (:inherit show-paren-mismatch))))

     ;; Parenthesis dimming (parenface)
     (paren-face ((,class (:foreground ,faintest :background nil))))

     (sh-heredoc ((,class (:foreground nil :inherit font-lock-string-face :weight normal))))
     (sh-quoted-exec ((,class (:foreground nil :inherit font-lock-preprocessor-face))))
     (slime-highlight-edits-face ((,class (:foreground ,strong))))
     (slime-repl-input-face ((,class (:weight normal :underline nil))))
     (slime-repl-prompt-face ((,class (:underline nil :weight bold :foreground ,magenta))))
     (slime-repl-result-face ((,class (:foreground ,green))))
     (slime-repl-output-face ((,class (:foreground ,blue :background ,background))))

     ;; diff-hl
     (diff-hl-insert ((,class (:foreground ,green :background ,green))))
     (diff-hl-delete ((,class (:foreground ,red :background ,red))))
     (diff-hl-change ((,class (:foreground ,blue :background ,blue))))

     ;; diff
     (diff-added ((,class (:foreground ,green :background nil))))
     (diff-changed ((,class (:foreground ,violet :background nil))))
     (diff-removed ((,class (:foreground ,orange :background nil))))
     (diff-header ((,class (:foreground ,cyan :background nil))))
     (diff-file-header ((,class (:foreground ,blue :background nil))))
     (diff-hunk-header ((,class (:foreground ,blue))))
     (diff-refine-change ((,class (:weight bold :background nil))))
     (diff-refine-removed ((,class (:foreground ,red :background nil))))
     (diff-refine-added ((,class (:foreground ,green :background nil))))

     ;; ediff
     (ediff-current-diff-A ((,class (:foreground nil :background ,alt-background))))
     (ediff-current-diff-B ((,class (:foreground nil :background ,alt-background))))
     (ediff-current-diff-C ((,class (:foreground nil :background ,alt-background))))
     (ediff-current-diff-Ancestor ((,class (:foreground nil :background ,alt-background))))
     (ediff-even-diff-A ((,class (:foreground nil :background ,faintest))))
     (ediff-even-diff-B ((,class (:foreground nil :background ,faintest))))
     (ediff-even-diff-C ((,class (:foreground nil :background ,faintest))))
     (ediff-even-diff-Ancestor ((,class (:foreground nil :background ,faintest))))
     (ediff-odd-diff-A ((,class (:foreground nil :background ,faintest))))
     (ediff-odd-diff-B ((,class (:foreground nil :background ,faintest))))
     (ediff-odd-diff-C ((,class (:foreground nil :background ,faintest))))
     (ediff-odd-diff-Ancestor ((,class (:foreground nil :background ,faintest))))
     (ediff-fine-diff-A ((,class (:foreground ,red :background nil :weight bold))))
     (ediff-fine-diff-B ((,class (:foreground ,red :background nil :weight bold))))
     (ediff-fine-diff-C ((,class (:foreground ,red :background nil :weight bold))))
     (ediff-fine-diff-Ancestor ((,class (:foreground ,green :background nil :weight bold))))

     ;; undo-tree
     (undo-tree-visualizer-default-face ((,class (:foreground ,normal))))
     (undo-tree-visualizer-current-face ((,class (:foreground ,green :weight bold))))
     (undo-tree-visualizer-active-branch-face ((,class (:foreground ,red))))
     (undo-tree-visualizer-register-face ((,class (:foreground ,yellow))))

     ;; dired+
     (diredp-dir-heading ((,class (:foreground nil :background nil :inherit heading))))
     (diredp-dir-priv ((,class (:foreground ,cyan :background nil))))
     (diredp-exec-priv ((,class (:foreground ,blue :background nil))))
     (diredp-file-name ((,class (:foreground ,yellow))))
     (diredp-file-suffix ((,class (:foreground ,green))))
     (diredp-compressed-file-suffix ((,class (:foreground ,yellow :background nil))))
     (diredp-flag-mark-line ((,class (:background nil :inherit highlight))))
     (diredp-ignored-file-name ((,class (:foreground ,faintest))))
     (diredp-link-priv ((,class (:background nil :foreground ,violet))))
     (diredp-no-priv ((,class (:background nil))))
     (diredp-number ((,class (:foreground ,yellow))))
     (diredp-other-priv ((,class (:background nil :foreground ,magenta))))
     (diredp-rare-priv ((,class (:foreground ,red :background nil))))
     (diredp-read-priv ((,class (:foreground ,green :background nil))))
     (diredp-symlink ((,class (:foreground ,violet))))
     (diredp-write-priv ((,class (:foreground ,yellow :background nil))))
     (diredp-deletion ((,class (:foreground ,red :background nil))))
     (diredp-deletion-file-name ((,class (:foreground ,red :background nil))))
     (diredp-date-time ((,class (:foreground ,cyan :background nil))))

     ;; Magit (a patch is pending in magit to make these standard upstream)
     (magit-branch ((,class (:foreground ,green :weight normal))))
     (magit-item-highlight ((,class (:inherit highlight :background nil))))
     (magit-log-graph ((,class (:foreground ,faintest))))
     (magit-log-sha1 ((,class (:foreground ,yellow))))
     (magit-log-head-label-bisect-bad ((,class (:foreground ,red))))
     (magit-log-head-label-bisect-good ((,class (:foreground ,green))))
     (magit-log-head-label-default ((,class (:foreground ,yellow :box nil :weight bold))))
     (magit-log-head-label-local ((,class (:foreground ,magenta :box nil :weight bold))))
     (magit-log-head-label-remote ((,class (:foreground ,violet :box nil :weight bold))))
     (magit-log-head-label-tags ((,class (:foreground ,cyan :box nil :weight bold))))
     (magit-header ((,class (:inherit nil :foreground ,blue :background nil :weight bold))))
     (magit-section-title ((,class (:foreground ,blue :background nil :weight bold))))

     (link ((,class (:foreground nil :underline nil :weight normal :slant italic))))
     (widget-button ((,class (:underline nil))))
     (widget-field ((,class (:background ,alt-background :box (:line-width 1 :color ,normal)))))

     ;; Compilation (most faces politely inherit from 'success, 'error, 'warning etc.)
     (compilation-column-number ((,class (:foreground ,yellow))))
     (compilation-line-number ((,class (:foreground ,yellow))))
     (compilation-message-face ((,class (:foreground ,blue :underline nil))))
     (compilation-error ((,class (:foreground ,red :weight normal))))
     (compilation-info ((,class (:foreground ,green :weight normal))))

     ;; Grep
     (grep-context-face ((,class (:foreground ,faint))))
     (grep-error-face ((,class (:foreground ,red :weight normal :underline t))))
     (grep-hit-face ((,class (:foreground ,blue :weight normal))))
     (grep-match-face ((,class (:foreground nil :background nil :inherit match))))

     ;; Stop outline-3 from inheriting font-lock-keyword-face, which we've made bold
     (outline-3 ((,class (:inherit nil :foreground ,green))))

     (org-date ((,class (:foreground ,blue :underline nil))))
     (org-agenda-structure ((,class (:foreground ,violet))))
     (org-agenda-date ((,class (:foreground ,blue :underline nil))))
     (org-time-grid ((,class (:foreground ,faintest :underline nil))))
     (org-agenda-date-today ((,class (:foreground ,blue :underline nil :weight normal))))
     (org-agenda-date-weekend ((,class (:foreground ,cyan :underline nil))))
     (org-agenda-dimmed-todo-face ((,class (:foreground ,faint))))
     (org-agenda-done ((,class (:foreground ,green))))
     (org-block ((,class (:foreground ,orange))))
     (org-code ((,class (:foreground ,yellow))))
     (org-column ((,class (:background ,alt-background))))
     (org-document-info ((,class (:foreground ,cyan))))
     (org-document-info-keyword ((,class (:foreground ,green))))
     (org-document-title ((,class (:weight bold :foreground ,yellow :height 1.0))))
     (org-done ((,class (:weight normal :foreground ,green))))
     (org-formula ((,class (:foreground ,orange))))
     (org-footnote ((,class (:foreground ,blue :underline nil))))
     (org-link ((,class (:foreground ,blue :underline nil))))
     (org-scheduled ((,class (:foreground ,green))))
     (org-scheduled-previously ((,class (:foreground ,yellow))))
     (org-scheduled-today ((,class (:foreground ,green))))
     (org-special-keyword ((,class (:foreground ,yellow))))
     (org-table ((,class (:foreground ,violet))))
     (org-todo ((,class (:weight normal :foreground ,red))))
     (org-upcoming-deadline ((,class (:foreground ,yellow))))
     (org-warning ((,class (:weight normal :foreground ,red))))
     (org-hide ((,class (:foreground ,background))))
     (org-habit-ready-face ((,class (:background nil :foreground ,cyan))))
     (org-habit-ready-future-face ((,class (:background nil :foreground ,alt-background))))
     (org-habit-clear-future-face ((,class (:background nil :foreground ,alt-background))))
     (org-habit-clear-face ((,class (:background nil :foreground ,green))))
     (org-habit-alert-future-face ((,class (:background nil :foreground ,orange))))
     (org-habit-alert-face ((,class (:background nil :foreground ,red))))
     (org-habit-overdue-face ((,class (:background nil :foreground ,red))))
     (org-habit-overdue-future-face ((,class (:background nil :foreground ,red))))

     (markdown-header-face ((,class (:inherit header-line))))
     (markdown-url-face ((,class (:inherit link))))
     (markdown-link-face ((,class (:foreground ,blue :underline t))))

     (hl-sexp-face ((,class (:background ,alt-background))))
     (highlight-80+ ((,class (:background ,alt-background))))

     ;; python-mode overrides
     (py-builtins-face ((,class (:foreground ,violet :weight normal))))

     ;; js2-mode
     (js2-warning-face ((,class (:foreground ,orange :underline nil))))
     (js2-error-face ((,class (:foreground ,red))))
     (js2-external-variable-face ((,class (:foreground ,violet))))
     (js2-function-param-face ((,class (:foreground ,green))))
     (js2-instance-member-face ((,class (:foreground ,violet))))
     (js2-jsdoc-html-tag-delimiter-face ((,class (:foreground ,faintest))))
     (js2-jsdoc-html-tag-name-face ((,class (:foreground ,green))))
     (js2-jsdoc-tag-face ((,class (:foreground ,faintest))))
     (js2-jsdoc-type-face ((,class (:foreground ,green))))
     (js2-jsdoc-value-face ((,class (:foreground ,yellow))))
     (js2-private-member-face ((,class (:foreground ,yellow))))
     (js2-private-function-call-face ((,class (:foreground ,orange))))

     ;; js3-mode
     (js3-warning-face ((,class (:underline ,yellow))))
     (js3-error-face ((,class (:underline ,red))))
     (js3-external-variable-face ((,class (:foreground ,magenta))))
     (js3-function-param-face ((,class (:foreground ,blue))))
     (js3-jsdoc-tag-face ((,class (:foreground ,magenta))))
     (js3-jsdoc-type-face ((,class (:foreground ,cyan))))
     (js3-jsdoc-value-face ((,class (:foreground ,violet))))
     (js3-jsdoc-html-tag-name-face ((,class (:foreground ,blue))))
     (js3-jsdoc-html-tag-delimiter-face ((,class (:foreground ,green))))
     (js3-instance-member-face ((,class (:foreground ,blue))))
     (js3-private-function-call-face ((,class (:foreground ,red))))

     ;; nxml
     (nxml-name-face ((,class (:foreground unspecified :inherit font-lock-constant-face))))
     (nxml-attribute-local-name-face ((,class (:foreground unspecified :inherit font-lock-variable-name-face))))
     (nxml-ref-face ((,class (:foreground unspecified :inherit font-lock-preprocessor-face))))
     (nxml-delimiter-face ((,class (:foreground unspecified :inherit font-lock-keyword-face))))
     (nxml-delimited-data-face ((,class (:foreground unspecified :inherit font-lock-string-face))))
     (rng-error-face ((,class (:underline ,red))))

     ;; RHTML
     (erb-delim-face ((,class (:background ,alt-background))))
     (erb-exec-face ((,class (:background ,alt-background :weight bold))))
     (erb-exec-delim-face ((,class (:background ,alt-background))))
     (erb-out-face ((,class (:background ,alt-background :weight bold))))
     (erb-out-delim-face ((,class (:background ,alt-background))))
     (erb-comment-face ((,class (:background ,alt-background :weight bold :slant italic))))
     (erb-comment-delim-face ((,class (:background ,alt-background))))

     ;; Message-mode
     (message-header-other ((,class (:inherit header-line :foreground nil :background nil :weight normal))))
     (message-header-subject ((,class (:inherit message-header-other :foreground ,blue :weight normal))))
     (message-header-to ((,class (:inherit message-header-other :foreground ,cyan :weight normal))))
     (message-header-cc ((,class (:inherit message-header-to :foreground nil :weight normal))))
     (message-header-name ((,class (:inherit header-line :foreground ,faintest :background nil))))
     (message-header-newsgroups ((,class (:foreground ,cyan :background nil :slant normal))))
     (message-header-xheader ((,class (:foreground nil))))
     (message-separator ((,class (:foreground ,magenta))))
     (message-mml ((,class (:foreground ,faintest))))
     (message-cited-text ((,class (:foreground ,faintest))))

     ;; notmuch
     (notmuch-crypto-signature-good-key ((,class (:background nil :foreground ,green))))
     (notmuch-crypto-signature-good ((,class (:background nil :foreground ,green))))
     (notmuch-crypto-signature-bad ((,class (:background nil :foreground ,red))))
     (notmuch-crypto-signature-unknown ((,class (:background nil :foreground ,red))))
     (notmuch-crypto-decryption ((,class (:background nil :foreground ,violet))))
     (notmuch-crypto-part-header ((,class (:background nil :foreground ,faintest))))
     (notmuch-message-summary-face ((,class (:background ,alt-background :foreground nil))))
     (notmuch-tag-face ((,class (:foreground ,blue :weight normal))))

     ;; Gnus
     (gnus-cite-1 ((,class (:inherit outline-1 :foreground nil))))
     (gnus-cite-2 ((,class (:inherit outline-2 :foreground nil))))
     (gnus-cite-3 ((,class (:inherit outline-3 :foreground nil))))
     (gnus-cite-4 ((,class (:inherit outline-4 :foreground nil))))
     (gnus-cite-5 ((,class (:inherit outline-5 :foreground nil))))
     (gnus-cite-6 ((,class (:inherit outline-6 :foreground nil))))
     (gnus-cite-7 ((,class (:inherit outline-7 :foreground nil))))
     (gnus-cite-8 ((,class (:inherit outline-8 :foreground nil))))
     ;; there are several more -cite- faces...
     (gnus-header-content ((,class (:inherit header-line :foreground nil :background nil :weight normal))))
     (gnus-header-subject ((,class (:inherit gnus-header-content :weight bold :foreground ,yellow))))
     (gnus-header-from ((,class (:inherit gnus-header-content :weight bold :foreground ,orange))))
     (gnus-header-name ((,class (:inherit header-line :foreground ,green :background nil))))
     (gnus-button ((,class (:inherit link :foreground nil))))
     (gnus-signature ((,class (:inherit font-lock-comment-face))))

     (gnus-summary-normal-unread ((,class (:foreground ,strong :weight normal))))
     (gnus-summary-normal-read ((,class (:foreground ,normal :weight normal))))
     (gnus-summary-normal-ancient ((,class (:foreground ,cyan :weight normal))))
     (gnus-summary-normal-ticked ((,class (:foreground ,orange :weight normal))))
     (gnus-summary-low-unread ((,class (:foreground ,faint :weight normal))))
     (gnus-summary-low-read ((,class (:foreground ,faintest :weight normal))))
     (gnus-summary-low-ancient ((,class (:foreground ,faintest :weight normal))))
     (gnus-summary-high-unread ((,class (:foreground ,yellow :weight normal))))
     (gnus-summary-high-read ((,class (:foreground ,green :weight normal))))
     (gnus-summary-high-ancient ((,class (:foreground ,green :weight normal))))
     (gnus-summary-high-ticked ((,class (:foreground ,orange :weight normal))))
     (gnus-summary-cancelled ((,class (:foreground ,red :background nil :weight normal))))

     (gnus-group-mail-low ((,class (:foreground ,faintest))))
     (gnus-group-mail-low-empty ((,class (:foreground ,faintest))))
     (gnus-group-mail-1 ((,class (:foreground nil :weight normal :inherit outline-1))))
     (gnus-group-mail-2 ((,class (:foreground nil :weight normal :inherit outline-2))))
     (gnus-group-mail-3 ((,class (:foreground nil :weight normal :inherit outline-3))))
     (gnus-group-mail-4 ((,class (:foreground nil :weight normal :inherit outline-4))))
     (gnus-group-mail-5 ((,class (:foreground nil :weight normal :inherit outline-5))))
     (gnus-group-mail-6 ((,class (:foreground nil :weight normal :inherit outline-6))))
     (gnus-group-mail-1-empty ((,class (:inherit gnus-group-mail-1 :foreground ,faint))))
     (gnus-group-mail-2-empty ((,class (:inherit gnus-group-mail-2 :foreground ,faint))))
     (gnus-group-mail-3-empty ((,class (:inherit gnus-group-mail-3 :foreground ,faint))))
     (gnus-group-mail-4-empty ((,class (:inherit gnus-group-mail-4 :foreground ,faint))))
     (gnus-group-mail-5-empty ((,class (:inherit gnus-group-mail-5 :foreground ,faint))))
     (gnus-group-mail-6-empty ((,class (:inherit gnus-group-mail-6 :foreground ,faint))))

     ;; erc
     (erc-direct-msg-face ((,class (:foreground ,yellow))))
     (erc-error-face ((,class (:foreground ,red))))
     (erc-header-face ((,class (:foreground ,strong :background ,alt-background))))
     (erc-input-face ((,class (:foreground ,green))))
     (erc-my-nick-face ((,class (:foreground ,green))))
     (erc-current-nick-face ((,class (:foreground ,green :weight normal))))
     (erc-nick-default-face ((,class (:weight normal :foreground ,blue))))
     (erc-nick-msg-face ((,class (:weight normal :foreground ,yellow))))
     (erc-notice-face ((,class (:foreground ,cyan :weight normal))))
     (erc-prompt-face ((,class (:foreground ,blue :background nil))))
     (erc-timestamp-face ((,class (:foreground ,faintest :weight normal))))

     (custom-variable-tag ((,class (:foreground ,blue))))
     (custom-group-tag ((,class (:foreground ,blue))))
     )))

(defmacro color-theme-sanityinc-solarized--frame-parameter-specs ()
  "Return a backquote which defines a list of frame parameter specs.

These are required by color-theme's `color-theme-install', but
not by the new `deftheme' mechanism. It expects to be evaluated
in a scope in which the various color names to which it refers
are bound."
  (quote
   `(((background-color . ,background)
      (background-mode . light)
      (border-color . ,normal)
      (cursor-color . ,magenta)
      (foreground-color . ,normal)
      (mouse-color . ,cyan)))))


(defmacro color-theme-sanityinc-solarized--define-theme (mode)
  "Define either the dark or the light theme."
  (let ((name (intern (format "sanityinc-solarized-%s" (symbol-name mode))))
        (doc (format "A version of Ethan Schoonover's 'Solarized' theme (%s version)" mode)))
    `(progn
       (deftheme ,name ,doc)
       (color-theme-sanityinc-solarized--with-colors
        ',mode
        (apply 'custom-theme-set-faces ',name
               (color-theme-sanityinc-solarized--face-specs))
        (custom-theme-set-variables
         ',name
         `(ansi-color-names-vector (vector ,normal ,red ,green ,yellow ,blue ,magenta ,cyan ,background))
         '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])))
       (provide-theme ',name))))


(defun color-theme-sanityinc-solarized (mode)
  "Apply either the dark or the light theme."
  (if (fboundp 'load-theme)
      (let ((name (cond
                    ((eq 'light mode) 'sanityinc-solarized-light)
                    ((eq 'dark mode) 'sanityinc-solarized-dark)
                    (t (error "invalid mode: %s" mode)))))
        (if (> emacs-major-version 23)
            (load-theme name t)
          (load-theme name)))
    (progn
      (require 'color-theme)
      (color-theme-sanityinc-solarized--with-colors
       mode
       (color-theme-install
        `(,(intern (concat "color-theme-sanityinc-solarized-" (symbol-name mode)))
          ,@(color-theme-sanityinc-solarized--frame-parameter-specs)
          ,@(color-theme-sanityinc-solarized--face-specs)))
       ;; ansi-color - comint and other modes that handle terminal color escape sequences
       (setq ansi-color-names-vector (vector normal red green yellow blue magenta cyan background))
       (setq ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])))))

;;;###autoload
(when (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;;###autoload
(defun color-theme-sanityinc-solarized-dark ()
  (interactive)
  (color-theme-sanityinc-solarized 'dark))

;;;###autoload
(defun color-theme-sanityinc-solarized-light ()
  (interactive)
  (color-theme-sanityinc-solarized 'light))


(provide 'color-theme-sanityinc-solarized)
;;; color-theme-sanityinc-solarized.el ends here
