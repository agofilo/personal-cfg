;; Disable magic loading (auto-mode-alist only)
(setq magic-mode-alist ())

;;; Set Custom file location
(setq custom-file "~/.emacs-custom.el")
(load custom-file)

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Locally-installed packages (non-ELPA)

(push "~/.emacs.d/local/" load-path)
(push "~/.emacs.d/local/org-mode/lisp" load-path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aquamacs / Cocoa Emacs stuff

(when (boundp 'aquamacs-version)
  (tool-bar-mode 0)
  (set-default-font "-apple-DejaVu_Sans_Mono-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1"))

(when (fboundp 'tabbar-mode) (tabbar-mode -1))

(when (boundp 'osx-key-mode-map)
  (define-key osx-key-mode-map (kbd "C-;") nil))

(when (fboundp 'fringe-mode) (fringe-mode 0))

;; from https://gist.github.com/1297644
(defun finder (location)
  "Fire up finder in a location relative to pwd."
  (interactive "sOpen finder at this location (relative to pwd): ")
  (start-process "finder" "finder" "open" "-a" "Finder.app" location))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Restrict dangerous functions

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sexp highlighting

(require 'mic-paren)
(paren-activate)
; (setq paren-sexp-mode t) ; Highlight entire sexp
(show-paren-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; paredit

(require 'paredit)

(defun enable-paredit-hook ()
  (paredit-mode +1))

(add-hook 'emacs-lisp-mode-hook       'enable-paredit-hook)
(add-hook 'lisp-mode-hook             'enable-paredit-hook)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-hook)
(add-hook 'scheme-mode-hook           'enable-paredit-hook)
(add-hook 'clojure-mode-hook          'enable-paredit-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLOJURE/SWANK/SLIME

(require 'clojure-mode)

(define-key clojure-mode-map "{" 'paredit-open-brace)
(define-key clojure-mode-map "}" 'paredit-close-brace)
(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)

;; Custom indentation rules; see clojure-indent-function
(define-clojure-indent
  (describe 'defun)
  (testing 'defun)
  (given 'defun)
  (using 'defun)
  (with 'defun)
  (it 'defun)
  (do-it 'defun))

(eval-after-load 'slime
  '(setq slime-protocol-version 'ignore))

(setq inferior-lisp-program "lein repl")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown Mode

(require 'markdown-mode)

(add-hook 'markdown-mode-hook '(lambda () (auto-fill-mode 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mmm-mode (for erb)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDO
;; http://www.emacswiki.org/emacs/InteractivelyDoThings

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SMEX
;; http://github.com/nonsequitur/smex/

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode and Day Pages
;; http://almostobsolete.net/daypage.html

(require 'org)
(require 'htmlize)

(when (fboundp 'set-word-wrap)
  (add-hook 'org-mode-hook 'set-word-wrap))

(setq org-src-fontify-natively t)
(setq font-lock-verbose nil)

(setq org-export-htmlize-output-type 'css)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line-numbering

(require 'hlinum)
(setq linum-format "%4d ")
(global-linum-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color Themes

(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)))

(require 'color-theme-twilight)
(color-theme-twilight)

(setq-default show-trailing-whitespace t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html mode

(add-hook 'html-mode-hook
          (lambda()
			(setq sgml-basic-offset 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FILE ASSOCIATIONS

(add-to-list 'auto-mode-alist '("\\.\\(rdfs?\\|owl\\)$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.st$" . fundamental-mode))
(add-to-list 'auto-mode-alist '("\\.cljs?$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . css-mode))