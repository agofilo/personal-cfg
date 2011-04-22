;;: ELPA
;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;; Color schemes
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(require 'color-theme)
(color-theme-initialize)
(color-theme-billw)

;; Linum
(require 'linum)
(global-linum-mode)
(setq linum-format "%3d ")

;; No menu bar!
(menu-bar-mode 0)

;; No tabs!
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

;; Move saved files elsewhere

(defvar user-temporary-file-directory "~/.emacs.d/tmp")
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transform
      `((".*" ,user-temporary-file-directory t)))

(setq auto-save-default nil)


;; SMEX
;; (require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;ido-mode
(ido-mode 1)

;; Enable paredit by default
(eval-after-load 'clojure-mode
  '(progn
     (require 'paredit)
     (defun clojure-paredit-hook () (paredit-mode +1))
     (add-hook 'clojure-mode-hook 'clojure-paredit-hook)
     
     (define-key clojure-mode-map "{" 'paredit-open-brace)
     (define-key clojure-mode-map "}" 'paredit-close-brace)

     (define-clojure-indent
       (describe 'defun)
       (testing 'defun)
       (given 'defun)
       (using 'defun)
       (with 'defun)
       (it 'defun)
       (do-it 'defun))))

;; Ignore slime protocol version
(eval-after-load 'slime
  '(setq slime-protocol-version 'ignore))

;; Window resize keys
(global-set-key (kbd "M-4") 'shrink-window-horizontally)
(global-set-key (kbd "M-6") 'enlarge-window-horizontally)
(global-set-key (kbd "M-8") 'shrink-window)
(global-set-key (kbd "M-2") 'enlarge-window)

;; 
(global-set-key (kbd "C-}") 'paredit-forward-slurp-sexp)

;; enable nxhtml-mode for erb
(setq 
 nxhtml-global-minor-mode t
 mumamo-chunk-coloring 'submode-colored
 nxhtml-skip-welcome t
 indent-region-mode t
 rng-nxm-auto-validate-flag nil
 nxml-degraded t)
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-nxhtml-mumamo))