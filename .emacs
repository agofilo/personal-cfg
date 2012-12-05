;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Setup only works with Emacs 24
(require 'package)

(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)

(add-to-list 'package-archives
	     '("tromey" . "http://tromey.com/elpa/") t)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)


(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ELPA packages

;(package-refresh-contents)

(defun conditional-install (name)
  (unless (require name nil t)
    (package-install name)))

(conditional-install 'css-mode)
(conditional-install 'paredit)
(conditional-install 'ruby-mode)
(conditional-install 'ruby-electric)
(conditional-install 'smex)
(conditional-install 'paredit)
(conditional-install 'magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nREPL

;; Get the latest version...
(unless (require 'nrepl nil t)
  (package-install-file "~/.emacs.d/local/nrepl.el"))

;; Disable prompt on killing buffer with a process
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(defun nrepl-kill ()
  "Kill all nrepl buffers and processes"
  (interactive)
  (when (get-process "nrepl-server")
    (set-process-sentinel (get-process "nrepl-server")
                          (lambda (proc evt) t)))
  (dolist (buffer (buffer-list))
    (when (string-prefix-p "*nrepl" (buffer-name buffer))
      (kill-buffer buffer))))

(defun nrepl-me ()
  (interactive)
  (nrepl-kill)
  (nrepl-jack-in nil))

;; Kill nrepl-errors with escape key
(defun kill-nrepl-error-buffer ()
  (interactive)
  (let ((buf (get-buffer "*nREPL error*")))
    (when buf (kill-buffer buf))))

(global-set-key (kbd "<escape>") 'kill-nrepl-error-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Locally-installed packages (non-ELPA)

(push "~/.emacs.d/local/" load-path)
(push "~/.emacs.d/local/org-mode/lisp" load-path)

;; Disable magic loading (auto-mode-alist only)
(setq magic-mode-alist ())

;;; Set Custom file location
(setq custom-file "~/.emacs-custom.el")
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Restrict dangerous functions

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sexp highlighting

;(require 'mic-paren)
;(paren-activate)
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
(add-hook 'nrepl-mode-hook            'enable-paredit-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Textmate Mode (fuzzy file finder, etc)

(require 'textmate)
(textmate-mode)

(global-set-key (kbd "C-x f") 'textmate-goto-file)
(define-key  *textmate-mode-map* (kbd "C-c C-k") nil) ;; reserved for nRepl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLOJURE

(require 'clojure-mode)

(eval-after-load 'slime
  '(setq slime-protocol-version 'ignore))

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

(add-hook 'clojure-mode-hook
          '(lambda ()
             (define-key clojure-mode-map
               "\e\C-x" 'lisp-eval-defun)
             (define-key clojure-mode-map
               "\C-x\C-e" 'lisp-eval-last-sexp)
             (define-key clojure-mode-map
               "\C-c\C-e" 'lisp-eval-last-sexp)
             (define-key clojure-mode-map
               "\C-c\C-r" 'lisp-eval-region)
             (define-key clojure-mode-map
               "\C-c\C-z" 'run-lisp)))

(defun repl ()
  "Run 'lein repl' in an inferior-lisp."
  (interactive)
  (setq inferior-lisp-program "lein trampoline run -m clojure.main")
  (inferior-lisp "lein trampoline run -m clojure.main"))


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

;; Make ido-mode list things vertically
(setq ido-decorations
      (quote
       ("\n-> "           ; Opening bracket around prospect list
        ""                ; Closing bracket around prospect list
        "\n   "           ; separator between prospects
        "\n   ..."        ; appears at end of truncated list of prospects
        "["               ; opening bracket around common match string
        "]"               ; closing bracket around common match string
        " [No match]"     ; displayed when there is no match
        " [Matched]"      ; displayed if there is a single match
        " [Not readable]" ; current diretory is not readable
        " [Too big]"      ; directory too big
        " [Confirm]")))   ; confirm creation of new file or buffer

;; And let us use standard navagation keys that make sense vertically
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map [down] 'ido-next-match)
            (define-key ido-completion-map [up] 'ido-prev-match)
            (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
            (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Change the way emacs handles buffer
;; names for files with the same name.

(require 'uniquify)
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":")


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

;;(require 'color-theme)
;;(eval-after-load "color-theme"
;; '(progn
;;(color-theme-initialize)))

(require 'color-theme-twilight)
  (color-theme-twilight)

 (setq-default show-trailing-whitespace t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; word-count fn

(defun count (&optional begin end)
  "count words between BEGIN and END (region); if no region defined, count words in buffer"
  (interactive "r")
  (let ((b (if mark-active begin (point-min)))
	(e (if mark-active end (point-max))))
    (message "Word count: %s" (how-many "\\w+" b e))))

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
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aquamacs / Cocoa Emacs stuff

(set-default-font "-apple-DejaVu_Sans_Mono-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")

(when (fboundp 'tabbar-mode) (tabbar-mode -1))

(when (boundp 'osx-key-mode-map)
  (define-key osx-key-mode-map (kbd "C-;") nil))

(when (fboundp 'fringe-mode) (fringe-mode 0))

;; from https://gist.github.com/1297644
(defun finder (location)
  "Fire up finder in a location relative to pwd."
  (interactive "sOpen finder at this location (relative to pwd): ")
  (start-process "finder" "finder" "open" "-a" "Finder.app" location))

;; Tabs are 2 spaces
(setq-default indent-tabs-mode nil)
(setq standard-indent 2)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
(setq js-indent-level 4)
(setq sgml-basic-offset 2)
(setq css-indent-offset 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text scaling

(define-globalized-minor-mode
  global-text-scale-mode
  text-scale-mode
  (lambda () (text-scale-mode 1)))

(defun global-text-scale-adjust (inc) (interactive)
  (text-scale-set 1)
  (kill-local-variable 'text-scale-mode-amount)
  (setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
  (global-text-scale-mode 1))

(global-set-key (kbd "M-0")
                '(lambda () (interactive)
                   (global-text-scale-adjust (- text-scale-mode-amount))
                   (global-text-scale-mode -1)))
(global-set-key (kbd "M-=")
                '(lambda () (interactive) (global-text-scale-adjust 1)))
(global-set-key (kbd "M--")
                '(lambda () (interactive) (global-text-scale-adjust -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc prefs

(global-auto-revert-mode t)
(auto-save-mode nil)
(setq auto-save-default nil)
(setq make-backup-files nil)
