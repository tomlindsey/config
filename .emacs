;;;; -*- mode: lisp; -*-
;;;; .emacs
;;;; tom@thomasclindsey.com

;;;
;;; functions
;;;

(defun google (term)
  "use google to search for term"
  (interactive "sSearch: ")
  (browse-url (concat "http://www.google.com/search?q="
                      (url-encode-url term))))
(defun dict (w)
  "use google to lookup word"
  (interactive "sDefine: ")
  (google (concat "definition:" w)))

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, sea ad clita sadipscing, mea id antiopam prodesset. Justo scripta vivendum eum id, in vis essent petentium. Qui mutat tritani epicuri et, utamur percipitur an sea. Ad nullam integre eum. Cu atqui inermis pri, tempor causae sanctus at pro. Ea cum tation hendrerit conclusionemque, veri hendrerit definitionem sit at. Vix adipiscing dissentiet eloquentiam eu, decore epicurei liberavisse eu eam."))

;;;
;;; customize
;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 160 :family "Source Code Pro")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode t nil (frame))
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(delete-selection-mode t nil (delsel))
 '(display-time-24hr-format t)
 '(indent-tabs-mode nil)
 '(ispell-program-name "/usr/local/bin/ispell")
 '(js-indent-level 2)
 '(menu-bar-mode t nil (menu-bar))
 '(org-export-backends (quote (ascii html icalendar latex md odt)))
 '(package-selected-packages
   (quote
    (ac-nrepl auto-compile clojure-mode clojure-mode-extra-font-locking csharp-mode csv-mode elm-mode enh-ruby-mode ensime exec-path-from-shell fsharp-mode graphviz-dot-mode haskell-mode haskell-tab-indent highlight-parentheses htmlize inf-ruby json-mode json-reformat magit markdown-mode org pandoc-mode paredit plantuml-mode python-mode rainbow-delimiters rainbow-mode scala-mode web-mode)))
 '(scroll-preserve-screen-position 1)
 '(send-mail-function (\` mailclient-send-it))
 '(show-paren-mode t)
 '(show-paren-style (\` mixed))
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil nil (tool-bar))
 '(tooltip-mode nil nil (tooltip))
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(user-full-name "Thomas C Lindsey")
 '(user-mail-address "tlindsey@ecisolutions.com")
 '(version-control t))

;;;
;;; modes
;;;
(defun modes-erlang ()
  (setq load-path
        (cons  "/usr/local/opt/erlang/lib/erlang/lib/tools-3.2/emacs"
               load-path))
  (setq erlang-root-dir "/usr/local/opt/")
  (require 'erlang-start))

(defun modes-json ()
  "marmalade package"
  (add-hook 'json-mode-hook (lambda () (setq js-indent-level 2))))

(defun modes-markdown ()
  "http://jblevins.org/projects/markdown-mode/"
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(defun modes-org ()
  "close items with time, key bindings for agenda and store link"
  (setq org-log-done 'time) ; 'note or buffer by buffer with : #+STARTUP: logdone or lognotedone
  (setq org-startup-indented t)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (add-to-list 'auto-mode-alist '("\\.dot$" . graphviz-dot-mode))
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cl" 'org-store-link)

  ;;
  ;; org mode setting for literate programming
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell      . t)
     (java       . t)
     (js         . t)
     (emacs-lisp . t)
     (clojure    . t)
     (python     . t)
     (ruby       . t)
     (css        . t)
     (dot        . t)
     (plantuml   . t))))

(defun modes-scala ()
  "scala mode is provided by scala-mode2 package"
    (setq auto-mode-alist
        (append '(("\\.scala" . scala-mode)
                  ("\\.sbt"   . scala-mode))
                auto-mode-alist)))

(defun modes-stats ()
  "ess.r-project.org for working with R"
  (add-to-list 'load-path "~/lib/emacs/elisp-downloaded/ess-13.09/lisp")
  (require 'ess-site)
  (setq auto-mode-alist
        (append '(("\\.R" . R-mode)
                  ("\\.r" . R-mode))
                auto-mode-alist)))

(defun modes-web ()
  "web-mode from packages.el"
  (let ((indent 4))
    (setq web-mode-markup-indent-offset indent
          web-mode-css-indent-offset indent
          web-mode-code-indent-offset indent))
  (setq auto-mode-alist
        (append '(("\\.phtml" . web-mode)
                  ("\\.php"   . web-mode))
                auto-mode-alist)))

(defun modes ()
  "manually loaded major/minor modes.  many other modes are loaded by package.el"
  (modes-erlang)
  (modes-markdown)
  (modes-org)
  (modes-scala)
  ;(modes-stats)
  (modes-web)
  (modes-json))

;;;
;;; tweaks
;;;

(defun tweak-backup ()
  "location and options for backup and autosave"
  (let ((backup-dir "~/backup/emacs"))
    (setq backup-directory-alist `((".*" . ,backup-dir)))
    (setq auto-save-file-name-transforms `((".*" ,backup-dir t)))
    (setq-default backup-by-copy t)
    (setq delete-old-versions t)))

(defun tweak-key-bindings()
  "home, end, and M-ret"
  (global-set-key [home] 'move-beginning-of-line)
  (global-set-key [end] 'move-end-of-line)
  (if (boundp 'ns-toggle-fullscreen)
      (global-set-key [(meta return)] 'ns-toggle-fullscreen)))

(defun tweak-lisps ()
  "clojure, cider, highlight-parenthesis, paredit loaded from package.el"
  (require 'clojure-mode)

  (setq auto-mode-alist
        (append '(("\\.edn$"    . clojure-mode)
                  ("\\.boot$"   . clojure-mode)
                  ("\\.cljs.*$" . clojure-mode)
                  ("lein.env"   . enh-ruby-mode))
                auto-mode-alist))

  ;;
  ;; clojure
  (add-hook 'clojure-mode-hook (lambda () (highlight-parentheses-mode t)))

  ;;
  ;; cider
  ;; depends on clojure's CLI tools. $ brew install clojure
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (setq cider-auto-select-error-buffer t)
  (setq cider-repl-history-file "~/.emacs.d/cider-history")
  (setq cider-repl-pop-to-buffer-on-connect t) ; go right to the REPL buffer when it's finished connecting
  (setq cider-repl-wrap-history t)             ; Wrap when navigating history.
  (setq cider-show-error-buffer t)             ; When there's a cider error, show its buffer and switch to it

  (setq nrepl-hide-special-buffers t)

  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)))

(defun tweak-package-manager ()
  (setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.org/packages/")
                           ("org"   . "http://orgmode.org/elpa/"))))

(defun tweak-python ()
  (add-hook 'python-mode-hook
            (lambda ()
              (setq python-indent 2)
              (add-to-list 'write-file-functions 'delete-trailing-whitespace))))

(defun tweak-window-manager ()
  (if (not (equal 'nil window-system))
      (progn

        ;; common to all window systems
        (global-font-lock-mode t)
        (add-to-list 'custom-theme-load-path "~/lib/emacs/themes/")
        (scroll-bar-mode -1)
        (add-to-list 'load-path "~/lib/emacs/elisp-downloaded/color-theme-6.6.0")
        (require 'color-theme)
        (eval-after-load "color-theme"
          '(progn
             (color-theme-initialize)
             (load-theme 'solarized-dark)))  ; (deep-blue,gtk-ide,late-night,jonadabian-slate,charcoal-black
        (load-theme 'solarized-dark)

        ;; specific for mac (my main system)
        (if (equal window-system 'ns)
            (progn
              (setq ns-command-modifier 'meta)
              (global-linum-mode t)
              (global-set-key [kp-delete] 'delete-char)
              (setq default-frame-alist
                    (append (list
                             '(active-alpha . 0.9)
                             '(inactive-alpha . 0.6))
                            default-frame-alist))))

        ;; position and size the frame, load a few files, set cwd
        (setq initial-frame-alist
              '((top . 0) (left . 100) (width . 120) (height . 60)))
        (find-file "~/Dropbox/.dates")
        (find-file "~/Dropbox/.links")
        (cd "~/scratch/"))))

(defun tweaks ()
  "remove annoyances, add highlighting, etc."
  ;; lauching emacs in os x which is not under a shell
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))

  ;; not sure why they hardcoded a path
  (setq org-plantuml-jar-path
        (expand-file-name "~/plantuml.jar"))

  ;; annoyances
  (setq inhibit-startup-message t)
  (setq initial-scratch-message nil)
  (setq ring-bell-function 'ignore)
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; highlighting
  (setq-default query-replace-highlight t)
  (setq-default search-highlight t)

  ;; other tweaks
  (setq kill-whole-line t)
  (add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
  (setq require-final-newline t)

  ;; groups of tweaks
  (tweak-python)
  (tweak-backup)
  (tweak-key-bindings)
  (tweak-package-manager)
  (tweak-lisps)
  (tweak-window-manager))

;;;
;;; make emacs my own
;;;
(package-initialize)
(modes)
(tweaks)
