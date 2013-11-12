;;;; -*- mode: lisp; -*-
;;;; .emacs
;;;; tom@thomasclindsey.com

;;;
;;; customize
;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 140 :family "Source Code Pro")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode t nil (frame))
 '(column-number-mode t)
 '(delete-selection-mode t nil (delsel))
 '(display-time-24hr-format t)
 '(indent-tabs-mode nil)
 '(menu-bar-mode t nil (menu-bar))
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil nil (tool-bar))
 '(tooltip-mode nil nil (tooltip))
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(user-full-name "Thomas C Lindsey")
 '(user-mail-address "tlindsey@foundationsource.com")
 '(version-control t))

;;;
;;; modes
;;;

(defun modes-git ()
  "install git and gitblame: magit was installed with package.el"
  (add-to-list 'load-path "/usr/local/opt/git/share/git-core/contrib/emacs") ; homebrew
  (require 'git)
  (require 'git-blame))

(defun modes-org ()
  "close items with time, key bindings for agenda and store link"
  (setq org-log-done 'time) ; 'note or buffer by buffer with : #+STARTUP: logdone or lognotedone
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cl" 'org-store-link))

(defun modes-scala ()
  ""
  (add-to-list 'auto-mode-alist '("\\.scala" . scala-mode)))

(defun modes-stats ()
  (add-to-list 'load-path "~/lib/emacs/elisp-downloaded/ess-13.09/lisp") ; ess.r-project.org
  (require 'ess-site)
  (setq auto-mode-alist
        (append '(("\\.R" . R-mode)
                  ("\\.r" . R-mode))
                auto-mode-alist)))

(defun modes-web ()
  "web-mode from packages.el"
  (setq auto-mode-alist
        (append '(
                  ("\\.phtml" . web-mode)
                  ("\\.php"   . web-mode))
                auto-mode-alist)))

(defun modes ()
  (modes-git)
  (modes-org)
  (modes-stats)
  (modes-web))

;;;
;;; tweaks
;;;

(defun tweak-backup ()
  "location and options for backup and autosave"
  (let ((backup-dir "~/backup/emacs"))
    (setq backup-directory-alist `((".*" . ,backup-dir)))
    (setq auto-save-file-name-transforms `((".*" ,backup-dir t)))
    (setq-default backup-by-copy t)
    (setq delete-old-versions t))
  nil)

(defun tweak-key-bindings()
  "home, end, and M-ret"
  (global-set-key [home] 'move-beginning-of-line)
  (global-set-key [end] 'move-end-of-line)
  (if (boundp 'ns-toggle-fullscreen)
      (global-set-key [(meta return)] 'ns-toggle-fullscreen))
  nil)

(defun tweak-lisps ()
  (setq inferior-lisp-program "/opt/local/bin/clisp")
  (setq hl-paren-colors '("orange1" "yellow1" "green1" "magenta1" "purple" "cyan" "slateblue1" "red1"))
;;  (autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)
  (add-hook 'clojure-mode-hook          (lambda () (highlight-parentheses-mode t)))
  (add-hook 'emacs-lisp-mode-hook       (lambda () (highlight-parentheses-mode t)))
  (add-hook 'lisp-mode-hook             (lambda () (highlight-parentheses-mode t)))
  (add-hook 'lisp-interaction-mode-hook (lambda () (highlight-parentheses-mode t))))

(defun tweak-package-manager ()
  (setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa"     .  "http://melpa.milkbox.net/packages/"))))

(defun tweak-window-manager ()
  (if (not (equal 'nil window-system))
      (progn
        ;; common to all window managers
        (global-font-lock-mode t)
        (scroll-bar-mode -1)
        (add-to-list 'load-path "~/lib/emacs/elisp-downloaded/color-theme-6.6.0")
        (require 'color-theme)
        (eval-after-load "color-theme"
          '(progn
             (color-theme-initialize)
             (color-theme-shaman)))  ; (deep-blue,gtk-ide,late-night,jonadabian-slate,charcoal-black
        ;;
        ;; for mac systems
        (if (equal window-system 'ns)
            (progn
              (setq ns-command-modifier 'meta)
              (global-set-key [kp-delete] 'delete-char)
              (setq default-frame-alist
                    (append (list
                             '(active-alpha . 0.9)
                             '(inactive-alpha . 0.6))
                            default-frame-alist)))))))

(defun tweaks ()
  "remove annoyances, add highlighting, etc."
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
  (tweak-backup)
  (tweak-key-bindings)
  (tweak-package-manager)
  (tweak-lisps)
  (tweak-window-manager))

;;;
;;; make emacs my own
;;;
(modes)
(tweaks)
(find-file "~/.dates")
(find-file "~/.links")
(cd "~/scratch/")
