(require 'cl) ;; Access to unprefixed names i.e. loop and return instead of cl-loop and cl-return
(load "package")
(package-initialize)

;; Add marmalade package archive
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.mlikbox.net/packages/") t)

;; User Info
(setq user-full-name "Yondon Fu")
(setq user-mail-address "yondon.fu@gmail.com")

;; Packages
(defvar yondon/packages '(auto-complete
						  autopair)
  "Default packages")

;; Install default packages
(defun yondon/packages-installed-p ()
  (loop for pkg in yondon/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (yondon/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg yondon/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; Load theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Splash screen
;; Skip to scratch buffer
(setq inhibit-splash-screen t
      initial-scratch-message nil)

(scroll-bar-mode -1) ;; Turn off scroll bar
(tool-bar-mode -1) ;; Turn off tool bar
(menu-bar-mode -1) ;; Turn off menu bar

(setq visible-bell t) ;; Turn off system beep
(setq font-lock-maximum-decoration t) ;; Maximum colors
(setq echo-keystrokes 0.1) ;; Turn down time to echo keystrokes

;; Marking text to treat regions like other text editors
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t) ;; So system clipboard and Emacs clipboard get along

;; Convert tabs to 4 spaces
(setq tab-width 4
      indent-tabs-mode nil)
(setq-default tab-width 4)

(setq column-number-mode t) ;; Turn on column numbers

(show-paren-mode t) ;; Always highlight parentheses

(require 'autopair) ;; Brace structures closed as soon as opening character is typed

(require 'auto-complete-config) ;; Turn on auto complete
(ac-config-default)

(setq make-backup-files nil) ;; Turn off backup files

;; See when file actually ends
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(defalias 'yes-or-no-p 'y-or-n-p) ;; y/n instead of yes/no

;; Key bindings
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("a71be4e5e9e418025daea651f8a1628953abb7af505da5e556e95061b6a6e389" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
