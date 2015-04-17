;;; init.el --- My Emacs customizations
;; Copyright (C) 2005-2013  Zak B. Elep

;;;_* Package description

;; Author       :       Zak B. Elep ( zakame@zakame.net )
;; Date Created :       Thu Sep 29 12:29:42 UTC 2005
;; Purpose      :       Set my personal customizations for the One True Editor.
;; Keywords     :       environment, customization
;; License      :       GNU General Public License (GPL), version 2

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License,
;; or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA


;;;_* Commentary

;; `init.el' (formerly `zakame.emacs') contains the special
;; customizations made by Zak B. Elep ( zakame@zakame.net ) for his own
;; use.  It includes certain customizations for the X interface,
;; font-locking, outlining support, printing, and programming
;; conveniences.
;;
;; This new version is tailored to be more portable (e.g. being able to
;; run on a Mac OS X, Debian, and OpenBSD.)

;;;_ + Usage

;; Put this file inside your `user-emacs-directory'.  Emacs will then
;; run it at startup by default, unless Emacs is invoked as `emacs -q'.

;;;_* Code

;;;_ + General setup

;; Set my full name and email address
(setq-default user-full-name "Zak B. Elep"
              user-mail-address "zakame@zakame.net")

;; Set my default colors
(setq default-frame-alist
;;       '((background-color . "Black")
;;         (foreground-color . "Grey")
;;         (cursor-color . "Green")
      '(
         (user-position t)))

(setq enable-local-eval t)              ; Tell Emacs to obey variables
                                        ; set by the files it reads
(setq visible-bell t)                   ; Blink the screen instead of
                                        ; beeping
(mwheel-install)                        ; Enable wheel mouse support
(set-language-environment "UTF-8")      ; Set my default language
                                        ; environment
(scroll-bar-mode -1)                    ; Remove the tool bar ...
(menu-bar-mode -1)                      ; ... menu bar ...
(tool-bar-mode -1)                      ; ... and the scroll bar in X
(icomplete-mode 1)                      ; Enable IComplete mode
(windmove-default-keybindings)          ; Enable windmove
(winner-mode 1)                         ; Enable winner-mode
(auto-image-file-mode 1)                ; Show images as images, not as
                                        ; semi-random bits
(setq inhibit-startup-message t)        ; No splash screen (well...)

;; Enable File-Name Shadows (currently only available in Emacs 22
(if (>= emacs-major-version 22)
    (file-name-shadow-mode 1))

;; package.el
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(setq package-enable-at-startup nil)

;; use-package
(unless (package-installed-p 'use-package)
  (progn
    (package-refresh-contents)
    (package-install 'use-package)))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)


;;;_ + Editing

;; I want backups in their own directory, and even backup while in VC
(setq backup-directory-alist
      `(("." . ,(expand-file-name (concat user-emacs-directory "backups"))))
      vc-make-backup-files t)

;; I want global font-locking
;(setq font-lock-maximum-size (* 70 1024)) ; buffers should be at most
                                          ; 250K to be fontified
(global-font-lock-mode 1)
(setq font-lock-support-mode 'jit-lock-mode) ; Just In Time font-locking
(setq font-lock-maximum-decoration t)

(add-hook 'text-mode-hook 'turn-on-auto-fill) ; Turn on auto-fill on all
                                              ; major modes
(setq-default fill-column 72)                 ; Set default fill-column
(transient-mark-mode 1)                 ; Show highlight when selecting
                                        ; regions
(line-number-mode 1)                    ; Show line number ...
(column-number-mode 1)                  ; ... and column number on
                                        ; modeline
(show-paren-mode 1)                     ; Automatically makes the
                                        ; matching parenthesis stand out
                                        ; in color
(setq show-paren-style 'expression)     ; Make the entire matched expression
                                        ; stand out
(mouse-avoidance-mode 'cat-and-mouse) ; This moves the mouse pointer out
                                      ; of my way when I type
(temp-buffer-resize-mode 1)             ; Temporary windows should not
                                        ; get into our way
(auto-compression-mode 1)               ; Load Auto-(De)Compression Mode
(setq next-line-add-newlines nil)       ; This disables down-arrow and
                                        ; C-n at the end of a buffer
                                        ; from adding a new line to that
                                        ; buffer

(setq auto-save-timeout 15              ; Auto-save after 15 sec of
                                        ; idleness
      require-final-newline t           ; Always add a newline to file's end
      search-highlight t                ; Highlight search strings
      compilation-window-height 10      ; Set a small window for
                                        ; compiles
      compilation-ask-about-save nil)

;; Enable some commands I need.
(put 'narrow-to-region 'disabled nil)   ; Restrict editing to narrowed
                                        ; region
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Use AllOut mode
(use-package allout
  :if (>= emacs-major-version 22)
  :config
  (allout-init t))

;; Use Gnus as a mail-user-agent
(setq mail-user-agent 'gnus-user-agent)

;; Rebind `dabbrev-expand' to `hippie-expand' for adaptive auto-completion.
(eval-after-load "dabbrev"
  '(defalias 'dabbrev-expand 'hippie-expand))

;; Use Markdown Mode
(use-package markdown-mode
  :load-path "site-lisp/markdown-mode"
  :mode "\\.md\\'"
  :config
  (setq markdown-command "Markdown.pl"))

;; Save point position between editing sessions
(use-package saveplace
  :config
  (setq-default save-place t
                save-place-file (expand-file-name ".places"
                                                  user-emacs-directory)))

;; Auto refresh buffers and dired, and be quiet about it
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; dired-x
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))

;; Powerline
(use-package powerline
  :load-path "site-lisp/powerline"
  :config
  (powerline-default-theme))

;; Moe-Theme
(use-package moe-theme
  :load-path "site-lisp/moe-theme"
  :init
  (add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/moe-theme")
  :config
  (setq moe-theme-highlight-buffer-id t)
  (moe-theme-set-color 'purple)
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (powerline-moe-theme))))
    (powerline-moe-theme))
  (if (string-equal (daemonp) "gnus")
      (moe-theme-set-color 'green)))

;; pretty-mode
(use-package pretty-mode
  :load-path "site-lisp/pretty-mode"
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-pretty-mode))

;; Async
(when (use-package dired-aux)
  (use-package dired-async
    :load-path "site-lisp/emacs-async"))

;; Helm
(use-package helm-config
  :ensure helm
  :bind (("M-x" . helm-M-x)
         ("C-c f" . helm-recentf)
         ("C-h r" . helm-info-emacs)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("M-s o" . helm-occur))
  :config
  (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
  (bind-key "C-i" 'helm-execute-persistent-action helm-map)
  (bind-key "C-z" 'helm-select-action helm-map)
  (setq helm-yank-symbol-first t
        helm-move-to-line-cycle-in-source t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-locate-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-ff-file-name-history-use-recentf t
        helm-ff-auto-update-initial-value t)
  (helm-mode 1)
  (helm-adaptive-mode 1)
  (helm-autoresize-mode 1))

;; Swiper
(use-package swiper
  :load-path "site-lisp/swiper")
(use-package swiper-helm
  :load-path "site-lisp/swiper-helm")

;; Emamux
(use-package emamux
  :load-path "site-lisp/emacs-emamux"
  :init
  (setq emamux:completing-read-type 'helm))

;; Disable automatic scrolling
(setq-default scroll-margin 1
              scroll-conservatively 0
              scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)

;; Multiple Cursors
(use-package multiple-cursors
  :load-path "site-lisp/multiple-cursors"
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; Hungry delete
(use-package hungry-delete
  :load-path "site-lisp/hungry-delete"
  :config
  (global-hungry-delete-mode))

;; Dockerfile
(use-package dockerfile-mode
  :load-path "site-lisp/dockerfile-mode"
  :mode "Dockerfile\\'")

;; Neater vertical separator in emacs -nw
(let ((display-table (or standard-display-table (make-display-table))))
  (set-display-table-slot display-table 'vertical-border (make-glyph-code ?┃))
  (setq standard-display-table display-table))

;; Autocomplete
(use-package auto-complete-config
  :load-path "site-lisp/auto-complete"
  :load-path "site-lisp/popup-el"
  :load-path "site-lisp/fuzzy-el"
  :config
  (ac-config-default))


;;;_ + Programming

;; Automatic online help for library functions
(autoload 'find-tag-tag "etags")
(autoload 'Info-find-node "info")
(defun libc-help (arg)
  (interactive (list (find-tag-tag "C library topic: ")))
  (Info-find-node "libc" arg))

;; Enable ElDoc for automatic documentation of elisp functions
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; Enable magic indents and newlines at C/C++ files
(add-hook 'c-mode-common-hook
          '(lambda () (c-toggle-auto-state 1)))

;; Set my C indentation mode to cc-mode
(setq c-default-style
      '((java-mode . "java")
        (c++-mode . "stroustrup")
        (other . "gnu")))

;; Always indent using spaces, no tabs
(setq-default indent-tabs-mode nil)

;; If perlbrew is used, get PERLBREW_PATH to be used in exec-path and PATH
;; Adapted from https://gist.github.com/960214
(use-package cl)
(let ((perlbrew-init "~/.perlbrew/init"))
  (if (file-readable-p perlbrew-init)
      (dolist (line (with-temp-buffer
                      (insert-file-contents perlbrew-init)
                      (split-string (buffer-string) "\n" t)))
        (with-temp-buffer
          (insert line)
          (goto-char (point-min))
          (search-forward "export " nil t)
          (when (= (point) 8)
            (let* ((idx (search-forward "="))
                   (val (buffer-substring idx (progn (end-of-line) (point))))
                   (key (buffer-substring 8 (decf idx))))
              (if (string-match "PERLBREW_PATH" key)
                  (dolist (perlbin (parse-colon-path val))
                    ; remove trailing /
                    (setq perlbin (substring perlbin 0 -1))
                    (setenv "PATH" (concat perlbin ":" (getenv "PATH")))
                    (add-to-list 'exec-path perlbin)))))))))

;; Use Emacs::PDE for editing Perl
(use-package pde-load
  :load-path "site-lisp/pde/lisp"
  :mode (("\\.psgi\\'" . perl-mode)))

;; Use Perl testing support for Emamux
(use-package emamux-perl-test
  :load-path "site-lisp/emamux-perl-test")

;; This loads generic modes which support e.g batch files
(use-package generic-x)

;; I want a better Scheme mode
(autoload 'scheme-mode
  "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme
  "cmuscheme" "Switch to interactive Scheme buffer." t)
(setq scheme-program-name "mit-scheme")

;; Ruby mode
(use-package ruby-mode
  :mode "\\.rb\\'")

;; VC-Git
(use-package vc-git
  :config
  (add-to-list 'vc-handled-backends 'git))
;; (use-package git
;;   :config
;;   (autoload git-blame-mode "git-blame"
;;     "Minor mode for incremental blame for Git." t))

;; Magit
(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status)
  :config
  (eval-after-load 'info
    '(progn (info-initialize)
            (add-to-list 'Info-additional-directory-list
                         (concat (expand-file-name user-emacs-directory)
                                 "site-lisp/magit"))))
  ; full screen magit
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defun magit-quit-session ()
    "Restores the previous window configuratoin and kills the magit buffer."
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

;; git-timemachine
(use-package git-timemachine
  :load-path "site-lisp/git-timemachine")

;; emmet-mode
(use-package emmet-mode
  :load-path "site-lisp/emmet-mode"
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))

;; AutoComplete for emmet
(use-package ac-emmet
  :load-path "site-lisp/ac-emmet"
  :config
  (add-hook 'sgml-mode-hook 'ac-emmet-html-setup)
  (add-hook 'css-mode-hook 'ac-emmet-css-setup))

;; web-mode
(use-package web-mode
  :load-path "site-lisp/web-mode"
  :mode (("\\.html?\\'" . web-mode)
         ("\\.tt\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.html\\.ep\\'" . web-mode))
  :config
  (setq web-mode-enable-auto-pairing t
        web-mode-enable-auto-closing t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t
        web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property ac-source-emmet-css-snippets))
          ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))))
  (add-hook 'web-mode-before-auto-complete-hooks
            '(lambda ()
               (let ((web-mode-cur-language
                      (web-mode-language-at-pos)))
                 (if (string= web-mode-cur-language "css")
                     (setq emmet-use-css-transform t)
                   (setq emmet-use-css-transform nil)))))
  (add-hook 'web-mode-hook
            '(lambda () (emmet-mode)))
  (setq web-mode-engines-alist
        '(("mojolicious" . "\\.html\\.ep\\'"))))

;; kolon-mode
(use-package kolon-mode
  :load-path "site-lisp/kolon-mode"
  :mode "\\.tx\\'")

;; Emacs Code Browser
(use-package ecb
  :commands ecb-activate
  :load-path "site-lisp/ecb")

;; JavaScript (js2-mode)
(use-package js2-mode
  :load-path "site-lisp/js2-mode"
  :mode "\\.js\\'")

;; use Google Chrome in incognito mode as our default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome-stable"
      browse-url-generic-args '("--incognito " "chrome://newtab"))

;; yaml-mode
(use-package yaml-mode
  :load-path "site-lisp/yaml-mode"
  :mode "\\.yml\\'")

;; show current function/sub in mode-line
(which-function-mode)

;; Go Mode
(use-package go-mode-autoloads
  :load-path "site-lisp/go-mode")

;; Haskell Mode
(use-package haskell-mode-autoloads
  :load-path "site-lisp/haskell-mode"
  :config
  (eval-after-load 'info
    '(progn (info-initialize)
            (add-to-list 'Info-additional-directory-list
                         (concat (expand-file-name user-emacs-directory) 
                                 "site-lisp/haskell-mode")))))

;; Coffee Mode
(use-package coffee-mode
  :load-path "site-lisp/coffee-mode")

;; SCSS-mode
(use-package scss-mode
  :load-path "site-lisp/scss-mode")

;; Haml-mode
(use-package haml-mode
  :load-path "site-lisp/haml-mode")

;; Sass-mode
(use-package sass-mode
  :load-path "site-lisp/sass-mode")

;; CEDET
(global-ede-mode 1)
(use-package semantic/ia)
(use-package semantic/sb)
(use-package semantic/bovine/gcc)
(semantic-mode 1)
(global-semantic-idle-scheduler-mode)
(global-semantic-idle-completions-mode)
(global-semantic-decoration-mode)
(global-semantic-highlight-func-mode)
(global-semantic-show-unmatched-syntax-mode)


;;;_ + Org-Mode
(use-package org
  :load-path "site-lisp/org-mode/lisp"
  :mode "\\.\\(org\\|org_archive\\|txt\\)$"
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)
         ("\C-cb" . org-iswitchb)))


;;;_ + SX.el
(use-package let-alist
  :ensure t)
(use-package sx
  :load-path "site-lisp/sx.el"
  :config
  (use-package sx-tab)
  (use-package sx-search)
  (use-package sx-switchto))


;;;_* Local emacs vars

;; Local variables:
;; mode: emacs-lisp
;; allout-layout: (* 0 : )
;; End:

;;; zakame.emacs ends here
