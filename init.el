;;; init.el --- My Emacs customizations
;; Copyright (C) 2005-2015  Zak B. Elep

;;; Package description

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


;;; Commentary:

;; `init.el' (formerly `zakame.emacs') contains the special
;; customizations made by Zak B. Elep ( zakame@zakame.net ) for his own
;; use.  It includes certain customizations for the X interface,
;; font-locking, outlining support, printing, and programming
;; conveniences.
;;
;; This new version is tailored to be more portable (e.g. being able to
;; run on a Mac OS X, Debian, and OpenBSD.)

;;; Usage

;; Put this file inside your `user-emacs-directory'.  Emacs will then
;; run it at startup by default, unless Emacs is invoked as `emacs -q'.

;;; Code:

;;; General setup

;; Set my full name and email address
(setq-default user-full-name "Zak B. Elep"
              user-mail-address "zakame@zakame.net")

;; Set my default colors
(setq default-frame-alist
      '((user-position t)))

;; Enable mouse wheel support
(if (fboundp 'mwheel-install) (mwheel-install))

;; Disable menu, tool, and scroll bars
(mapc (lambda (mode)
        (when (fboundp mode) (funcall mode -1)))
      '(menu-bar-mode tool-bar-mode scroll-bar-mode))

(setq load-prefer-newer t)              ; Always load newer elisp
(setq enable-local-eval t)              ; Tell Emacs to obey variables
                                        ; set by the files it reads
(setq visible-bell t)                   ; Blink the screen instead of
                                        ; beeping
(set-language-environment "UTF-8")      ; Set my default language
                                        ; environment
(windmove-default-keybindings)          ; Enable windmove
(winner-mode 1)                         ; Enable winner-mode
(auto-image-file-mode 1)                ; Show images as images, not as
                                        ; semi-random bits
(setq inhibit-startup-message t)        ; No splash screen (well...)
(if (fboundp 'fringe-mode) (fringe-mode 0)) ; no fringes too, please!

;; Temporarily show the menu bar when activated
(defun zakame/toggle-menu-bar (int)
  "Toggle the menu bar, given INT as a switch."
  (unless (display-graphic-p)
    (menu-bar-mode int)))
(when (fboundp 'advice-add)
  (advice-add 'menu-bar-open
              :before '(lambda ()
                         "Toggle the menu bar on."
                         (zakame/toggle-menu-bar 1)))
  (advice-add 'menu-bar-open
              :after '(lambda ()
                        "Toggle the menu bar off."
                        (zakame/toggle-menu-bar -1))))

;; Enable File-Name Shadows (currently only available in Emacs 22
(if (>= emacs-major-version 22)
    (file-name-shadow-mode 1))

;; package.el
(require 'package)
(nconc package-archives
       '(("melpa" . "http://melpa.org/packages/")
         ("org" . "http://orgmode.org/elpa/")))
(setq package-enable-at-startup nil)
(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (progn
    (package-refresh-contents)
    (package-install 'use-package)))
(eval-when-compile
  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept))
  (require 'use-package))
(require 'diminish)
(require 'bind-key)


;;; Editing

;; I want backups in their own directory, and even backup while in VC
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      vc-make-backup-files t)

;; Disable backups for TRAMP files, though
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

(global-font-lock-mode 1)
(setq font-lock-support-mode 'jit-lock-mode) ; Just In Time font-locking
(setq font-lock-maximum-decoration t)

;; Turn on auto-fill on all major modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq-default fill-column 72)           ; Set default fill-column
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
(mouse-avoidance-mode 'cat-and-mouse)   ; Move the mouse pointer out
                                        ; of my way when I type
(temp-buffer-resize-mode 1)             ; Temporary windows should not
                                        ; get into our way
(auto-compression-mode 1)               ; Load Auto-(De)Compression Mode
(setq next-line-add-newlines nil)         ; This disables down-arrow and
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

;; Use imenu to browse use-package blocks
(defun zakame/imenu-use-package ()
  "Extract use-package lines to be used as anchors in imenu."
  (add-to-list 'imenu-generic-expression
               '("Used Packages"
                 "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))
(add-hook 'emacs-lisp-mode-hook #'zakame/imenu-use-package)

;; I want more descriptive unique buffer names when on Emacs <= 24.3
(when (version<= emacs-version "24.3.1")
  (use-package uniquify
    :config
    (setq uniquify-buffer-name-style 'post-forward-angle-brackets)))

;; Enable some commands I need.
(put 'narrow-to-region 'disabled nil)     ; Restrict editing to narrowed
                                        ; region
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Diminish auto-fill
(eval-after-load "simple"
  '(diminish 'auto-fill-function))

;; Use Gnus as a mail-user-agent
(setq mail-user-agent 'gnus-user-agent)


;; hippie-exp
(use-package hippie-exp
  :config
  (global-set-key (kbd "M-/") 'hippie-expand)
  (setq hippie-expand-try-functions-list
        '(
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

;; Use Markdown Mode
(use-package markdown-mode
  :ensure t
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

;; set human-readable sizes in dired
(setq dired-listing-switches "-alh")

;; Powerline
(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

;; Moe-Theme
(use-package moe-theme
  :ensure t
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
      (moe-theme-set-color 'green))
  (moe-dark))

;; pretty-mode
(use-package pretty-mode
  :defer t
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook 'turn-on-pretty-mode))

;; Projectile
(use-package projectile
  :defer t
  :diminish projectile-mode
  :ensure t
  :config
  (projectile-global-mode))

;; recentf tweaks
(use-package recentf
  :defer t
  :config
  (setq recentf-exclude '("TAGS" ".*-autoloads\\.el\\'")))

;; Helm
(use-package helm
  :defer 2
  :diminish helm-mode
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-c f" . helm-recentf)
         ("C-h a" . helm-apropos)
         ("C-h r" . helm-info-emacs)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("M-s o" . helm-occur))
  :config
  (use-package helm-config)
  (setq enable-recursive-minibuffers t
        helm-split-window-in-side-p t
        helm-yank-symbol-first t
        helm-move-to-line-cycle-in-source t
        helm-buffers-fuzzy-matching t
        helm-apropos-fuzzy-match t
        helm-recentf-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-locate-fuzzy-match t
        helm-ff-file-name-history-use-recentf t
        helm-su-or-sudo "su"
        helm-ff-auto-update-initial-value t)
  (helm-mode 1)
  (helm-adaptive-mode 1)
  (helm-autoresize-mode 1)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map
                [remap eshell-pcomplete]
                'helm-esh-pcomplete)
              (define-key eshell-mode-map
                (kbd "M-p")
                'helm-eshell-history)))
  (ido-mode -1))

;; Use helm to describe bindings
(use-package helm-descbinds
  :ensure t
  :bind (("C-h b" . helm-descbinds)))

;; Helm-swoop
(use-package helm-swoop
  :ensure t
  :bind (("M-s p" . helm-swoop)))

;; if Helm is available, use it for projectile completion
(use-package helm-projectile
  :ensure t
  :bind (("C-c p h" . helm-projectile)
         ("C-c p p" . helm-projectile-switch-project))
  :init
  (setq projectile-completion-system 'helm)
  :config
  (helm-projectile-on)
  (setq projectile-switch-project-action 'helm-projectile))

;; Eshell
(use-package eshell
  :defer t
  :config
  (defun zakame/eshell-rename-buffer-before-command ()
    (let* ((last-input
            (buffer-substring eshell-last-input-start eshell-last-input-end)))
      (rename-buffer
       (format "*eshell[%s]$ %s...*" default-directory last-input) t)))
  (defun zakame/eshell-rename-buffer-after-command ()
    (rename-buffer
     (format "*eshell[%s]$ %s*" default-directory
             (eshell-previous-input-string 0)) t))
  (add-hook 'eshell-pre-command-hook
            'zakame/eshell-rename-buffer-before-command)
  (add-hook 'eshell-post-command-hook
            'zakame/eshell-rename-buffer-after-command)
  (use-package em-smart)
  (setq eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell-smart-initialize))))

;; Async
(use-package dired-async
  :config
  (dired-async-mode 1))

;; Emamux
(use-package emamux
  :defer t
  :ensure t
  :init
  (setq emamux:completing-read-type 'helm))

;; Disable automatic scrolling
(setq-default scroll-margin 1
              scroll-conservatively 0
              scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)

;; Multiple Cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; Hungry delete
(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

;; Dockerfile
(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

;; Neater vertical separator in emacs -nw
(let ((display-table (or standard-display-table (make-display-table))))
  (set-display-table-slot display-table
                          'vertical-border (make-glyph-code ?┃))
  (setq standard-display-table display-table))

;; yasnippet
(use-package yasnippet
  :defer 5
  :diminish yas-minor-mode
  :ensure t
  :init
  (setq yas-verbosity 2)
  :config
  (yas-global-mode 1)
  (push 'yas-hippie-try-expand hippie-expand-try-functions-list)
  (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1))))

;; Autocomplete
(use-package auto-complete
  :diminish auto-complete-mode
  :ensure t
  :config
  (use-package auto-complete-config)
  (ac-config-default)
  (setq ac-use-menu-map t)
  (ac-set-trigger-key "TAB")
  (ac-set-trigger-key "<tab>"))

;; Smartparens
(use-package smartparens
  :diminish smartparens-mode
  :ensure t
  :config
  (use-package smartparens-config)
  (smartparens-global-mode 1))

;; xclip-mode
(use-package xclip
  :if (executable-find "xclip")
  :ensure t
  :config
  (turn-on-xclip))

;; Enable tail mode for logs
(use-package autorevert
  :mode (("\\.log\\'" . auto-revert-tail-mode)))

;; Ace Jump mode
(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . ace-jump-mode)
         ("C-c C-0" . ace-jump-mode)))

;; Enable TRAMP and editing files as root (via sudo) on remote hosts
(eval-after-load "tramp"
  '(progn
     (setq tramp-default-method "ssh")
     (add-to-list 'tramp-default-proxies-alist
                  '(nil "\\`root\\'" "/ssh:%h:"))
     (add-to-list 'tramp-default-proxies-alist
                  '((regexp-quote (system-name)) nil nil))))

;; use tramp for connecting to Docker containers
(use-package docker-tramp
  :defer 5
  :ensure t)

;; Turn on flyspell on all text buffers
(add-to-list 'text-mode-hook 'flyspell-mode)
(eval-after-load "flyspell"
  '(diminish 'flyspell-mode))

;; adaptive wrap for long lines
(use-package adaptive-wrap
  :diminish visual-line-mode
  :ensure t
  :config
  (add-hook 'text-mode-hook #'visual-line-mode)
  (add-hook 'text-mode-hook #'adaptive-wrap-prefix-mode))

;; Flycheck
(use-package flycheck
  :diminish flycheck-mode
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Add Perl6 support in flycheck
(use-package flycheck-perl6
  :ensure t)

;; undo-tree
(use-package undo-tree
  :diminish undo-tree-mode
  :ensure t
  :config
  (global-undo-tree-mode 1))

;; scratch buffer with current major mode
(use-package scratch
  :defer t
  :ensure t)


;;; Programming

;; Enable ElDoc for automatic documentation of elisp functions
(dolist (hook
         '(emacs-lisp-mode-hook lisp-interaction-mode-hook ielm-mode-hook))
  (add-hook hook #'eldoc-mode))

;; Don't mention ElDoc mode in modeline
(eval-after-load "eldoc"
  '(diminish 'eldoc-mode))

;; Enable magic indents and newlines at C/C++ files
(add-hook 'c-mode-common-hook
          '(lambda () (c-toggle-auto-newline 1)))

;; Set my C indentation mode to cc-mode
(setq c-default-style
      '((java-mode . "java")
        (c++-mode . "stroustrup")
        (other . "linux")))

;; Always indent using spaces, no tabs
(setq-default indent-tabs-mode nil)

;; Use Emacs::PDE for editing Perl
(use-package pde-load
  :load-path "site-lisp/pde/lisp"
  :init
  (setq pde-extra-setting nil)
  (add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))
  (add-to-list 'auto-mode-alist '("\\.psgi\\'" . perl-mode))
  :config
  (use-package pde-patch)
  (setq cperl-invalid-face nil
        cperl-lazy-help-time 2))

;; diminish abbrevs if loaded
(eval-after-load "abbrev"
  '(diminish 'abbrev-mode))

;; Use Perl testing support for Emamux
(use-package emamux-perl-test
  :load-path "site-lisp/emamux-perl-test")

;; Add Perl6 editing support
(use-package perl6-mode
  :defer t
  :ensure t)

;; If Helm is present, enable helm-perldoc as well
(use-package helm-perldoc
  :bind* (("C-x c h p" . helm-perldoc))
  :ensure t
  :config
  (helm-perldoc:setup))

;; This loads generic modes which support e.g batch files
(use-package generic-x)

;; I want a better Scheme mode
(autoload 'scheme-mode
  "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme
  "cmuscheme" "Switch to interactive Scheme buffer." t)
(setq scheme-program-name "mit-scheme")

;; enable projectile mode for rails projects
(use-package projectile-rails
  :defer t
  :ensure t
  :init
  (add-hook 'projectile-mode-hook 'projectile-rails-on))

;; VC-Git
(use-package vc-git
  :config
  (add-to-list 'vc-handled-backends 'Git))

;; Magit
(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status)
  :init
  (setq magit-last-seen-setup-instructions "2.1.0")
  (setq magit-push-always-verify nil)
  :config
  (defun endless/visit-pull-request-url ()
    "Visit the current branch's PR on Github."
    (interactive)
    (browse-url
     (format "https://github.com/%s/compare/%s"
             (replace-regexp-in-string
              "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
              (magit-get "remote"
                         (magit-get-remote)
                         "url"))
             (cdr (magit-get-remote-branch)))))
  (bind-key "v" #'endless/visit-pull-request-url magit-mode-map)
  (defun endless/add-PR-fetch ()
    "If refs/pull is not defined on a GH repo, define it."
    (let ((fetch-address
           "+refs/pull/*/head:refs/pull/origin/*")
          (magit-remotes
           (magit-get-all "remote" "origin" "fetch")))
      (unless (or (not magit-remotes)
                 (member fetch-address magit-remotes))
        (when (string-match
               "github" (magit-get "remote" "origin" "url"))
          (magit-git-string
           "config" "--add" "remote.origin.fetch"
           fetch-address)))))
  (add-hook 'magit-mode-hook #'endless/add-PR-fetch)
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defun zakame/magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer."
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))
  (bind-key "q" #'zakame/magit-quit-session magit-status-mode-map))

;; git-timemachine
(use-package git-timemachine
  :bind (("M-g t" . git-timemachine))
  :ensure t)

;; diff-hl
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (setq diff-hl-side 'left)
  (diff-hl-margin-mode))

;; emmet-mode
(use-package emmet-mode
  :diminish emmet-mode
  :ensure t
  :init
  (dolist (hook '(sgml-mode-hook css-mode-hook kolon-mode-hook))
    (add-hook hook 'emmet-mode)))

;; AutoComplete for emmet
(use-package ac-emmet
  :ensure t
  :init
  (add-hook 'sgml-mode-hook 'ac-emmet-html-setup)
  (add-hook 'css-mode-hook 'ac-emmet-css-setup))

;; web-mode
(use-package web-mode
  :ensure t
  :mode (("\\.tt\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.html\\.ep\\'" . web-mode))
  :config
  (setq web-mode-enable-auto-pairing t
        web-mode-enable-auto-closing t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t
        web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property ac-source-emmet-css-snippets))
          ("html" . (ac-source-emmet-html-aliases
                     ac-source-emmet-html-snippets))))
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
  :ensure t
  :mode "\\.tx\\'")

;; JavaScript (js2-mode)
(use-package js2-mode
  :ensure t
  :interpreter (("node" . js2-mode))
  :mode "\\.js\\'"
  :config
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (setq js2-highlight-level 3))

;; JavaScript refactoring
(use-package js2-refactor
  :defer t
  :commands js2-refactor-mode
  :diminish js2-refactor-mode
  :ensure t
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))

;; AutoComplete for JavaScript
(use-package ac-js2
  :defer t
  :ensure t
  :init
  (add-hook 'js2-mode-hook 'ac-js2-mode)
  (setq ac-js2-evaluate-calls t))

;; Skewer
(use-package skewer-mode
  :defer t
  :diminish skewer-mode
  :ensure t
  :init
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))

;; use Google Chrome in incognito mode as our default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome-stable"
      browse-url-generic-args '("--incognito " "chrome://newtab"))

;; yaml-mode
(use-package yaml-mode
  :ensure t
  :mode "\\.yml$")

;; show current function/sub in mode-line
(which-function-mode)

;; Go Mode
(use-package go-mode
  :defer t
  :ensure t
  :init
  (add-hook 'before-save-hook #'gofmt-before-save))

;; Go autocompletion
(use-package go-autocomplete
  :ensure t)

;; Errcheck support for Go
(use-package go-errcheck
  :defer t
  :ensure t)

;; Add Go language support to Projectile
(use-package go-projectile
  :ensure t)

;; Haskell Mode
(use-package haskell-mode
  :defer t
  :ensure t)

;; Coffee Mode
(use-package coffee-mode
  :defer t
  :ensure t)

;; SCSS-mode
(use-package scss-mode
  :defer t
  :ensure t)

;; Sass-mode (and haml-mode)
(use-package sass-mode
  :defer t
  :ensure t)

;; Rust
(use-package rust-mode
  :defer t
  :ensure t)

;; Clojure
(use-package clojure-mode
  :defer t
  :ensure t)

;; Cider
(use-package cider
  :defer t
  :ensure t
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (setq nrepl-log-messages t))

;; rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :init
  (dolist (hook '(emacs-lisp-mode-hook cperl-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))


;;; SLIME
(use-package slime
  :defer t
  :ensure t
  :init
  (setq inferior-lisp-program "sbcl"
        slime-contribs '(slime-fancy)))


;;; Org-Mode
(use-package org
  :ensure org-plus-contrib
  :mode ("\\.\\(org\\|org_archive\\)$" . org-mode)
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :bind (("\C-cl" . org-store-link)
         ("\C-cc" . org-capture)
         ("\C-ca" . org-agenda)
         ("\C-cb" . org-iswitchb))
  :config
  ; don't let headline sublevels inherit project tags
  (setq org-tags-exclude-from-inheritance '("PROJECT"))
  ; add some custom commands to the agenda
  (setq org-agenda-custom-commands
        '(("P" "Projects"
           ((tags "PROJECT")))
          ("H" "Home"
           ((agenda)
            (tags-todo "HOME")))
          ("O" "Office"
           ((agenda)
            (tags-todo "OFFICE")))
          ("W" "Weekly Plan"
           ((agenda)
            (todo "TODO")
            (tags "PROJECT")))))
  ; configure org-protocol
  (use-package org-protocol)
  ; configure org-capture
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-capture-templates
        '(("p" "Protocol" entry
           (file+headline (concat org-directory "/notes.org") "Inbox")
           "* %^{Title}\nSource: %u, %c\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n%?")
          ("L" "Protocol Link" entry
           (file+headline (concat org-directory "/notes.org") "Inbox")
           "* %? [[%:link][%:description]] \nCaptured On: %U")))
  ; make windmove work well with org-mode
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)
  ; add some languages we use to babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((perl . t)
     (ruby . t)
     (clojure . t)
     (haskell . t)
     (http . t)
     (sh . t)
     (sql . t)
     (sqlite . t)))
  (setq org-log-done 'time
        org-src-fontify-natively t
        org-src-preserve-indentation t))

;; ob-http (REST client)
(use-package ob-http
  :defer t
  :ensure t)

;; org-dashboard
(use-package org-dashboard
  :defer t
  :ensure t)

;; htmlize
(use-package htmlize
  :defer t
  :ensure t)


;;; SX.el
(use-package sx
  :defer t
  :ensure t
  :config
  (use-package sx-tab)
  (use-package sx-search)
  (use-package sx-switchto))


;;; tmux xterm-keys support (for windmove, etc. to work)
(use-package tmux-xterm-keys
  :load-path "site-lisp/tmux-xterm-keys")


;;; Local customizations
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; init.el ends here
