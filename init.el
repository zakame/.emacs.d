;;; init.el --- My Emacs customizations
;; Copyright (C) 2005-2015  Zak B. Elep

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
      '((user-position t)))

;; Enable mouse wheel support
(if (fboundp 'mwheel-install) (mwheel-install))

;; Disable menu, tool, and scroll bars
(mapc (lambda (mode)
        (when (fboundp mode) (funcall mode -1)))
      '(menu-bar-mode tool-bar-mode scroll-bar-mode))

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
(when (fboundp 'advice-add)
  (advice-add 'menu-bar-open
              :before '(lambda ()
                         (unless (display-graphic-p)
                           (menu-bar-mode 1))))
  (advice-add 'menu-bar-open
              :after '(lambda ()
                        (unless (display-graphic-p)
                          (menu-bar-mode -1)))))

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
      `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      vc-make-backup-files t)

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

;; I want more descriptive unique buffer names when on Emacs <= 24.3
(when (version<= emacs-version "24.3.1")
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; Enable some commands I need.
(put 'narrow-to-region 'disabled nil)     ; Restrict editing to narrowed
                                        ; region
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Diminish auto-fill
(eval-after-load "simple"
  '(diminish 'auto-fill-function))

;; Use AllOut mode
(use-package allout
  :diminish allout-mode
  :if (>= emacs-major-version 22)
  :init
  (setq allout-auto-activation t))

;; Use Gnus as a mail-user-agent
(setq mail-user-agent 'gnus-user-agent)

;; Rebind `dabbrev-expand' to `hippie-expand' for adaptive auto-completion.
(eval-after-load "dabbrev"
  '(defalias 'dabbrev-expand 'hippie-expand))

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
        helm-apropos-fuzzy-match t
        helm-ff-file-name-history-use-recentf t
        helm-su-or-sudo "su"
        helm-ff-auto-update-initial-value t)
  (helm-mode 1)
  (helm-adaptive-mode 1)
  (helm-autoresize-mode 1)
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
  (helm-projectile-on))

;; Async
(when (use-package dired-aux)
  (use-package dired-async))

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

;; Autocomplete
(use-package auto-complete
  :diminish auto-complete-mode
  :ensure t
  :config
  (use-package auto-complete-config)
  (ac-config-default))

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


;;;_ + Programming

;; Enable ElDoc for automatic documentation of elisp functions
(dolist (hook 
         '(emacs-lisp-mode-hook lisp-interaction-mode-hook ielm-mode-hook))
  (add-hook hook #'turn-on-eldoc-mode))

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
  :init
  (setq pde-extra-setting nil)
  (add-to-list 'auto-mode-alist '("cpanfile\\'" . cperl-mode))
  (add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))
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
  :diminish magit-auto-revert-mode
  :ensure t
  :bind ("C-c g" . magit-status)
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (defun endless/visit-pull-request-url ()
    "Visit the current branch's PR on Github."
    (interactive)
    (browse-url
     (format "https://github.com/%s/compare/%s"
             (replace-regexp-in-string
              "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
              (magit-get "remote"
                         (magit-get-current-remote)
                         "url"))
             (magit-get-current-branch))))
  (define-key magit-mode-map (kbd "V") #'endless/visit-pull-request-url)
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
  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer."
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

;; git-timemachine
(use-package git-timemachine
  :bind (("M-g t" . git-timemachine))
  :ensure t)

;; git-gutter
(use-package git-gutter
  :diminish git-gutter-mode
  :ensure t
  :config
  (global-git-gutter-mode +1)
  (setq git-gutter:hide-gutter t))

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

;; Emacs Code Browser
(use-package ecb
  :defer t
  :ensure t)

;; JavaScript (js2-mode)
(use-package js2-mode
  :ensure t
  :interpreter (("node" . js2-mode))
  :mode "\\.js\\'"
  :config
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
  :mode "\\.yml\\'")

;; show current function/sub in mode-line
(which-function-mode)

;; Go Mode
(use-package go-mode
  :defer t
  :ensure t
  :init
  (add-hook 'before-save-hook #'gofmt-before-save))

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

;; CEDET
(use-package semantic
  :defer 10
  :config
  (use-package semantic/ia)
  (use-package semantic/sb)
  (use-package semantic/bovine/gcc)
  (global-ede-mode 1)
  (semantic-mode 1)
  (global-semantic-idle-scheduler-mode)
  (global-semantic-idle-completions-mode)
  (global-semantic-decoration-mode)
  (global-semantic-highlight-func-mode)
  (global-semantic-show-unmatched-syntax-mode))

;; yasnippet
(use-package yasnippet
  :defer 5
  :diminish yas-minor-mode
  :ensure t
  :init
  (setq yas-verbosity 2)
  :config
  (yas-global-mode 1))


;;;_ + Org-Mode
(use-package org
  :ensure t
  :mode ("\\.\\(org\\|org_archive\\)$" . org-mode)
  :diminish orgstruct-mode
  :init
  (dolist (hook '(text-mode-hook message-mode-hook))
          (add-hook hook #'turn-on-orgstruct))
  :bind (("\C-cl" . org-store-link)
         ("\C-cc" . org-capture)
         ("\C-ca" . org-agenda)
         ("\C-cb" . org-iswitchb))
  :config
  (setq org-log-done 'time))


;;;_ + SX.el
(use-package sx
  :defer t
  :ensure t
  :config
  (use-package sx-tab)
  (use-package sx-search)
  (use-package sx-switchto))


;;;_ + tmux xterm-keys support (for windmove, etc. to work)
(use-package tmux-xterm-keys
  :load-path "site-lisp/tmux-xterm-keys")


;;;_ + Local customizations
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;;;_* Local emacs vars

;; Local variables:
;; mode: emacs-lisp
;; allout-layout: (* 0 : )
;; End:

;;; zakame.emacs ends here
