;;; init.el --- My Emacs customizations
;; Copyright (C) 2005-2016  Zak B. Elep

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

;; High GC threshold for Emacs
(setq gc-cons-threshold 20000000)

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

;; quelpa
(use-package quelpa-use-package
  :ensure t
  :config
  (setq quelpa-checkout-melpa-p nil))


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

(setq search-whitespace-regexp ".*?")   ; match anything (non-greedy)

(setq auto-save-timeout 15              ; Auto-save after 15 sec of
                                        ; idleness
      require-final-newline t           ; Always add a newline to file's end
      search-highlight t                ; Highlight search strings
      compilation-window-height 10      ; Set a small window for
                                        ; compiles
      compilation-scroll-output
      'first-error                      ; Follow compilation scrolling
                                        ; until the first error
      compilation-ask-about-save nil)

;; diminish "Compiling" mode line
(eval-after-load "compile"
  '(diminish 'compilation-in-progress))

;; Use imenu to browse use-package blocks
(defun zakame/imenu-use-package ()
  "Extract use-package lines to be used as anchors in imenu."
  (add-to-list 'imenu-generic-expression
               '("Used Packages"
                 "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))
(add-hook 'emacs-lisp-mode-hook #'zakame/imenu-use-package)

;; I want more descriptive unique buffer names when on Emacs <= 24.3
(use-package uniquify
  :if (version<= emacs-version "24.3.1")
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

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
          try-expand-dabbrev-from-kill
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

;; Markdown previewer (especially for GFH)
(use-package gh-md
  :ensure t
  :after markdown-mode
  :bind (:map markdown-mode-map
              ("C-c C-r" . gh-md-render-buffer)))

;; Save point position between editing sessions
(use-package saveplace
  :config
  (setq-default save-place t
                save-place-file (expand-file-name ".places"
                                                  user-emacs-directory)))

(use-package tramp
  :defer t
  :init
  ;; Before loading TRAMP, fix up its detection for ssh ControlMaster
  ;; feature
  (setq tramp-ssh-controlmaster-options
        (concat "-o ControlMaster=auto "
                "-o ControlPath='tramp.%%C' "
                "-o ControlPersist=no"))
  :config
  (setq tramp-default-method "ssh")
  ;; Enable TRAMP and editing files as root (via sudo) on remote hosts
  (add-to-list 'tramp-default-proxies-alist
               '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist
               '((regexp-quote (system-name)) nil nil)))

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
  ;; I don't like the recent powerline window focus change, so undo it
  (remove-hook 'focus-out-hook 'powerline-unset-selected-window)
  (add-hook 'focus-out-hook 'powerline-set-selected-window)
  (powerline-default-theme))

;; Moe-Theme
(use-package moe-theme
  :ensure t
  :config
  (setq moe-theme-highlight-buffer-id t)
  (moe-theme-set-color 'red)
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

;; recentf tweaks
(use-package recentf
  :defer t
  :config
  (setq recentf-exclude '("TAGS" ".*-autoloads\\.el\\'")))

;; Projectile
(use-package projectile
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
        helm-imenu-fuzzy-match t
        helm-M-x-fuzzy-match t
        helm-ff-file-name-history-use-recentf t
        helm-ff-guess-ffap-filenames t
        helm-ff-newfile-prompt-p nil
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
  :after helm
  :bind (("C-h b" . helm-descbinds)))

;; Helm-swoop
(use-package helm-swoop
  :ensure t
  :after helm
  :bind (("M-s p" . helm-swoop)))

;; Helm-flx
(use-package helm-flx
  :ensure t
  :config
  (helm-flx-mode +1))

;; Helm projectile
(use-package helm-projectile
  :ensure t
  :after helm
  :bind (("C-c p h" . helm-projectile)
         ("C-c p p" . helm-projectile-switch-project))
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  (setq projectile-switch-project-action 'projectile-dired))

;; wgrep-helm
(use-package wgrep-helm
  :ensure t
  :after helm
  :config
  (setq wgrep-auto-save-buffer t))

;; Ansi-Term tweaks
(use-package term
  :bind (("C-c t" . ansi-term))
  :config
  (defadvice term-sentinel (around ansi-term-kill-buffer (proc msg))
    (if (memq (process-status proc) '(signal exit))
        (let ((buffer (process-buffer proc)))
          ad-do-it
          (kill-buffer buffer))
      ad-do-it))
  (ad-activate 'term-sentinel)
  (defadvice ansi-term (before ansi-term-force-shell)
    (interactive (list (getenv "SHELL"))))
  (ad-activate 'ansi-term)
  (add-hook 'term-mode-hook 'goto-address-mode)
  (add-hook 'term-exec-hook
            '(lambda ()
               (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))

;; Eshell
(use-package eshell
  :bind (("C-c e" . eshell))
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
  :diminish hungry-delete-mode
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

;; pos-tip
(use-package pos-tip
  :ensure t)

;; yasnippet
(use-package yasnippet
  :after helm
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
  :after helm
  :config
  (use-package auto-complete-config)
  (ac-config-default)
  (add-to-list 'ac-modes 'html-mode)
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

;; Enable tail mode for logs
(use-package autorevert
  :diminish auto-revert-mode
  :mode (("\\.log\\'" . auto-revert-tail-mode)))

;; Ace Jump mode
(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . ace-jump-mode)
         ("C-c C-0" . ace-jump-mode)))

;; use tramp for connecting to Docker containers
(use-package docker-tramp
  :after tramp
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

;; undo-tree
(use-package undo-tree
  :diminish undo-tree-mode
  :ensure t
  :config
  (global-undo-tree-mode 1))

;; expand-region
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

;; pdf-tools (Linux only unfortunately)
(use-package pdf-tools
  :after yasnippet
  :ensure t
  :if (string= system-type 'gnu/linux)
  :config
  (pdf-tools-install))

;; make window splits much smarter especially when on widescreen
(defun zakame/split-window-prefer-side-by-side (window)
  "Split WINDOW, preferably side by side."
  (let ((split-height-threshold (and (< (window-width window)
                                        split-width-threshold)
                                     split-height-threshold)))
    (split-window-sensibly window)))
(setq split-window-preferred-function
      #'zakame/split-window-prefer-side-by-side)

;; which-key-mode
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))


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

;; Use cperl-mode for editing Perl
(use-package cperl-mode
  :mode "\\.\\([pP]\\([Llm]\\|erl\\|od\\|sgi\\)\\|al\\|t\\)\\'"
  :preface
  (defalias 'perl-mode 'cperl-mode)
  :config
  (cperl-lazy-install)
  (setq cperl-invalid-face nil
        cperl-indent-level 4
        cperl-indent-parens-as-block t
        cperl-close-paren-offset -4
        cperl-continued-statement-offset 4
        cperl-tab-always-indent t
        cperl-lazy-help-time 2)
  (dolist (face '(cperl-array-face cperl-hash-face))
    (set-face-attribute face nil
                        :background 'unspecified)))

;; Add perltidy.el from the wiki
(use-package perltidy
  :quelpa (perltidy :fetcher url :url "https://www.emacswiki.org/emacs/download/perltidy.el")
  :after cperl-mode
  :bind (:map cperl-mode-map
              ("C-c <tab> r" . perltidy-region)
              ("C-c <tab> b" . perltidy-buffer)
              ("C-c <tab> s" . perltidy-subroutine)
              ("C-c <tab> t" . perltidy-dwim-safe)))

;; diminish abbrevs if loaded
(eval-after-load "abbrev"
  '(diminish 'abbrev-mode))

;; Add Perl6 editing support
(use-package perl6-mode
  :defer t
  :ensure t)

;; Add Perl6 support in flycheck
(use-package flycheck-perl6
  :after flycheck
  :ensure t)

;; If Helm is present, enable helm-perldoc as well
(use-package helm-perldoc
  :after cperl-mode
  :ensure t
  :bind (:map cperl-mode-map
              ("C-c C-h p" . helm-perldoc))
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
  :after ruby-mode
  :ensure t
  :config
  (add-hook 'projectile-mode-hook 'projectile-rails-on))

;; extra tools for Ruby
(use-package ruby-tools
  :diminish ruby-tools-mode
  :ensure t)

;; robe (require pry, pry-doc, and method_source gems)
(use-package robe
  :diminish robe-mode
  :ensure t
  :after ruby-mode
  :config
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'robe-mode-hook 'ac-robe-setup))

;; VC-Git
(use-package vc-git
  :if (version<= emacs-version "24.3.1")
  :config
  (add-to-list 'vc-handled-backends 'Git))

;; Magit
(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)
         ("M-g b" . magit-blame)
         :map magit-mode-map
         ("v" . endless/visit-pull-request-url)
         :map magit-status-mode-map
         ("q" . zakame/magit-quit-session))
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
                         (magit-get-upstream-remote)
                         "url"))
             (magit-get-current-branch))))
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
    (jump-to-register :magit-fullscreen)))

;; magit-gitflow
(use-package magit-gitflow
  :defer t
  :diminish magit-gitflow-mode
  :ensure t
  :after magit
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

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
  (diff-hl-margin-mode)
  (unless (version<= emacs-version "24.4")
    (diff-hl-flydiff-mode))
  (eval-after-load "magit"
    '(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

;; php-mode
(use-package php-mode
  :ensure t
  :mode "\\.php\\'")

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
  :commands (ac-emmet-html-setup ac-emmet-css-setup)
  :init
  (add-hook 'sgml-mode-hook 'ac-emmet-html-setup)
  (add-hook 'css-mode-hook 'ac-emmet-css-setup))

;; web-mode
(use-package web-mode
  :ensure t
  :mode (("\\.tt\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.html\\.ep\\'" . web-mode)
         ("\\.blade\\.php\\'" . web-mode)
         ("\\.hbs\\'" . web-mode))
  :init
  (dolist (hook '(emmet-mode ac-emmet-html-setup ac-emmet-css-setup))
    (add-hook 'web-mode-hook hook))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-pairing nil
        web-mode-enable-auto-closing t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t
        web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property ac-source-emmet-css-snippets))
          ("php" . (ac-source-yasnippet))
          ("html" . (ac-source-emmet-html-aliases
                     ac-source-emmet-html-snippets))))
  (add-hook 'web-mode-before-auto-complete-hooks
            '(lambda ()
               (let ((web-mode-cur-language
                      (web-mode-language-at-pos)))
                 (if (string= web-mode-cur-language "php")
                     (yas-activate-extra-mode 'php-mode)
                   (yas-deactivate-extra-mode 'php-mode))
                 (if (string= web-mode-cur-language "css")
                     (setq emmet-use-css-transform t)
                   (setq emmet-use-css-transform nil)))))
  (defun zakame/sp-web-mode-code-context-p (id action context)
    "Set smartparens context when in web-mode."
    (and (eq action 'insert)
         (not (or (get-text-property (point) 'part-side)
                  (get-text-property (point) 'block-side)))))
  (sp-local-pair 'web-mode "<" nil :when '(zakame/sp-web-mode-code-context-p))
  (setq web-mode-engines-alist
        '(("mojolicious" . "\\.html\\.ep\\'")
          ("blade" . "\\.blade\\.")
          ("ctemplate" . "\\.hbs\\'"))))

;; kolon-mode
(use-package kolon-mode
  :ensure t
  :mode "\\.tx\\'")

;; JavaScript (js2-mode)
(use-package js2-mode
  :ensure t
  :interpreter (("node" . js2-mode))
  :mode "\\.\\(js\\|json\\)$"
  :bind (:map js2-mode-map
              ("C-c C-p" . js2-print-json-path))
  :config
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (setq js2-highlight-level 3
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil))

;; JavaScript beautifier
(use-package web-beautify
  :after js2-mode
  :ensure t
  :bind (:map js2-mode-map
              ("C-c C-b" . web-beautify-js)))

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

;; JSON snatcher
(use-package json-snatcher
  :ensure t
  :after js2-mode
  :bind (:map js2-mode-map
              ("C-c C-g" . jsons-print-path)))

;; Tern
(use-package tern
  :defer t
  :ensure t
  :diminish tern-mode
  :init
  (add-hook 'js2-mode-hook 'tern-mode))

;; auto-completion for Tern
(use-package tern-auto-complete
  :ensure t
  :after tern
  :config
  (tern-ac-setup))

;; Jade templates
(use-package jade-mode
  :ensure t
  :mode "\\.jade\\'")

;; Python mode
(use-package python-mode
  :ensure t
  :defer 2
  :mode "\\.py\\'"
  :interpreter (("python" . python-mode))
  :config
  (setq py-outline-minor-mode-p nil
        py-which-def-or-class-function nil))

;; Jedi autocompletion for Python
(use-package jedi
  :ensure t
  :after python-mode
  :commands jedi:setup
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  :bind (:map python-mode-map
              ("C-c d" . jedi:show-doc)
              ("M-/" . jedi:complete)
              ("M-." . jedi:goto-definition))
  :config
  (setq jedi:complete-on-dot t))

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
  (setq gofmt-command "goimports")
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
  :ensure t
  :after go-mode
  :commands go-projectile-mode
  :config
  (add-hook 'go-mode-hook #'go-projectile-mode))

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
  :init
  (setq org-babel-clojure-backend 'cider)
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (setq nrepl-log-messages t))

;; rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :init
  (dolist (hook '(prog-mode-hook java-mode-hook cperl-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode))
  :config
  (setq rainbow-delimiters-max-face-count 1)
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil
                      :foreground "dim gray")
  (dolist (face '(rainbow-delimiters-unmatched-face
                  rainbow-delimiters-mismatched-face))
    (set-face-attribute face nil
                        :foreground 'unspecified
                        :inherit 'error)))

;; android-mode
(use-package android-mode
  :ensure t
  :config
  (setq android-mode-builder 'gradle)
  (add-to-list 'compilation-error-regexp-alist 'gradle)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(gradle ":compile.*?\\(/.*?\\):\\([0-9]+\\): " 1 2)))

;; groovy-mode (mainly for Gradle files)
(use-package groovy-mode
  :ensure t
  :mode "\\.gradle\\'")

;; easy import of android/java libraries
(use-package java-imports
  :ensure t
  :after cc-mode
  :bind (:map java-mode-map
              ("C-c i" . java-imports-add-import-dwim))
  :config
  (setq java-imports-find-block-function 'java-imports-find-place-sorted-block)
  (add-hook 'java-mode-hook 'java-imports-scan-file))

;; Scala
(use-package scala-mode2
  :ensure t
  :mode (("\\.scala\\'" . scala-mode)
         ("\\.sbt\\'" . scala-mode)
         ("\\.sc\\'" . scala-mode)))

;; Scala build tool (sbt)
(use-package sbt-mode
  :ensure t
  :defer t)

;; ggtags
(use-package ggtags
  :ensure t
  :after projectile
  :diminish ggtags-mode
  :if (executable-find "gtags")
  :init
  (add-hook 'cperl-mode-hook 'ggtags-mode)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1)))))

;; aggressive indent
(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :config
  (mapc
   (lambda (mode)
     (add-to-list 'aggressive-indent-excluded-modes mode))
   '(jade-mode cperl-mode web-mode html-mode))
  (global-aggressive-indent-mode 1))


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
  ;; don't let headline sublevels inherit project tags
  (setq org-tags-exclude-from-inheritance '("PROJECT"))
  ;; add some custom commands to the agenda
  (setq org-agenda-custom-commands
        '(("P" "Projects"
           ((tags "PROJECT")))
          ("H" "Home"
           ((agenda "HOME")
            (tags-todo "HOME")))
          ("O" "Office"
           ((agenda "OFFICE")
            (tags-todo "OFFICE")))
          ("W" "Weekly Plan"
           ((agenda)
            (todo "TODO")
            (tags "PROJECT")))))
  ;; configure org-protocol
  (use-package org-protocol)
  ;; configure org-capture
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-capture-templates
        '(("p" "Protocol" entry
           (file+headline (concat org-directory "/notes.org") "Inbox")
           "* %^{Title}\nSource: %u, %c\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n%?")
          ("T" "TODO" entry
           (file+headline (concat org-directory "/gtd.org") "Tasks")
           "* TODO %^{Brief Description} %^g\n%?\nAdded: %U")
          ("L" "Protocol Link" entry
           (file+headline (concat org-directory "/notes.org") "Inbox")
           "* %? [[%:link][%:description]] \nCaptured On: %U")))
  (setq org-refile-targets '((nil . (:level . 1))
                             (org-agenda-files . (:maxlevel . 9))))
  ;; make windmove work well with org-mode
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)
  ;; add some languages we use to babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((perl . t)
     (ruby . t)
     (clojure . t)
     (haskell . t)
     (sh . t)
     (sql . t)
     (sqlite . t)))
  (setq org-log-done 'time
        org-hide-emphasis-markers t
        org-ellipsis "▼"
        org-src-fontify-natively t
        org-src-preserve-indentation nil
        org-edit-src-content-indentation 0))

;; ob-http (REST client)
(use-package ob-http
  :after org
  :ensure t
  :config
  (add-to-list 'org-babel-load-languages '(http . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

;; ob-go (Golang in Babel)
(use-package ob-go
  :ensure t
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(go . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

;; org-dashboard
(use-package org-dashboard
  :defer t
  :ensure t)

;; htmlize
(use-package htmlize
  :defer t
  :ensure t)

;; org-present (for meetups)
(use-package org-present
  :defer t
  :ensure t
  :config
  (add-hook 'org-present-mode-hook
            (lambda ()
              (flyspell-mode -1)
              (org-present-big)
              (org-display-inline-images)
              (org-present-hide-cursor)
              (org-present-read-only)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (flyspell-mode 1)
              (org-present-small)
              (org-remove-inline-images)
              (org-present-show-cursor)
              (org-present-read-write))))


;;; Hacker News
(use-package hackernews
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


;;; EMMS
(use-package emms-setup
  :bind (("C-c m" . emms))
  :ensure emms
  :config
  (emms-standard)
  (use-package emms-streams)
  (setq emms-source-file-default-directory "~/Music/")
  (use-package emms-history)
  (emms-history-load))

;; use mpv as the default player
(use-package emms-player-mpv
  :ensure t
  :init
  (setq emms-player-list '(emms-player-mpv)))

;; use Helm as the default EMMS interface for streams
(use-package helm-emms
  :after helm
  :ensure t
  :bind (("C-c s" . helm-emms)))


;;; tmux xterm-keys support (for windmove, etc. to work)
(use-package tmux-xterm-keys
  :load-path "site-lisp/tmux-xterm-keys")


;;; emojis :+1:
(use-package emojify
  :ensure t
  :config
  (unless (file-exists-p emojify-image-dir)
    (emojify-download-emoji emojify-emoji-set))
  (add-hook 'after-init-hook #'global-emojify-mode))


;;; zone-nyan :3
(use-package zone-nyan
  :ensure t
  :preface
  (use-package zone)
  :config
  (setq zone-programs [zone-nyan])
  (zone-when-idle 120))


;;; ESUP
(use-package esup
  :ensure t
  :defer t)


;; Docker mode
(use-package docker
  :if (file-exists-p "/var/run/docker.sock")
  :ensure t
  :bind (("C-c d c" . docker-containers)
         ("C-c d i" . docker-images)))


;; Sprunge pastebin
(use-package sprunge
  :ensure t)


;; helm-aws
(use-package helm-aws
  :if (file-exists-p (expand-file-name ".aws/config" (getenv "HOME")))
  :ensure t
  :bind (("C-c w" . helm-aws)))


;;; Local customizations
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; init.el ends here
