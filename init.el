;;; init.el --- My Emacs customizations
;; Copyright (C) 2005-2013  Zak B. Elep

;;;_* Package description

;; Author	:	Zak B. Elep ( zakame@zakame.net )
;; Date Created	:	Thu Sep 29 12:29:42 UTC 2005
;; Purpose	:	Set my personal customizations for the One True Editor.
;; Keywords	:	environment, customization
;; License	:	GNU General Public License (GPL), version 2

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
;; conveniences.  It also starts the `gnuserv' or `server' Emacs editing
;; server if it is available.
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
      '((background-color . "Black")
        (foreground-color . "Grey")
        (cursor-color . "Green")
        (user-position t)))

(setq enable-local-eval t)              ; Tell Emacs to obey variables
                                        ; set by the files it reads
(display-time)                          ; Display the time on modeline
(setq visible-bell t)                   ; Blink the screen instead of
                                        ; beeping
(mwheel-install)                        ; Enable wheel mouse support
(set-language-environment "UTF-8")      ; Set my default language
                                        ; environment
(scroll-bar-mode -1)                    ; Remove the tool bar ...
(menu-bar-mode -1)                      ; ... menu bar ...
(tool-bar-mode -1)                      ; ... and the scroll bar in X
(icomplete-mode 1)                      ; Enable IComplete mode
(iswitchb-mode 1)                       ; Enable ISwitchB mode
(windmove-default-keybindings)          ; Enable windmove
(auto-image-file-mode 1)                ; Show images as images, not as
                                        ; semi-random bits
(setq inhibit-startup-message t)        ; No splash screen (well...)

;; Enable File-Name Shadows (currently only available in Emacs 22
(if (>= emacs-major-version 22)
    (file-name-shadow-mode 1))


;;;_ + Editing

;; I want backups in their own directory, and even backup while in VC
(setq backup-directory-alist
      `(("." . ,(expand-file-name (concat user-emacs-directory "backups"))))
      vc-make-backup-files t)

;; I want global font-locking
(setq font-lock-maximum-size (* 70 1024)) ; buffers should be at most
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
(require 'allout)
(if (>= emacs-major-version 22)
    (allout-init t)
  (outline-init t))

;; Use Gnus as a mail-user-agent
(setq mail-user-agent 'gnus-user-agent)

;; Use `gnuserv' if it is available, and start it up.  Set `$EDITOR' to
;; `gnuclient', and use Emacs at any instance where `$EDITOR' is needed.
(condition-case err
    (progn
      (require 'gnuserv-compat)
      (gnuserv-start))
  (error
   (message "Could not load gnuserv: %s" (cdr err))))

;; Else, use Emacs 22's own server, and set `$EDITOR' to `emacsclient'.
(condition-case err
    (progn
      (require 'server)
      (server-start))
  (error
   (message "Could not load server: %s" (cdr err))))

;; Rebind `dabbrev-expand' to `hippie-expand' for adaptive auto-completion.
(eval-after-load "dabbrev"
  '(defalias 'dabbrev-expand 'hippie-expand))

;; Use BoxQuote
(condition-case err
    (progn
      (require 'boxquote))
  (error
   (message "Could not load boxquote: %s" (cdr err))))

;; Use Markdown Mode
(add-to-list 'load-path "~/.emacs.d/markdown-mode")
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Save point position between editing sessions
(require 'saveplace)
(setq-default save-place t
              save-place-file (expand-file-name ".places" user-emacs-directory))

;; Auto refresh buffers and dired, and be quiet about it
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)


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
(require 'cl)
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
                    (setq perlbin (substring perlbin 0 -1)) ; remove trailing /
                    (setenv "PATH" (concat perlbin ":" (getenv "PATH")))
                    (add-to-list 'exec-path perlbin)))))))))

;; Use Emacs::PDE for editing Perl
(add-to-list 'load-path "~/.emacs.d/pde/lisp")
(load "pde-load")

;; Associate PSGI files to Perl modes
(add-to-list 'auto-mode-alist '("\\.psgi\\'" . perl-mode))

;; This loads generic modes which support e.g batch files
(require 'generic-x)

;; This turns on develock if it is available
(condition-case err
    (progn
      (cond ((featurep 'xemacs)
             (require 'develock)
             ;; `turn-on-develock' is equivalent to `turn-on-font-lock',
             ;;  except that it does not highlight the startup screen.
             (add-hook 'lisp-interaction-mode-hook 'turn-on-develock)
             (add-hook 'mail-setup-hook 'turn-on-font-lock))
            ((>= emacs-major-version 20)
             (require 'develock)
             (global-font-lock-mode t))))
  (error
   (message "Could not load develock: %s " (cdr err))))

;; I want a better Scheme mode
(autoload 'scheme-mode
  "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme
  "cmuscheme" "Switch to interactive Scheme buffer." t)
(setq scheme-program-name "mit-scheme")

;; Ruby mode
(condition-case err
    (progn
      (require 'ruby-mode)
      (setq auto-mode-alist (append (list (cons "\\.rb\\'" 'ruby-mode))
                                    auto-mode-alist)))
  (error
   (message "Could not load ruby-mode: %s" (cdr err))))

;; VC-Git
(condition-case err
    (progn
      (require 'vc-git)
      (when (featurep 'vc-git)
        (add-to-list 'vc-handled-backends 'git))
      (require 'git)
      (autoload 'git-blame-mode "git-blame"
        "Minor mode for incremental blame for Git." t))
  (error
   (message "Could not load git: %s" (cdr err))))

;; Magit
(add-to-list 'load-path "~/.emacs.d/magit")
(add-to-list 'load-path "~/.emacs.d/magit/contrib")
(require 'magit)

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
(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;; nXhtml
(load "~/.emacs.d/nxhtml/autostart.el")

;; HTML5 support for nxml-mode
(add-to-list 'load-path "~/.emacs.d/html5-el")
(eval-after-load "rng-loc"
  '(add-to-list 'rng-schema-locating-files "~/.emacs.d/html5-el/schemas.xml"))
(require 'whattf-dt)

;; Add some modes for Template Toolkit support
(require 'mumamo-fun)
(setq auto-mode-alist (append '(("\\.tt$" . tt-html-mumamo)) auto-mode-alist))

;; Less warnings for nxhtml
(setq warning-minimum-level :error)

;; Emacs Code Browser
(add-to-list 'load-path "~/.emacs.d/ecb")
(require 'ecb)


;;;_* Local emacs vars

;; Local variables:
;; mode: emacs-lisp
;; allout-layout: (* 0 : )
;; End:

;;; zakame.emacs ends here
