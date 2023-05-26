;;; init.el
;;;
;;; Commentary:
;;; Built on beginner init.el from the Emacs tutorial
;;; https://eecs280staff.github.io/tutorials/setup_emacs.html#configure
;;;
;;; Melina O'Dell <melodell@umich.edu>

;;; Code:

;; Relocate automatically modified files.  These are lines added to
;; the file when you use the customise system. They're generated when
;; you use customize-*. By default, the customisation options are
;; stored in the init.el (or .emacs) file. You don't usually edit
;; these by hand.
;; https://stackoverflow.com/questions/5052088/what-is-custom-set-variables-and-faces-in-my-emacs
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file) (write-region "" nil custom-file))

;; Modified keyboard shortcuts
(global-set-key "\C-x\C-b" 'electric-buffer-list)  ; easier buffer switching
(global-set-key "\M-o" 'other-window)

;; Don't show a startup message
(setq inhibit-startup-message t)

;; Show line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Show line numbers in file
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Show syntax highlighting
(global-font-lock-mode t)

;; Highlight marked regions
(setq-default transient-mark-mode t)

;; Parentheses
(electric-pair-mode 1)                  ; automatically close parentheses, etc.
(show-paren-mode t)                     ; show matching parentheses

;; Smooth scrolling (one line at a time)
(setq scroll-step 1)

;; Tab settings: 2 spaces.  See also: language-specific customizations below.
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; Remove scrollbars, menu bars, and toolbars
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Dialog settings.  No more typing the whole yes or no. Just y or n
;; will do. Disable GUI dialogs and use emacs text interface.
(fset 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)

;; Default window size
(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 60))

;; macOS modifier keys
(setq mac-command-modifier 'meta) ; Command == Meta
(setq mac-option-modifier 'super) ; Option == Super

;; Disable backup files
(setq make-backup-files nil)

;; Package Management.  Configure the built-in emacs package manager to use
;; several publicly available repositories.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)

;; Bootstrap 'use-package' and enable it.  Later, 'use-package- will
;; download and install third-party packages automatically.
;; http://cachestocaches.com/2015/8/getting-started-use-package/
;;
;; EXAMPLE:
;; (use-package foo-mode
;;   :after bar      ; load after bar package
;;   :mode "\\.foo"  ; load and enable foo-mode for *.foo files
;;   :init           ; run this code when init.el is read
;;   :config         ; run this code after loading foo-mode
;;   :ensure t       ; automatically install foo-mode if not present
;;   :defer t        ; defer loading for performance (usually the default)
;; )
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Automatically update packages installed by use-package periodically
(use-package auto-package-update
  :config
  (setq auto-package-update-prompt-before-update t)
  (setq auto-package-update-delete-old-versions t)
  :ensure t
  :defer t
  )

;; More intuitive undo/redo.  C-_ undo, C-M-_ redo
;; https://www.emacswiki.org/emacs/UndoTree
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :ensure t
  :defer t
  )

;; Spacemacs dark mode
(use-package spacemacs-theme
  :ensure t
  :defer t
  :init (load-theme 'spacemacs-dark t)
  )

;; Intellisense syntax checking
;; http://www.flycheck.org/en/latest/
(use-package flycheck
  :config
  ;; enable in all modes
  (global-flycheck-mode)
  ;; C++11
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))
  :ensure t
  :defer t
  )

;; Autocomplete for code
;; Company docs: https://company-mode.github.io/
;; Company TNG: https://github.com/company-mode/company-mode/issues/526
(use-package company
  :config
  (company-tng-mode)       ; use default configuration
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :ensure t
  :defer t                              ; lazy loading
  )

;; Python backend for autocomplete
;; https://github.com/syohex/emacs-company-jedi
;; You may need to:
;; $ pip install virtualenv
;; Only works on Emacs 24.4 +
(unless (version< emacs-version "24.4")
(use-package company-jedi
  :after company                        ; lazy loading
  :init
  (add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends 'company-jedi)))
  :ensure t
  )
)

;; Intellisense syntax checking
;; http://www.flycheck.org/en/latest/
(use-package flycheck
  :config

  ;; enable in all modes
  (global-flycheck-mode)

  ;; disable jshint since we prefer eslint checking
  ;; http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))

  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  ;; use local eslint from node_modules before global
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

  ;; C++11
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))

  :ensure t
  :defer 1  ; lazy loading
)

;; Remote file editing with TRAMP.  Configure TRAMP to use the same SSH
;; multiplexing as in ~/.ssh/config.  By default, TRAMP ignores
;; SSH config's multiplexing configuration, so configure the same settings here.
;; https://www.emacswiki.org/emacs/TrampMode
;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-default-user "melodell")
  (setq tramp-ssh-controlmaster-options
        (concat
         "-o ControlMaster auto "
         "-o ControlPath ~/.ssh/socket-%%C "
         ))
  (setq tramp-use-ssh-controlmaster-options nil)
  :defer 1  ; lazy loading
)

;; Python
;; (use-package elpy
;;   :ensure t
;;   :defer t
;;   :init
;;   (advice-add 'python-mode :before 'elpy-enable))

;; C and C++ programming.  Build with C-c m.  Rebuild with C-c c.  Put
;; this in c-mode-base-map because c-mode-map, c++-mode-map, and so
;; on, inherit from it.
(add-hook 'c-initialization-hook
          (lambda () (define-key c-mode-base-map (kbd "C-c m") 'compile)))
(add-hook 'c-initialization-hook
          (lambda () (define-key c-mode-base-map (kbd "C-c c") 'recompile)))

;; Indentation
(setq-default c-basic-offset 4
                  tab-width 4
                  indent-tabs-mode t)

(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))  ; assume C++ for .h files

;; LLDB support
(use-package realgud
  :ensure t
  )
(use-package realgud-lldb
  :ensure t
  )

;; Web Development
(use-package web-mode
  :mode "\\.jsx?\\'"
  :mode "\\.html?\\'"
  :mode "\\.phtml\\'"
  :mode "\\.tpl\\.php\\'"
  :mode "\\.[agj]sp\\'"
  :mode "\\.as[cp]x\\'"
  :mode "\\.erb\\'"
  :mode "\\.mustache\\'"
  :mode "\\.djhtml\\'"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-enable-auto-indentation nil)
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  :ensure t
  )

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  (add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1)))  ;; Always check spelling
  )

;; Preview Markdown
;; $ pip install grip
;; M-x grip-mode
;; C-c C-c g to start/kill preview
(use-package grip-mode
  :ensure t
  :bind (:map markdown-mode-command-map
			  ("g" . grip-mode))
  )

;; Dockerfile syntax highlighting
(use-package dockerfile-mode
  :mode "Dockerfile\\'"
  :ensure t
  )

;; Always check spelling in text mode
(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))

;; Org Mode
(use-package org
  :config
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done 't)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)")))

  (setq org-agenda-files (list "~/org"))
  )
