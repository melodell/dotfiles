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
(global-set-key "\M-o" 'other-window)  ; easier window switching
(global-set-key "\C-x\." 'xref-find-definitions-other-window)  ; find definition and open in new window

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
(setq-default tab-width 2)

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

;; Keep auto-save files in separate directory
(setq backup-directory-alist
      `((".*" . "~/.saves/")))
(setq auto-save-file-name-transforms
      `((".*" "~/.saves/" t)))

;; Delete trailing whitespace on save
(defun remove-trailing-whitespace ()
  (when (derived-mode-p 'prog-mode)
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'remove-trailing-whitespace)

;; Package Management.  Configure the built-in emacs package manager to use
;; several publicly available repositories.
(require 'package)
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
(when (not (package-installed-p 'use-package))
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

  ;; use eslint with web-mode (for jsx and tsx files)
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

;; Always display flycheck error list (C-c ! l) at bottom of frame, taking up 1/4 of the height
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
              (display-buffer-reuse-window
               display-buffer-in-side-window)
              (side            . bottom)
              (reusable-frames . visible)
              (window-height   . 0.25)))

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

;; C++ Indentation
(setq-default c-basic-offset tab-width)

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
  :ensure t
  :mode "\\.jsx?\\'"
  :mode "\\.html?\\'"
  :mode "\\.phtml\\'"
  :mode "\\.tpl\\.php\\'"
  :mode "\\.[agj]sp\\'"
  :mode "\\.as[cp]x\\'"
  :mode "\\.erb\\'"
  :mode "\\.mustache\\'"
  :mode "\\.djhtml\\'"
  :mode "\\.tsx?\\'"
  :mode "\\.ts?\\'"
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

  ;; Use TIDE for TSX files
  ;; Use Prettier for JSX and TSX files
  (defun setup-tide-prettier ()
	(when (string-equal "tsx" (file-name-extension buffer-file-name))
	  (add-node-modules-path)
	  (tide-setup)
	  (tide-hl-identifier-mode)
	  (prettier-js-mode)
	  )
	(when (string-equal "jsx" (file-name-extension buffer-file-name))
	  (add-node-modules-path)
	  (prettier-js-mode)
	  )
	)
  (add-hook 'web-mode-hook 'setup-tide-prettier)
  )

;; Add node_modules to PATH
;; https://github.com/codesuki/add-node-modules-path
(use-package add-node-modules-path
  :ensure t
  :defer t
  ;; We need to define a custom command to get the path to local executables
  ;; because "npm bin" (used by this package) is deprecated with npm > 8
  ;; https://github.com/codesuki/add-node-modules-path/issues/23
  :custom
  (add-node-modules-path-command '("echo $(npm root)/.bin"))
  )

;; Prettier autoformatting for JS/TS
(use-package prettier-js
  :ensure t
  :defer t
  )

;; TIDE for TypeScript autocomplete/backend
;; https://github.com/ananthakumaran/tide
(use-package tide
  :ensure t
  :config
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)  ; Use eslint
  (setq tide-sync-request-timeout 10)  ; Increase request timeout from default 2
  )

;; Build AST with tree-sitter
;; https://emacs-tree-sitter.github.io/
;; Emacs < 29
(use-package tree-sitter
  :ensure t
  :config
  ;; Enable for all (supported) modes
  (global-tree-sitter-mode)
  )

;; Language bundle for tree-sitter (required)
;; https://github.com/emacs-tree-sitter/tree-sitter-langs
(use-package tree-sitter-langs
  :ensure t
  :defer t
  :after tree-sitter
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

;; Always wrap text and check spelling in text mode
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; Projectile for project navigation
;; https://github.com/bbatsov/projectile
(use-package projectile
  :ensure t
  :defer t
  :init
  (projectile-mode +1)

  :config
  ;; Cache projects for quicker repeated searches
  (setq projectile-enable-caching t)

  ;; Use helm-projectile for completion (See helm-projectile below)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)

  ;; s-p to use projectile search
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
			  ))

;; Integrate Projectile with Helm for improved navigation
;; https://github.com/bbatsov/helm-projectile
;; https://tuhdo.github.io/helm-projectile.html
(use-package helm-projectile
  :ensure t
  :defer t
  )

;; Use ag for finding references. It's faster than the built-in xref-find-references.
;; Pitfall: Does straight-up string comparision, ignoring actual usages
;; https://agel.readthedocs.io/en/latest/configuration.html
(use-package ag
  :ensure t
  :defer t
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers 't)
  :init
  (add-hook 'ag-mode-hook (lambda() (next-error-follow-minor-mode 1)))  ; Always enable following search results
  )
;; (define-key global-map [remap xref-find-references] 'ag-project)  ; Remap M-? to use ag
;; Always display ag search results at bottom of frame, taking up 1/4 of the height
(add-to-list 'display-buffer-alist
             `(,(rx bos "*ag search*" eos)
              (display-buffer-reuse-window
               display-buffer-in-side-window)
              (side            . bottom)
              (reusable-frames . visible)
              (window-height   . 0.25)))

;; Always display xref search results at bottom of frame, taking up 1/4 of the height
(add-to-list 'display-buffer-alist
             `(,(rx bos "*xref*" eos)
              (display-buffer-reuse-window
               display-buffer-in-side-window)
              (side            . bottom)
              (reusable-frames . visible)
              (window-height   . 0.25)))

;; Neotree for viewing project structure
(use-package neotree
  :ensure t
  :config
  (setq neo-smart-open t)        ;; Jump to current file in tree view
  :bind ("M-n" . neotree-show))  ;; M-n toggle file tree view, q to close

;; Org Mode
;;
;; Resources:
;; https://github.com/james-stoup/emacs-org-mode-tutorial
;; https://systemcrafters.net/emacs-from-scratch/organize-your-life-with-org-mode/
(use-package org
  :ensure t
  :config
  ;; Store links, view agenda, open capture
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (define-key global-map "\C-cc" 'org-capture)

  ;; Completed tasks record a timestamp
  (setq org-log-done 't)

  ;; Move tags column
  (setq org-tags-column 50)

    ;; Indent content with <TAB>
  (setq org-adapt-indentation t)

  ;; Hide markers (ex. bold_text instead of *bold_text*)
  (setq org-hide-emphasis-markers t)

  ;; .org files => org mode
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

  ;; Look for agenda files in org directory
  (setq org-agenda-files (list "~/org"))

  ;; Set refile targets
  (setq org-refile-targets
        '(("archive.org" :maxlevel . 3)
          ("todo.org" :maxlevel . 3))
        )

  ;; Save Org buffers after refiling
  ;; We need this because default behavior doesn't save buffers in the background
  ;; when you move something to them
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; TODO keywords and custom colors
  ;; M-x list-colors-display
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "IN PROGRESS(i)" "ON HOLD(h)" "|" "DONE(d)" "CANCELLED(c)")))
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "red" :weight bold))
          ("NEXT" . (:foreground "light slate blue" :weight bold))
          ("IN PROGRESS" . (:foreground "yellow" :weight bold))
          ("ON HOLD" . (:foreground "orange" :weight bold))
          ("DONE" . (:foreground "green" :weight bold))
          ("CANCELLED" . (:foreground "dim gray" :weight bold))
          ))

  ;; Custom font colors and sizes
  (let* (
         (base-font-color     (face-foreground 'default nil 'default))
         (headline-base      `(:inherit default :weight bold :foreground ,base-font-color))
         (headline           `(:inherit default :weight bold :foreground "SlateGray1"))
         )
    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline))))
     `(org-level-7 ((t (,@headline))))
     `(org-level-6 ((t (,@headline))))
     `(org-level-5 ((t (,@headline))))
     `(org-level-4 ((t (,@headline))))
     `(org-level-3 ((t (,@headline :height 1.2))))
     `(org-level-2 ((t (,@headline :height 1.3))))
     `(org-level-1 ((t (,@headline :height 1.4))))
     `(org-date ((t (:foreground "SteelBlue1"))))
     `(org-special-keyword ((t (:foreground "DodgerBlue1"))))
     `(org-priority ((t (:foreground "orchid"))))
     `(org-tag ((t (:foreground "DeepSkyBlue"))))
     ))

  ;; Capture templates (S23)
  (setq org-capture-templates
        '(
          ("t" "TODO"
           entry (file+headline "todo.org" "Inbox")
           "* TODO %?\n  %t\n" :empty-lines 1)
          ("q" "Quick TODO"
           entry (file+headline "todo.org" "Quick")
           "*** NEXT %?\n :quick: %u\n" :empty-lines 1)
          ("m" "Meeting Entries")
          ("mm" "Meeting Notes"
           entry (file+datetree "meetings.org")
           "* %? \n** " :empty-lines 1
           )
          ("ms" "Standup Notes"
           entry (file+datetree "meetings.org")
           "* Standup \n** Pre-meeting \n%? \n** Meeting \n" :empty-lines 1
           )
          ("n" "Quick Note"
           entry (file "notes.org")
           "* %?\n  %U\n  %a\n" :empty-lines 1)
          )
        )

  ;; Custom agenda views
  (setq org-agenda-custom-commands
        '(
          ("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 14)))
            (todo "NEXT" ((org-agenda-overriding-header "Next Tasks"))))
           )

          ("n" "Next Tasks"
           ((todo "NEXT" ((org-agenda-overriding-header "Next Tasks"))))
           )

          ("q" "Quick Tasks" tags-todo "+quick")
          )
        )

  ;; Wrap lines and use nicer indentation
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode))
  )
