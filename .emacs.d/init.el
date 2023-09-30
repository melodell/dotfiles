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
(global-set-key (kbd "s-r") 'revert-buffer)  ; revert buffer to pull new changes

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

;; Require final newline
(setq require-final-newline t)

;; Remove scrollbars, menu bars, and toolbars
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Dialog settings.  No more typing the whole yes or no. Just y or n
;; will do. Disable GUI dialogs and use emacs text interface.
(fset 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)

;; Default window size and placement
(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 70))
(add-to-list 'default-frame-alist '(top . 0))

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
;; Make a minor mode to enable/disable so you have control over
;; accidentally reformatting the entire file :')
(defun remove-trailing-whitespace ()
  (when (derived-mode-p 'prog-mode)
    (delete-trailing-whitespace)))
(define-minor-mode remove-ws-on-save-mode
  "Remove trailing whitespace on save."
  :lighter "RWS"
  (add-hook 'before-save-hook 'remove-trailing-whitespace)

    (if remove-ws-on-save-mode
      (message "Removing whitespace on save enabled")
    (message "Removing whitespace on save disabled"))
  )

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
;; (use-package spacemacs-theme
;;   :ensure t
;;   :defer t
;;   :init (load-theme 'spacemacs-dark t)
;;   )

;; Doom Themes
;; https://github.com/doomemacs/themes/tree/master
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-vibrant t)

  ;; Corrects (and improves) org-mode's native fontification.
  ;; (doom-themes-org-config)
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
;;   (advice-add 'python-mode :before 'elpy-enable)
;;   )

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
  :mode "\\.css?\\'"
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

  ;; Autocomplete for CSS
  (add-hook 'web-mode-hook (lambda () (add-to-list 'company-backends 'company-css)))

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

;; rjsx for JSX files
(use-package rjsx-mode
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
;;
;; Language bundle for tree-sitter (required)
;; https://github.com/emacs-tree-sitter/tree-sitter-langs
;; (use-package tree-sitter-langs
;;   :ensure t
;;   :defer t
;;   :after tree-sitter
;;   )

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

;; Dockerfile editing
(use-package dockerfile-mode
  :mode "Dockerfile\\'"
  :ensure t
  )

;; YAML editing
(use-package yaml-mode
  :mode "\\.yml\\'"
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

;; Highlight symbols at point
;; https://codeberg.org/ideasman42/emacs-idle-highlight-mode
(use-package idle-highlight-mode
  :ensure t
  :defer t
  :config
  (setq idle-highlight-idle-time 0.4)
  :hook
  (python-mode . idle-highlight-mode)
  )

;; Better org agenda views
;; https://github.com/alphapapa/org-super-agenda/
(use-package org-super-agenda
  :ensure t
  :defer t
  )

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
  (setq org-tags-column 70)

  ;; Indent content with <TAB>
  (setq org-adapt-indentation t)

  ;; Hide markers (ex. bold_text instead of *bold_text*)
  (setq org-hide-emphasis-markers t)

  ;; .org files => org mode
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

  ;; Set org directory
  (setq org-directory "~/org/todo")

  ;; Look for agenda files in TODO directory
  (setq org-agenda-files (list "~/org/todo"))

  ;; Set refile targets
  (setq org-refile-targets
        '(("archive.org" :maxlevel . 3)
          ("school.org" :maxlevel . 2)
          )
        )

  ;; Save Org buffers after refiling
  ;; We need this because default behavior doesn't save buffers in the background
  ;; when you move something to them
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; Hide double entires for prewarning if entry is scheduled
  (setq org-agenda-skip-deadline-prewarning-if-scheduled t)

  ;; Hide DONE deadline entries
  ;; org-agenda-skip-scheduled-if-done also exists
  (setq org-agenda-skip-deadline-if-done t)

  ;; Always show group headers
  (setq org-super-agenda-hide-empty-groups nil)

  ;; TODO keywords and custom colors
  ;; M-x list-colors-display
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN PROGRESS(i)" "ON HOLD(h)" "|" "DONE(d)" "CANCELLED(c)")))
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "red" :weight bold))
          ("IN PROGRESS" . (:foreground "yellow" :weight bold))
          ("ON HOLD" . (:foreground "orange" :weight bold))
          ("DONE" . (:foreground "green" :weight bold))
          ("CANCELLED" . (:foreground "dim gray" :weight bold))
          ))

  ;; Org header size
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
   )

  ;; Capture templates (F23)
  (setq org-capture-templates
        '(
          ("t" "TODO")
          ("tq" "Career"
           entry (file+headline "school.org" "Career")
           "* TODO %? :career:\nCREATED: %u\n" :empty-lines-after 1)
          ("tw" "EECS 485"
           entry (file+headline "school.org" "EECS 485")
           "* TODO %? :eecs485:\nCREATED: %u\n" :empty-lines-after 1)
          ("te" "TMD Web Team"
           entry (file+headline "school.org" "TMD Web Team")
           "* TODO %? :tmd:\nCREATED: %u\n" :empty-lines-after 1)
          ("tr" "EECS 574"
           entry (file+headline "school.org" "EECS 574")
           "* TODO %? :eecs574:\nCREATED: %u\n" :empty-lines-after 1)
          ("tt" "EECS 593"
           entry (file+headline "school.org" "EECS 593")
           "* TODO %? :eecs593:\nCREATED: %u\n" :empty-lines-after 1)
          ("ta" "Research"
           entry (file+headline "school.org" "LIT Research")
           "* TODO %? :lit:\nCREATED: %u\n" :empty-lines-after 1)
          )
        )

  ;; Custom agenda views
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           
           ;; Show weekly agenda
           ((agenda "" ((org-agenda-span 'week)
                        (org-agenda-sorting-strategy '(todo-state-down priority-down deadline-up))))
            
            ;; Show TODO items scheduled for today
            (todo "" ((org-agenda-overriding-header "Do Today")
                      (org-super-agenda-groups
                       '((:name "" :scheduled past)
                         (:name "" :scheduled today)
                         ;; Hide "other items" that don't match this grouping
                         ;; https://github.com/alphapapa/org-super-agenda/issues/145
                         (:discard (:anything))
                         )
                       )
                      ))
            
            ;; Show TODO items with deadlines today
            (todo "" ((org-agenda-overriding-header "Due Today")
                      (org-super-agenda-groups
                       '((:name "" :deadline past)
                         (:name "" :deadline today)
                         ;; Hide "other items" that don't match this grouping
                         ;; https://github.com/alphapapa/org-super-agenda/issues/145
                         (:discard (:anything))
                         )
                       )
                      ))
            
            ;; Show TODO items marked as done
            (todo "DONE" ((org-agenda-overriding-header "Completed")))
            
            ;; Show all TODO items grouped by tag
            (todo "" ((org-agenda-overriding-header "All Tasks")
                   (org-super-agenda-groups
                    '(
                      (:name "eecs574"
                             :tag "eecs574"
                             )
                      (:name "eecs593"
                             :tag "eecs593"
                             )
                      (:name "eecs485"
                             :tag "eecs485"
                             )
                      (:name "Research"
                             :tag "lit"
                             )
                      (:name "career"
                             :tag "career"
                             )
                      (:name "TMD"
                             :tag "tmd"
                             )
                      (:discard (:anything))
                      )
                    )
                   ))
            ))
          )
        )

  ;; Archive all tasks in this file marked DONE
  ;; (Stolen from awdeorio)
  (defun org-archive-done-tasks-file ()
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
     "/DONE" 'file)
    (org-save-all-org-buffers)
    )

  ;; Wrap lines and use nicer indentation
  ;; Enable super agenda mode for nicer org agenda views
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode)
         (org-mode . org-super-agenda-mode))
  )
