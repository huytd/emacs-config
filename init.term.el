;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Custom packages
(add-to-list 'load-path "/Users/huy/.emacs.d/custom-scripts/")
(require 'dvorak-mode)
(global-dvorak-mode 1)

;; Other configs
(setq confirm-kill-emacs 'yes-or-no-p)
(global-auto-revert-mode 1)
(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)

(let ((path (shell-command-to-string ". /Users/huy/.bash_profile; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq lazy-highlight-cleanup nil)
(setq-default tab-width 2)
(setq-default default-tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun enhance-ui-for-orgmode ()
  "Enhance UI for orgmode."
  (org-bullets-mode 1)
  (org-autolist-mode 1)
  (toggle-truncate-lines)
  (linum-mode -1)
  (dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5))
    (set-face-attribute face nil :height 1.0 :background nil))
  )

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; Deleting
(delete-selection-mode 1)

;; Expand Region (for vim-like textobject)
(use-package expand-region :ensure t)

;; Multiple Cursors
(use-package multiple-cursors :ensure t)

;; Splash Screen
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Happy Hacking")

;; Show and jump between matching parens
(setq show-paren-delay 0)
(show-paren-mode  1)
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; Custom copy line function
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

;; Custom keybinding
;; Movement and editing
(global-set-key (kbd "C-o") (lambda ()
                              "insert new line"
                              (interactive)
                              (end-of-line)
                              (newline-and-indent)))
(global-set-key (kbd "C-c C-l") 'copy-line)
(global-set-key (kbd "C-c >") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "C-c <") 'indent-rigidly-left-to-tab-stop)
(global-set-key (kbd "C-x SPC") 'cua-rectangle-mark-mode)
(global-set-key (kbd "C-c l") 'join-line)
(global-set-key (kbd "C-c n") (lambda () (interactive) (join-line -1)))
;; Searching
(global-set-key (kbd "C-c s") 'helm-projectile-ag)
(global-set-key (kbd "C-x .") 'helm-resume)
(global-unset-key (kbd "C-s"))
(global-set-key (kbd "C-s") 'helm-occur)
;; Functions
(global-set-key (kbd "C-c f f") 'json-pretty-print-buffer)
(global-set-key (kbd "C-v") 'er/expand-region)
(global-set-key (kbd "C-c m m") 'mc/mark-all-dwim)

(global-set-key (kbd "C-c j") 'lsp-find-definition)
(global-set-key (kbd "C-0") 'quickrun)
(global-set-key (kbd "C-c SPC") 'lsp-ui-imenu)
(global-set-key (kbd "C-.") 'repeat)
(global-unset-key (kbd "C-\\"))
(global-set-key (kbd "C-\\") 'helm-M-x)
(global-set-key (kbd "C-c p p") 'helm-projectile-switch-project)
(global-set-key (kbd "C-c p f") 'helm-projectile-find-file)
(global-set-key (kbd "C-c f e d") (lambda ()
                                    "open emacs config"
                                    (interactive)
                                    (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c f e R") (lambda ()
                                    "reload emacs config"
                                    (interactive)
                                    (load-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c a t") 'multi-term)
(global-set-key (kbd "C-c f t") 'neotree-project-dir)
(global-set-key (kbd "C-c C-c") 'lazy-highlight-cleanup)
(global-set-key (kbd "C-c TAB") 'previous-buffer)
(global-set-key (kbd "C-x p r") 'helm-show-kill-ring)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-d") (lambda ()
                              "delete word sexp"
                              (interactive)
                              (backward-sexp)
                              (kill-sexp)))
;; Window management
(global-set-key (kbd "C-c =") 'balance-windows)
(global-set-key (kbd "C-c /") 'split-window-right)
(global-set-key (kbd "C-c \\") 'split-window-below)
(global-set-key (kbd "C-x w n") 'make-frame)
(global-set-key (kbd "C-x w k") 'delete-frame)
(global-set-key (kbd "C-c k") 'delete-window)
(global-set-key (kbd "C-x w .") 'kill-buffer-and-window)
(global-set-key (kbd "C-x r <down>") (lambda () (interactive) (shrink-window 10)))
(global-set-key (kbd "C-x r <up>") (lambda () (interactive) (enlarge-window 10)))
(global-set-key (kbd "C-x r <left>") (lambda () (interactive) (shrink-window-horizontally 10)))
(global-set-key (kbd "C-x r <right>") (lambda () (interactive) (enlarge-window-horizontally 10)))
;; org journal
(global-set-key (kbd "C-c t n") 'org-journal-list--start)
(global-set-key (kbd "C-c t d") (lambda ()
                                  "open agenda"
                                  (interactive)
                                  (org-agenda nil "c")))

(use-package htmlize :ensure t)

(use-package org-autolist :ensure t)

(use-package org-bullets :ensure t)

(use-package deft
  :ensure t
  :config
  (setq deft-directory "/Users/huy/notes/"
        deft-recursive t
        deft-default-extension "org"
        deft-text-mode 'org-mode
        deft-use-filter-string-for-filename t)
  (global-set-key (kbd "C-c d") 'deft))

;; OrgMode Configs
;;(use-package org-capture-pop-frame :ensure t)
(setq org-startup-with-inline-images t)
(setq org-image-actual-width nil)

(add-hook 'org-mode-hook 'enhance-ui-for-orgmode)

(setq org-return-follows-link t)
(setq org-hide-emphasis-markers t)
(setq org-html-validation-link nil)
(setq org-todo-keywords
      '((sequence "TODO" "WORKING" "HOLD" "|" "DONE")))
(setq org-todo-keyword-faces
      '(("TODO"    . "#eb4d4b")
        ("WORKING" . "#f0932b")
        ("HOLD"    . "#eb4d4b")
        ("DONE"    . "#6ab04c")))

;; UI configurations
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(global-linum-mode -1)
(setq-default line-spacing 0.1)
(add-to-list 'default-frame-alist '(font . "Tamzen-14:antialias=true:hinting=false"))
(add-to-list 'default-frame-alist '(height . 38))
(add-to-list 'default-frame-alist '(width . 128))
(setq left-fringe-width 20)

;; scroll one line at a time (less "jumpy" than defaults)
;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;;(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Anzu for search matching
(use-package anzu
  :ensure t
  :config
  (global-anzu-mode 1)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-set-key [remap query-replace] 'anzu-query-replace))

(setq show-trailing-whitespace t)

;; Helm
(use-package helm
  :ensure t
  :init
  (setq helm-M-x-fuzzy-match t
        helm-mode-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-locate-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        helm-candidate-number-list 80
        helm-split-window-in-side-p t
        helm-move-to-line-cycle-in-source t
        helm-echo-input-in-header-line t
        helm-autoresize-max-height 0
        helm-autoresize-min-height 20)
  :config
  (helm-mode 1)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action))

;; RipGrep
(use-package helm-rg :ensure t)
;; AG
(use-package helm-ag
  :ensure t
  :init
  (setq helm-ag-insert-at-point 'symbol))

;; Projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-mode 1))

;; Helm Projectile
(use-package helm-projectile
  :ensure t
  :init
  (setq helm-projectile-fuzzy-match t)
  (setq projectile-switch-project-action 'projectile-find-file-dwim)
  :config
  (helm-projectile-on))

;; All The Icons
(use-package all-the-icons :ensure t)
(use-package all-the-icons-dired :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

;; NeoTree
(use-package neotree
  :ensure t
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-vc-integration nil)
  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))
  :config
  (define-key neotree-mode-map (kbd "C-c C-m") 'neotree-create-node))


;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

;; Editorconfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Git Diff
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode 1))

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)
(setq mac-allow-anti-aliasing t)
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
(global-set-key (kbd "M-`") 'other-frame)

;; Groovy
(use-package groovy-mode :ensure t)
(use-package grails-mode :ensure t)

;; Arduino
(use-package arduino-mode
  :ensure t
  :config
  (setq arduino-executable "/Applications/Arduino.app/Contents/MacOS/Arduino"))
(use-package company-arduino
  :after (arduino-mode company)
  :ensure t)

;; JavaScript
(use-package js2-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :hook ((js2-mode . js2-imenu-extras-mode)))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (js2-mode . tide-setup)
         (js2-mode . tide-hl-identifier-mode)
         (before-save . tide-formater-before-save)))

(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
  (setq web-mode-enable-current-element-highlight t))

;; Purescript
(use-package purescript-mode
  :ensure t
  :hook ((purescript-mode . turn-on-purescript-indentation)))

;; Journal List
(use-package org-journal-list
  :load-path "~/code/play/org-journal-list/"
  :config
  (setq org-journal-list-default-directory "/Users/huy/notes/src/")
  (setq org-journal-list-create-temp-buffer t))

;; Flycheck
(use-package flycheck :ensure t)

;; LSP
(use-package lsp-mode
  :ensure t
  :init
  (add-hook 'rust-mode-hook 'lsp))

(use-package lsp-ui
  :ensure t
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (setq lsp-ui-doc-enable nil
        lsp-ui-peek-enable nil
        eldoc-echo-area-use-multiline-p nil))

;; Company mode
(use-package company
  :ensure t
  :init
  (setq company-minimum-prefix-length 3)
  (setq company-auto-complete nil)
  (setq company-idle-delay 0.1)
  (setq company-require-match 'never)
  (setq company-frontends
        '(company-pseudo-tooltip-unless-just-one-frontend
          company-preview-frontend
          company-echo-metadata-frontend))
  (setq tab-always-indent 'complete)
  (defvar completion-at-point-functions-saved nil)
  :config
  (global-company-mode 1)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-mode-map [remap indent-for-tab-command] 'company-indent-for-tab-command)
  (defun company-indent-for-tab-command (&optional arg)
    (interactive "P")
    (let ((completion-at-point-functions-saved completion-at-point-functions)
          (completion-at-point-functions '(company-complete-common-wrapper)))
      (indent-for-tab-command arg)))

  (defun company-complete-common-wrapper ()
    (let ((completion-at-point-functions completion-at-point-functions-saved))
      (company-complete-common))))

;; Rust
(use-package rust-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

(use-package cargo
  :ensure t
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; Quickrun
(use-package quickrun
  :ensure t
  :init
  (global-set-key (kbd "s-<return>") 'quickrun)
  :config
  (quickrun-add-command "typescript"
    '((:command . "ts-node")
      (:exec . ("%c %s")))
    :mode "typescript"
    :override t)
  )

;; Magit
(use-package magit :ensure t)

;; A hack for fixing projectile with ag/rg
;; Source: https://github.com/syohex/emacs-helm-ag/issues/283
(defun helm-projectile-ag (&optional options)
  "Helm version of projectile-ag."
  (interactive (if current-prefix-arg (list (read-string "option: " "" 'helm-ag--extra-options-history))))
  (if (require 'helm-ag nil  'noerror)
      (if (projectile-project-p)
          (let ((helm-ag-command-option options)
                (current-prefix-arg nil))
            (helm-do-ag (projectile-project-root) (car (projectile-parse-dirconfig-file))))
        (error "You're not in a project"))
    (error "helm-ag not available")))

;; Smooth scroll
(pixel-scroll-mode 1)

;; Theme
(add-to-list 'custom-theme-load-path "/Users/huy/.emacs.d/custom-themes/")

(defun set-dark-theme ()
  "Set the dark theme with some customization if needed."
  (interactive)
  (load-theme 'ayu t)
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :background "#2b2836" :foreground "#dbe6fb" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight thin :width normal))))
   '(company-template-field ((t (:background "#f7cc62" :foreground "black"))))
   '(company-tooltip ((t (:background "#f7cc62" :foreground "black"))))
   '(company-tooltip-selection ((t (:background "#f58c31"))))
   '(helm-candidate-number ((t (:background "#f7cc62" :foreground "black"))))
   '(helm-ff-pipe ((t (:background "black" :foreground "#f7cc62"))))
   '(helm-ff-prefix ((t (:background "#f7cc62" :foreground "black"))))
   '(helm-header-line-left-margin ((t (:background "#f7cc62" :foreground "black"))))
   '(helm-match ((t (:foreground "white" :inverse-video nil))))
   '(helm-rg-preview-line-highlight ((t (:background "#46b866" :foreground "black"))))
   '(helm-selection ((t (:foreground "#f7cc62" :inverse-video t))))
   '(helm-source-header ((t (:foreground "white" :weight bold :height 1.0))))
   '(neo-dir-link-face ((t (:foreground "gray85"))))
   '(vertical-border ((t (:background "#161616" :foreground "#413e52"))))
   '(window-divider ((t (:foreground "#413e52"))))
   '(linum ((t (:inherit default :background nil :foreground "#413e52" :strike-through nil :underline nil :slant normal :weight normal))))
   '(mode-line ((t (:background "#413e52" :foreground "#8b86a4" :box (:line-width 1 :color "#413e52" :style unspecified) :overline "#413e52" :underline nil))))
   '(mode-line-inactive ((t (:background "#2a2835" :foreground "#4d4961" :box nil))))
   '(window-divider-first-pixel ((t (:foreground "#655f7f"))))))

(defun set-term-theme ()
  "Set the dark theme with some customization if needed."
  (interactive)
  (load-theme 'ayu t)
  (global-linum-mode -1)
  (setq-default mode-line-format nil)
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :background "#202020" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight thin :width normal))))))

(if (display-graphic-p)
    (progn
      (set-dark-theme))
  (set-term-theme))

;; Automatically generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-cons-mode-line-p nil)
 '(appt-disp-window-function (quote user-appt-display))
 '(company-idle-delay 0.1)
 '(company-require-match (quote never))
 '(custom-safe-themes
   (quote
    ("1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" "2dfbe4e74de7139da2bb031054a651e1587d821b10439ca64d469c31ac3cafa5" "54472f6db535c18d72ca876a97ec4a575b5b51d7a3c1b384293b28f1708f961a" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" "3898b4f9c3f6f2994f5010f766a7f7dac4ee2a5c5eb18c429ab8e71c5dad6947" "896e853cbacc010573cd82b6cf582a45c46abe2e45a2f17b74b4349ff7b29e34" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "ecba61c2239fbef776a72b65295b88e5534e458dfe3e6d7d9f9cb353448a569e" "3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "a566448baba25f48e1833d86807b77876a899fc0c3d33394094cf267c970749f" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "b4c13d25b1f9f66eb769e05889ee000f89d64b089f96851b6da643cee4fdab08" "9d9fda57c476672acd8c6efeb9dc801abea906634575ad2c7688d055878e69d6" default)))
 '(flycheck-disabled-checkers (quote (javascript-jshint)))
 '(font-lock-maximum-decoration t)
 '(global-company-mode t)
 '(haskell-process-type (quote stack-ghci))
 '(helm-M-x-fuzzy-match t)
 '(helm-ag-base-command "rg --no-heading --ignore-case -M300")
 '(helm-ag-use-temp-buffer t)
 '(helm-autoresize-max-height 0)
 '(helm-autoresize-min-height 20)
 '(helm-buffers-fuzzy-matching t)
 '(helm-completion-in-region-fuzzy-match t)
 '(helm-echo-input-in-header-line t)
 '(helm-grep-ag-command "")
 '(helm-locate-fuzzy-match t)
 '(helm-mode t)
 '(helm-mode-fuzzy-match t)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-split-window-inside-p t)
 '(magit-dispatch-arguments nil)
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n32")))
 '(neo-window-fixed-size nil)
 '(newsticker-date-format "(%A)")
 '(newsticker-heading-format "%l
%t %d %s")
 '(newsticker-treeview-date-format "%d.%m.%y %H:%M ")
 '(newsticker-url-list
   (quote
    (("Reddit" "https://www.reddit.com/r/technologies+worldnews+emacs+javascript+rust.rss" nil nil nil))))
 '(newsticker-url-list-defaults
   (quote
    (("LWN (Linux Weekly News)" "https://lwn.net/headlines/rss")
     ("slashdot" "http://rss.slashdot.org/Slashdot/slashdot" nil 3600)
     ("Wired News" "https://www.wired.com/feed/rss"))))
 '(org-agenda-files nil)
 '(org-agenda-window-setup (quote only-window) t)
 '(org-directory "~/notes/")
 '(org-journal-list-create-list-buffer nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(shr-width 75)
 '(vc-annotate-background "#282c34")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))
