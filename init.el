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

(eval-when-compile
  (require 'use-package))

;; Custom packages
(let ((default-directory  "/Users/huy/.emacs.d/custom-scripts/"))
  (normal-top-level-add-to-load-path '("dvorak")))

;; Dvorak Mode
(require 'dvorak-mode)
(global-dvorak-mode 1)

;; Server
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
   (server-start))

;; Solving $PATH
(let ((path "/Users/huy/.cargo/bin:/Users/huy/.nvm/versions/node/v9.6.1/bin:/usr/local/opt/texinfo/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Library/TeX/texbin:/opt/X11/bin:/Applications/Postgres.app/Contents/Versions/latest/bin:/Users/huy/.local/bin:/Users/huy/go:/Users/huy/go/bin:/usr/local/opt/go/libexec/bin"))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))

;; Other configs
(setq confirm-kill-emacs 'yes-or-no-p)
(global-auto-revert-mode 1)
(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Smooth scroll
(pixel-scroll-mode 1)

;; Make mouse wheel / trackpad scrolling less jerky
(setq mouse-wheel-scroll-amount '(1
                                  ((shift) . 5)
                                  ((control))))
(dolist (multiple '("" "double-" "triple-"))
  (dolist (direction '("right" "left"))
    (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))

;; PACKAGES INSTALL

;; Highlight Indent (like Sublime)
(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\│)
  :hook ((prog-mode . highlight-indent-guides-mode)))

;; Smart Parens
(use-package smartparens
  :ensure t
  :init (smartparens-global-mode 1)
  :diminish smartparens-mode)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq lazy-highlight-cleanup nil)
(setq-default tab-width 2)
(setq-default default-tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines -1)

;; Linum enhancement
(setq linum-format " %2d ")

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

;; Some term enhancement
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(defadvice term (before force-bash)
  (interactive (list "/bin/zsh")))
(ad-activate 'term)
(add-hook 'term-mode-hook (lambda ()
                            (linum-mode -1)
                            (local-unset-key (kbd "C-r"))))

(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/bin/zsh"))

;; Expand Region (for vim-like textobject)
(use-package expand-region :ensure t)

;; Multiple Cursors
(use-package multiple-cursors :ensure t)

;; Splash Screen
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Happy Hacking")

;; Golden ratio
;;(use-package golden-ratio
;;  :ensure t
;;  :init
;;  (golden-ratio-mode 1))

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

;; Ace Jump
(use-package ace-jump-mode :ensure t)

;; Custom keybinding
;; Movement and editing
(global-set-key (kbd "C-o") (lambda ()
                              "insert new line"
                              (interactive)
                              (end-of-line)
                              (newline-and-indent)))
(global-set-key (kbd "C-c C-l") 'copy-line)
(global-set-key (kbd "M-<up>") 'beginning-of-buffer)
(global-set-key (kbd "M-<down>") 'end-of-buffer)
(global-set-key (kbd "M-<right>") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "M-<left>") 'indent-rigidly-left-to-tab-stop)
(global-set-key (kbd "C-x SPC") 'cua-rectangle-mark-mode)
(global-set-key (kbd "C-c l") 'join-line)
(global-set-key (kbd "C-c n") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "M-s") 'ace-jump-mode)
(global-set-key (kbd "M-l") 'ace-jump-line-mode)
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
(global-set-key (kbd "C-M-<down>") (lambda () (interactive) (shrink-window 10)))
(global-set-key (kbd "C-M-<up>") (lambda () (interactive) (enlarge-window 10)))
(global-set-key (kbd "C-M-<left>") (lambda () (interactive) (shrink-window-horizontally 10)))
(global-set-key (kbd "C-M-<right>") (lambda () (interactive) (enlarge-window-horizontally 10)))
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
(setq org-link-frame-setup '((file . find-file)))
(setq org-default-notes-file (concat org-directory "capture.org"))
(defvar org-capture-target-path "/Users/huy/notes/capture.org")

;; Kill the frame if one was created for the capture
(defun delete-frame-if-neccessary (&rest r)
    (delete-frame))

(advice-add 'org-capture-finalize :after 'delete-frame-if-neccessary)
(advice-add 'org-capture-kill :after 'delete-frame-if-neccessary)
(advice-add 'org-capture-refile :after 'delete-frame-if-neccessary)

(setq org-capture-templates `(
	                            ("p" "Protocol" entry (file+headline ,'org-capture-target-path "Inbox")
                               "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
	                            ("L" "Protocol Link" entry (file+headline ,'org-capture-target-path "Inbox")
                               "* %? [[%:link][%:description]] \nCaptured On: %U"
                               :empty-lines 1)))

(defun org-sitemap-custom-entry-format (entry style project)
  (let ((filename (org-publish-find-title entry project)))
    (if (= (length filename) 0)
        (format "*%s*" entry)
      (format "%s - [[file:%s][%s]]"
              (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
              entry
              filename))))

(setq org-publish-project-alist
      '(("org-notes"
         :base-directory "/Users/huy/notes/src/"
         :base-extension "org"
         :publishing-directory "/Users/huy/code/play/huytd.github.io/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :exclude "rss.org\\|index.org\\|.*/private/.*"
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "jft :: CoffeeAddict\n#+SUBTITLE: jft = ( + coffee 'huy' )"
         :sitemap-format-entry org-sitemap-custom-entry-format
         :html-head-extra "<link rel=\"stylesheet\" href=\"/_css/style.css\">"
         :html-link-home "/")
        ("org-notes-static"
         :base-directory "/Users/huy/notes/src/"
         :recursive t
         :base-extension "jpg\\|png\\|gif\\|mp4"
         :publishing-directory "/Users/huy/code/play/huytd.github.io/"
         :publishing-function org-publish-attachment)
        ("notes" :components ("org-notes" "org-notes-static"))))

(add-to-list 'org-structure-template-alist
             '("o" "#+TITLE: ?\n#+DATE: "))

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(add-hook 'org-mode-hook 'enhance-ui-for-orgmode)

(defun filter-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.
   PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(load-library "find-lisp")
(setq org-agenda-files (find-lisp-find-files "/Users/huy/notes/src/" "\.org$"))
(setq org-agenda-window-setup 'only-window)
(setq org-agenda-custom-commands
      '(("c" "Custom agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-overriding-header "High-priority unfinished tasks:")
                 (org-agenda-skip-function '(org-agenda-skip-if nil '(todo done)))))
          (agenda "")
          (alltodo ""
                   ((org-agenda-skip-function '(or (filter-org-skip-subtree-if-priority ?A)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))))
                   ))))
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
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode   -1))
(when (fboundp 'tooltip-mode)
  (tooltip-mode    -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode   -1))
(when (fboundp 'global-linum-mode)
  (global-linum-mode 1))
(setq-default line-spacing 0)
(add-to-list 'default-frame-alist '(font . "Tamzen-14:antialias=true:hinting=false"))
(add-to-list 'default-frame-alist '(height . 38))
(add-to-list 'default-frame-alist '(width . 128))
(setq left-fringe-width 20)

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
  (setq helm-ag-insert-at-point 'symbol)
  :config
  (define-key helm-ag-mode-map (kbd "<RET>") 'helm-ag-mode-jump-other-window)
  (define-key helm-ag-mode-map (kbd "C-o") 'helm-ag-mode-jump))

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
;;(use-package js2-mode
;;  :ensure t
;;  :init
;;  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;;  :hook ((js2-mode . js2-imenu-extras-mode)))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (typescript-mode . flycheck-mode)
         (js2-mode . tide-setup)
         (js2-mode . tide-hl-identifier-mode)
         (before-save . tide-formater-before-save)))

(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-mode))
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

;; Shrink Path
(use-package shrink-path
  :ensure t
  :demand t)

;; Modeline
(setq auto-revert-check-vc-info t)

(defun pretty-buffername ()
  (if buffer-file-truename
      (let* ((cur-dir (file-name-directory buffer-file-truename))
             (two-up-dir (-as-> cur-dir it (or (f-parent it) "") (or (f-parent it) "")))
             (shrunk (shrink-path-file-mixed two-up-dir cur-dir buffer-file-truename)))
        (concat (car shrunk)
                (mapconcat #'identity (butlast (cdr shrunk)) "/")
                (car (last shrunk))))
    (buffer-name)))

;; Well, this is stupid, but the icons I have on my modeline measured at approximate
;; 2 characters length each. So, in order for the simple-mode-line-render to render
;; properly, I need to add these length for each icon added.
(defun calculate-icons-width ()
  (let ((left-icon-length 2)
        (right-icon-length 2))
  (+ left-icon-length (pcase flycheck-last-status-change
         (`finished (if flycheck-current-errors
                        (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                       (+ (or .warning 0) (or .error 0)))))
                          right-icon-length)
                      right-icon-length))
         (`running  right-icon-length)
         (`no-checker  right-icon-length)
         (`not-checked 0)
         (`errored     right-icon-length)
         (`interrupted right-icon-length)
         (`suspicious  0)))))

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT aligned respectively."
  (let* ((available-width (- (window-total-width) (length left) (calculate-icons-width))))
    (format (format "%%s %%%ds" available-width) left right)))

(defun insert-icon (type name &optional valign)
  "Insert an icon based on the TYPE and NAME and VALIGN optional."
  (or valign (setq valign -0.1))
  (funcall type name :height (/ all-the-icons-scale-factor 1.5) :v-adjust valign))

(defun custom-modeline-flycheck-status ()
  "Custom status for flycheck with icons."
  (let* ((text (pcase flycheck-last-status-change
                 (`finished (if flycheck-current-errors
                    (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                   (+ (or .warning 0) (or .error 0)))))
                       (format "%s %s" (insert-icon 'all-the-icons-faicon "bug") count))
                       (format "%s" (insert-icon 'all-the-icons-faicon "check"))))
                 (`running  (format "%s Running" (insert-icon 'all-the-icons-faicon "spinner" -0.15)))
                 (`no-checker  (format "%s No Checker" (insert-icon 'all-the-icons-material "warning" -0.15)))
                 (`not-checked "")
                 (`errored     (format "%s Error" (insert-icon 'all-the-icons-material "warning" -0.15)))
                 (`interrupted (format "%s Interrupted" (insert-icon 'all-the-icons-faicon "stop" -0.15)))
                 (`suspicious  ""))))
    (propertize text
                'help-echo "Show Flycheck Errors"
                'mouse-face '(:box 1)
                'local-map (make-mode-line-mouse-map
                            'mouse-1 (lambda () (interactive) (flycheck-list-errors))))))

(setq-default mode-line-format
  '((:eval (simple-mode-line-render
    ;; left
    (format-mode-line (list
     '((:eval
        (cond
         (buffer-read-only
          (format " %s"
                  (propertize (insert-icon 'all-the-icons-faicon "coffee")
                              'face '(:foreground "red"))))
         ((buffer-modified-p)
          (format " %s"
                  (propertize (insert-icon 'all-the-icons-faicon "chain-broken")
                              'face '(:foreground "orange")))))))
     " "
     '(:eval (propertize (insert-icon 'all-the-icons-icon-for-mode major-mode)))
     '(:eval (format " %s " (pretty-buffername)))
     "| %I L%l"))
    ;; right
    (format-mode-line (list
      '(:eval
        (format "%s %s %s"
                mode-name
                (if vc-mode
                    (let* ((noback (replace-regexp-in-string (format "^ %s" (vc-backend buffer-file-name)) " " vc-mode))
                           (face (cond ((string-match "^ -" noback) 'mode-line-vc)
                                       ((string-match "^ [:@]" noback) 'mode-line-vc-edit)
                                       ((string-match "^ [!\\?]" noback) 'mode-line-vc-modified)))
                           (icon (propertize (insert-icon 'all-the-icons-octicon "git-branch" -0.03))))
                      (format "%s %s" icon (substring noback 2)))
                  "")
                (custom-modeline-flycheck-status)))))))))

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

;; Elm
(use-package elm-mode
  :ensure t
  :init
  (add-to-list 'company-backends 'company-elm))

;; Haskell
(use-package haskell-mode
  :ensure t)

(use-package intero
  :ensure t
  :hook ((haskell-mode . intero-mode)))

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

;; Theme
(add-to-list 'custom-theme-load-path "/Users/huy/.emacs.d/custom-themes/")

(defun set-dark-theme ()
  "Set the dark theme with some customization if needed."
  (interactive)
  (load-theme 'ayu t)
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :background "#2C2525" :foreground "#EAEDF3" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight thin :width normal))))
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
   '(vertical-border ((t (:background "#161616" :foreground "#211C1C"))))
   '(window-divider ((t (:foreground "#211C1C"))))
   '(linum ((t (:inherit default :background nil :foreground "#5A5353" :strike-through nil :underline nil :slant normal :weight normal))))
   '(mode-line ((t (:background "#3B3333" :foreground "#EAEDF3" :box (:line-width 1 :color "#3B3333" :style unspecified) :overline "#3B3333" :underline nil))))
   '(mode-line-inactive ((t (:background "#3B3333" :foreground "#71696A" :box nil))))
   '(window-divider-first-pixel ((t (:foreground "#211C1C"))))))

(set-dark-theme)

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
 '(haskell-compile-cabal-build-alt-command
   "cd %s && stack build --fast --ghc-options=\\\"-j +RTS -A32M -RTS\\\"")
 '(haskell-compile-cabal-build-command
   "cd %s && stack build --fast --ghc-options=\\\"-j +RTS -A32M -RTS -Wall\\\"")
 '(haskell-compile-command "stack build")
 '(haskell-process-args-ghci (quote ("ghci")))
 '(haskell-process-path-ghci "stack")
 '(haskell-process-type (quote stack-ghci))
 '(helm-M-x-fuzzy-match t)
 '(helm-ag-base-command "rg --no-heading --ignore-case -F -M300")
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
 '(package-selected-packages
   (quote
    (shrink-path highlight-indent-guides dap-mode ace-jump lsp-haskell indium multiple-cursors expand-region org-capture-pop-frame purescript-mode company-arduino all-the-icons-dired groovy-mode multi-term deft ace-jump-mode package-lint emacs-htmlize go-eldoc go-complete go-stacktracer go-mode helm-ag cargo org-autolist smartparens wrap-region lsp-javascript-typescript haskell-mode magit elm-mode lsp-symbol-outline outline-magic company-lsp web-mode tide quickrun org-bullets lsp-ui flycheck-rust flycheck-inline lsp-rust f lsp-mode rust-mode company js2-mode diff-hl editorconfig general which-key helm use-package)))
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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#2C2525" :foreground "#EAEDF3" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight thin :width normal))))
 '(bold ((t (:foreground "orange1" :weight extra-bold))))
 '(company-template-field ((t (:background "#f7cc62" :foreground "black"))))
 '(company-tooltip ((t (:background "#f7cc62" :foreground "black"))))
 '(company-tooltip-selection ((t (:background "#f58c31"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#71696A" :slant italic))))
 '(font-lock-comment-face ((t (:foreground "#71696A" :slant italic))))
 '(fringe ((t (:background nil))))
 '(helm-candidate-number ((t (:background "#f7cc62" :foreground "black"))))
 '(helm-ff-directory ((t (:foreground "OrangeRed1"))))
 '(helm-ff-executable ((t (:inherit font-lock-string-face))))
 '(helm-ff-pipe ((t (:background "black" :foreground "#f7cc62"))))
 '(helm-ff-prefix ((t (:background "#f7cc62" :foreground "black"))))
 '(helm-grep-finish ((t (:inherit font-lock-string-face))))
 '(helm-header-line-left-margin ((t (:background "#f7cc62" :foreground "black"))))
 '(helm-locate-finish ((t (:inherit font-lock-string-face))))
 '(helm-match ((t (:foreground "white" :inverse-video nil))))
 '(helm-moccur-buffer ((t (:inherit font-lock-string-face :underline t))))
 '(helm-prefarg ((t (:inherit font-lock-string-face))))
 '(helm-rg-active-arg-face ((t (:inherit font-lock-string-face))))
 '(helm-rg-file-match-face ((t (:inherit font-lock-string-face :underline t))))
 '(helm-rg-preview-line-highlight ((t (:background "#46b866" :foreground "black"))))
 '(helm-selection ((t (:foreground "#f7cc62" :inverse-video t))))
 '(helm-source-header ((t (:foreground "white" :weight bold :height 1.0))))
 '(helm-visible-mark ((t nil)))
 '(js2-function-param ((t (:foreground "#F18D73"))))
 '(linum ((t (:inherit default :background nil :foreground "#5A5353" :strike-through nil :underline nil :slant normal :weight normal))))
 '(mode-line ((t (:background "#3B3333" :foreground "#EAEDF3" :box (:line-width 1 :color "#3B3333" :style unspecified) :overline "#3B3333" :underline nil))))
 '(mode-line-inactive ((t (:background "#3B3333" :foreground "#71696A" :box nil))))
 '(neo-dir-link-face ((t (:foreground "gray85"))))
 '(vertical-border ((t (:background "#161616" :foreground "#211C1C"))))
 '(window-divider ((t (:foreground "#211C1C"))))
 '(window-divider-first-pixel ((t (:foreground "#211C1C")))))
(put 'narrow-to-region 'disabled nil)
