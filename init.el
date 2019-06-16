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
  (normal-top-level-add-to-load-path '("qwerty")))

;; Dvorak Mode
(require 'qwerty-mode)
(global-qwerty-mode 1)

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
(pixel-scroll-mode -1)

;; Make mouse wheel / trackpad scrolling less jerky
(setq mouse-wheel-scroll-amount '(1
                                  ((control))))
(dolist (multiple '("" "double-" "triple-"))
  (dolist (direction '("right" "left"))
    (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))

;; Appointment Setup
(require 'appt)
(setq appt-time-msg-list nil
      appt-display-interval '5
      appt-message-warning-time '10
      appt-display-mode-line t
      appt-display-format 'window)

; Use appointment data from org-mode
(defun sync-org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))
(sync-org-agenda-to-appt)
(add-hook 'after-save-hook
          '(lambda ()
             (if (string= (buffer-name) "project.org")
                 (sync-org-agenda-to-appt))))

(appt-activate 1)

(defvar terminal-notifier-path
  "/usr/local/bin/terminal-notifier")

(defun user-appt-send-notification (title msg)
  (shell-command (concat terminal-notifier-path
                         " -message " msg
                         " -title " title
                         " -appIcon /Applications/Calendar.app/Contents/Resources/App-empty.icns")))

(defun user-appt-display (min-to-app new-time msg)
  (user-appt-send-notification
   (format "'🔥 In %s minutes'" min-to-app)
   (format "'%s'" msg)))

(setq appt-disp-window-function (function user-appt-display))

(setq org-agenda-window-setup 'current-window)

(setq org-agenda-custom-commands
      '(("c" "Custom agenda view"
         ((agenda "")))))

(defun show-custom-agenda-view ()
  (interactive)
  (org-agenda nil "c"))

;; PACKAGES INSTALL

;; Dashboard
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(use-package page-break-lines :ensure t)
(use-package dashboard
    :ensure t
    :diminish dashboard-mode
    :config
    (setq dashboard-center-content t)
    (setq dashboard-show-shortcuts nil)
    (setq dashboard-startup-banner 2)
    (setq dashboard-items '((recents  . 5)
                            (projects . 5)
                            (bookmarks . 5)))
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-init-info t)
    (dashboard-setup-startup-hook))

;; Ranger
(use-package ranger
  :ensure t
  :config
  (setq ranger-hide-cursor nil)
  (setq ranger-show-literal t)
  (setq ranger-dont-show-binary t)
  :init
  (ranger-override-dired-mode 1))

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
(setq linum-format "  %3d ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default fill-column 70)
;; Strikethrough
(defface org-checkbox-done-text
  '((t (:foreground "#71696A" :strike-through t)))
  "Face for the text part of a checked org-mode checkbox.")
(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-checkbox-done-text prepend))
 'append)

(defun enhance-ui-for-orgmode ()
  "Enhance UI for orgmode."
  (org-bullets-mode 1)
  (org-autolist-mode 1)
  (toggle-truncate-lines)
  (linum-mode -1)
  ;; Beautify Org Checkbox Symbol
  (push '("[ ]" . "☐") prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)
  (push '("#+BEGIN_SRC" . "⌜" ) prettify-symbols-alist)
  (push '("#+END_SRC" . "⌞" ) prettify-symbols-alist)
  (push '("TODO" . "☐" ) prettify-symbols-alist)
  (push '("WORK" . "⚑" ) prettify-symbols-alist)
  (push '("DONE" . "☑" ) prettify-symbols-alist)
  (prettify-symbols-mode)
  (set-face-attribute 'org-level-1 nil :height 1.15 :background nil :weight 'bold)
  (set-face-attribute 'org-level-2 nil :height 1.1 :background nil :weight 'semi-bold)
  (dolist (face '(org-level-3 org-level-4 org-level-5))
    (set-face-attribute face nil :weight 'normal :height 1.0))
  (face-remap-add-relative 'default :family "iA Writer Duospace" :height 120))

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
  (interactive (list "/usr/local/bin/fish")))
(ad-activate 'term)
(add-hook 'term-mode-hook #'hide-mode-line-mode)
(add-hook 'term-mode-hook (lambda ()
                            (linum-mode -1)
                            (setq left-fringe-width 0)
                            (setq right-fringe-width 0)
                            (local-unset-key (kbd "C-r"))))

(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/usr/local/bin/fish"))

;; Expand Region (for vim-like textobject)
(use-package expand-region :ensure t)

;; Multiple Cursors
(use-package multiple-cursors :ensure t)

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
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)
(global-set-key (kbd "s-<right>") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "s-<left>") 'indent-rigidly-left-to-tab-stop)
(global-set-key (kbd "C-x SPC") 'cua-rectangle-mark-mode)
(global-set-key (kbd "C-c l") 'join-line)
(global-set-key (kbd "C-c n") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "s-s") 'ace-jump-mode)
(global-set-key (kbd "s-l") 'ace-jump-line-mode)
(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "s-_") 'text-scale-decrease)
;; Searching
(global-set-key (kbd "C-c s") 'helm-projectile-ag)
(global-set-key (kbd "C-c ;") 'helm-projectile-ag)
(global-set-key (kbd "C-x .") 'helm-resume)
(global-unset-key (kbd "C-s"))
(global-set-key (kbd "C-s") 'helm-occur)
(global-set-key (kbd "C-;") 'helm-occur)
;; Macro
(defun toggle-kbd-macro-recording-on ()
  "One-key keyboard macros: turn recording on."
  (interactive)
  (define-key global-map (this-command-keys)
    'toggle-kbd-macro-recording-off)
  (start-kbd-macro nil))

(defun toggle-kbd-macro-recording-off ()
  "One-key keyboard macros: turn recording off."
  (interactive)
  (define-key global-map (this-command-keys)
    'toggle-kbd-macro-recording-on)
  (end-kbd-macro))

(global-set-key (kbd "C-,") 'toggle-kbd-macro-recording-on)
(global-set-key (kbd "C-.") 'call-last-kbd-macro)
;; Functions
(global-set-key (kbd "C-c a g") 'show-custom-agenda-view)
(global-set-key (kbd "C-c f f") 'json-pretty-print-buffer)
(global-set-key (kbd "C-v") 'er/expand-region)
(global-set-key (kbd "C-c m m") 'mc/mark-all-dwim)
(global-set-key (kbd "C-c j") 'lsp-find-definition)
(global-set-key (kbd "C-0") 'quickrun)
(global-set-key (kbd "C-c SPC") 'lsp-ui-imenu)
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
(global-set-key (kbd "C-c f t") 'treemacs-select-window)
(global-set-key (kbd "C-c f .") 'treemacs-add-and-display-current-project)
(global-set-key (kbd "C-c C-c") 'lazy-highlight-cleanup)
(global-set-key (kbd "C-c TAB") 'previous-buffer)
(global-set-key (kbd "C-x p r") 'helm-show-kill-ring)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "s-d") (lambda ()
                              "delete word sexp"
                              (interactive)
                              (backward-sexp)
                              (kill-sexp)))
(global-unset-key (kbd "C-x d"))
(global-set-key (kbd "C-x d") 'ranger)
;; Window management
(global-set-key (kbd "C-c =") 'balance-windows)
(global-set-key (kbd "C-c /") 'split-window-right)
(global-set-key (kbd "C-c \\") 'split-window-below)
(global-set-key (kbd "C-x w n") 'make-frame)
(global-set-key (kbd "C-x w k") 'delete-frame)
(global-set-key (kbd "C-c k") 'delete-window)
(global-set-key (kbd "C-x w .") 'kill-buffer-and-window)
(global-set-key (kbd "C-s-<down>") (lambda () (interactive) (shrink-window 10)))
(global-set-key (kbd "C-s-<up>") (lambda () (interactive) (enlarge-window 10)))
(global-set-key (kbd "C-s-<left>") (lambda () (interactive) (shrink-window-horizontally 10)))
(global-set-key (kbd "C-s-<right>") (lambda () (interactive) (enlarge-window-horizontally 10)))
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
(setq org-startup-with-inline-images t)
(setq org-image-actual-width nil)
(setq org-link-frame-setup '((file . find-file)))

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

(add-hook 'org-mode-hook 'enhance-ui-for-orgmode)

(setq org-return-follows-link t)
(setq org-hide-emphasis-markers t)
(setq org-html-validation-link nil)
(setq org-todo-keywords
      '((sequence "TODO(t!)" "WORK(w!)" "|" "DONE(d!)")))
(setq org-todo-keyword-faces
      '(("TODO"    . "#eb4d4b")
        ("WORK"    . "#eb4d4b")
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
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))
(setq left-fringe-width 20)

(add-to-list 'default-frame-alist '(font . "Fira Code-12:antialias=true:hinting=true"))
(setq-default line-spacing 1)

(add-hook 'markdown-mode-hook
          (lambda () (face-remap-add-relative 'default :family "iA Writer Duospace" :height 120)))

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

;; Treemacs
(use-package treemacs
  :ensure t
  :config
  (treemacs-follow-mode t)
  (treemacs-git-mode 'simple)
  (treemacs-filewatch-mode nil)
  (treemacs-fringe-indicator-mode nil)
  (setq treemacs-width 35
        treemacs-max-git-entries 100
        treemacs-display-in-side-window t
        treemacs-deferred-git-apply-delay 0.5
        treemacs-indentation-string (propertize " " 'face 'font-lock-comment-face)
        treemacs-indentation 1
        treemacs-file-follow-delay 0.2
        treemacs-silent-filewatch t
        treemacs-silent-refresh t)
  (add-hook 'treemacs-mode-hook #'hide-mode-line-mode)
  (add-hook 'treemacs-mode-hook (lambda ()
                                  (linum-mode -1)
                                  (fringe-mode 0)))
  ;; Improve treemacs icons
  (with-eval-after-load 'treemacs
    (with-eval-after-load 'all-the-icons
      (let ((all-the-icons-default-adjust 0)
            (tab-width 1))
        ;; Root icon
        (setq treemacs-icon-root-png (concat (all-the-icons-octicon "repo" :height 0.8 :v-adjust -0.2)  " "))
        ;; File icons
        (setq treemacs-icon-open-png
              (concat
               (all-the-icons-octicon "chevron-down" :height 0.8 :v-adjust 0.1)
               "\t"
               (all-the-icons-octicon "file-directory" :v-adjust 0)
               "\t")
              treemacs-icon-closed-png
              (concat
               (all-the-icons-octicon "chevron-right" :height 0.8 :v-adjust 0.1 :face 'font-lock-doc-face)
               "\t"
               (all-the-icons-octicon "file-directory" :v-adjust 0 :face 'font-lock-doc-face)
               "\t"))
        ;; File type icons
        (setq treemacs-icons-hash (make-hash-table :size 200 :test #'equal)
              treemacs-icon-fallback (concat
                                      "\t\t"
                                      (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
                                      "\t")
              treemacs-icon-text treemacs-icon-fallback)
        
        (dolist (item all-the-icons-icon-alist)
          (let* ((extension (car item))
                 (func (cadr item))
                 (args (append (list (caddr item)) '(:v-adjust -0.05) (cdddr item)))
                 (icon (apply func args))
                 (key (s-replace-all '(("^" . "") ("\\" . "") ("$" . "") ("." . "")) extension))
                 (value (concat "\t\t" icon "\t")))
            (unless (ht-get treemacs-icons-hash (s-replace-regexp "\\?" "" key))
              (ht-set! treemacs-icons-hash (s-replace-regexp "\\?" "" key) value))
            (unless (ht-get treemacs-icons-hash (s-replace-regexp ".\\?" "" key))
              (ht-set! treemacs-icons-hash (s-replace-regexp ".\\?" "" key) value))))))))

(use-package treemacs-projectile
  :ensure t)

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
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;(add-to-list 'default-frame-alist '(undecorated . t))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)
(setq mac-allow-anti-aliasing t)
(setq mac-option-key-is-meta nil)
(setq mac-option-modifier 'meta)
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

;; Prettier
(use-package prettier-js
  :ensure t
  :config
  (add-hook 'typescript-mode-hook (lambda ()
                                    (if (and (not (string= (projectile-project-name) "frontend"))
                                             (not (string= (projectile-project-name) "roboshop")))
                                        (prettier-js-mode 1)))))

;; JS TS and Web
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (typescript-mode . flycheck-mode)
         (typescript-mode . lsp-ui-mode)
         (before-save . tide-formater-before-save))
  :config
  (define-key typescript-mode-map (kbd "C-c r") 'tide-refactor))

(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-mode))
  (setq web-mode-enable-current-element-highlight t)
  :config
  (define-key web-mode-map (kbd "%") 'web-mode-tag-match))

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
(use-package lsp-mode :ensure t)

(use-package lsp-ui
  :ensure t
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (setq lsp-ui-doc-enable nil
        lsp-ui-peek-enable nil
        eldoc-echo-area-use-multiline-p nil)
  :config
  (add-hook 'lsp-ui-imenu-mode-hook (lambda ()
                                      (linum-mode -1)
                                      (fringe-mode 0)))
  ;; Custom bar
  (defun lsp-ui-imenu--get-bar (bars index depth for-title is-last)
    (cond
     ((>= index lsp-ui-imenu--max-bars)
      ;; Exceeding maximum bars
      "   ")
     ((not (aref bars index))
      ;; No bar for this level
      "   ")
     ((and (= depth 1) (not for-title))
      ;; For the first level, the title is rendered differently, so leaf items are
      ;; decorated with the full height bar regardless if it's the last item or
      ;; not.
      " │ ")
     ((< (1+ index) depth)
      ;; Full height bar for levels other than the rightmost one.
      " │ ")
     (is-last
      ;; The rightmost bar for the last item.
      " └ " )
     (for-title
      ;; The rightmost bar for the title items other than the last one.
      " ├ ")
     (t
      ;; The rightmost bar for the leaf items other than the last one.
      " │ "))))

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
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  :hook ((rust-mode . lsp-mode)
         (rust-mode . flycheck-mode)))

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
(use-package hide-mode-line :ensure t)

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
    (+ left-icon-length right-icon-length (pcase flycheck-last-status-change
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

(setq display-time-string-forms
       '((propertize (concat 24-hours ":" minutes)
 		                 'face 'egoge-display-time)))
(display-time-mode 1)

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
                                             (format "%s %s %s %s"
                                                     mode-name
                                                     (if vc-mode
                                                         (let* ((noback (replace-regexp-in-string (format "^ %s" (vc-backend buffer-file-name)) " " vc-mode))
                                                                (face (cond ((string-match "^ -" noback) 'mode-line-vc)
                                                                            ((string-match "^ [:@]" noback) 'mode-line-vc-edit)
                                                                            ((string-match "^ [!\\?]" noback) 'mode-line-vc-modified)))
                                                                (icon (propertize (insert-icon 'all-the-icons-octicon "git-branch" -0.03))))
                                                           (format "%s %s" icon (substring noback 2)))
                                                       "")
                                                     (format "%s %s" (insert-icon 'all-the-icons-faicon "clock-o") display-time-string)
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
(use-package doom-themes :ensure t)
(add-to-list 'custom-theme-load-path "/Users/huy/.emacs.d/custom-themes/")

(defun set-dark-theme ()
  "Set the dark theme with some customization if needed."
  (interactive)
  (load-theme 'doom-tomorrow-day t))

(set-dark-theme)

;; Automatically generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-cons-mode-line-p nil)
 '(company-idle-delay 0.1)
 '(company-require-match (quote never))
 '(display-time-mode t)
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
 '(lsp-ui-imenu-colors (quote ("#a9dc76" "#fc9867")))
 '(magit-dispatch-arguments nil)
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n32")))
 '(multi-term-program "/usr/local/bin/fish")
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
 '(org-agenda-files
   (quote
    ("~/notes/scratch.org" "~/code/play/stockonwatch/project.org")))
 '(org-directory "~/notes/")
 '(org-journal-list-create-list-buffer nil)
 '(org-log-into-drawer t)
 '(org-startup-folded nil)
 '(package-selected-packages
   (quote
    (org-ql prettier-js treemacs-projectile treemacs hide-mode-line ranger shrink-path ace-jump lsp-haskell multiple-cursors expand-region purescript-mode company-arduino all-the-icons-dired groovy-mode multi-term deft ace-jump-mode package-lint emacs-htmlize helm-ag cargo org-autolist smartparens wrap-region lsp-javascript-typescript haskell-mode magit elm-mode lsp-symbol-outline outline-magic company-lsp web-mode tide quickrun org-bullets lsp-ui flycheck-rust flycheck-inline lsp-rust f lsp-mode rust-mode company diff-hl editorconfig general which-key helm use-package)))
 '(send-mail-function (quote smtpmail-send-it))
 '(shr-width 75)
 '(term-default-bg-color "#3B3333")
 '(timeclock-mode-line-display t)
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
 '(bold ((t (:foreground "orange1" :weight extra-bold))))
 '(fixed-pitch ((t (:family "CodingFontTobi"))))
 '(fixed-pitch-serif ((t (:family "CodingFontTobi"))))
 '(fringe ((t (:background nil))))
 '(helm-grep-finish ((t (:inherit font-lock-string-face))))
 '(helm-locate-finish ((t (:inherit font-lock-string-face))))
 '(helm-match ((t (:foreground "white" :inverse-video nil))))
 '(helm-prefarg ((t (:inherit font-lock-string-face))))
 '(helm-rg-active-arg-face ((t (:inherit font-lock-string-face))))
 '(helm-rg-file-match-face ((t (:inherit font-lock-string-face :underline t))))
 '(helm-selection ((t (:inherit bold :background "#000000" :inverse-video t))))
 '(helm-source-header ((t (:weight bold :height 1.0))))
 '(helm-visible-mark ((t nil)))
 '(js2-function-param ((t (:foreground "#F18D73"))))
 '(term ((t (:inherit default :background "#3B3333"))))
 '(term-bold ((t (:background "#3B3333" :weight bold))))
 '(term-color-black ((t (:background "#211C1C" :foreground "#211C1C"))))
 '(term-color-blue ((t (:background "#a4c1ef" :foreground "#a4c1ef"))))
 '(term-color-cyan ((t (:background "#78dce8" :foreground "#78dce8"))))
 '(term-color-green ((t (:background "#a9dc76" :foreground "#a9dc76"))))
 '(term-color-magenta ((t (:background "#ab9df2" :foreground "#ab9df2"))))
 '(term-color-red ((t (:background "#ff6188" :foreground "#ff6188"))))
 '(term-color-yellow ((t (:background "#ffd866" :foreground "#ffd866"))))
 '(term-underline ((t (:background "#3B3333" :underline t))))
 '(tide-hl-identifier-face ((t (:inherit highlight :inverse-video t))))
 '(treemacs-root-face ((t (:inherit font-lock-constant-face :underline t :weight bold :height 1.0)))))
(put 'narrow-to-region 'disabled nil)
