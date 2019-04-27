;; dvorak-mode --- Dvorak Key Navigation
;;; Commentary:
;; * Movement:
;;   C-n next line
;;   C-t previous line
;;   C-h forward character
;;   C-b backward character
;;   M-h forward word
;;   M-b backward word
;; * Window Navigation
;;   C-c C-n jump to bottom window
;;   C-c C-t jump to top window
;;   C-c C-h jump to right window
;;   C-c C-b jump to left window
;;; Code:
(define-minor-mode dvorak-mode
  "Navigation in Dvorak"
  :lighter " dvorak"
  :keymap (let ((map (make-sparse-keymap)))
            ;; Movement
            (define-key map (kbd "C-h") 'forward-char)
            (define-key map (kbd "C-t") 'previous-line)
            (define-key map (kbd "M-h") 'forward-word)
            (define-key map (kbd "M-t") 'backward-paragraph)
            (define-key map (kbd "M-n") 'forward-paragraph)
            ;; Window Navigation
            (define-key map (kbd "C-c C-n") 'windmove-down)
            (define-key map (kbd "C-c C-t") 'windmove-up)
            (define-key map (kbd "C-c C-h") 'windmove-right)
            (define-key map (kbd "C-c C-b") 'windmove-left)
	    ;; Helm
	    (with-eval-after-load 'helm
	      (define-key helm-map (kbd "C-t") 'helm-previous-line))
            map))

(define-globalized-minor-mode global-dvorak-mode dvorak-mode
  (lambda () (dvorak-mode 1)))

(provide 'dvorak-mode)
;;; dvorak-mode.el ends here
