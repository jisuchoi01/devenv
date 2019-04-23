;; Basic Setting ;;

; Line Wrap mode on for all buffers
(global-visual-line-mode t)

; Line Number
(global-linum-mode t)

; Save last cursor position
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode t))

; Match braket and braces
(show-paren-mode t)
(setq show-paren-delay 0)

;Tab width
(setq-default tab-width 4)

; C coding style
(setq c-default-style "bsd")
(setq c-default-style "bsd" c-basic-offset 4)

; Time Display
(display-time)

;; Basic mode ;;
(cua-mode t)
(blink-cursor-mode t)
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.el\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(semantic-mode t)
;The idle scheduler with automatically reparse buffers in idle time.
(global-semantic-idle-scheduler-mode t)
(global-semantic-idle-completions-mode t) 
(global-semantic-idle-summary-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode t)


;; Basic Shortcut Keys ;;

(global-set-key [f12] 'describe-key)
(global-set-key [f10] 'describe-function)
(global-set-key [f1] 'shell)

(global-set-key (kbd "C-s") 'save-buffer)				 

; find word
(define-key isearch-mode-map [(control f)] 'isearch-repeat-forward)
(global-set-key (kbd "C-f") 'isearch-forward)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-d") 'isearch-repeat-backward)

; arrow key
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-j") 'left-char)
(global-set-key (kbd "M-l") 'right-char)

; buffer arrow key
(global-set-key (kbd "C-x C-j") 'previous-buffer)
(global-set-key (kbd "C-x C-l") 'next-buffer)

; window key
(global-set-key (kbd "M-9") 'split-window-vertically)
(global-set-key (kbd "M-0") 'split-window-horizontally)
(global-set-key (kbd "M-p") 'delete-window)
(global-set-key (kbd "M-_") 'shrink-window-horizontally)  
(global-set-key (kbd "M-+") 'enlarge-window-horizontally)
(global-set-key (kbd "M--") 'shrink-window)  
(global-set-key (kbd "M-=") 'enlarge-window)  

(global-set-key (kbd "C-c C-j")  'windmove-left)
(global-set-key (kbd "C-c C-l") 'windmove-right)
(global-set-key (kbd "C-c C-i")    'windmove-up)
(global-set-key (kbd "C-c C-k")  'windmove-down)

(global-set-key (kbd "C-c C-k")  'goto-line)
(global-set-key (kbd "TAB") 'self-insert-command)

;; Mode keymap setting

; unbind mark set key
(global-unset-key (kbd "C-SPC"))
(global-unset-key (kbd "C-@"))

; unbind python shell send to file key
(add-hook 'python-mode-hook
          (lambda()
            (local-unset-key (kbd "C-c C-j"))
			(local-unset-key (kbd "C-c C-l"))
			))


; input method : set default input method as hangul
(set-input-method "korean-hangul")

; load themek
(load-theme 'wombat)
