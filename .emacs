;; Basic Setting ;;

; Line Wrap mode on for all buffers
(global-visual-line-mode t)

; Line Number
(global-linum-mode t)
(setq linum-format "%4d \u2502    ")

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
(setq-default tab-stop-list 4)

; C coding style
(setq c-default-style "bsd")
(setq c-default-style "bsd" c-basic-offset 4)

; Time Display
(display-time)

;; Basic mode ;;
(cua-mode t)
(blink-cursor-mode t)
(electric-layout-mode t)
(electric-indent-local-mode t)
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

(global-set-key [f12] 'describe-key)
(global-set-key [f10] 'describe-function)
(global-set-key [f1] 'shell)
(global-set-key [f3] 'execute-extended-command)

;; Basic Shortcut Keys ;;

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

(global-set-key (kbd "C-l")  'goto-line)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-d") 'backward-delete-char-untabify)

(global-set-key (kbd "TAB") 'self-insert-command)

(global-set-key "\M-q" 'beginning-of-line-text)
(global-set-key "\M-e" 'end-of-line)
;; (global-set-key "\M-x" (lambda () (interactive) (scroll-up 4)))
;; (global-set-key "\M-z" (lambda () (interactive) (scroll-down 4)))
(defun sfp-page-down ()
      (interactive)
      (next-line
       (- (window-text-height)
          next-screen-context-lines)))
    
(defun sfp-page-up ()
      (interactive)
      (previous-line
       (- (window-text-height)
          next-screen-context-lines)))
    
(global-set-key [next] 'sfp-page-down)
(global-set-key [prior] 'sfp-page-up)

;; Mode keymap setting

; unbind mark set key
(global-unset-key (kbd "C-SPC"))
(global-unset-key (kbd "C-@"))

; unbind python shell send to file key
(add-hook 'python-mode-hook
          (lambda()
            (local-unset-key (kbd "C-c C-j"))
			(local-unset-key (kbd "C-c C-l"))
			(local-unset-key (kbd "C-c C-i"))
			(local-unset-key (kbd "C-c C-k"))
			))

; input method : set default input method as hangul
(set-input-method "korean-hangul")

; backup files
; put autosave files (ie #foo#) and backup files (ie foo~) in ./.emacs_backup_src/".
(setq backup-directory-alist '(("" . "./.emacs_backup_src/")))
(setq delete-by-moving-to-trash t
	  version-control t)

; load themek
(load-theme 'wombat)

;; PACKAGE
(require 'package)
(setq package-enable-at-startup t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.kmilkbox.net/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(current-language-environment "UTF-8")
 '(display-time-mode t)
 '(ecb-options-version "2.50")
 '(package-selected-packages
   (quote
	(ecb virtualenv auto-complete auto-complete-c-headers auto-complete-chunk jedi company-anaconda)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "나눔고딕코딩" :foundry "SAND" :slant normal :weight normal :height 141 :width normal)))))


;;; Package Dependency mode
;(add-to-list 'auto-mode-alist '("\\.py\\'" . anaconda-mode))

;; Auto complete
(if (package-installed-p 'auto-complete)
	(progn
	  (require 'auto-complete)
	)
  )

(with-eval-after-load 'auto-complete
  (ac-config-default)
  (global-auto-complete-mode t)
  (setq ac-use-menu-map t)				;
  (setq ac-quick-help-delay 0.5)
  (define-key ac-menu-map (kbd "M-i") 'ac-previous)
  (define-key ac-menu-map (kbd "M-k") 'ac-next)
  ; candiate menu popup
  (define-key ac-mode-map (kbd "C-SPC") 'auto-complete)
  (define-key ac-completing-map (kbd "C-SPC") 'ac-quick-help)
)

;;; Custom File.
;; Load Custome : Below is example
;(add-to-list 'load-path "~/.emacs.d/custom")

;; Jedi Mode
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'anaconda-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
