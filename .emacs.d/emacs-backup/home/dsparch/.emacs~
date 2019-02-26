(require 'package)
(setq package-enable-at-startup t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.milkbox.net/packages/") t)
(package-initialize)

;;; 커스텀 모듈 로드 ;;;
;(setq custom-file "~/.emacs.d/custom.el")
;(load custom-file)


;;; 자동 완성 ;;;

;; python 자동 완성 기능 추가
(if (package-installed-p 'auto-virtualenv)
	(require 'auto-virtualenv)
	(add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
	(add-hook 'projectile-after-switch-project-hook 'auto-virtualenv-set-virtualenv)
	(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  )

(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; C++ 
;auto-complete c/c++ 헤더 자동 완성 포함 내용
(defun my:ac-c-headers-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (setq achead:include-directories
	(append '("/usr/include"
		  "/usr/include/c++/7"
		  "/usr/include/c++/7.2.0"
		  "/usr/include/c++/7.2.0/backward"
			"/usr/global/include"
		  "/usr/lib/gcc/x86_64-linux-gnu"
		  "/usr/lib/gcc/x86_64-linux-gnu/c++"
		  "/usr/lib/gcc/x86_64-linux-gnu"
		  "/usr/lib/gcc/x86_64-linux-gnu/7.2.0/include/"
		  "/usr/lib/gcc/x86_64-linux-gnu/7.2.0/include-fixed/"
			"/usr/global/include")
            achead:include-directories))
  (setq c-basic-offset 4)
	)

(require 'auto-complete)
(with-eval-after-load 'auto-complete (ac-flyspell-workaround))

;; auto complet config
(if (package-installed-p 'auto-complete-config)
		(require 'auto-complete-config)
	(ac-config-default)
	(global-auto-complete-mode t)
  (setq ac-disable-faces nil) ; quotation 안에도 자동 완성되게 해줌. 헤더 자동완성할 때 필요.
	)

(add-hook 'c++-mode-hook 'my:ac-c-headers-init)
(add-hook 'c-mode-hook 'my:ac-c-headers-init)

;; sematic
(semantic-mode t)
(setq semanticdb-default-save-directory "~/.emacs.d/#semanticdb.cache#")
(semantic-add-system-include "/usr/include/" 'c++-mode)
(semantic-add-system-include "/usr/include/" 'c-mode)
(semantic-add-system-include "/usr/global/include" 'c++-mode)
(semantic-add-system-include "/usr/global/include" 'c-mode)

(global-ede-mode 1)                      ; Enable the Project management system
(global-semantic-idle-scheduler-mode t) ;The idle scheduler with automatically reparse buffers in idle time.
(global-semantic-idle-completions-mode t) ;Display a tooltip with a list of possible completions near the cursor.
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode 1)(global-semantic-idle-summary-mode t)
;; (global-set-key [(control tab)] 'semantic-ia-complete-symbol-menu)

(defun my:add-semantic-to-autocomplete()
	(add-to-list 'ac-sources 'ac-source-semantic)
)
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)

;; 처음 부터 . -> :: 기능을 사용하시려면 아래 "nil" 을 "t" 바꾸세요. 
;; (defvar semantic-complete-self-insert-p t)
;; (defun semantic-complete-self-insert-toggle-switch ()
;;   (interactive)
;;   (if semantic-complete-self-insert-p
;;       (progn (setq semantic-complete-self-insert-p nil)
;; 	     (message "semantic-complete-self-insert off") )
;;     (progn (setq semantic-complete-self-insert-p t)
;; 	   (message "semantic-complete-self-insert on") )  )   )


;; (defun semantic-complete-self-insert-for-dot-operator (arg)
;;   (interactive "p")
;;   (if semantic-complete-self-insert-p
;;       (call-interactively 'semantic-complete-self-insert)
;;     (self-insert-command arg) )  )

;; (defun semantic-complete-self-insert-for-arrow-operator (arg)
;;   (interactive "p")
;;   (if (and semantic-complete-self-insert-p
;; 	   (string= "-" (char-to-string (char-before (point)) )  )   )
;;       (call-interactively 'semantic-complete-self-insert)
;;     (self-insert-command arg) )  )

;; (defun semantic-complete-self-insert-for-scope-operator (arg)
;;   (interactive "p")
;;   (if (and semantic-complete-self-insert-p
;; 	   (string= ":" (char-to-string (char-before (point)) )  )   )
;;       (call-interactively 'semantic-complete-self-insert)
;;     (self-insert-command arg) )  )

;; (defun c++-mode-additional-semantic-keys ()
;;   "Key bindings to add to `c++-mode'."
;;   (define-key c++-mode-map [(control c)(control .)] 'semantic-complete-self-insert-toggle-switch)
;;   (define-key c++-mode-map "." 'semantic-complete-self-insert-for-dot-operator)
;;   (define-key c++-mode-map ">" 'semantic-complete-self-insert-for-arrow-operator)
;;   (define-key c++-mode-map ":" 'semantic-complete-self-insert-for-scope-operator)
;;   )
;; (add-hook 'c++-mode-hook 'c++-mode-additional-semantic-keys)

;; irony-mode, company-irony
(if (package-installed-p 'company-quickhelp)
  (company-quickhelp-mode)
	)

(eval-after-load 'company '(add-to-list 'company-backends 'company-irony))

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'c-mode-common-hook (lambda () (global-set-key (kbd "<C-tab>") 'company-complete)))
(add-hook 'c++-mode-hook (lambda () (global-set-key (kbd "<C-tab>") 'company-complete)))

;;; 편의성 설정 ;;;

(global-visual-line-mode 1)

;; move cursor by camelCase
(subword-mode 1)

;;copy & paste, undo redo 일반키(C-c, C-v, C-z, C-shift-z)사용
(cua-mode t)

;; remember cursor position
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))

;; 대괄호 매칭 
(show-paren-mode t)
(setq show-paren-delay 0)

;; 디스플레이 하단에 시간 표시
(display-time)

;; linum 모드 켜기
(global-linum-mode t)
(setq linum-format "%4d \u2502 ")

;; 탭 넓이
(setq-default tab-width 4)

;; not use tab instead space
(setq-default indent-tabs-mode nil)

;; c코딩 스타일 지정(글로벌)
(setq c-default-style "bsd" c-basic-offset 4)
(setq c-defun-tactic "go-outward")

;; iedit :전부 바꾸기
(define-key global-map (kbd "C-h C-k") `iedit-mode)

;;; Shortcut ;;;
(global-set-key [f5] 'compile)
(global-set-key [f1] 'manual-entry)

(global-set-key (kbd "C-a") 'mark-whole-buffer) 
(global-set-key (kbd "C-w") 'goto-line)
(global-set-key (kbd "C-s") 'save-buffer)

;; arrow key
(global-set-key (kbd "M-i") 'previous-line) ; 【Alt+3】
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-j") 'left-char)
(global-set-key (kbd "M-l") 'right-char) ;

;; buffer arrow key
(global-set-key (kbd "C-x C-j") 'previous-buffer) ; 【Alt+3】
(global-set-key (kbd "C-x C-l") 'next-buffer)

;; auto complete dropdown arrow key
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-i") 'company-select-previous)
  (define-key company-active-map (kbd "M-k") 'company-select-next))

;; window control
(global-set-key (kbd "M-*") 'delete-other-windows) ; 【Alt+3】
(global-set-key (kbd "M-(") 'split-window-vertically)
(global-set-key (kbd "M-)") 'split-window-horizontally)

(global-set-key (kbd "<M-RET>") 'other-window) ; 【Alt+Return】 move cursor to next pane
(global-set-key (kbd "M-p") 'delete-window)  ; remove current pane
(global-set-key (kbd "M-_") 'shrink-window-horizontally)  
(global-set-key (kbd "M-+") 'enlarge-window-horizontally)
(global-set-key (kbd "M--") 'shrink-window)  
(global-set-key (kbd "M-=") 'enlarge-window)  

;; after copy Ctrl+c in Linux X11, you can paste by `yank' in emacs
(setq gui-select-enable-clipboard t)
(setq x-select-enable-clipboard t)      

;; after mouse selection in X11, you can paste by `yank' in emacs
(setq gui-select-enable-primary t)
(setq x-select-enable-primary t)
(setq save-interprogram-paste-before-kill t)

;; search keyword
(global-set-key (kbd "C-f") 'isearch-forward) 
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-d") 'isearch-repeat-backward)

;;; custom face set
;(load-theme 'cyberpunk)

;;; backup files
(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* (
        (backupRootDir "~/.emacs.d/emacs-backup/")
        (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path, for example, “C:”
        (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") ))
        )
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath
  )
)

(setq make-backup-file-name-function 'my-backup-file-name)

;;; custom-set-variables ;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("59e82a683db7129c0142b4b5a35dbbeaf8e01a4b81588f8c163bd255b76f4d21" default)))
 '(font-use-system-font t)
 '(global-display-line-numbers-mode t)
 '(package-selected-packages
   (quote
    (company-anaconda company-quickhelp auto-virtualenv anaconda-mode virtualenvwrapper virtualenv wconf jedi-direx jedi company-jedi iedit irony-eldoc auto-complete-clang-async ac-clang auto-complete-clang yasnippet company-irony-c-headers company-irony auto-complete-chunk auto-complete-c-headers ac-c-headers)))
 '(size-indication-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

