(require 'package)
(setq package-enable-at-startup t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.milkbox.net/packages/") t)
(package-initialize)

;;; 커스텀 모듈 로드 ;;;
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

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

;;auto-complete c/c++ 헤더 자동 완성 포함 내용
(defun my:ac-c-headers-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (setq achead:include-directories
	(append '("/usr/include"
		  "/usr/include/c++/7"
		  "/usr/include/c++/7.2.0"
		  "/usr/include/c++/7.2.0/backward"
			"/usr/local/include"
		  "/usr/lib/gcc/x86_64-linux-gnu"
		  "/usr/lib/gcc/x86_64-linux-gnu/c++"
		  "/usr/lib/gcc/x86_64-linux-gnu"
		  "/usr/lib/gcc/x86_64-linux-gnu/7.2.0/include/"
		  "/usr/lib/gcc/x86_64-linux-gnu/7.2.0/include-fixed/"
			"/usr/local/include")
             achead:include-directories))
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
(semantic-add-system-include "/usr/local/include" 'c++-mode)
(semantic-add-system-include "/usr/local/include" 'c-mode)

(global-ede-mode 1)                      ; Enable the Project management system
(global-semantic-idle-scheduler-mode t) ;The idle scheduler with automatically reparse buffers in idle time.
(global-semantic-idle-completions-mode t) ;Display a tooltip with a list of possible completions near the cursor.
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode 1)(global-semantic-idle-summary-mode t)
(global-set-key [(control tab)] 'semantic-ia-complete-symbol-menu)

(defun my:add-semantic-to-autocomplete()
	(add-to-list 'ac-sources 'ac-source-semantic)
)
:(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)

;; 처음 부터 . -> :: 기능을 사용하시려면 아래 "nil" 을 "t" 바꾸세요. 
(defvar semantic-complete-self-insert-p t)
(defun semantic-complete-self-insert-toggle-switch ()
  (interactive)
  (if semantic-complete-self-insert-p
      (progn (setq semantic-complete-self-insert-p nil)
	     (message "semantic-complete-self-insert off") )
    (progn (setq semantic-complete-self-insert-p t)
	   (message "semantic-complete-self-insert on") )  )   )


(defun semantic-complete-self-insert-for-dot-operator (arg)
  (interactive "p")
  (if semantic-complete-self-insert-p
      (call-interactively 'semantic-complete-self-insert)
    (self-insert-command arg) )  )

(defun semantic-complete-self-insert-for-arrow-operator (arg)
  (interactive "p")
  (if (and semantic-complete-self-insert-p
	   (string= "-" (char-to-string (char-before (point)) )  )   )
      (call-interactively 'semantic-complete-self-insert)
    (self-insert-command arg) )  )

(defun semantic-complete-self-insert-for-scope-operator (arg)
  (interactive "p")
  (if (and semantic-complete-self-insert-p
	   (string= ":" (char-to-string (char-before (point)) )  )   )
      (call-interactively 'semantic-complete-self-insert)
    (self-insert-command arg) )  )


(defun c++-mode-additional-semantic-keys ()
  "Key bindings to add to `c++-mode'."
  (define-key c++-mode-map [(control c)(control .)] 'semantic-complete-self-insert-toggle-switch)
  (define-key c++-mode-map "." 'semantic-complete-self-insert-for-dot-operator)
  (define-key c++-mode-map ">" 'semantic-complete-self-insert-for-arrow-operator)
  (define-key c++-mode-map ":" 'semantic-complete-self-insert-for-scope-operator)
  )
(add-hook 'c++-mode-hook 'c++-mode-additional-semantic-keys)

;; irony-mode, company-irony
(if (package-installed-p 'company-quickhelp)
  (company-quickhelp-mode)
	)

(eval-after-load 'company '(add-to-list 'company-backends 'company-irony))
(add-hook 'c-mode-common-hook (lambda () (local-set-key (kbd "<C-tab>") 'company-complete)))

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; (Conditional) C/C++ Keybinds
(add-hook 'c-mode-common-hook
          (lambda () (local-set-key (kbd "C-c j") 'find-tag)))


;;; 편의성 설정 ;;;
;;copy & paste, undo redo 일반키(C-c, C-v, C-z, C-shift-z)사용
(cua-mode t)

;; 대괄호 매칭 
(show-paren-mode t)
(setq show-paren-delay 0)

;; 디스플레이 하단에 시간 표시
(display-time)

;; linum 모드 켜기
(global-linum-mode t)

;; 탭 넓이
(setq-default tab-width 2)

;; c코딩 스타일 지정(글로벌)
(setq c-default-style "bsd")

;; iedit :전부 바꾸기
(define-key global-map (kbd "C-h C-k") `iedit-mode)

;;; 단축키 ;;;
(global-set-key [f5] 'compile)
(global-set-key [f1] 'manual-entry)

(global-set-key (kbd "C-f") 'isearch-forward) 
(global-set-key (kbd "C-s") 'save-buffer) 
(global-set-key (kbd "C-a") 'mark-whole-buffer) 
(global-set-key (kbd "C-l") 'goto-line)

; split window 키
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

;;; custom face set
(load-theme 'cyberpunk)


;;; custom-set-variables ;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
	 (quote
		(company-anaconda company-quickhelp auto-virtualenv anaconda-mode virtualenvwrapper virtualenv wconf jedi-direx jedi company-jedi iedit irony-eldoc auto-complete-clang-async ac-clang auto-complete-clang yasnippet company-irony-c-headers company-irony auto-complete-chunk auto-complete-c-headers ac-c-headers))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
