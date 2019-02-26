(defun save-package-list-file () (interactive)
			 (dolist (pack package-selected-packages)
				 (setq pack (format "%s" pack))
   			 (if (not(string= pack ""))
						 (setq packs (concat packs pack "\n"))		
					 )
				 )

			 (interactive)
       (setq filename (read-file-name "Enter file name:"))
       (write-region packs nil filename nil)
)


(defun mydebug() (interactive)
			 (setq package "cmake-mode")
			 (unless (package-installed-p package)
			 	 (if (y-or-n-p (format "Package %s is missing. Install it? " package))
						 (package-install 'cmake-mode)
					 )
				 )
)

(defun load-package-list-and-install() (interactive)
  "Prompt user to enter a file name, with completion and history support."
  (interactive)
  (setq filename (read-file-name "Enter file name:"))

  	; install package list
	(setq install-needed-packages
				(with-temp-buffer
					(insert-file-contents filename)
					(split-string (buffer-string) "\n" t)))

	 ;activate installed packages
  (package-initialize)

	; fetch the list of packages available
	(package-refresh-contents)

	;install the missing packages
	(dolist (package install-needed-packages)
	  (setq package (intern package))
		(unless (package-installed-p package)
		  (if (y-or-n-p (format "Package %s is missing. Install it? " package))
		      (package-install package))))
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 (quote
		("d6922c974e8a78378eacb01414183ce32bc8dbf2de78aabcc6ad8172547cb074" default)))
 '(package-selected-packages
	 (quote
		(company-anaconda company-quickhelp auto-virtualenv anaconda-mode virtualenvwrapper virtualenv wconf jedi-direx jedi company-jedi iedit irony-eldoc auto-complete-clang-async ac-clang auto-complete-clang yasnippet company-irony-c-headers company-irony auto-complete-chunk auto-complete-c-headers ac-c-headers))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
