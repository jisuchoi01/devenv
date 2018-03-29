(defun save-package-list-file () (interactive)
       (setq packs "")
			 (dolist (pack package-activated-list)
				 (setq pack (format "%s" pack))
   			 (if (not(string= pack ""))
						 (setq packs (concat packs "\n" pack))		
					 )
				 )
       (write-region packs nil ".emacs-package-list" nil)
)

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))


(defun load-package-list-and-install() (interactive)
  "Prompt user to enter a file name, with completion and history support."
  (interactive)
  (setq filename (read-file-name "Enter file name:"))

	; install package list
	(setq install-needed-packages
				(with-temp-buffer
					(insert-file-contents filename)
					(split-string (buffer-string) "\n" t)))

	; fetch the list of packages available
	(unless package-archive-contents
		(package-refresh-contents))

	; install the missing packages
	(dolist (package install-needed-packages)
		(unless (package-installed-p package)
			(if (y-or-n-p (format "Package %s is missing. Install it? " package))
					(message "installed")
				;	(package-install package)
				)
			)
		)
	
	;activate installed packages
  (package-initialize)
	)
