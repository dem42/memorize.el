(defun scan-github-vocab ()
  "Returns github vocab files"
  (let ((found-something))
    (save-excursion
      (with-temp-buffer
	(browse-url-emacs "https://github.com/dem42/memorize.el/tree/master/vocabulary")
	(beginning-of-buffer)
	(while (re-search-forward "title=\"\\(.*?\\.vocab\\)\"" nil t)
	  (let ((match (match-string 1)))
	    (message match)
	    (setq found-something (cons match found-something))
	    (save-excursion
	      (with-temp-buffer
		(browse-url-emacs (concat "https://raw.githubusercontent.com/dem42/memorize.el/master/vocabulary/" match))
		(write-file (concat memorize/vocabulary-folder "/" match))
		(kill-buffer)))))
	(kill-buffer)))
    found-something))

(provide 'vocab-downloader)



