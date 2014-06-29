(defun scan-github-vocab ()
  "Returns github vocab files"
  (let ((found-something))
    (with-temp-buffer
      (browse-url-emacs "https://github.com/dem42/memorize.el/tree/master/vocabulary")
      (beginning-of-buffer)
      (while (re-search-forward "href=\".*/\\(.*?\\.vocab\\)\"" nil t)
	(message (match-string 1))
	(setq found-something (cons (match-string 1) found-something))))
    found-something))


(provide 'vocab-downloader)



