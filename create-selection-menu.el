;; enhancing will not work with font-lock mode on because it removes text-properties
(defun enhance-string (str enhance-callback)
  "Add menu properties to string"
  (interactive "s")
  ;;some text properties in emacs are stick which means they will by inherited by text that follows the enhanced text .. this disables it
  (put-text-property 0 (length str) 'rear-nonsticky t str)
					;(lexical-let ((lfun `(lambda () (message "%s" ,str)))
					;  (put-text-property 0 (length str) 'point-entered (lambda (b a) (funcall lfun)) str))
					; (put-text-property 0 (length str) 'point-left 
					;		     (lambda (b a) (remove-text-properties 0 (length str) '(face '(:underline t)) str)) str)
  ;;(put-text-property 0 (length str) 'face '(:background "DarkViolet" :foreground "white") str)
  (add-text-properties 0 (length str) '(mouse-face underline) str)
  (lexical-let ((s str) (map (make-sparse-keymap)) (callback enhance-callback))
    (define-key map [mouse-2] (lambda () (interactive) (message s) (funcall callback s)))
    (define-key map [mouse-1] (lambda () (interactive) (message s) (funcall callback s)))
    (define-key map (kbd "RET") (lambda () (interactive) (message s) (funcall callback s)))
    (put-text-property 0 (length s) 'keymap map s)
    (insert s))
  str)




;; a function to construct the menu and insert it into the current buffer
(defun insert-menu-items (list enhance-callback)
  (let ((i 0)) 
    (dolist (elem list)
      (newline)
      (setq i (1+ i))
      (insert-string (enhance-string (concat (number-to-string i) ". " elem) enhance-callback)))))

(provide 'create-selection-menu)



(defun operate-this-button (str)
  (interactive "s")
  (message "hello world %s" str))





hello

hello
