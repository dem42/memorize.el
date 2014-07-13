(setq listo '("aha" "nono"))

;; enhanicng will not work with font-lock mode on because it removes text-properties
(defun enhance-string (str)
  "Add menu properties to string"
  (interactive "s")
  ;;some text properties in emacs are stick which means they will by inherited by text that follows the enhanced text .. this disables it
  (put-text-property 0 (length str) 'rear-nonsticky t str)
  (lexical-let ((lfun `(lambda () (add-text-properties 0 (length ,str) '(face '(:underline t)) ,str))))
    (put-text-property 0 (length str) 'point-entered (lambda (b a) (funcall lfun)) str))
 ; (put-text-property 0 (length str) 'point-left 
;		     (lambda (b a) (remove-text-properties 0 (length str) '(face '(:underline t)) str)) str)
  (put-text-property 0 (length str) 'face '(:background "DarkViolet" :foreground "white") str)
  (add-text-properties 0 (length str) '(mouse-face underline) str)
  str)


(setq z 
      (let ((a 3))
	`(lambda () (+ ,a 2))))

(funcall z)

		     (lambda (b a) (message "from %d to %d" b a))

(defun foo (n)
  (lexical-let ((n n)) #'(lambda() n)))

(funcall (foo 10))

(foo 10)
((lambda () 3))


;(add-text-properties 0 (length str) '(face '(:underline t)) str)

(defun insert-menu-item ()
  (interactive)
  (progn 
    (newline)
    (insert-string (enhance-string "hello"))))

