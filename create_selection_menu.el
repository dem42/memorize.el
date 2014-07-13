;; enhancing will not work with font-lock mode on because it removes text-properties
(defun enhance-string (str)
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
  str)


;; lambdas
;; (setq z 
;;       (let ((a 3))
;; 	`(lambda () (+ ,a 2))))

;; (funcall z)

;; (defun foo (n)
;;   (lexical-let ((n n)) #'(lambda() n)))

;; (funcall (foo 10))

;; (foo 10)
;; ((lambda () 3))


;; a function to construct the menu and insert it into the current buffer
(defun insert-menu-items (list)
  (let ((i 0)) 
    (dolist (elem list)
      (newline)
      (setq i (1+ i))
      (insert-string (concat (number-to-string i) ". " (enhance-string elem))))))

