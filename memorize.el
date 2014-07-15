;;; memorize.el
;; --- interactive mode for memorize foreign language vocabulary

;; Copyright MIT License 

;; Emacs Lisp Archive Entry
;; Filename:      memorize.el
;; Version:       0.1
;; Keywords:      memorization languages
;; Author:        Martin P
;; Description:   memorizing foreign language vocabulary in emacs


(add-to-list 'load-path ".")
(require 'vocab-downloader)
(require 'create-selection-menu)

;; ideally we would like it to ask us which vocabulary we want to memorize when we 
;; enter the mode
(defvar memorize/vocabulary-folder (concat library-dir "/.emacs.d/memorize/vocabulary/"))
(defvar memorize/vocabulary-type "spanish.vocab")
;; equality test is not default, default is eql which seems to be object id test 
(defvar memorize/vocabulary-map (make-hash-table :test 'equal))
 
;; Parses the user chosen file to build the vocabulary map 
;; use Edebug to get interactive lisp debugging (C-u C-M-x)
;; the vocab files have the format:
;;foreign word=translation
(defun memorize/reload-vocabulary-map ()
  "clears and updates the vocabulary map based on the value of \(memorize/vocabulary-type)"
  (interactive)
  (clrhash memorize/vocabulary-map)
  (let ((file (concat memorize/vocabulary-folder memorize/vocabulary-type)))
    (with-temp-buffer
      (insert-file-contents file)
      (while (not (eobp))
	(let ((cp (point)))
	  (end-of-line)
	  (let* ((line (buffer-substring cp (point))) 
		 (chunks (split-string line "="))
		 (vocab (car chunks))
		 (translation (car (cdr chunks))))
	    (puthash vocab translation  memorize/vocabulary-map))
	  (next-line)
	  (beginning-of-line))))))

;; Main checker method. Checks entered word against user loaded dictionary 
;; and updates the buffer local hash table
;; TODO: would be awesome to get fuzzy matching so that it finds the
;; closest words and highlights the errors?
(defun memorize/check-word-and-newline ()
  "checks whether the last word is in the dictionary and inserts newline or OK"
  (interactive)
  (let ((cp (point)) (word-to-check))
    (save-excursion
      (backward-word)
      (setq word-to-check (buffer-substring (point) cp)))
    (if (eq (gethash word-to-check memorize/vocabulary-map) nil)
	(progn 
	  (insert " WRONG")
	  (newline))
      (progn
	(insert " OK")
	(memorize/add-to-local-dictionary word-to-check)
	(newline)))))

;; This interactive method clears the local hash table
;; this lets the user start a new memorize session
(defun memorize/clear-buffer ()
  "Clear the buffer in preparation for a new memorize session"
  (interactive)
  (erase-buffer)
  (clrhash memorize/buffer-local-map))

(defun memorize/initalize-local-var ()
  "Intialize the buffer local hash table which contains correct guesses"
  (make-local-variable 'memorize/buffer-local-map)
  (setq memorize/buffer-local-map (make-hash-table :test 'equal)))

(defun memorize/add-to-local-dictionary (word)
  "Adds words to local dictionary"
  (puthash word t memorize/buffer-local-map))


(setq memorize/vocabulary-type "spanish.vocab")

(defun memorize/vocab-selection ()
  "Downloads vocab and shows the user a menu of avaiable vocab sessions.
   The menu is interactive so the user can start a session in the selected vocab
   by clicking enter or double-clicking on the selected vocab file."
  (erase-buffer)
  (insert-menu-items (scan-github-vocab) 
		     (lambda (str) 
		       (setq memorize/vocabulary-type str)
		       (memorize/reload-vocabulary-map)
		       (memorize/clear-buffer))))

;; A vocabulary session is tied closely to a vocab file
;; this function will display options based on which vocab files are available
;; (defun memorize/choose-vocabulary-session ()
;;   "Allows the user the pick the vocabulary session"
;;   (interactive)
;;   (dolist (file (directory-files memorize/vocabulary-folder))
;;     (insert file)
;;     (newline)))

;;;(regexp-opt '("OK" "WRONG")) <- to generate an optimal regexp
(defvar memorize-font-lock-keywords
  (list '("\\(?:OK\\|WRONG\\)" . font-lock-constant-face))
  "Highlighting for memorize mode")

(defvar memorize-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'memorize/check-word-and-newline)
    (define-key map (kbd "C-c C-k") 'memorize/clear-buffer)
    map)
  "Keymap for memorize mode")

;; major mode definition
(define-derived-mode memorize-mode text-mode "Memorize"
  "Major mode for memorizing vocabulary"
  (memorize/initalize-local-var)
  (setq font-lock-defaults '(memorize-font-lock-keywords))
  (setq header-line-format 
	(list " Vocab guessed: " 
	      '(:eval (number-to-string (hash-table-count memorize/buffer-local-map))) 
	      "/" 
	      '(:eval (number-to-string (hash-table-count memorize/vocabulary-map))))
	))

;(add-hook 'memorize-mode-hook (lambda () (memorize/choose-vocabulary-session)))
;;(add-hook 'memorize-mode-hook (lambda () (memorize/clear-buffer)))
(add-hook 'memorize-mode-hook (lambda () (memorize/vocab-selection)))

;;; autoload
(add-to-list 'auto-mode-alist '("\\.memorize\\'" . memorize-mode))

(provide 'memorize)
