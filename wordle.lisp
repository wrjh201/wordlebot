(defpackage :wordle
  (:use :cl))
(in-package :wordle)

(defun load-words (path)
  (coerce (uiop:read-file-lines path) 'vector)
  )

(defun validate-words-file (list)
  "Validate the output of the words file. It must be a list of strings of length 5."
  (every (lambda (line) (= (length line) 5)) list)
  )

(defparameter *words* (load-words #P"words-small.txt")
  "List of 5 letter words that are valid guesses in Wordle.")

(defun nloops (lst n)
  "Return the N-ary cartesian power of LST."
  ;; https://codegolf.stackexchange.com/a/165665
  (if (< n 1)
      '(())
      (if (< n 2)
          (loop for x in lst collect (list x))
          (loop for a in lst
                nconc (loop for b in (nloops lst (1- n))
                            collect (cons a b))))))

(defun result-green-pass (guess word)
  "Only set matching letters green. Leave the rest grey."
  (loop for lg across guess
	for lw across word
	collect (cons lg (if (char= lg lw) 'green 'grey))))

(defun get-result (guess word)
  "Generate the result of GUESS when the wordle is WORD."
  (loop with result = (result-green-pass guess word)
	for r in result
	do (print result)
	unless (eq (cdr r) 'green)
	  do (when (some (lambda (lw rs) (and (char= (car r) lw)
					      (eq rs 'grey)))
			 word
			 (map 'list #'cdr result))
	       (setf (cdr r) 'yellow))
	finally (return result)
	)
  )

;; result-matches-p
;; TODO: write a function that takes a word, a guess and a result and returns T
;;       if the word is a possible candidate for the answer given the guess and
;;       its result.

(defun result-matches-p (guess word result)
  "Returns T when WORD is a possible candidate for GUESS with RESULT. NIL otherwise."
  (and
   (loop for guess-char across guess
	 for word-char across word
	 for res in result
	 always (and
		 (not (and (eq res 'grey)
			   (find guess-char word)))

		 (case res
		   (green (char= guess-char word-char)) ;; check each green letter matches
		   (grey (not (char= guess-char word-char))) ;; check each grey letter doesnt
		   (yellow (not (char= guess-char word-char))))
		 
		 (>= (count guess-char word)
		     (loop for gc across guess
			   for res in result
			   count (and (eq res 'yellow)
				      (char= gc guess-char))))
		 ))))

(defun wordle-emoji (result)
  "When RESULT is a symbol, return its wordle colour symbol. If RESULT is a list, do this for each element. Errors on incorrect symbol."
  (cond ((symbolp result) (ecase result
			    (green #\????)
			    (yellow #\????)
			    (grey #\???)))
	((listp result) (map 'string 'wordle-emoji result))))

(defun possible-words (guess result &optional (words *words*))
  "Return a list of possible words matching result with guess from words."
  (loop for word across words
	if (result-matches-p guess word result)
	  collect word)
  )

(defun simple-px (guess result &optional (words *words*))
  "Naive implementation of p(x). The probability of the pattern of guess with result occuring randomly."
  (/ (length (possible-words guess result words))
     (length words)))

(defun expected-information (guess &optional (words *words*))
  "The expected information from guessing GUESS from the word pool WORDS."
  (loop for result in (nloops '(green yellow grey) 5)
	for px = (simple-px guess result words)
	unless (zerop px)
	  sum (* px (- (log px 2)))
	;; sum px
	))

(defun all-expected-info (&optional (words *words*))
  (loop for word across words
	for idx from 1
	with total-words = (length words)
	if (zerop (mod idx 100))
	  do (format t "~f%~%" (* 100 (/ idx total-words)))
	collect (cons word (expected-information word words))))
