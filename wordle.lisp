(defun load-words (path)
  (uiop:read-file-lines path)
  )

(defun validate-words-file (list)
  "Validate the output of the words file. It must be a list of strings of length 5."
  (every (lambda (line) (= (length line) 5)) list)
  )

(defun nloops (lst n)
  ;; https://codegolf.stackexchange.com/a/165665
  (if (< n 1)
      '(())
      (if (< n 2)
          (loop for x in lst collect (list x))
          (loop for a in lst
                nconc (loop for b in (nloops lst (1- n))
                            collect (cons a b))))))

(defun word-does-match (guess word result)
  "Correct if the guess, with the result is a candidate word. "
  (every (lambda (l1 l2 res)
	   (ecase res
	     (green (char-equal l1 l2))
	     (yellow (some (lambda (x) (char-equal x l1)) word))
	     (grey (notany (lambda (x) (char-equal x l1)) word))
	     ))
	 guess word result))
	   

(defun get-possible-words (guess result &optional (words *words*))
  (loop for word in words
	if (word-does-match guess word result)
	  collect word))

(defun expected-info (word &optional (words *words*))
  "Take a guess and return the expected information gathered from it."
  (let ((total-words (length words)))
    (loop for result in (nloops '(green yellow grey) 5)
	  sum (let ((px (/ (length (get-possible-words word result)) total-words)))
		(if (= 0 px)
		    0
		    (* px (log (/ 1 px) 2))
		)))
    ))

(defparameter *words* (load-words #P"words.txt"))

(defun information-from-probability (p)
  "Get the amount of information in bits from the probability."
  (- (log p 2))
  )

(defun generate-letter-result (letter pos answer)
  (cond ((char= letter (char answer pos)) 'green)
	((find letter answer) 'yellow)
	(t 'grey))
  )

(defun wordle-emoji (result)
  (ecase result
    (green "🟩")
    (yellow "🟨")
    (grey "⬛"))
  )

(defun generate-score (guess answer)
  (loop for letter across guess
	for index from 0
	collect (generate-letter-result letter index answer))
  )
