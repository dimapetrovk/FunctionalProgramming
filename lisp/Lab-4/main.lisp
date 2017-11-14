(defclass sentence()
  (
  	(sentence-content :accessor sentence-content)
    (words-count  :accessor words-count)
  )
)

(defun price-setter (s wordcount)
"set word count for each sentance"
  (setf (words-count s) wordcount)
)

(defun printer (bag-of-sentences)
"print all sentances"
	(loop for sentence in bag-of-sentences
		do
		(format t "~D <==> ~:S  ~%" (sentence-content sentence) (words-count sentence))
	)
)

(defun split-to-sentences (sentence-bag string)
"split text sentances"
  (loop for i = 0 then (1+ j)
    as j = (position #\. string :start i)
    do
    	 (setf snt (make-instance 'sentence))
    	 (setf _place (string-left-trim " " (subseq string i j)))
         (setf (sentence-content snt) _place)
         (if (string/= "" _place)
             (push snt sentence-bag))
    while j)
  sentence-bag
)

(defun sorter-by-words (bag-of-sentences)
"sord by words count sentances"
	(sort bag-of-sentences #'< :key (lambda (p) (words-count p) ))
)

(defun cooker (bag-of-sentences)
   (loop for sentence in bag-of-sentences
   do
   	(word-counter sentence)
   )
   (sorter-by-words bag-of-sentences)
   (printer bag-of-sentences)
)

(defun word-counter (s)
"count words in string"
   	(setf (words-count s) (1+ (count-sub (sentence-content s) " ")))
)

(defun count-sub (str pat)
"count pattern occurrences in string"
 (loop with z = 0 with s = 0 while s do
       (when (setf s (search pat str :start2 s))
         (incf z) (incf s (length pat)))
       finally (return z)))

(setq fname "/Users/dimapetrov/Projects/univ/4.1/functional/lisp/Lab-4/lab4.txt")
(setf bag-of-sentences '())
(with-open-file (stream fname)
    (loop for line = (read-line stream nil)
          while line
          do (
              setf bag-of-sentences (split-to-sentences bag-of-sentences line)
          )
     ))
(cooker bag-of-sentences)
