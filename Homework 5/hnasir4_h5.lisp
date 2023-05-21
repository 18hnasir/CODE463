;; Name: Hammadullah Nasir
;; G#: 01112406

;; I am mirroring my haskell functions as much as possible

;;Function that returns the prime factors of n
(defun prime-factors (n)
	(cond ((= 1 n) NIL)
				((is-prime n 2) (cons n NIL))
				(t (get-prime-factors 2 n))))

;;Function that gets the next current prime
(defun get-current-prime (n) 
	(cond ((eq T (is-prime n 2)) n)
				(t (get-current-prime (+ n 1)))))

;;Function that adds the actual prime factors of the given n
(defun get-prime-factors (cp cn)
	(cond ((= 0 (mod cn cp)) 
		(cond ((eq T (is-prime (/ cn cp) 2)) (cons cp (cons (/ cn cp) NIL)))
					(t (cons cp (get-prime-factors cp (/ cn cp))))))
		(t (get-prime-factors (get-current-prime (+ cp 1)) cn))))

;;Function to see if a number is prime or not
;;Always pass a "2" for i when calling the function, Ex (is-prime 5 2)
(defun is-prime (n i) 
	(cond ((= n 1) T)
				((= n 2) T)
				((= n i) T)
				((= 0 (mod n i)) NIL)
				(t (is-prime n (+ i 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Function that tells if two numbers are coprimes
;;Coprime numbers have a great common divisor of 1
(defun coprime (a b) 
	(cond ((= (gcd-custom a b) 1) T)
				(t NIL)))

;;Function that gets the greatest common divisor for two given numbers
(defun gcd-custom (x y) 
	(cond ((= 0 y) x)
				(t (gcd-custom y (mod x y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Function returns the nth tribonacci number
(defun trib (n) 
	(cond ((< n 3) 1)
				(t (get-trib-num n 0 1 1 1))))

;; Pass in the value of n, currentsum, bottomthree, middlethree, and upperthree
;; update as the function is recalled unitl n <= 2
(defun get-trib-num (n cs bt mt ut) 
	(cond ((<= n 2) cs)
				(t (get-trib-num (- n 1) (+ bt mt ut) mt ut (+ bt mt ut)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Function finds the max two numbers in a list
(defun max-two (xs) 
	(cond ((null xs) NIL)
				((= (length xs) 1) xs)
				(t (get-the-maxes (get-current-max (first xs) (rest xs)) xs))))

;;returns a list with the current max
;;if there is a duplicate, kt only removes the first occurence
(defun get-second-max-list (cm xs) 
	(cond ((null xs) NIL)
				((= cm (first xs)) (rest xs))
				(t (cons (first xs) (get-second-max-list cm (rest xs))))))

;;gets the current max of the given list
(defun get-current-max (cm xs) 
	(cond ((null xs) cm)
				((< cm (first xs)) (get-current-max (first xs) (rest xs)))
				(t (get-current-max cm (rest xs)))))

;;Returns the list of the two maxes
(defun get-the-maxes (cm xs)
	(get-the-maxes-2 cm (get-second-max-list cm xs)))

;;Returns the list of the town maxes
(defun get-the-maxes-2 (cm xs) 
	(cons cm (cons (get-current-max (first xs) (rest xs)) NIL)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Function that reverses the list
(defun reversed (xs) 
	(cond ((null xs) NIL)
				(t (append (reversed (rest xs)) (list (first xs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Function to return a clockwise double list
(defun clockwise (grid)
	(cond ((null grid) NIL)
				(t (reverse-it (cw-helper-1 (first grid) (rest grid))))))

;;Jumps starts the mergin process for a list of length 2 or more
;;or just returns the clockwised version if the list length = 1
(defun cw-helper-1 (is xs) 
	(cond ((null xs) (cw-helper-3 is))
				(t (cw-helper-2 (cw-helper-3 is) xs))))

;;Continues to merge each list together until their are no more
(defun cw-helper-2 (fs xs) 
	(cond ((null xs) fs)
				(t (cw-helper-2 (merge-1 fs (cw-helper-3 (first xs))) (rest xs)))))

;;Makes each inner list its own 2D list
(defun cw-helper-3 (is) 
	(cond ((null is) NIL)
				(t (cons (list (first is)) (cw-helper-3 (rest is))))))

;;Concatenates each innerlist of two lists
(defun merge-1 (is us) 
	(cond ((and (null is) (null us)) NIL)
				(t (cons (merge-2 (first is) (first us)) (merge-1 (rest is) (rest us)))))) 

;;Concatenates two lists
(defun merge-2 (is us) 
	(cond ((and (null is) (null us)) NIL)
				(t (append is us))))

;;Reverses each inner list
(defun reverse-it (xs) 
	(cond ((null xs) NIL)
				(t (cons (reversed (first xs)) (reverse-it (rest xs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Function that returns true if any element is true or NIL otherwise
(defun any (bs) 
	(cond ((null bs) NIL)
				((eq (first bs) NIL) (any (rest bs)))
				((eq (first bs) t) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Function returns list of passed values for a given predicate 
(defun select (p xs) 
	(cond ((null xs) NIL)
				((eq (funcall p (first xs)) t) (cons (first xs) (select p (rest xs))))
				(t (select p (rest xs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Function returns results of applied values to the given function
(defun zip-with (f as bs) 
	(cond ((null as) NIL)
				((null bs) NIL) 
				(t (cons (funcall f (first as) (first bs)) (zip-with f (rest as) (rest bs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Function returns a 2D augmented identity matrix
(defun augdentity (r c) 
	(aug-helper 0 r c))

;;Constructs the matrix by making the rows
(defun aug-helper (ri r c) 
	(cond ((< ri r) (cons (create-row 0 ri c) (aug-helper (+ ri 1) r c)))
				((= ri r) NIL)))

;;Creates a row array with the 1 being in the correct position
;;i = index (starts off as 0) oi = one index (where the one will be placed)
(defun create-row (i oi c)
	(cond ((and (= i oi) (< i c)) (cons 1 (create-row (+ i 1) oi c)))
				((= i c) NIL)
				(t (cons 0 (create-row (+ i 1) oi c)))))
