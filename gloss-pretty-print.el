(defun --- (s)
  (list (list (length s) 1) (list s)))

(defun make-vlist (width height contents)
  (list (list width height) contents))

(defun empty-vlist (width height)
  (list (list width height) (-repeat height (s-repeat width " "))))

(defun vlist-height (vlist)
  (car (cdr (car vlist))))

(defun vlist-width (vlist)
  (car (car vlist)))

(defun vlist-contents (vlist)
  (car (cdr vlist)))

(defun vlist-contents-pad-to-height (target-height vlist)
  (let ((actual-height (vlist-height vlist))
	(width (vlist-width vlist))
	(contents (vlist-contents vlist)))
    (if (>= actual-height target-height)
	contents
      (-concat contents (-repeat
			 (- target-height actual-height)
			 (s-repeat width "*")))))) 

(defun vlist-contents-pad-to-width (target-width vlist)
  (let ((actual-width (vlist-width vlist))
	(height (vlist-height vlist))
	(contents (vlist-contents vlist)))
    (if (>= actual-width target-width)
	contents
      (-map (lambda (x) (s-concat
			 x
			 (s-repeat (- target-width actual-width) " " )))
	    contents))))

(defun vert (&rest xs)
  (let ((width (apply 'max (-map 'vlist-width xs)))
	(height (apply '+ (-map 'vlist-height xs))))
    (make-vlist width
		height
		(apply '-concat (-map (-partial 'vlist-contents-pad-to-width width) xs)))))


(defun horiz (&rest xs)
  (let ((width (apply '+ (-map 'vlist-width xs)))
	(height (apply 'max (-map 'vlist-height xs))))
    (make-vlist width
		height
		(apply 'h-concat* (-map (-partial 'vlist-contents-pad-to-height height) xs)))))


(defun h-concat (xs ys)
  (-zip-with 'space-concat xs ys))

(defun h-concat* (&rest xs)
  (-reduce 'h-concat xs))

(h-concat* '("abc" "def") '("ghi" "jkl") '("mnop" "qrst"))
(defun horiz* (&rest xs)
  (-reduce 'horiz xs))

(vert (--- "katinwiloh")
       (horiz (vert (--- "k-")
		     (--- "inc-")
		     (--- "extra-"))
	       (vert (--- "at-")
		     (--- "b2s-"))
	       (vert (--- "inw-")
		     (--- "a1s-"))
	       (vert (--- "il")
		     (--- "see"))
	       (vert (--- "-oh")
		      (--- "-ss")
		      (--- "-EXTRA!")))
       (--- "VTIF"))

(horiz (--- "hijk") (--- "abcd") (vert (--- "12") (--- "34")))

				       (defun horiz (a b)
 (vlist-contents (vlist "string")) 
 (vlist-contents-pad-to-height 1 (vert (--- "abc") (--- "defgh")))

(width (atom "string"))
