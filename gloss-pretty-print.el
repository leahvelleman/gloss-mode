(defun make-vlist (width height contents)
  (list (list width height) contents))

(defun atomic-vlist (s)
  (make-vlist (length s) 1 (list s)))

(defun empty-vlist (width height)
  (list (list width height) (-repeat height (s-repeat width " "))))

(defun vlist-height (vlist)
  (car (cdr (car vlist))))

(defun vlist-width (vlist)
  (car (car vlist)))

(defun vlist-contents (vlist)
  (car (cdr vlist)))

(defun suffix? (vlist) (s-starts-with? "-" (car (vlist-contents vlist))))
(defun prefix? (vlist) (s-ends-with? "-" (s-trim-right (car (vlist-contents vlist)))))

(defun vlist-contents-pad-to-height (target-height vlist)
  (let ((actual-height (vlist-height vlist))
	(width (vlist-width vlist))
	(contents (vlist-contents vlist)))
    (if (>= actual-height target-height)
	contents
      (-concat contents (-repeat
			 (- target-height actual-height)
			 (cond ((prefix? vlist) (concat     (s-repeat (- width 1) "*") "-"))
			       ((suffix? vlist) (concat "-" (s-repeat (- width 1) "*")))
			       (t                           (s-repeat width "*"))))))))

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

(defun space-concat (a b) (s-concat a " " b))

(defun horiz-concat (&rest xs)
  (-reduce (-partial '-zip-with 'space-concat) xs))

(defun horiz (&rest xs)
  (let ((width (apply '+ (-map 'vlist-width xs)))
	(height (apply 'max (-map 'vlist-height xs))))
    (make-vlist width
		height
		(apply 'horiz-concat (-map (-partial 'vlist-contents-pad-to-height height) xs)))))


(h-concat* '("abc" "def") '("ghi" "jkl") '("mnop" "qrst"))

(vert (atomic-vlist "katinwiloh")
       (horiz (vert  (atomic-vlist "k-")
		     (atomic-vlist "inc-")
		     (atomic-vlist "extra-"))
	       (vert (atomic-vlist "at-")
		     (atomic-vlist "b2s-"))
	       (vert (atomic-vlist "inw-")
		     (atomic-vlist "a1s-"))
	       (vert (atomic-vlist "il")
		     (atomic-vlist "see"))
	       (vert (atomic-vlist "-oh")
		      (atomic-vlist "-ss")))
       (atomic-vlist "VTIF"))

;; Repeat until done: If any of the initial tokens are prefixes, join them into a (pre ...)
;; 
