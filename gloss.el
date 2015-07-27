;; Open questions
;; - What's the correct interpretation for <+>? Does it bind tighter than <-> or looser than <=>? (Is there a fixed answer?)
;; - How should we handle ambiguous affixes (e.g. <k-at-inw-il-oh> rather than <k- at- inw- il -oh>)?
;; - Is there ANYTHING sensible we can do with infixes?

;; ROADMAP. At least a blog post's worth of features at each numbered level.

;; 1. Consolidating current features
;; - Handle decoration correctly in the following situation:
;;       k-   at- inw- il  -oh
;;       INC-          see -VTIF
;; - UNIT TESTING: Test features both in and out of evil-mode

;; 2. Basic new features required for next steps
;; - Recognize when we are in glosstext
;; - Correctly jump to the same line in a new group
;; - Line wrapping and carriage returns
;; - Handle line prefixes \ft etc. (one possibility: use display-margins?)
;; - Recognize lines which should not be aligned (and alter restrict-to-group to ignore them?)
;; - - Also this interacts with line wrapping in tricky ways

;; 3. Think about keybindings for text entry
;; - Straighten-a-line mapped to C-c C-c
;; - How to handle [ =+-] in glosstext?
;; - What should TAB do?

;; 4. Functions and keybindings for editing and revising in evil-mode
;; - <da > <dat > <da=> <dat=> etc (and ya..., va..., ca...)
;; - - What should "inner" textobjects do? (Leave the delimiter?)
;; - <mrm> <mrtm> "merge right morpheme," "merge right tall morpheme", plus <mlm> <mltm> etc
;; - Sensible movement commands:
;; - - "Go to the next blank morph slot on this line"
;; - - "Go up/down a group but stay on this line"
;; - Boundary-changing commands a la surround.vim?
;; - - <crt=-> "change right-hand clitic boundary to an affix boundary"?
;; - - <crt-.> "change right-hand clitic boundary to a non-breaking period"?

;; 5. Autocomplete
;; - Levels:
;; - - Simple: based on what's elsewhere in the current file
;; - - Harder: based on a tagfile for a larger directory of files
;; - - Hardest: based on an external fsm
;; - Interface:
;; - - "Fill in this cell" vs. "Fill in this slot all the way down"?
;; - - 


;; A thought:
;; 
;; Normal textobjects occupy a single line and are defined by their delimiters: <da > is a synonym for <daw>, <da-> deletes a prefix or suffix, etc.
;; (Will we need a hierarchy of delimiters? In e.g. k- at= inw- il -oh, with cursor over "at=", should <da=> delete the k- too or not?)
;; (If <da=> does delete the k- too, we will also need something like <dam> ("delete-a-morph") that will just delete the at= in the above situation.)
;; 
;; "Tall" textobjects occupy multiple lines: <da^ > means "delete all lines of the current space-delimited word".
;; 
;; Eventually the tall textobject system could be extended to things like <da^s> meaning "Delete all lines of the current sentence of glosstext."

(require 's)
(require 'dash)
(require 'cl)

;; Helper functions

(defun xmn ()
  (interactive)
  (message "%s" (text-properties-at (point))))

(defun max-non-nil (list)
  (let ((list* (-non-nil list)))
    (if list* 
	(apply 'max (-non-nil list))
      nil)))

;; Iterating through a line group

(defmacro restrict-to-group (&rest body)
  `(save-excursion
     (save-restriction
       (forward-paragraph)
       (let ((end (point)))
	 (backward-paragraph)
	 (narrow-to-region (point) end)
	 (progn ,@body)))))

(defmacro each-line-in-group (&rest body)
  `(let ((column (current-column))
	 (accumulator '())
	 (more-lines t))
     (restrict-to-group
      (while more-lines
	(move-to-column column)
	(setq accumulator (cons (progn ,@body) accumulator))
	(setq more-lines (= 0 (forward-line 1)))))
     accumulator))

;; Decorating morph breaks

(setq levels '(("[^-=+ ] *\\( \\)[^-=+ ]" 1)   ; last space in a group that is not
	                         ; delimited on either side by any other boundary char
	       ("+ *\\( \\)" 2)  ; last space in a group after a plus 
	       (" +\\(+\\)" 3)   ; plus after a group of spaces
	       ("= *\\( \\)" 4)  ; last space in a group after a equals 
	       (" +\\(=\\)" 5)   ; equals after a group of spaces
	       ("- *\\( \\)" 6)  ; last space in a group after a hyphen 
	       (" +\\(-\\)" 7))) ; hyphen after a group of spaces

(defun decorate-group ()
  (interactive)
  (dolist (l levels)
    (let ((regex (car l))
	  (level (cadr l)))
      (restrict-to-group
       (while (re-search-forward regex nil t)
	 (add-text-properties (match-beginning 1)
			      (match-end 1)
			      `(break-level ,level face highlight)))))))

;; Moving by morph breaks

(defun next-break (level)
  (pos-to-column (text-property-any (point) (line-end-position) 'break-level level)))

(defun next-breaks-up-to (level)
  (-map 'next-break
	(number-sequence 0 (- level 1))))
			       
(defun next-break-if-accessible (level)
  (let ((candidate (next-break level))
	(competitors (next-breaks-up-to level)))
    (if (and candidate (-all? (-partial '< candidate) competitors))
	candidate
      nil)))

;; Straightening morph breaks

(defun pad-to-column (target-breakpoint level)
  (save-excursion
    (let ((actual-breakpoint (next-break-if-accessible level)))
      (when actual-breakpoint
	(move-to-column actual-breakpoint)
	(insert-char ? (- target-breakpoint actual-breakpoint))))))

(defun straighten-one-break (level)
  (let ((target-column (max-non-nil
			 (each-line-in-group (next-break-if-accessible level)))))
    (when target-column
      (each-line-in-group
       (pad-to-column target-column level)
       (loop for i from 1 below level do
	     (pad-to-column (+ target-column 2) i)))
      (move-to-column (+ target-column 1)))))
      
(defun straighten-to-next-break ()
  (loop for i downfrom 7 above 0
	until (straighten-one-break i))
  (unless (get-text-property (- (point) 1) 'break-level)
    (straighten-to-next-break)))

(defun sd (times)
  (interactive "p")
  (decorate-group)
  (loop repeat times do
	(straighten-to-next-break)))



(each-line-in-group (line-end-position))


;;adf= asdf
;;asdf= a -s -d -f -g -h-
;;aassdf= a -ss -dd -ff -gg -hh-

(line-end-position)

;;asdasd= asdf =asdfa0dfas =asdf 
;;adsf= as -asdfasdf asdfasdfa 
;;a- asdfa= a -s -s =asdfa 
;;asdfasdf= asdfasdfasd =asdfadsf 
;;a- sdf -asdf= asdf -s -d =a- asdfads 
;;1sg- plu -fuck= this -is -a =test- so 
