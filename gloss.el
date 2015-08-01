;; Open questions
;; - What's the correct interpretation for <+>? Does it bind tighter than <-> or looser than <=>? (Is there a fixed answer?)
;; - How should we handle ambiguous affixes (e.g. <k-at-inw-il-oh> rather than <k- at- inw- il -oh>)?
;; - Is there ANYTHING sensible we can do with infixes?

;; BUGS TO FIX:

;; space-pad-group also space-pads the blank line following the group
;; (and does this repeatedly, even once that line already ends in a
;; space --- possibly because the inserted spaces are after point-max?)


;; ROADMAP. At least a blog post's worth of features at each numbered level.

;; 1. Refactoring!
;(hyphen
; (cond
;  ((we just inserted a space) (insert a suffix break))
;  ((there is a parallel prefix break) (insert a prefix break))
;  ((there is a parallel suffix break) (insert a suffix break))
;  ((there are preceding suffix breaks on current line) (insert a suffix break))
;  (t (insert a prefix break)))
; (forward-cell -1) ;; puts us in the cell before the break regardless of which kind of break we inserted
; (straighten-current-cell)) ;; puts us back after the break again

;; Rules:
;; -- Space-hyphen will always give you a suffix
;; -- Otherwise it tries to guess what you meant, erring on the side of prefix if it's not sure
;; -- To change, use C--

;; C-- : turn a preceding, accessible prefix boundary to a suffix boundary and vice versa
;; C-= : likewise
;; C-+ : likewise




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
(require 'thingatpt)

;; Helper functions

(defun xmn ()
  (interactive)
  (message "%s" (text-properties-at (point))))

(defun max-non-nil (list)
  (let ((list* (-non-nil list)))
    (if list* 
	(apply 'max (-non-nil list))
      nil)))

(defun pos-to-column (pos)
  (save-excursion
    (goto-char pos)
    (current-column)))

;; Cells are delimited by morph breaks

(setq cell-breaks
      ["[^-=+ ] *\\( \\)[^-=+ ]"   ; last space in a group that is not delimited on either side by any other boundary char
       "= *\\( \\)[^ ]"    ; last space in a group after a equals 
       " *\\( \\)="    ; last space in a group before an equals
       "- *\\( \\)[^ ]"    ; last space in a group after a hyphen 
       " *\\( \\)-"])  ; last space in a group before a hyphen

(setq cell-end " *\\( \\)[^ ]")

(defun forward-cell (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (while (< arg 0)
    (let ((pos (point))
	  (par-beg (line-beginning-position)))
      (if (and (re-search-backward cell-end par-beg t)
	       (or (< (match-end 1) pos)
		   (re-search-backward cell-end par-beg t)))
	  (goto-char (match-end 1))
	(goto-char par-beg)))
    (setq arg (1+ arg)))
  (while (> arg 0)
    (let ((par-end (line-end-position)))
      (if (re-search-forward cell-end par-end t)
	  (progn
	    (goto-char (match-beginning 1))
	    (forward-char))
	(goto-char par-end)))
    (setq arg (- arg 1))))


(defun beginning-of-cell ()
  (interactive)
  (beginning-of-thing 'cell))

(defun end-of-cell ()
  (interactive)
  (end-of-thing 'cell))

;; Iterating through a line group

(defmacro restrict-to-group (&rest body)
  `(save-excursion
     (save-restriction
       (end-of-paragraph-text)
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
	(setq more-lines (progn (forward-line) (= (point) (line-beginning-position))))))
     accumulator))

;; Locating morph breaks on the current line

(defun next-break (level)
  (save-excursion
    (if (re-search-forward (aref cell-breaks level) (line-end-position) t)
	(- (pos-to-column (point)) 1)
      (pos-to-column (line-end-position)))))

(defun next-breaks-up-to (level)
  (-map 'next-break
	(number-sequence 0 (- level 1))))
			       
(defun next-break-if-accessible (level)
  (let ((candidate (next-break level))
	(competitors (next-breaks-up-to level)))
    (if (and candidate (-all? (-partial '< candidate) competitors))
	candidate
      nil)))

;; TODO: REFACTOR
(defun accessible-break-in-group? (level)
  (save-excursion
    (beginning-of-cell)
    (-non-nil
     (each-line-in-group
      (next-break-if-accessible level)))))
  
;; Straightening morph breaks

(defun pad-to-column (target-breakpoint level)
  (save-excursion
    (let ((actual-breakpoint (next-break-if-accessible level)))
      (when actual-breakpoint
	(move-to-column actual-breakpoint)
	(insert-char ? (- target-breakpoint actual-breakpoint))))))

(defun straighten-one-break (&optional level)      
  (interactive "P")                                
  (or level (setq level 0))
  (let ((target-column (max-non-nil                
			(each-line-in-group        
			 (beginning-of-cell)       
			 (next-break-if-accessible level)))))
    (when target-column                            
      (each-line-in-group                          
       (beginning-of-cell)
       (pad-to-column target-column level)
       (loop for i from 0 below level do
	     (pad-to-column (+ target-column 2) i)))
      (move-to-column target-column))))
      
(defun straighten-cell (times)
  (interactive "p")
  (labels ((straighten-more ()
	      (loop for i downfrom 4 to 0
		       until (straighten-one-break i))
	      (unless (and (looking-at "[^ ]")
			   (looking-back " "))
		(straighten-more))))
    (beginning-of-cell)
    (loop repeat times do
	  (straighten-more))))

(define-minor-mode gloss-mode
  "Foo"
  :init-value nil
  :lighter " Gloss")




