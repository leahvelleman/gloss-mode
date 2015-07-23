;; Open questions
;; - What's the correct interpretation for <+>? Does it bind tighter than <-> or looser than <=>? (Is there a fixed answer?)
;; - How should we handle ambiguous affixes (e.g. <k-at-inw-il-oh> rather than <k- at- inw- il -oh>)?
;; - Is there ANYTHING sensible we can do with infixes?

;; ROADMAP. At least a blog post's worth of features at each numbered level.

;; 1. Consolidating current features
;; - Fixes to (sd)
;; - - Handle straightening at the end of a line. This will sometimes
;; - -   have to include the insertion of new morph-breaks on some
;; - -   lines, and will also have to include a sensible behavior when
;; - -   we get to the end of a line. (Jump to start of next group?)
;; - - Give a better name
;; - - Interact correctly with the universal prefix
;; - Refactor each-line-in-group to use restrict-to-group
;; - Possibly get rid of by-cell and by-group movement commands if they are no longer relevant --- we will reimpliment this better in 4
;; - Handle decoration correctly in the following situation:
;;       k-   at- inw- il  -oh
;;       INC-          see -VTIF
;; - UNIT TESTING: Test features both in and out of evil-mode

;; 2. Basic new features required for next steps
;; - Recognize when we are in glosstext
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

(defun blank? ()
  (or (s-equals? (thing-at-point 'line) "")
      (s-equals? (thing-at-point 'line) "\n")))

(defun max-non-nil (list)
  (let ((list* (-non-nil list)))
    (if list* 
	(apply 'max (-non-nil list))
      nil)))

(defun pos-to-column (pos)
  (if pos
      (save-excursion (goto-char pos) (current-column))
    (line-end-column)))

(defun line-end-column ()
  (pos-to-column (line-end-position)))

;; Horizontal movement by cell

(defun beginning-of-cell ()
  (interactive)
  (beginning-of-thing 'word))

(defun cell-beginning-position ()
  (save-excursion
    (beginning-of-cell)
    (current-column)))

(defun end-of-cell ()
  (interactive)
  (end-of-thing 'word)
  (cond ((= 0 (current-column))
	 (forward-line -1)
	 (end-of-line)
	 (current-column))
	(t
	 (search-forward-regexp "[[:space:]]*")
	 (forward-char -1)
	 (current-column))))

(defun cell-end-position ()
  (save-excursion
    (end-of-cell)
    (current-column)))

;; Vertical movement by line group

(defun beginning-of-group ()
  ;;; TODO
  )

(defun group-beginning-line ()
  ;;; TODO
  )

(defun end-of-group ()
  (interactive)
  (forward-paragraph)
  (if (blank?)
      (forward-line -1)))

(defun group-end-line ()
  ;;; TODO
  )

;; Iterating through a line group

;;; TODO: refactor this in the same terms as restrict-to-group.
(defmacro each-line-in-group (f)
  `(save-excursion
     (let ((column (current-column)))
       (end-of-group) ;; We work through the group backwards so that accumulator ends up in an intuitive order.
       (setq accumulator '())
       (while (not (blank?))
	 (move-to-column column)
	 (setq accumulator (cons ,f accumulator))
	 (forward-line -1))
     accumulator)))

(defmacro restrict-to-group (&rest body)
  `(save-excursion
     (save-restriction
       (forward-paragraph)
       (let ((end (point)))
	 (backward-paragraph)
	 (narrow-to-region (point) end)
	 (progn ,@body)))))

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

(defun straighten (level)
  (let ((target-column (max-non-nil
			 (each-line-in-group (next-break-if-accessible level)))))
    (when target-column
      (each-line-in-group (progn
			    (pad-to-column target-column level)
			    (loop for i from 1 below level do
				  (pad-to-column (+ target-column 2) i))))
      (move-to-column (+ target-column 1)))))
      
(defun sd ()
  (interactive)
  (decorate-group)
  (loop for i downfrom 7 above 0
	until (straighten i))
  (unless (get-text-property (- (point) 1) 'break-level)
    (sd)))


;;asdasd=  asdf =asdfadfas
;;adsf=  as -asdfasdf =asdfasdfa 
;;a- asdfa= a  -s -s =asdfa
;;asdfasdf=  asdfasdfasd =asdfadsf
;;a-  sdf -asdf= asdf -s -d =a-  asdfads
;;1sg- plu -fuck= this -is -a =test- so
