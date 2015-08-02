;;; align-gloss.el --- Align interlinear glossed text.

;; Author: Leah Grace Velleman <leah.velleman@gmail.com>
;; Version: 0.1
;; Keywords: linguistics, gloss, interlinear, align
;; Created: 1 August 2015

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; These functions handle the alignment of interlinear glosses.  One
;; user-facing command is provided:
;;
;;    M-x align-gloss    Align glossed text within the region.
;;
;; It is assumed that glosses follow certain conventions:
;;
;;   - Word boundaries are indicated by spaces.
;;
;;   - Clitic boundaries are indicated by an equals sign preceded
;;     or followed by a space.
;;
;;   - Affix boundaries are indicated by a hyphen preceded or followed
;;     by a space.
;;
;;   - Morphemes and their glosses do not begin or end with non-word
;;     characters.
;;
;;   - Full lines of material that should be left un-aligned (such as
;;     a free translation) are surrounded by double quotation marks.
;;
;; No assumptions are made about the number of levels of glossing provided,
;; or about what sort of detail is provided on what level.

;;; Code:


(require 'align)

;; Setting up alignment rules

(defun agl/make-morph-break-rule (pair)
  "Set up an alignment rule for a morpheme break. Takes a pair
   consisting of a rule name and a regular expression, returns a full
   rule statement."
  (let ((rule-name (car pair))
        (rule-regexp (cdr pair)))
    `(,rule-name
       (regexp   . ,rule-regexp) 
       (tab-stop .  nil)
;; This was formerly used to tell the aligner what to ignore. Including these in the
;; separator regexp SEEMS to have the same effect, making this redundant.
;       (valid    .  (lambda ()
;                      (save-excursion
;                        (goto-char (match-beginning 1))
;                        (beginning-of-line)
;                        (not (looking-at "\\(\\s-*\"\\|\\s-*\\\\glft\\)")))))
       (repeat   .  1))))

(setq agl/alignment-rules
      `(,@(mapcar 'agl/make-morph-break-rule
		  '((prefix-boundary    . "\\b-\\(\\s-*\\)\\b")
		    (suffix-boundary    . "\\b\\(\\s-*\\)-\\b")
		    (proclitic-boundary . "\\b=\\(\\s-*\\)\\b")
		    (enclitic-boundary  . "\\b\\(\\s-*\\)=\\b") 
		    (word-boundary      . "\\b\\(\\s-+\\)\\b")))
	(example-number
	 (regexp   . "^\\(?:([0-9]+)\\s-\\)?\\(\\s-*\\)")
	 (tab-stop . nil)
	 (spacing  . 0)
	 (repeat   . nil))
	(expex-suffix
	 (regexp   . "\\(\\s-*\\)//")
	 (tab-stop . nil)
	 (spacing  . 1)
	 (repeat   . nil))
		 
	))

(setq agl/separator-regexp "^\\s-*$\\|\\s-*\"\\|\\s-*\\\\glft")

;; Aligning glossed text

(defun agl/align-gloss (start end)
  "Align interlinear glossed text within the region."
  (interactive "r")
  (save-excursion
    (let ((start-marker (copy-marker start nil))
          (end-marker   (copy-marker end   t)))
      (agl/buffer-fixed-point (agl/try-align-gloss start-marker end-marker))
      (set-marker start-marker nil)
      (set-marker end-marker nil))))

(defun agl/try-align-gloss (start end)
  "Apply all of the alignment rules for interlinear glossed text
  exactly once to the region."
  (interactive "r")
    (align-region start end agl/separator-regexp agl/alignment-rules nil))

(defmacro agl/buffer-fixed-point (&rest body)
  "Repeat a series of commands until they no longer have any
  additional effect on the buffer."
  `(cl-labels ((do (tick)
		   ,@body
		   (unless (= tick (buffer-chars-modified-tick))
		     (do (buffer-chars-modified-tick)))))
     (do (buffer-chars-modified-tick))))
	   
;;; align-gloss.el ends here


(1) katinwiloh
    k-   at-  inw- il  -oh
    inc- b2s- a1s- see -ss
    "asd- fasdasdfasdfasdff"
\ex\begingl
\gla katinwiloh		    ri	jun  //
\glb k-	  at-  inw- il	-oh ri	jun  //
\glb inc- b2s- a1s- see -ss asd asdf //
\glft asdfasdf asdf asdf //
\endgl
