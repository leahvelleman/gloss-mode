;;; align-gloss.el --- Align interlinear glossed text.

;;; Pre-release TODO:

;; Turn the list of alignment rules into a parameter.

;; Handle line-begin and line-end sequences:
;;   SIL codes (\ft etc)
;;   Plain text example numbers 
;;   \ex \gla \glb \glft
;;   Line-end markers like // in expex
;;   Line-initial comment characters? (e.g. ;; here...)

;; More thorough list of ignore-line indicators:
;;   SIL codes (\ref etc)
;;   \glft

;; Turn the above into parameters?

;; Put in a proper provide statement.

;; Handle aligning large regions in which there are multiple
;; independent gloss groups.

;; If no region is given, align the gloss group that the point is currently in.






;; Author: Leah Grace Velleman <leah.velleman@gmail.com>
;; Version: 0.1
;; Keywords: linguistics, gloss, interlinear, align
;; Created: 1 August 2015

;;; Commentary:

;; These functions handle the alignment of interlinear glosses,
;; commonly used in linguistics to indicate the structure of example
;; sentences like this one:
;;
;;    "Jas kawaaj kab'anoh?"
;;    jas  kaw=      aaj      ka=       b'anoh
;;    jas  k-   aw=  a    -aj k-   a=   b'an -oh
;;    what INC- A2s= want -SS INC- A2s= do   -SS
;;    "What do you want to do?"
;;
;;
;; One user-facing command is provided:
;;
;;    M-x align-gloss    Align glossed text within the region.
;;
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
;; These assumptions will hopefully be loosened in future versions.
;;
;; No assumptions are made about the number of levels of glossing provided,
;; or about what sort of detail is provided on what level.

;;; Code:

(require 'align)

;; Setting up alignment rules

(defun agl/make-gloss-alignment-rule (pair)
  "Set up an alignment rule. Takes a pair consisting of a rule name and a 
   regular expression, returns a full rule statement."
  (let ((rule-name (car pair))
        (rule-regexp (cdr pair)))
    `(,rule-name
       (regexp   . ,rule-regexp) 
       (tab-stop .  nil)
       (valid    .  (lambda ()
                      (save-excursion
                        (goto-char (match-beginning 1))
                        (beginning-of-line)
                        (not (looking-at "\"")))))
       (repeat   .  1))))

(setq agl/alignment-rules
      (mapcar 'agl/make-gloss-alignment-rule
              '((prefix-boundary    . "\\b-\\(\\s-*\\)\\b")
                (suffix-boundary    . "\\b\\(\\s-*\\)-\\b")
                (proclitic-boundary . "\\b=\\(\\s-*\\)\\b")
                (enclitic-boundary  . "\\b\\(\\s-*\\)=\\b") 
                (word-boundary      . "\\b\\(\\s-+\\)\\b"))))

;; Aligning glossed text

(defun agl/align-gloss (start end)
  "Align interlinear glossed text within the region (or in lines
  partly contained within the region)."
  (interactive "r")
  (agl/buffer-fixed-point
   (agl/try-align-gloss start end)))

(defun agl/try-align-gloss (start0 end0)
  "Apply all of the alignment rules for interlinear glossed text
  exactly once to the region (or lines partly contained within it)."
  (interactive "r")
  (save-excursion
    (let ((start (progn
		   (goto-char start0)
		   (beginning-of-line)
		   (point)))
	  (end (progn
		 (goto-char end0)
		 (end-of-line)
		 (point))))
    (align-region start end nil agl/alignment-rules nil))))

(defmacro agl/buffer-fixed-point (&rest body)
  "Repeat a series of commands until they no longer have any
  additional effect on the buffer."
  `(cl-labels ((do (tick)
		   ,@body
		   (unless (= tick (buffer-chars-modified-tick))
		     (do (buffer-chars-modified-tick)))))
     (do (buffer-chars-modified-tick))))
	   
;;; align-gloss.el ends here
