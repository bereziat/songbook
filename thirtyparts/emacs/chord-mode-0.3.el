;;; chord-mode.el --- major mode for chord files

;; Copyright 2013 Keith WACLENA

;; Author: Keith WACLENA <http://www.lib.uchicago.edu/keith/>
;; Version: 0.3
;; Keywords: music

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Major mode for editing "chordpro" files, with assists for
;; converting two-line notation e.g.:
;;
;;   Bbdim             Gvii   D             G
;;   Wenn meine Mutter selber wuﬂte all das vor mir
;;
;; to chordpro notation:
;;
;;   [Bbdim]Wenn meine Mutter [Gvii]selber [D]wuﬂte all das [G]vor mir
;;
;; support for running chordii(1) under docview, etc.
;;
;; Add to your init file:
;; (autoload 'chord-mode "chord-mode" "Major mode for editing chord files" t)
;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'doc-view)
(require 'picture)

(defconst chord-mode-version "0" "Current version of `chord-mode'.")

;;; -- User Options --------------------------------------------------------

(defvar chord-default-strings '(G C E A) ;ukulele
  "*Default strings (low to high).")

(defvar chord-mode-tablature-line-width 28
  "*Width of hyphens in a tablature line.")

(defvar chordii "chordii"
  "*Name of chordii program for displaying typeset song sheets.")

(defvar chord-chord-program "chord"
  "*Name of chord program for computing fretboard diagrams.")

(defconst chord-default-trivial-chords
  (list 'chord-default-trivial-chords-function)
  "Default set of \"trivial\" chords -- those that won't trigger a warning if not defined.
This is a list of predicate functions, strings (chord name suffixes, sans the root) or
lists of such.")

(defvar chord-trivial-chords chord-default-trivial-chords
  "*Definitiona of \"trivial\" chords -- those that won't trigger a warning if not defined.")

(defvar chord-warn-about-missing-defs t
  "*After saving file, warn about any non-trivial chords that aren't defined.")

;;; -- CHORD-MODE Functions -----------------------------------------------------------

(defun system (cmd &optional stderr errorout)
  "If CMD is a string, run as a shell command under your shell
and return stdout as string; else if CMD is a non-null list,
run (car CMD) directly with CMD as argv (i.e., no shell), again
returning stdout as string; elements of CMD may be strings or
symbols (if symbols, the symbol-name is used). Optional arg
STDERR says what to do with standard error: STDERR may be
nil (discard standard error output), the default, t (mix it with
ordinary output), or a file name string.  In both versions, we
strip a final newline as usual."
  (let ((buf (get-buffer-create " *tmp*"))
	(stderr (and stderr t))
	status)
    (save-excursion
      (unwind-protect
	  (progn
	    (setq status (typecase cmd
			   (cons (progn
				   (apply (function call-process) (prin1-to-string (car cmd) t) nil
					  (list buf stderr) nil
					  (mapcar (lambda (a) (prin1-to-string a t)) (cdr cmd)))))
			   (string (call-process "sh" nil buf nil "-c" cmd))))
	    (set-buffer buf)
	    (goto-char (1- (point-max)))
	    (let ((result (buffer-substring (point-min)
					    (if (looking-at "\n") (point) (point-max)))))
	      (if (and errorout (or (eq 'string (type-of status)) (not (= status 0))))
		  (error (if (eq 'string (type-of status))
			     status
			   (if (string= "" result) (format "exit %d" status) result)))
		result)))
	(set-buffer buf)
	(set-buffer-modified-p nil)
	(kill-buffer buf)))))

(defun kw-join (list separator)
  "Join a LIST to a string where the list elements are separated by SEPARATOR.
The inverse function of `kw-split'.

This function is stolen from bbdb, where it's called
bbdb-join; this is too fundamental a function to justify
the dependency."
  (when list
    (mapconcat 'identity list separator)))

(defun chord-chordify (n)
  "Convert this form of notation:

  Bbdim             Gvii   D             G
  Wenn meine Mutter selber wuﬂte all das vor mir

to this (ChordPro format):

  [Bbdim]Wenn meine Mutter [Gvii]selber [D]wuﬂte all das [G]vor mir

one chord at a time.  Bind it to an easy keystroke,
then execute repeatedly on the \"chord line\"."

  ;; change so that we start it on a non-chord line!

  (interactive "p")
  (require 'picture)
  (cl-flet ((previous-line-is-blank ()
				 (save-excursion
				   (forward-line -1)
				   (looking-at "^\\s-*$"))))
    (dotimes (_ n)
      (end-of-line 1)
      (delete-horizontal-space)
      (let ((here (point)) chord)
	(backward-sexp)
	(setq chord (delete-and-extract-region here (point)))
	(picture-move-down 1)
	(insert "[" chord "]")
	(picture-move-up 1)
	(when (save-excursion (beginning-of-line 1) (looking-at "^\\s-*$"))
	  (delete-blank-lines)
	  (forward-line))))))

(defun chord-parse-define (str)
  (save-match-data
    (when (string-match "^{define:\\s-*\\(.*?\\)\\s-*}" str)
      (let ((guts (match-string-no-properties 1 str))
            chord bf frets)
        (if (string-match "\\(\\S-+\\)\\s-+base-fret\\s-+\\([[:digit:]]+\\)\\s-+frets\\s-+\\(.*\\)" guts)
            (progn
              (setq chord (match-string-no-properties 1 guts)
                    bf    (match-string-no-properties 2 guts)
                    frets (split-string (match-string-no-properties 3 guts) "\\s-+" t))
              (list chord bf frets))
          (if (string-match "\\(\\S-+\\)\\s-+\\(.*\\)" guts)
              (progn
                (setq chord (match-string-no-properties 1 guts)
                      frets (split-string (match-string-no-properties 2 guts) "\\s-+" t))
                (setq bf (car frets))
                (list chord bf (cdr frets)))
            nil))))))

(defun chord-render-define (chord &optional n)
  (let* ((name  (elt chord 0))
         (bf    (elt chord 1))
         (frets (elt chord 2))
         (frets (reverse
                 (mapcar (lambda (f) (or f "x"))
                         (subseq (reverse frets) 0 (or n (length frets)))))))
    (format "{define: %s base-fret %s frets %s}" name bf (mapconcat 'identity frets " "))))

;; this was quite elegant before I discovered the need for chord-mode-uke-hack!
(defun chord-run-chordii ()
  "Run chordii on this buffer, displaying the result in another buffer."
  (interactive)
  (unless (condition-case nil (system `(,chordii -V) nil t) (error nil))
    (error "Can't exec %s" chordii))
  (let ((bname (buffer-name))
	(text (buffer-string))
	(tmpfile (make-temp-file "chord-mode")))
    (save-some-buffers nil `(lambda () (eq ,(current-buffer) (current-buffer))))
    (unwind-protect
	(progn
	  (let ((file (if chord-mode-uke-hack
			  (with-temp-file tmpfile
			    (insert text)
			    (goto-char (point-min))
			    (while (re-search-forward "{define:[^}]*}" nil t)
			      (replace-match
                               (chord-render-define
                                (chord-parse-define (match-string-no-properties 0))
                                6)
                               t t))
			    tmpfile)
			(buffer-file-name))))
	    (switch-to-buffer (format "%s CHORDII" bname))
	    (doc-view-kill-proc)
            (let ((inhibit-read-only t))
              (delete-region (point-min) (point-max))
              (insert (system `(,chordii ,file))))
	    (doc-view-mode)))
      (delete-file tmpfile))))

(defun chord-insert-tablature (&optional strings width)
  "Insert tablature block."
  (interactive)
  (setq strings (mapcar (lambda (s) (elt (upcase (prin1-to-string s t)) 0)) (or strings chord-default-strings)))
  (setq width (or width chord-mode-tablature-line-width))
  (insert "{sot}\n")
  (dolist (note (reverse strings))
    (insert (format "%c |%s\n" note (concat (make-list width ?-)))))
  (insert "{eot}\n"))

(defun insert-chord (chord)
  "For Chord Pro."
  (interactive "sChord: ")
  (insert (format "[%s]" (concat (capitalize (substring chord 0 1)) (substring chord 1)))))

(defun chord-next-chord-position ()
  "Move cursor to next chord position."
  (interactive)
  (let (col)
    (save-excursion
      (let ((col (current-column)))
	(forward-line)
	(move-to-column col t))
      (unless (eolp)
	(if (not (bolp)) (forward-word))
	(unless (eolp)
	  (unless (looking-at "\\<")
	    (re-search-forward "\\<" nil t))))
      (setq col (current-column)))
    (move-to-column col t)))

(defun chord-goto-defines ()
  "Goto the beginning of the {define:}'s and return point; if none, return nil."
  (let (defs)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "{define:" nil t)
	(progn
	  (beginning-of-line)
	  (setq defs (point)))))
    (if defs
      (goto-char defs))))

(defun chord-align-defs ()
  "Implement me!")

(defun chord-sort-defs ()
  "Sort all the {define:}'s in the file."
  (save-excursion
    (when (chord-goto-defines)
      (let ((top (point)))
	(goto-char (point-max))
	(re-search-backward "{define:")	;assert: must succeed
	(unless (= top (point))		;only one define: already sorted
	  (end-of-line)
	  (forward-char)
	  (sort-regexp-fields nil "^ *{define: +\\([^ ]+\\).*$" "\\1" top (point)))))))

(defun chord-define-chord (chord base frets)
  "Insert a chord definition amongst the definitions at the top of the file,
sorting them all by chord name.  Chords can be entered as space separated fret
numbers, or, as a shortcut, a single string of digits, which is taken as a
string of single-digit fret numbers, e.g. 0201 = 0 2 0 1."
  (interactive
   (list (read-string "Chord: " (thing-at-point 'symbol))
	 (read-number "Base fret: " 1)
	 (read-string "Frets: ")))
  (cl-flet ((parse ()
		(kw-join
		 (let ((splits (split-string frets)))
		   (when (= 1 (length splits))
		     (setq splits (split-string (car splits) "" t)))
		   (append (make-list (max 0 (- 6 (length splits))) "-") splits))
		 " ")))
    (let ((found-brace nil) def)
      (save-excursion
	(goto-char (point-min))
	;; move to beginning of defines
	(if (re-search-forward "{define:" nil t)
	    (beginning-of-line)
	  (while (re-search-forward "{\\(title\\|st\\):" nil t)
	    (setq found-brace t))
	  (if found-brace
	      (progn
		(end-of-line)
		(forward-char))))
	(setq def (format "{define: %s base-fret %d frets %s}" chord base (parse)))
	(insert def ?\n)
	(chord-align-defs)
	(chord-sort-defs)
	(message def)))))

(defun chord-song-title ()
  (save-match-data
    (goto-char (point-min))
    (if (re-search-forward "{title: +\\(.*?\\)}" nil t)
	(match-string 1)
      "[Untitled]")))

(defun chord-gather-chords (beg end)
  "Return a list of the unique chords between BEG and END."
  (let (chords)
    (save-excursion
      (save-match-data
	(goto-char beg)
	(while (re-search-forward "\\[\\([^]]+\\)\\]" end t)
	  (setq chords (cons (substring-no-properties (match-string 1)) chords)))
        (remove-duplicates (sort chords 'string<) :test 'string=)))))

(defun chord-default-trivial-chords-function (name)
  "Function to match any M, m, sus4, m7 or M7 chord."
  (let ((roots (let (cs) (dotimes (i 7 cs) (push (+ ?A i) cs)))))
    (string-match-p (format "^[%s][b#]?\\([mM]?7?\\|sus4?\\)?$" (mapconcat (apply-partially 'format "%c") roots "")) name)))

(defun chord-gather-defined-chords ()
  "Return a list of all the chords defined in {define: ...} constructs in this buffer."
  (save-excursion
    (goto-char (point-min))
    (let (cs)
      (while (re-search-forward "^\\s-*{define:\\s-+\\(\\S-+\\)\\(\\s-+base-fret\\s-+\\([0-9]+\\)\\)?\\s-+frets\\s-+\\(.*\\)\\s-*}" nil t)
        (push (list (match-string-no-properties 1)
                    (match-string-no-properties 3)
                    (match-string-no-properties 4)) cs))
      (remove-duplicates (sort cs (lambda (c1 c2) (string< (car c1) (car c2)))) :test 'equal))))

(defun chord-gather-defined-chord-names ()
  (mapcar 'car (chord-gather-defined-chords)))

(defun kw-invert (p x)
  "Return the result of applying the inversion of predicate P to X i.e. (not (p x))."
  (not (funcall p x)))

(defun kw-disjunction (preds x)
  "Apply a predicate which is the disjunction of the predicates in PREDS to X."
  (pcase preds
    (`(,p . ,ps) (or (funcall p x) (kw-disjunction ps x)))
    (_ nil)))

(defun chord-remove-trivial-chords (chords)
  (when chord-warn-about-missing-defs
    (let* ((trivial-funcs (reduce
                           (lambda (acc c)
                             (if (functionp c)
                                 (cons c acc)
                               acc))
                           chord-trivial-chords
                           :initial-value nil))
           (trivial-chords (reduce
                            (lambda (acc c)
                              (cond
                               ((listp c)
                                (append c acc))
                               ((stringp c)
                                (cons c acc))
                               (t acc)))
                            chord-trivial-chords
                            :initial-value nil)))
      (filter
       (apply-partially
        'kw-invert
        (apply-partially 'kw-disjunction trivial-funcs))
      (cl-set-difference chords trivial-chords)))))

(defun chord-after-save-warnings ()
  (require 'cl)
  (when chord-warn-about-missing-defs
    (let ((missing (cl-set-difference
                    (chord-remove-trivial-chords (chord-gather-chords (point-min) (point-max)))
                    (chord-gather-defined-chord-names)
                    :test 'equal)))
      (when missing
        (message "Warning: %d chords missing definitions: %s" (length missing) missing)))))

(defun chord-display-fretboards ()
  "Pop up a window showing fretboards for all chords in song."
  (interactive)
  (let ((buf (get-buffer-create (format "*Chords for %s" (substring-no-properties (chord-song-title)))))
	chords                          ;all chords "used" in song
        defined                         ;all chords defined with {define}, as parsed triples, inc. bad base-frets
        defnames                        ;all chords defined with {define}, just the names
        defs                            ;chords defined with {define}, as parsed triples, W/O. bad base-frets
        cant)                           ;chords defined with {define}, just names, WITH bad base-frets
    (save-excursion
      (save-match-data
        (setq chords (chord-gather-chords (point-min) (point-max))
              defined (chord-gather-defined-chords))
	(goto-char (point-min))
	(while (re-search-forward "{ *\\(sot\\|start_of_tab\\) *} *\n\\([^|]+\\)\n" nil t)
	  (setq chords (append (split-string (substring-no-properties (match-string 2))) chords)))
        (dolist (c defined)
          (if (= 1 (string-to-number (elt c 1)))
              (progn
                (push c defs)
                (push (elt c 0) defnames))
            (push (elt c 0) cant)))
        (when cant
          (message "Can't handle base-fret > 1: %s" (mapconcat 'identity cant ", ")))
        (setq chords (cl-set-difference chords defnames :test 'string=))
	(set-buffer buf)
	(delete-region (point-min) (point-max))
	(apply 'call-process chord-chord-program nil buf nil "-f" (chord-remove-trivial-chords chords))
        (let ((from-chord (buffer-substring (point-min) (point-max))))
          (delete-region (point-min) (point-max))
          (mapc (lambda (x) (insert (mapconcat 'identity x "  ") "\n"))
                (chord-transpose (list (split-string from-chord "\n") (split-string (chord-fake-it defs) "\n"))))
          (goto-char (point-min))
          (display-buffer buf)
          (resize-temp-buffer-window))))))

(defun chord-transpose (list)
  (cl-labels ((transpose (acc m)
                       (if (or (null m) (null (car m)))
                           acc
                         (transpose (cons (mapcar 'car m) acc) (mapcar 'cdr m)))))
    (reverse (transpose nil list))))

(defun chord-fake-one (c)
  (let ((fs (mapconcat 'identity (filter (lambda (s) (not (string= s "-"))) (split-string (elt c 2))) ""))
        lines)
    (push (elt c 0) lines)
    (push (replace-regexp-in-string "[^0]" " " fs) lines)
    (push "====" lines)
    (dotimes (i 9)
      (push (replace-regexp-in-string (format "[^%d]" (+ i 1)) "|" fs) lines))
     (reverse lines)))

(defun chord-fake-it (defs)
  (let* ((lol (mapcar 'chord-fake-one defs))
         (width (apply 'max (length chord-default-strings) (mapcar (lambda (l) (length (car l))) lol)))
         (widened (mapcar (lambda (l) (mapcar (lambda (x) (format (format "%%%ds" width) x)) l)) lol))
         (trans (chord-transpose widened)))
    (mapconcat 'identity (mapcar (lambda (x) (mapconcat 'identity x " ")) trans) "\n")))

;;;###autoload
(define-derived-mode chord-mode text-mode "Chord"
  "Major mode for editing chord files.
\\{chord-mode-map}."
  (setq chord-mode-keywords
	'(("\\[.*?\\]" . font-lock-keyword-face)
	  ("{.*?}" . font-lock-builtin-face)
	  ("^#.*$" . font-lock-comment-face)))
  (modify-syntax-entry ?# "_")
  (setq font-lock-defaults '(chord-mode-keywords)
	truncate-lines t
	tab-always-indent nil
	indent-tabs-mode nil		;never insert tabs
	comment-start "# ")
  (auto-fill-mode 0)
  (add-hook 'after-save-hook 'chord-after-save-warnings t t)
  (defvar chord-mode-uke-hack t
    "Work around a bug in chordii by deleting all {define: ...}'s."))

(define-key chord-mode-map [f11] 'chord-chordify)
(define-key chord-mode-map (kbd "S-SPC") 'chord-next-chord-position)
(define-key chord-mode-map [(control c) (-)] 'chord-insert-tablature)
(define-key chord-mode-map [(control c) (control c)] 'chord-run-chordii)
(define-key chord-mode-map [(control c) (control f)] 'chord-display-fretboards)
(define-key chord-mode-map [(control c) (d)] 'chord-define-chord)

(provide 'chord-mode)

;;; chord-mode.el ends here
