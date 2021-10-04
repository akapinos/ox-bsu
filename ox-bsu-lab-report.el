(require 'ox-latex)

(add-to-list 'org-latex-classes
	     '("labreport"
	       "\\documentclass{labreport}
                [NO-DEFAULT-PACKAGES]
                [PACKAGES]
                [EXTRA]"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(org-export-define-derived-backend 'bsu-lab-report 'latex
  :options-alist
  '((:faculty "FACULTY" nil "Факультет Радиофизики и Компьютерных Технологий")
    (:department "DEPARTMENT" nil "Кафедра физики и аэрокосмических технологий")
    (:labnum "LABNUM" nil "1")
    (:year "YEAR" nil "3")
    (:group "GROUP" nil "8")
    (:instructor "INSTRUCTOR" nil "" t))
  :translate-alist '((template . bsu-lab-report-template))
  :menu-entry
  '(?b "Export with BSU Lab Report"
       ((?L "As LaTeX buffer" bsu-lab-report-export-as-latex)
	(?l "As LaTeX file" bsu-lab-report-export-to-latex)
	(?p "As PDF file" bsu-lab-report-export-to-pdf)
	(?o "As PDF file and open"
	    (lambda (a s v b)
	      (if a (bsu-lab-report-export-to pdf t s v b)
		(org-open-file (bsu-lab-report-export-to-pdf nil s v b))))))))

(defun bsu-lab-report-template (contents info)
  "Return complete document string after LaTeX conversion.
CONTENTS is the transcoded string. INFO is a plist
holding export options."
  (let ((title (org-export-data (plist-get info :title) info)))
    (concat
     ;; Time-stamp.
     (and (plist-get info :time-stamp-file)
	  (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
     ;; LaTeX compiler.
     (org-latex--insert-compiler info)
     ;; Document class and packages.
     (let* ((class (plist-get info :latex-class))
	    (class-options (plist-get info :latex-class-options))
	    (header (nth 1 (assoc class (plist-get info :latex-classes))))
	    (document-class-string
	     (and (stringp header)
		  (if (not class-options) header
		    (replace-regexp-in-string
		     "^[ \t]*\\\\documentclass\\(\\(\\[[^]]*\\]\\)?\\)"
		     class-options header t nil 1)))))
       (if (not document-class-string)
	   (user-error "Unknown LaTeX class `%s'" class)
	 (org-latex-guess-babel-language
	  (org-latex-guess-inputenc
	   (org-element-normalize-string
	    (org-splice-latex-header
	     document-class-string
	     org-latex-default-packages-alist
	     org-latex-packages-alist nil
	     (concat (org-element-normalize-string
		      (plist-get info :latex-header))
		     (plist-get info :latex-header-extra)))))
	  info)))
     ;; Author.
     (let ((author (and (plist-get info :with-author)
			(let ((auth (plist-get info :author)))
			  (and auth (org-export-data auth info)))))
	   (email (and (plist-get info :with-email)
		       (org-export-data (plist-get info :email) info))))
       (cond ((and author email (not (string= "" email)))
	      (format "\\author{%s\\thanks{%s}}\n" author email))
	     ((or author email) (format "\\author{%s}\n" (or author email)))))
     ;; Date.
     (let ((date (and (plist-get info :with-date) (org-export-get-date info))))
       (format "\\date{%s}\n" (org-export-data date info)))
     ;; Title and subtitle.
     (concat
      (format "\\title{%s}\n" title))
     ;; Hyperref options.
     (let ((template (plist-get info :latex-hyperref-template)))
       (and (stringp template)
	    (format-spec template (org-latex--format-spec info))))
     ;; Faculty.
     (let ((faculty (plist-get info :faculty)))
       (format "\\faculty{%s}\n" (org-export-data faculty info)))
     ;; Department
     (let ((department (plist-get info :department)))
       (format "\\department{%s}\n" (org-export-data department info)))
     ;; Lab report number,
     (let ((labnum (plist-get info :labnum)))
       (format "\\labnum{%s}\n" (org-export-data labnum info)))
     ;; Student year.
     (let ((year (plist-get info :year)))
       (format "\\studentyear{%s}\n" (org-export-data year info)))
     ;; Student group.
     (let ((group (plist-get info :group)))
       (format "\\studentgroup{%s}\n" (org-export-data group info)))
     ;; Instructor name.
     (let ((instructor (plist-get info :instructor)))
       (format "\\instructor{%s}\n\n" (org-export-data instructor info)))
     ;; Document start
     "\\begin{document}\n\n"
     ;; Title command.
     (org-element-normalize-string
      (cond ((not (plist-get info :with-title)) nil)
	    ((string= "" title) nil)
	    ((not (stringp org-latex-title-command)) nil)
	    ((string-match "\\(?:[^%]\\|^\\)%s"
			   org-latex-title-command)
	     (format org-latex-title-command title))
	    (t org-latex-title-command)))
     ;; Document's body.
     contents
     ;; Document end.
     "\\end{document}")))

;;;###autoload
(defun bsu-lab-report-export-as-latex
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a LaTeX buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org BSU Lab Report Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'bsu-lab-report "*Org BSU Lab Report Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (LaTeX-mode))))

;;;###autoload
(defun bsu-lab-report-export-to-latex
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a LaTeX file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'bsu-lab-report outfile
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun bsu-lab-report-export-to-pdf
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to LaTeX then process through to PDF.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'bsu-lab-report outfile
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-latex-compile file)))))

(provide 'ox-bsu-lab-report)
