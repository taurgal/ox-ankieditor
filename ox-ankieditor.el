;;; ox-ankieditor.el --- HTML Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2018 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;;      Jambunathan K <kjambunathan at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, wp

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a HTML back-end for Org generic exporter.
;; See Org manual for more information.

;;; Code:

;;; Dependencies

(require 'cl-lib)
(require 'dash)
(require 'json)
(require 'org-element)
(require 'ox)
(require 'ox-html)
(require 'request)

(defconst anki-editor-prop-note-type "ANKI_NOTE_TYPE")
(defconst anki-editor-prop-failure-reason "ANKI_FAILURE_REASON")

(defgroup org-export-anki nil
  "Options for exporting Org mode files to HTML."
  :tag "Org Export HTML"
  :group 'org-export)


(defcustom org-anki-checkbox-type 'unicode
  "The type of checkboxes to use for HTML export.
See `org-html-checkbox-types' for for the values used for each
option."
  :group 'org-export-anki
  :type '(choice
	      (const :tag "ASCII characters" ascii)
	      (const :tag "Unicode characters" unicode)
	      (const :tag "HTML checkboxes" html)))

(defun anki-editor--set-note-id (id errmsg action note)
  (unless (or id errmsg)
    (error "Note creation failed for unknown reason"))
  (and id (push `(note-id . ,id) note))
  (and errmsg (push `(reason . ,errmsg) note)))

(defun anki-editor--update-note (note)
  "Request AnkiConnect for updating fields and tags of NOTE."
  (let ((queue (anki-editor--anki-connect-invoke-queue)))
    (funcall queue
             note
             'updateNoteFields
             `((note . ,(anki-editor--anki-connect-map-note note))))
    (funcall queue
             note
             'notesInfo
             `((notes . (,(alist-get 'note-id note))))
             (lambda (result action)
               ;; update tags
               (let* ((existing-note (car result))
                      (tags-to-add (-difference (-difference (alist-get 'tags note)
                                                             (alist-get 'tags existing-note))
                                                anki-editor-ignored-org-tags))
                      (tags-to-remove (-difference (-difference (alist-get 'tags existing-note)
                                                                (alist-get 'tags note))
                                                   anki-editor-protected-tags))
                      (tag-queue (anki-editor--anki-connect-invoke-queue)))

                 (when tags-to-add
                   (funcall tag-queue
                            note
                            'addTags `((notes . (,(alist-get 'note-id note)))
                                       (tags . ,(mapconcat #'identity tags-to-add " ")))))

                 (when tags-to-remove
                   (funcall tag-queue
                            note
                            'removeTags `((notes . (,(alist-get 'note-id note)))
                                          (tags . ,(mapconcat #'identity tags-to-remove " ")))))

                 (funcall tag-queue note))))

    (funcall queue note)))

(defun anki-editor-maybe-call-handler (result-action-note-list handler)
  (let* ((result-and-error (car result-action-note-list))
         (result (alist-get 'result result-and-error))
         (errmsg (alist-get 'error result-and-error))
         (action (cadr result-action-note-list))
         (note (caddr result-action-note-list)))
    (or errmsg (push '(status . success) note))
    (and errmsg
         (push '(status . failed) note)
         (push `(reason . ,errmsg) note)
         (error errmsg))
    (and handler (funcall handler result errmsg action note))
    ))

(defun anki-editor--anki-connect-invoke-multi (&rest actions)
  (let* ((results-list (anki-editor--anki-connect-invoke-result
                    "multi" `((actions . ,(mapcar #'car actions)))))
         (result-action-note-list
          (-zip-pair results-list
                     (mapcar #'car actions)
                     (mapcar (lambda (x) (alist-get 'note (cdr x))) actions)
                     ))
         (handler-list (mapcar (lambda (x) (alist-get 'handler (cdr x))) actions) ))
    (-zip-with
     #'anki-editor-maybe-call-handler
     result-action-note-list
     handler-list
     )))

(defun anki-editor--anki-connect-invoke-queue ()
  (let (action-queue)
    (lambda (note &optional action params handler)
      (if action
          (push (cons (anki-editor--anki-connect-action action params 5)
                      `((handler . ,handler)(note . ,note))) action-queue)
        (when action-queue
          (apply #'anki-editor--anki-connect-invoke-multi (nreverse action-queue))
          (setq action-queue nil))))))


(defun anki-editor--create-note (note)
  "Request AnkiConnect for creating NOTE."
  (let ((queue (anki-editor--anki-connect-invoke-queue)))
    (when anki-editor-create-decks
      (funcall queue
               note
               'createDeck
               `((deck . ,(alist-get 'deck note)))))
    (funcall queue
             note
             'addNote
             `((note . ,(anki-editor--anki-connect-map-note note)))
             #'anki-editor--set-note-id)
    (funcall queue note)))

(defgroup org-export-anki nil
  "Options for exporting Org mode files to HTML."
  :tag "Org Export HTML"
  :group 'org-export)



(defcustom org-anki-container-element "div"
  "HTML element to use for wrapping top level sections.
Can be set with the in-buffer HTML_CONTAINER property or for
publishing, with :html-container."
  :group 'org-export-anki
  :type 'string)

(defun org-anki-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  contents)

(defun org-anki-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat "~BEGINNING~\n" contents "\n~END~"))

(defconst org-anki-doctype-alist
  '(("html4-strict" . "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\"
\"http://www.w3.org/TR/html4/strict.dtd\">")
    ("html4-transitional" . "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
\"http://www.w3.org/TR/html4/loose.dtd\">")
    ("html4-frameset" . "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\"
\"http://www.w3.org/TR/html4/frameset.dtd\">")

    ("xhtml-strict" . "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
    ("xhtml-transitional" . "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
    ("xhtml-frameset" . "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">")
    ("xhtml-11" . "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml11.dtd\">")

    ("html5" . "<!DOCTYPE html>")
    ("xhtml5" . "<!DOCTYPE html>"))
  "An alist mapping (x)html flavors to specific doctypes.")

(defcustom org-anki-container-element "div"
  "HTML element to use for wrapping top level sections.
Can be set with the in-buffer HTML_CONTAINER property or for
publishing, with :html-container.

Note that changing the default will prevent you from using
org-info.js for your website."
  :group 'org-export-anki
  :type 'string)

(defcustom org-anki-html5-fancy nil
  "Non-nil means using new HTML5 elements.
This variable is ignored for anything other than HTML5 export.

For compatibility with Internet Explorer, it's probably a good
idea to download some form of the html5shiv (for instance
https://code.google.com/p/html5shiv/) and add it to your
HTML_HEAD_EXTRA, so that your pages don't break for users of IE
versions 8 and below."
  :group 'org-export-anki
  :type 'boolean)

(defcustom org-anki-link-use-abs-url nil
  "Should we prepend relative links with HTML_LINK_HOME?"
  :group 'org-export-anki
  :type 'boolean)

(defcustom org-anki-link-home ""
  "Where should the \"HOME\" link of exported HTML pages lead?"
  :group 'org-export-anki
  :type '(string :tag "File or URL"))

(defcustom org-anki-allow-name-attribute-in-anchors nil
  "When nil, do not set \"name\" attribute in anchors.
By default, when appropriate, anchors are formatted with \"id\"
but without \"name\" attribute."
  :group 'org-export-anki
  :type 'boolean)

(defcustom org-anki-divs
  '((preamble  "div" "preamble")
    (content   "div" "content")
    (postamble "div" "postamble"))
  "Alist of the three section elements for HTML export.
The car of each entry is one of `preamble', `content' or `postamble'.
The cdrs of each entry are the ELEMENT_TYPE and ID for each
section of the exported document.

Note that changing the default will prevent you from using
org-info.js for your website."
  :group 'org-export-anki
  :type '(list :greedy t
	       (list :tag "Preamble"
		     (const :format "" preamble)
		     (string :tag "element") (string :tag "     id"))
	       (list :tag "Content"
		     (const :format "" content)
		     (string :tag "element") (string :tag "     id"))
	       (list :tag "Postamble" (const :format "" postamble)
		     (string :tag "     id") (string :tag "element"))))

(defconst org-anki-checkbox-types
  '((unicode .
     ((on . "&#x2611;") (off . "&#x2610;") (trans . "&#x2610;")))
    (ascii .
     ((on . "<code>[X]</code>")
      (off . "<code>[&#xa0;]</code>")
      (trans . "<code>[-]</code>")))
    (html .
	  ((on . "<input type='checkbox' checked='checked' />")
	  (off . "<input type='checkbox' />")
	  (trans . "<input type='checkbox' />"))))
  "Alist of checkbox types.
The cdr of each entry is an alist list three checkbox types for
HTML export: `on', `off' and `trans'.

The choices are:
  `unicode' Unicode characters (HTML entities)
  `ascii'   ASCII characters
  `html'    HTML checkboxes

Note that only the ascii characters implement tri-state
checkboxes. The other two use the `off' checkbox for `trans'.")

(defcustom org-anki-extension "html"
  "The extension for exported HTML files."
  :group 'org-export-anki
  :type 'string)

(defcustom org-anki-footnote-format "<sup>%s</sup>"
  "The format for the footnote reference.
%s will be replaced by the footnote reference itself."
  :group 'org-export-anki
  :type 'string)

(defcustom org-anki-footnote-separator "<sup>, </sup>"
  "Text used to separate footnotes."
  :group 'org-export-anki
  :type 'string)

(defcustom org-anki-footnotes-section "<div id=\"footnotes\">
<h2 class=\"footnotes\">%s: </h2>
<div id=\"text-footnotes\">
%s
</div>
</div>"
  "Format for the footnotes section.
Should contain a two instances of %s.  The first will be replaced with the
language-specific word for \"Footnotes\", the second one will be replaced
by the footnotes themselves."
  :group 'org-export-anki
  :type 'string)

(defcustom org-anki-format-drawer-function (lambda (_name contents) contents)
  "Function called to format a drawer in HTML code.

The function must accept two parameters:
  NAME      the drawer name, like \"LOGBOOK\"
  CONTENTS  the contents of the drawer.

The function should return the string to be exported.

For example, the variable could be set to the following function
in order to mimic default behavior:

The default value simply returns the value of CONTENTS."
  :group 'org-export-anki
  :type 'function)

(defcustom org-anki-format-headline-function
  'org-html-format-headline-default-function
  "Function to format headline text.

This function will be called with six arguments:
TODO      the todo keyword (string or nil).
TODO-TYPE the type of todo (symbol: `todo', `done', nil)
PRIORITY  the priority of the headline (integer or nil)
TEXT      the main headline text (string).
TAGS      the tags (string or nil).
INFO      the export options (plist).

The function result will be used in the section format string."
  :group 'org-export-anki
  :type 'function)

(defcustom org-anki-format-inlinetask-function
  'org-html-format-inlinetask-default-function
  "Function called to format an inlinetask in HTML code.

The function must accept seven parameters:
  TODO      the todo keyword, as a string
  TODO-TYPE the todo type, a symbol among `todo', `done' and nil.
  PRIORITY  the inlinetask priority, as a string
  NAME      the inlinetask name, as a string.
  TAGS      the inlinetask tags, as a list of strings.
  CONTENTS  the contents of the inlinetask, as a string.
  INFO      the export options, as a plist

The function should return the string to be exported."
  :group 'org-export-anki
  :type 'function)

(defcustom org-anki-indent nil
  "Non-nil means to indent the generated HTML.
Warning: non-nil may break indentation of source code blocks."
  :group 'org-export-anki
  :type 'boolean)

(defcustom org-anki-inline-image-rules
  '(("file" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'")
    ("http" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'")
    ("https" . "\\.\\(jpeg\\|jpg\\|png\\|gif\\|svg\\)\\'"))
  "Rules characterizing image files that can be inlined into HTML.
A rule consists in an association whose key is the type of link
to consider, and value is a regexp that will be matched against
link's path."
  :group 'org-export-anki
  :type '(alist :key-type (string :tag "Type")
		:value-type (regexp :tag "Path")))

(defcustom org-anki-table-caption-above t
  "When non-nil, place caption string at the beginning of the table.
Otherwise, place it near the end."
  :group 'org-export-anki
  :type 'boolean)

(defcustom org-anki-with-latex org-export-with-latex
  "Non-nil means process LaTeX math snippets.

When set, the exporter will process LaTeX environments and
fragments.

This option can also be set with the +OPTIONS line,
e.g. \"tex:mathjax\".  Allowed values are:

  nil           Ignore math snippets.
  `verbatim'    Keep everything in verbatim
  `mathjax', t  Do MathJax preprocessing and arrange for MathJax.js to
                be loaded.
  SYMBOL        Any symbol defined in `org-preview-latex-process-alist',
                e.g., `dvipng'."
  :group 'org-export-anki
  :type '(choice
	  (const :tag "Do not process math in any way" nil)
	  (const :tag "Leave math verbatim" verbatim)
	  (const :tag "Use MathJax to display math" mathjax)
	  (symbol :tag "Convert to image to display math" :value dvipng)))


(defun org-anki-note-from-headline (headline contents info)
  "Construct an alist representing a note from current entry."
  (goto-char (org-element-property :begin headline))
  (let ((org-trust-scanner-tags t)
        (deck (org-entry-get-with-inheritance anki-editor-prop-deck))
        (note-id (org-entry-get nil anki-editor-prop-note-id))
        (note-type (org-entry-get nil anki-editor-prop-note-type))
        (tags (anki-editor--get-tags))
        (fields (anki-editor--build-fields))
        (custom-id (org-entry-get nil "CUSTOM_ID"))
        (begin (org-element-property :begin headline)))

    (unless deck (error "No deck specified"))
    (unless note-type (error "Missing note type"))
    (unless fields (error "Missing fields"))

    `((deck . ,deck)
      (note-id . ,(string-to-number (or note-id "-1")))
      (note-type . ,note-type)
      (tags . ,tags)
      (fields . ,fields)
      (custom-id . ,custom-id)
      (begin . ,begin))))


(defun org-anki-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (if (org-element-property (intern (concat ":" anki-editor-prop-note-type)) headline)
      (let ((note (org-anki-note-from-headline headline contents info)))
          (condition-case err
              (progn
                (anki-editor--push-note note)
                (push '(status . success) note)
                (cl-incf org-anki-current-note-number)
                )
                (error
                 (push '(status . failed) note)
                 (push `(reason . ,(error-message-string err)) note)))
          (push note org-anki-note-status-list)
          ))
  (unless (org-element-property :footnote-section-p headline)
    (let* ((numberedp (org-export-numbered-headline-p headline info))
           (numbers (org-export-get-headline-number headline info))
           (level (+ (org-export-get-relative-level headline info)
                     (1- (plist-get info :html-toplevel-hlevel))))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword headline)))
                        (and todo (org-export-data todo info)))))
           (todo-type (and todo (org-element-property :todo-type headline)))
           (priority (and (plist-get info :with-priority)
                          (org-element-property :priority headline)))
           (text (org-export-data (org-element-property :title headline) info))
           (tags (and (plist-get info :with-tags)
                      (org-export-get-tags headline info)))
           (full-text (funcall (plist-get info :html-format-headline-function)
                               todo todo-type priority text tags info))
           (contents (or contents ""))
	   (ids (delq nil
                      (list (org-element-property :CUSTOM_ID headline)
                            (org-export-get-reference headline info)
                            (org-element-property :ID headline))))
           (preferred-id (car ids))
           (extra-ids
	    (mapconcat
	     (lambda (id)
	       (org-html--anchor
		(if (org-uuidgen-p id) (concat "ID-" id) id)
		nil nil info))
	     (cdr ids) "")))
      (if (org-export-low-level-p headline info)
          ;; This is a deep sub-tree: export it as a list item.
          (let* ((html-type (if numberedp "ol" "ul")))
	    (concat
	     (and (org-export-first-sibling-p headline info)
		  (apply #'format "<%s class=\"org-%s\">\n"
			 (make-list 2 html-type)))
	     (org-html-format-list-item
                   contents (if numberedp 'ordered 'unordered)
		   nil info nil
                   (concat (org-html--anchor preferred-id nil nil info)
                           extra-ids
                           full-text)) "\n"
	     (and (org-export-last-sibling-p headline info)
		  (format "</%s>\n" html-type))))
	;; Standard headline.  Export it as a section.
        (let ((extra-class (org-element-property :HTML_CONTAINER_CLASS headline))
              (first-content (car (org-element-contents headline))))
          (format "<%s id=\"%s\" class=\"%s\">%s%s</%s>\n"
                  (org-html--container headline info)
                  (concat "outline-container-"
			  (org-export-get-reference headline info))
                  (concat (format "outline-%d" level)
                          (and extra-class " ")
                          extra-class)
                  (format "\n<h%d id=\"%s\">%s%s</h%d>\n"
                          level
                          preferred-id
                          extra-ids
                          (concat
                           (and numberedp
                                (format
                                 "<span class=\"section-number-%d\">%s</span> "
                                 level
                                 (mapconcat #'number-to-string numbers ".")))
                           full-text)
                          level)
                  ;; When there is no section, pretend there is an
                  ;; empty one to get the correct <div
                  ;; class="outline-...> which is needed by
                  ;; `org-info.js'.
                  (if (eq (org-element-type first-content) 'section) contents
                    (concat (org-html-section first-content "" info) contents))
                  (org-html--container headline info)))))))


(defun anki-editor-map-note-entries (func &optional match scope &rest skip)
  "Simple wrapper that calls `org-map-entries' with
  `&ANKI_NOTE_TYPE<>\"\"' appended to MATCH."
  ;; disable property inheritance temporarily, or all subheadings of a
  ;; note heading will be counted as note headings as well
  (let ((org-use-property-inheritance nil))
    (org-map-entries func (concat match "&" anki-editor-prop-note-type "<>\"\"") scope skip)))


(defun anki-editor--clear-failure-reason ()
  "Clear failure reason in property drawer at point."
  (org-entry-delete nil anki-editor-prop-failure-reason))

(defmacro anki-editor--anki-connect-invoke-result (&rest args)
  "Invoke AnkiConnect with ARGS, return the result from response or raise an error."
  `(let-alist (anki-editor--anki-connect-invoke ,@args)
     (when .error (error .error))
     .result))

(defun org-anki-export-as-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer.

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
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org HTML Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
   (let* ((org-footnote-section nil)
          (org-buffer-name (buffer-name)))
     (setq org-anki-note-status-list nil)
     (org-with-wide-buffer
      (anki-editor-map-note-entries
       (lambda ()
         (anki-editor--clear-failure-reason)
         (unless (org-entry-get (point) "CUSTOM_ID")
           (org-entry-put (point) "CUSTOM_ID" (org-id-uuid))))
         ))
     (org-export-to-buffer 'anki "*Org Anki Export*"
       async subtreep visible-only body-only ext-plist
       (lambda ()
         (let ((success-notes
                (-keep (lambda (note) (and (eq (alist-get 'status note) 'success) note))
                       org-anki-note-status-list))
               (failed-notes
                (-keep (lambda (note) (and (eq (alist-get 'status note) 'failed) note))
                       org-anki-note-status-list)))
           (erase-buffer)
           (insert (format "* %d success\n" (length success-notes)))
           (insert (format "* %d failed\n" (length failed-notes)))
           (insert
            (mapconcat
             (lambda (note)
               (format "   - [[#%d][%s]]"
                       (alist-get 'note-id note)
                       (alist-get 'reason note)))
             failed-notes
             "\n"))
           (org-mode)
           )))
     (with-current-buffer org-buffer-name
       (sort org-anki-note-status-list (lambda (a b)
                                         (> (alist-get 'begin a)
                                            (alist-get 'begin b))))
      (mapcar #'insert-note-status-in-drawer org-anki-note-status-list)
      )))

(defun insert-note-status-in-drawer (note)
  (let* ((status (alist-get 'status note))
         (note-custom-id (alist-get 'custom-id note))
         (note-id (alist-get 'note-id note)))
    (cond
     ((eq status 'failed)
      (org-entry-put
       (org-find-property "CUSTOM_ID" note-custom-id)
       anki-editor-prop-failure-reason
       (alist-get 'reason note)))
     ((eq status 'success)
      (org-entry-put
       (org-find-property "CUSTOM_ID" note-custom-id)
       anki-editor-prop-note-id
       (int-to-string note-id)))
     (t
      (error "Unknown status '%s' for note with %s=%s"
             status "CUSTOM_ID" note-custom-id))
     )))

(org-export-define-derived-backend 'anki 'html
  :translate-alist
  '((headline . org-anki-headline)
    (template . org-anki-template)
    (inner-template . org-anki-inner-template))
  :filters-alist '((:filter-options . org-html-infojs-install-script)
		           (:filter-parse-tree . org-html-image-link-filter)
		           (:filter-final-output . org-html-final-function))
  :menu-entry
  '(?a "Export to Anki" org-anki-export-as-html)
  :options-alist
  ;; Commented options are the same as in ox-html
  '(
    ;; (:html-doctype "HTML_DOCTYPE" nil org-html-doctype)
    (:html-container "HTML_CONTAINER" nil org-anki-container-element)
    (:description "DESCRIPTION" nil nil nil)
    (:keywords "KEYWORDS" nil nil nil)
    (:html-html5-fancy nil "html5-fancy" org-anki-html5-fancy)
    (:html-link-use-abs-url nil "html-link-use-abs-url" org-anki-link-use-abs-url)
    (:html-link-home "HTML_LINK_HOME" nil org-anki-link-home)
    ;; (:html-mathjax "HTML_MATHJAX" nil "" space)
    (:html-postamble nil "html-postamble" nil)
    (:html-preamble nil "html-preamble" nil)
    (:html-head "HTML_HEAD" nil nil)
    (:html-head-extra "HTML_HEAD_EXTRA" nil nil)
    (:subtitle "SUBTITLE" nil nil parse)
    (:html-head-include-default-style
     nil "html-style" nil)
    (:html-head-include-scripts nil "html-scripts" nil)
    (:html-allow-name-attribute-in-anchors
     nil nil org-anki-allow-name-attribute-in-anchors)
    (:html-divs nil nil org-anki-divs)
    (:html-checkbox-type nil nil org-anki-checkbox-type)
    (:html-extension nil nil org-anki-extension)
    (:html-footnote-format nil nil org-anki-footnote-format)
    (:html-footnote-separator nil nil org-anki-footnote-separator)
    (:html-footnotes-section nil nil org-anki-footnotes-section)
    (:html-format-drawer-function nil nil org-anki-format-drawer-function)
    (:html-format-headline-function nil nil org-anki-format-headline-function)
    (:html-format-inlinetask-function
     nil nil org-anki-format-inlinetask-function)
    (:html-home/up-format nil nil nil)
    (:html-indent nil nil org-anki-indent)
    (:html-infojs-options nil nil nil)
    (:html-infojs-template nil nil nil)
    (:html-inline-image-rules nil nil org-anki-inline-image-rules)
    ;; (:html-link-org-files-as-html nil nil org-html-link-org-files-as-html)
    ;; (:html-mathjax-options nil nil org-anki-mathjax-options)
    ;; (:html-mathjax-template nil nil org-html-mathjax-template)
    ;; (:html-metadata-timestamp-format nil nil org-html-metadata-timestamp-format)
    (:html-postamble-format nil nil nil)
    (:html-preamble-format nil nil nil)
    ;; (:html-table-align-individual-fields
    ;;  nil nil org-html-table-align-individual-fields)
    (:html-table-caption-above nil nil org-anki-table-caption-above)
    ;; (:html-table-data-tags nil nil org-html-table-data-tags)
    ;; (:html-table-header-tags nil nil org-html-table-header-tags)
    ;; (:html-table-use-header-tags-for-first-column
    ;;  nil nil org-html-table-use-header-tags-for-first-column)
    ;; (:html-tag-class-prefix nil nil org-html-tag-class-prefix)
    ;; (:html-text-markup-alist nil nil org-html-text-markup-alist)
    ;; (:html-todo-kwd-class-prefix nil nil org-html-todo-kwd-class-prefix)
    ;; (:html-toplevel-hlevel nil nil org-html-toplevel-hlevel)
    (:html-use-infojs nil nil nil)
    (:html-validation-link nil nil "")
    ;; (:html-viewport nil nil org-html-viewport)
    (:html-inline-images nil nil t)
    ;; (:html-table-attributes nil nil org-html-table-default-attributes)
    ;; (:html-table-row-open-tag nil nil org-html-table-row-open-tag)
    ;; (:html-table-row-close-tag nil nil org-html-table-row-close-tag)
    (:html-xml-declaration nil nil "")
    ;; (:html-klipsify-src nil nil org-html-klipsify-src)
    ;; (:html-klipse-css nil nil org-html-klipse-css)
    ;; (:html-klipse-js nil nil org-html-klipse-js)
    ;; (:html-klipse-selection-script nil nil org-html-klipse-selection-script)
    ;; (:infojs-opt "INFOJS_OPT" nil nil)
    ;; Redefine regular options.
    ;; (:creator "CREATOR" nil org-html-creator-string)
    (:with-latex nil "tex" org-anki-with-latex)
    ;; Retrieve LaTeX header for fragments.
    ;; (:latex-header "LATEX_HEADER" nil nil newline)
    ))



(provide 'ox-ankieditor)

;;; ox-ankieditor.el ends here
