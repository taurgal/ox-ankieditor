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
(defconst anki-editor-prop-note-id "ANKI_NOTE_ID")
(defconst anki-editor-prop-failure-reason "ANKI_FAILURE_REASON")
(defconst anki-editor-prop-deck "ANKI_DECK")
(defconst anki-editor-org-tag-regexp "^\\([[:alnum:]_@#%]+\\)+$")
(defconst anki-editor-prop-tags "ANKI_TAGS")

(defun anki-editor--get-tags ()
  (let ((tags (anki-editor--entry-get-multivalued-property-with-inheritance
               nil
               anki-editor-prop-tags)))
    (if anki-editor-org-tags-as-anki-tags
        (append tags (org-get-tags-at))
      tags)))

(defun anki-editor--entry-get-multivalued-property-with-inheritance (pom property)
  "Return a list of values in a multivalued property with inheritance."
  (let* ((value (org-entry-get pom property t))
	     (values (and value (split-string value))))
    (mapcar #'org-entry-restore-space values)))

(defcustom anki-editor-ignored-org-tags
  (append org-export-select-tags org-export-exclude-tags "deck" "note")
  "A list of Org tags that are ignored when constructing notes form entries."
  :type '(repeat string)
  :tag "Org tags not mapped to Anki tags"
  :group 'org-export)

(defcustom anki-editor-anki-connect-listening-address
  "127.0.0.1"
  "The network address AnkiConnect is listening."
  :type 'string
  :tag "Network adress"
  :group 'org-export)

(defcustom anki-editor-anki-connect-listening-port
  "8765"
  "The port number AnkiConnect is listening."
  :type 'string
  :tag "Communication port"
  :group 'org-export)

(defcustom anki-editor-break-consecutive-braces-in-latex
  nil
  "If non-nil, consecutive `}' will be automatically separated by spaces to prevent early-closing of cloze.
See https://apps.ankiweb.net/docs/manual.html#latex-conflicts."
  :type 'boolean
  :tag "Communication port"
  :group 'org-export)

(defcustom anki-editor-create-decks
  nil
  "If non-nil, creates deck before creating a note."
  :type 'boolean
  :tag "Should we create nonexistent decks?"
  :group 'org-export)

(defcustom anki-editor-org-tags-as-anki-tags
  t
  "If nil, tags of entries won't be counted as Anki tags."
  :type 'boolean
  :tag "Use Org tags as Anki tags?"
  :group 'org-export)

(defcustom anki-editor-protected-tags
  '("marked" "leech")
  "A list of tags that won't be deleted from Anki even though they're absent in Org entries, such as special tags `marked', `leech'."
  :tag "List of tags not to use as Anki tags"
  :type '(repeat string))

(defconst anki-editor--ox-anki-html-backend
  (org-export-create-backend
   :parent 'html
   :transcoders '((latex-fragment . anki-editor--ox-latex)
                  (latex-environment . anki-editor--ox-latex))))

(defconst anki-editor--ox-export-ext-plist
  '(:with-toc nil :anki-editor-mode t))

(defun anki-editor--translate-latex-delimiters (latex-code)
  (catch 'done
    (let ((delimiter-map (list (list (cons (format "^%s" (regexp-quote "$$")) "[$$]")
                                     (cons (format "%s$" (regexp-quote "$$")) "[/$$]"))
                               (list (cons (format "^%s" (regexp-quote "$")) "[$]")
                                     (cons (format "%s$" (regexp-quote "$")) "[/$]"))
                               (list (cons (format "^%s" (regexp-quote "\\(")) "[$]")
                                     (cons (format "%s$" (regexp-quote "\\)")) "[/$]"))
                               (list (cons (format "^%s" (regexp-quote "\\[")) "[$$]")
                                     (cons (format "%s$" (regexp-quote "\\]")) "[/$$]"))))
          (matched nil))
      (save-match-data
        (dolist (pair delimiter-map)
          (dolist (delimiter pair)
            (when (setq matched (string-match (car delimiter) latex-code))
              (setq latex-code (replace-match (cdr delimiter) t t latex-code))))
          (when matched (throw 'done latex-code)))))
    latex-code))

(defun anki-editor--ox-latex (latex _contents _info)
  "Transcode LATEX from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((code (org-remove-indentation (org-element-property :value latex))))
    (setq code
          (pcase (org-element-type latex)
            ('latex-fragment (anki-editor--translate-latex-delimiters code))
            ('latex-environment (anki-editor--wrap-latex
                                 (mapconcat #'anki-editor--wrap-div
                                            (split-string (org-html-encode-plain-text code) "\n")
                                            "")))))

    (if anki-editor-break-consecutive-braces-in-latex
        (replace-regexp-in-string "}}" "} } " code)
      code)))

(defun org-ankieditor--build-fields (heading)
  "Build a list of fields from subheadings of current heading.

Each element of this list is a cons cell representing a field,
the car being the field name and the cdr the field content."
  (save-excursion
  (let (fields
        (point-of-last-child (point)))
    (org-element-map heading 'headline
      (lambda (field-heading)
        (unless (eq heading field-heading)
          (let ((field-name (org-element-property :raw-value field-heading))
                (contents-begin (org-element-property :contents-begin field-heading))
                (contents-end (org-element-property :contents-end field-heading)))
            (push (cons field-name
                        (cond
                         ((and contents-begin contents-end)
                          (or (org-export-string-as
                               (buffer-substring
                                contents-begin
                                ;; in case the buffer is narrowed,
                                ;; e.g. by `org-map-entries' when
                                ;; scope is `tree'
                                (min (point-max) contents-end))
                               anki-editor--ox-anki-html-backend
                               t
                               anki-editor--ox-export-ext-plist)
                              
                              ;; 8.2.10 version of
                              ;; `org-export-filter-apply-functions'
                              ;; returns nil for an input of empty
                              ;; string, which causes AnkiConnect to
                              ;; fail
                              ""))
                         (t "")))
                  fields)))))
    (reverse fields))))


(defgroup org-export-anki nil
  "Options for exporting Org mode files to HTML."
  :tag "Org Export HTML"
  :group 'org-export)


(defcustom org-ankieditor-checkbox-type 'unicode
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
  (car action)  
  (setcdr (assq 'note-id note) id)
  (and errmsg (push `(reason . ,errmsg) note)))

(defun org-ankieditor-notesinfo-handler (result errmsg action note)
  "Update tags in NOTE"
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
    
    (funcall tag-queue note)))

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
             #'org-ankieditor-notesinfo-handler
             )
    (funcall queue note)))

(defun anki-editor--anki-connect-map-note (note)
  "Convert NOTE to the form that AnkiConnect accepts."
  (let-alist note
    (list (cons "id" (or .note-id "-1"))
          (cons "deckName" .deck)
          (cons "modelName" .note-type)
          (cons "fields" .fields)
          ;; Convert tags to a vector since empty list is identical to nil
          ;; which will become None in Python, but AnkiConnect requires it
          ;; to be type of list.
          (cons "tags" (vconcat .tags)))))

(defun anki-editor--anki-connect-invoke (action &optional params)
  "Invoke AnkiConnect with ACTION and PARAMS."
  (let ((request-body (json-encode (anki-editor--anki-connect-action action params 5)))
        (request-backend 'curl)
        (json-array-type 'list)
        reply err)

    (let ((response (request (format "http://%s:%s"
                                     anki-editor-anki-connect-listening-address
                                     anki-editor-anki-connect-listening-port)
                             :type "POST"
                             :parser 'json-read
                             :data (encode-coding-string request-body 'utf-8)
                             :success (cl-function (lambda (&key data &allow-other-keys)
                                                     (setq reply data)))
                             :error (cl-function (lambda (&key _ error-thrown &allow-other-keys)
                                                   (setq err (string-trim (cdr error-thrown)))))
                             :sync t)))

      ;; HACK: I expect the behavior of the sync mode to be that
      ;; callbacks get called before the invocation to `request' ends,
      ;; but it seems not to be the case (or I get it wrong ?) that
      ;; sometimes when the curl process finishes, the
      ;; `request--curl-callback' (the sentinel of the curl process,
      ;; which calls `request--callback', which subsequently calls the
      ;; callbacks) get called after `request--curl-sync' ends. Here I
      ;; check if the `done-p' is nil (which will be set to `t' after
      ;; callbacks have been called) and call `request--curl-callback'
      ;; manually.
      (unless (request-response-done-p response)
        (request--curl-callback (get-buffer-process (request-response--buffer response)) "finished\n")))

    (when err (error "Error communicating with AnkiConnect using cURL: %s" err))
    (or reply (error "Got empty reply from AnkiConnect"))))


(defun anki-editor-maybe-call-handler (result-action-note-triplet handler)
  (let* ((result-and-error (car result-action-note-triplet))
         (result (alist-get 'result result-and-error))
         (errmsg (alist-get 'error result-and-error))
         (action (cadr result-action-note-triplet))
         (note (caddr result-action-note-triplet)))
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
         (result-action-note-triplet
          (-zip-pair results-list
                     (mapcar #'car actions)
                     (mapcar (lambda (x) (alist-get 'note (cdr x))) actions)
                     ))
         (handler-list (mapcar (lambda (x) (alist-get 'handler (cdr x))) actions) ))
    (-zip-with
     #'anki-editor-maybe-call-handler
     result-action-note-triplet
     handler-list
     )))

(defun anki-editor--anki-connect-action (action &optional params version)
  (let (a)
    (when version
      (push `(version . ,version) a))
    (when params
      (push `(params . ,params) a))
    (push `(action . ,action) a)))

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



(defcustom org-ankieditor-container-element "div"
  "HTML element to use for wrapping top level sections.
Can be set with the in-buffer HTML_CONTAINER property or for
publishing, with :html-container."
  :group 'org-export-anki
  :type 'string)

(defun org-ankieditor-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  contents)

(defun org-ankieditor-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat "~BEGINNING~\n" contents "\n~END~"))

(defconst org-ankieditor-doctype-alist
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

(defcustom org-ankieditor-container-element "div"
  "HTML element to use for wrapping top level sections.
Can be set with the in-buffer HTML_CONTAINER property or for
publishing, with :html-container.

Note that changing the default will prevent you from using
org-info.js for your website."
  :group 'org-export-anki
  :type 'string)

(defcustom org-ankieditor-html5-fancy nil
  "Non-nil means using new HTML5 elements.
This variable is ignored for anything other than HTML5 export.

For compatibility with Internet Explorer, it's probably a good
idea to download some form of the html5shiv (for instance
https://code.google.com/p/html5shiv/) and add it to your
HTML_HEAD_EXTRA, so that your pages don't break for users of IE
versions 8 and below."
  :group 'org-export-anki
  :type 'boolean)

(defcustom org-ankieditor-link-use-abs-url nil
  "Should we prepend relative links with HTML_LINK_HOME?"
  :group 'org-export-anki
  :type 'boolean)

(defcustom org-ankieditor-link-home ""
  "Where should the \"HOME\" link of exported HTML pages lead?"
  :group 'org-export-anki
  :type '(string :tag "File or URL"))

(defcustom org-ankieditor-allow-name-attribute-in-anchors nil
  "When nil, do not set \"name\" attribute in anchors.
By default, when appropriate, anchors are formatted with \"id\"
but without \"name\" attribute."
  :group 'org-export-anki
  :type 'boolean)

(defcustom org-ankieditor-divs
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

(defconst org-ankieditor-checkbox-types
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

(defcustom org-ankieditor-extension "html"
  "The extension for exported HTML files."
  :group 'org-export-anki
  :type 'string)

(defcustom org-ankieditor-footnote-format "<sup>%s</sup>"
  "The format for the footnote reference.
%s will be replaced by the footnote reference itself."
  :group 'org-export-anki
  :type 'string)

(defcustom org-ankieditor-footnote-separator "<sup>, </sup>"
  "Text used to separate footnotes."
  :group 'org-export-anki
  :type 'string)

(defcustom org-ankieditor-footnotes-section "<div id=\"footnotes\">
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

(defcustom org-ankieditor-format-drawer-function (lambda (_name contents) contents)
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

(defcustom org-ankieditor-format-headline-function
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

(defcustom org-ankieditor-format-inlinetask-function
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

(defcustom org-ankieditor-indent nil
  "Non-nil means to indent the generated HTML.
Warning: non-nil may break indentation of source code blocks."
  :group 'org-export-anki
  :type 'boolean)

(defcustom org-ankieditor-inline-image-rules
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

(defcustom org-ankieditor-table-caption-above t
  "When non-nil, place caption string at the beginning of the table.
Otherwise, place it near the end."
  :group 'org-export-anki
  :type 'boolean)

(defcustom org-ankieditor-with-latex org-export-with-latex
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


(defun org-ankieditor-note-from-field-alist (headline deck note-id note-type tags fields)
  "Construct an alist representing a note from its arguments.

HEADLINE should be the org-element headline correspondng to the
note, NOTE-ID is nil or he note id as int, NOTE-TYPE is an Anki
note type, TAGS a list of Anki tags as strings and FIELDS is an
alist with field names as keys and content strings as values."
  (let* ((custom-id (org-element-property :CUSTOM_ID headline))
         (begin (org-element-property :begin headline)))

    (unless deck (error "No deck specified"))
    (unless note-type (error "Missing note type"))
    (unless fields (error "Missing fields"))

    `((deck . ,deck)
      (note-id . ,note-id)
      (note-type . ,note-type)
      (tags . ,tags)
      (fields . ,fields)
      (custom-id . ,custom-id)
      (begin . ,begin))))

(defun anki-editor--push-note (note)
  "Request AnkiConnect for updating or creating NOTE."
  (if (alist-get 'note-id note)
      (anki-editor--update-note note)
    (anki-editor--create-note note)
    ))

(defun org-ankieditor-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (cond
   ((org-element-property (intern (concat ":" anki-editor-prop-note-type)) headline)
    ;; New note
    (goto-char (org-element-property :begin headline)) ; For property inheritance (deck and tags)
    (let* ((deck (org-entry-get-with-inheritance anki-editor-prop-deck))
           (note-id-nil-or-string
            (org-element-property (intern(concat ":" anki-editor-prop-note-id)) headline))
           (note-id (and note-id-nil-or-string (string-to-number note-id-nil-or-string)))
           (note-type
            (org-element-property (intern(concat ":" anki-editor-prop-note-type)) headline))
           (tags (anki-editor--get-tags))
           (fields (plist-get info :anki-fields))
           (note
            (org-ankieditor-note-from-field-alist
             headline deck note-id note-type tags fields)))
      (condition-case err
          (progn
            (when (< (length fields) 1) (error "Note has no field"))
            (anki-editor--push-note note)
            (push '(status . success) note)
            )
        (error
         (push '(status . failed) note)
         (push `(reason . ,(error-message-string err)) note)))
      (push note org-ankieditor-note-status-list)
      )
      ;; Prepare for next note
      (plist-put info :anki-fields nil)
      "") ; Act by sideeffect
     (t
      ;; New field for the current note
      ;; TODO: Check for duplicate field
      (let ((field-name (substring-no-properties (org-element-property :raw-value headline))))
        (when (alist-get field-name (plist-get info :anki-fields)  nil nil #'string=)
          (error "Duplicate field"))
        (plist-put info :anki-fields (cons (cons field-name contents) (plist-get info :anki-fields)))
          
        ;; Allow headings in fields. Copied from ox-html
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
                        (org-html--container headline info))))))))))


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

(defun org-ankieditor-export-all
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
     (setq org-ankieditor-note-status-list nil)
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
                       org-ankieditor-note-status-list))
               (failed-notes
                (-keep (lambda (note) (and (eq (alist-get 'status note) 'failed) note))
                       org-ankieditor-note-status-list)))
           (erase-buffer)
           (insert (format "* %d success\n" (length success-notes)))
           (insert (format "* %d failed\n" (length failed-notes)))
           (insert
            (mapconcat
             (lambda (note)
               (format "   - [[#%s][%s]]"
                       (alist-get 'custom-id note)
                       (alist-get 'reason note)))
             failed-notes
             "\n"))
           (org-mode)
           )))
     (with-current-buffer org-buffer-name
       (sort org-ankieditor-note-status-list (lambda (a b)
                                         (> (alist-get 'begin a)
                                            (alist-get 'begin b))))
      (mapcar #'insert-note-status-in-drawer org-ankieditor-note-status-list)
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
       (number-to-string note-id)))
     (t
      (error "Unknown status '%s' for note with %s=%s"
             status "CUSTOM_ID" note-custom-id))
     )))

(defun anki-editor--anki-connect-store-media-file (path)
  "Store media file for PATH, which is an absolute file name.
The result is the path to the newly stored media file."
  (let* ((hash (secure-hash 'sha1 path))
         (media-file-name (format "%s-%s%s"
                                  (file-name-base path)
                                  hash
                                  (file-name-extension path t)))
         content)
    (when (equal :json-false (anki-editor--anki-connect-invoke-result
                              "retrieveMediaFile"
                              `((filename . ,media-file-name))))
      (message "Storing media file to Anki for %s..." path)
      (setq content (base64-encode-string
		     (with-temp-buffer
		       (insert-file-contents path)
		       (buffer-string))))
      (anki-editor--anki-connect-invoke-result
       "storeMediaFile"
       `((filename . ,media-file-name)
         (data . ,content))))
    media-file-name))

(defun org-ankieditor-link (link desc info)
  "When LINK is a link to local file, transcodes it to html and stores the target file to Anki, otherwise calls OLDFUN for help.
The implementation is borrowed from anki-editor which is borrowed
from ox-html."
  (or (catch 'giveup
        (let* ((type (org-element-property :type link))
               (raw-path (org-element-property :path link))
               (desc (org-string-nw-p desc))
               (path
                (cond
                 ((string= type "file")
                  ;; Possibly append `:html-link-home' to relative file
                  ;; name.
                  (let ((inhibit-message nil)
                        (home (and (plist-get info :html-link-home)
                                   (org-trim (plist-get info :html-link-home)))))
                    (when (and home
                               (plist-get info :html-link-use-abs-url)
                               (file-name-absolute-p raw-path))
                      (setq raw-path (concat (file-name-as-directory home) raw-path)))
                    ;; storing file to Anki and return the modified path
                    (anki-editor--anki-connect-store-media-file (expand-file-name (url-unhex-string raw-path)))))
                 (t (throw 'giveup nil))))
               (attributes-plist
                (let* ((parent (org-export-get-parent-element link))
                       (link (let ((container (org-export-get-parent link)))
                               (if (and (eq (org-element-type container) 'link)
                                        (org-html-inline-image-p link info))
                                   container
                                 link))))
                  (and (eq (org-element-map parent 'link 'identity info t) link)
                       (org-export-read-attribute :attr_html parent))))
               (attributes
                (let ((attr (org-html--make-attribute-string attributes-plist)))
                  (if (org-string-nw-p attr) (concat " " attr) ""))))
          (cond
           ;; Image file.
           ((and (plist-get info :html-inline-images)
                 (org-export-inline-image-p
                  link (plist-get info :html-inline-image-rules)))
            (org-html--format-image path attributes-plist info))

           ;; Audio file.
           ((string-suffix-p ".mp3" path t)
              (format "[sound:%s]" path))

           ;; External link with a description part.
           ((and path desc) (format "<a href=\"%s\"%s>%s</a>"
                                    (org-html-encode-plain-text path)
                                    attributes
                                    desc))

           ;; External link without a description part.
           (path (let ((path (org-html-encode-plain-text path)))
                   (format "<a href=\"%s\"%s>%s</a>"
                           path
                           attributes
                           (org-link-unescape path))))

           (t (throw 'giveup nil)))))
      (org-html-link link desc info)))

(org-export-define-derived-backend 'anki 'html
  :translate-alist
  '((headline . org-ankieditor-headline)
    (template . org-ankieditor-template)
    (inner-template . org-ankieditor-inner-template)
    (link . org-ankieditor-link)
    (latex-fragment . anki-editor--ox-latex)
    (latex-environment . anki-editor--ox-latex))
  :filters-alist '()
  :menu-entry
  '(?a "Export to Anki"
       ((?a "Export all cards" org-ankieditor-export-all)
        (?f "Export failed cards" org-ankieditor-export-failed)))
  :options-alist
  ;; Commented options are the same as in ox-html
  '(
    ;; (:html-doctype "HTML_DOCTYPE" nil org-html-doctype)
    (:html-container "HTML_CONTAINER" nil org-ankieditor-container-element)
    (:description "DESCRIPTION" nil nil nil)
    (:keywords "KEYWORDS" nil nil nil)
    (:html-html5-fancy nil "html5-fancy" org-ankieditor-html5-fancy)
    (:html-link-use-abs-url nil "html-link-use-abs-url" org-ankieditor-link-use-abs-url)
    (:html-link-home "HTML_LINK_HOME" nil org-ankieditor-link-home)
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
     nil nil org-ankieditor-allow-name-attribute-in-anchors)
    (:html-divs nil nil org-ankieditor-divs)
    (:html-checkbox-type nil nil org-ankieditor-checkbox-type)
    (:html-extension nil nil org-ankieditor-extension)
    (:html-footnote-format nil nil org-ankieditor-footnote-format)
    (:html-footnote-separator nil nil org-ankieditor-footnote-separator)
    (:html-footnotes-section nil nil org-ankieditor-footnotes-section)
    (:html-format-drawer-function nil nil org-ankieditor-format-drawer-function)
    (:html-format-headline-function nil nil org-ankieditor-format-headline-function)
    (:html-format-inlinetask-function
     nil nil org-ankieditor-format-inlinetask-function)
    (:html-home/up-format nil nil nil)
    (:html-indent nil nil org-ankieditor-indent)
    (:html-infojs-options nil nil nil)
    (:html-infojs-template nil nil nil)
    (:html-inline-image-rules nil nil org-ankieditor-inline-image-rules)
    ;; (:html-link-org-files-as-html nil nil org-html-link-org-files-as-html)
    ;; (:html-mathjax-options nil nil org-ankieditor-mathjax-options)
    ;; (:html-mathjax-template nil nil org-html-mathjax-template)
    ;; (:html-metadata-timestamp-format nil nil org-html-metadata-timestamp-format)
    (:html-postamble-format nil nil nil)
    (:html-preamble-format nil nil nil)
    ;; (:html-table-align-individual-fields
    ;;  nil nil org-html-table-align-individual-fields)
    (:html-table-caption-above nil nil org-ankieditor-table-caption-above)
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
    (:with-latex nil "tex" org-ankieditor-with-latex)
    ;; Retrieve LaTeX header for fragments.
    ;; (:latex-header "LATEX_HEADER" nil nil newline)
    )
  )



(provide 'ox-ankieditor)

;;; ox-ankieditor.el ends here
