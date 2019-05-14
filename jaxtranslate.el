(require 'md5)

(defvar translate-cache nil "translate-cache")
(defvar translate-use-translate t)
(defvar translate-debug t)
(defvar translate-debug-node-path
  (expand-file-name "~/.npm-packages/lib/node_modules"))


(defun translate-process-hash-to-hash-with-call-process (inhash cmd)
  (let* ((tmp-infilenameprefix "input-")
         (tmp-infile (make-temp-file tmp-infilenameprefix nil ".json" (json-encode hash)))
         (stdout-buffname (format "*translation stdout for %s*" "translate"))
         (stderr-buffname (format"*translation stderr for %s*" "translate"))
         (stdout-buff (get-buffer-create stdout-buffname))
         (json-object-type 'hash-table)
         stderr-buff parsedres)
    (with-current-buffer stdout-buff (erase-buffer))
    (if (not translate-debug)
        (setq stderr-buff nil)
      (progn
        (setq stderr-buff (get-buffer-create stderr-buffname))
        (with-current-buffer stderr-buff (erase-buffer))
        ))
    (call-process cmd tmp-infile (list stdout-buffname stderr-buffname))
    (with-current-buffer (get-buffer stdout-buffname)
      (goto-char (point-min))
      (json-read)
      )
  ))

(defun translate-latex-fragment-processor (hash)
  (translate-process-hash-to-hash-with-call-process
   hash
   (expand-file-name "./mathjax-node-translator.js"))
  )

(defun translate-latex-environment-processor (hash)
  (translate-process-hash-to-hash-with-call-process
   hash
   "/home/taurgal/MPSI/Data/Anki/mathjax-node-translator.js")
  )

(defun translate-src-block-environment-processor(inhash)
  (let* ((tmp-infile (make-temp-file "input" nil ".json" (json-encode hash)))
         (stdout-buffname (format "*translation stdout for %s*" cmd))
         (stderr-buffname (format"*translation stderr for %s*" cmd))
         (stdout-buff (get-buffer-create stdout-buffname))
         (json-object-type 'hash-table)
         stderr-buff parsedres)
    (with-current-buffer stdout-buff (erase-buffer))
    (if (not translate-debug)
        (setq stderr-buff nil)
      (progn
        (setq stderr-buff (get-buffer-create stderr-buffname))
        (with-current-buffer stderr-buff (erase-buffer))
        ))
    (call-process cmd tmp-infile (list stdout-buffname stderr-buffname) nil)
    (with-current-buffer (get-buffer stdout-buffname)
      (goto-char (point-min))
      (json-read)
      )
  ))

(defun translate-scan-and-cache nil
  (let* ((tree (org-element-parse-buffer 'object))
         (latex-and-src-elmts (org-element-map tree
                            '(latex-fragment latex-environment src-block inline-src-block)
                          #'identity))
         (org-type-to-res
          `((latex-fragment :processor ,#'translate-latex-fragment-processor)
            (latex-environment :processor ,#'translate-latex-environment-processor)
            (src-block :processor nil)
            (inline-src-block :processor nil)
            ))
         (all-typesaplist-with-processors (-filter (lambda (l) (plist-get (cdr l) :processor)) org-type-to-res))
         (all-types-with-processors (mapcar 'car all-typesaplist-with-processors))
         (res nil)
         )
    (dolist (type all-typesaplist-with-processors)
      (let* ((element-this-type (-filter (lambda (elmt) (eq (car elmt) (car type))) latex-and-src-elmts))
             (type-plist (cdr type))
             (processor (plist-get type-plist :processor))
             (type-hash (make-hash-table :size 900 :test #'eql)))
        (dolist (elmt element-this-type)
          (let* ((elmt-type (car elmt))
                 (elmt-plist (cadr elmt))
                 (elmt-value (s-trim (plist-get elmt-plist :value)))
                 (elmt-begin  (plist-get elmt-plist :begin))
                 (elmt-md5 (md5 elmt-value))
                 (elmt-as-plist
                  (list :begin (number-to-string elmt-begin) :value elmt-value :md5 elmt-md5
                  :type (symbol-name elmt-type))))
            (puthash (md5 elmt-value)  elmt-as-plist type-hash)
            ))
        (setq res
              (add-to-list 'res (cons (car type) (apply processor (list type-hash)))))
        ))
    (setq translate-cache res)
    translate-cache
    ))

(defun anki-editor--ox-latex-to-html (latex _contents _info)
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
    (cond
     (translate-use-translate
      (gethash "html"
      (gethash (md5 (s-trim (org-element-property :value latex)))
                        (alist-get (org-element-type latex) translate-cache))))
      (anki-editor-break-consecutive-braces-in-latex
       (replace-regexp-in-string "}}" "} } " code))
      (t code))
    ))


;; (defun translate-scan-and-cache-before-push (&optional ARG MATCH SCOPE)
;;   (translate-scan-and-cache))
;; (advice-add 'anki-editor-push-notes :before #'translate-scan-and-cache-before-push)

(provide 'jaxtranslate);;; jaxtranslate ends here
