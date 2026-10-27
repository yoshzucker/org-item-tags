;;; org-item-tags.el --- Tags on list items, as on headings  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.7"))
;; Keywords: outlines
;; URL: https://github.com/yoshzucker/org-item-tags

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org puts tags on headings only.  Here an item of a plain list takes
;; them too, written as a heading writes them, at the end of its first
;; line:
;;
;;   - Ask the team before Friday :waiting:
;;   - term :: words :draft:review:
;;
;; These are tags as headings have them, not the term of a description
;; item (what org-element calls an item's :tag, before " :: ").
;;
;; With `org-item-tags-mode' on, Org's own commands carry them:
;;
;;   C-c C-q   on an item's first line, sets the item's tags, as on a heading;
;;             with C-u, aligns the items' tags with the headings'
;;   C-c -     a heading turned into an item keeps its tags
;;   C-c *     an item turned into a heading has them as its tags, aligned
;;   export    an item tagged with one of the export's exclude tags is left
;;             out; with tags:nil, item tags are not written
;;
;; and tags on items are shown in the `org-tag' face and offered when
;; tags are completed.  They stand where a heading's do, at
;; `org-tags-column'.  An item inherits the tags of the items above it
;; and of its heading, as `org-use-tag-inheritance' says.
;;
;; For Lisp: `org-item-tags-get', `org-item-tags-set',
;; `org-item-tags-toggle' work on an item or a heading alike, and
;; `org-item-tags-split' parts a line's words from its tags.
;;
;; Not covered: agenda views and tag searches still see headings only.

;;; Code:

(require 'org)
(require 'org-list)
(require 'org-element)
(require 'cl-lib)

(defgroup org-item-tags nil
  "Tags on list items, as on headings."
  :group 'org-tags)

(defconst org-item-tags--tags-re
  "[ \t]+\\(:\\(?:[[:alnum:]_@#%]+:\\)+\\)[ \t]*"
  "Tags and the blanks around them, group 1 the tags with their colons.")

(defconst org-item-tags--re (concat org-item-tags--tags-re "$")
  "Tags at the end of a line, group 1 the tags with their colons.")

(defconst org-item-tags--hidden "\x1f"
  "Put before a heading's tags while Org makes it an item, so it keeps them.")

;;;; Reading and writing

(defun org-item-tags-split (line)
  "Return LINE parted into its words and its tags, as (WORDS . TAGS).
TAGS is a list of strings, nil when LINE ends in none."
  (if (string-match (concat org-item-tags--tags-re "\\'")
                    (string-trim-right line "\n"))
      (cons (substring line 0 (match-beginning 0))
            (org-split-string (match-string 1 line) ":"))
    (cons line nil)))

(defun org-item-tags--item-start (&optional pos)
  "Return the start of the line at POS when it is an item's first line."
  (save-excursion
    (when pos (goto-char pos))
    (forward-line 0)
    (and (derived-mode-p 'org-mode)
         (org-at-item-p)
         (point))))

(defun org-item-tags--local (bol)
  "Return the tags at the end of the line starting at BOL."
  (save-excursion
    (goto-char bol)
    (when (re-search-forward org-item-tags--re (line-end-position) t)
      (org-split-string (match-string-no-properties 1) ":"))))

(defun org-item-tags--inherited (bol)
  "Return the tags the item at BOL inherits, each marked `inherited'.
From the items above it, nearest last, and from its heading -- or, before
the first heading, the file's tags."
  (let* ((above
          (save-excursion
            (goto-char bol)
            (let* ((struct (org-list-struct))
                   (parents (org-list-parents-alist struct))
                   (item bol)
                   tags)
              (while (setq item (org-list-get-parent item struct parents))
                (setq tags (append (org-item-tags--local item) tags)))
              tags)))
         (heading (save-excursion
                    (goto-char bol)
                    (if (org-before-first-heading-p)
                        (copy-sequence org-file-tags)
                      (org-get-tags))))
         (all (delete-dups (append heading above))))
    (mapcar (lambda (tag) (propertize (substring-no-properties tag) 'inherited t))
            (org-remove-uninherited-tags all))))

(defun org-item-tags-get (&optional pos local)
  "Return the tags of the item or heading at POS, as `org-get-tags' does.
On an item's first line, its own tags, after those it inherits unless
LOCAL: tags of the items above it and of its heading, each with the text
property `inherited'.  Anywhere else, what `org-get-tags' returns."
  (let ((bol (org-item-tags--item-start pos)))
    (if (not bol)
        (org-get-tags pos local)
      (let ((own (org-item-tags--local bol)))
        (if (or local (not org-use-tag-inheritance))
            own
          (append (cl-remove-if (lambda (tag) (member tag own))
                                (org-item-tags--inherited bol))
                  own))))))

(defun org-item-tags--normalize (tags)
  "Return TAGS, a list or a string \"a:b\", as a list of distinct tags."
  (delete-dups
   (seq-remove #'string-empty-p
               (mapcar #'substring-no-properties
                       (if (stringp tags) (org-split-string tags ":") tags)))))

(defun org-item-tags--write (bol tags)
  "Write TAGS, a list, at the end of the line starting at BOL.
Replace the tags there; with none, leave the line without."
  (save-excursion
    (goto-char bol)
    (if (re-search-forward org-item-tags--re (line-end-position) t)
        (delete-region (match-beginning 0) (match-end 0))
      (end-of-line)
      (delete-horizontal-space t))
    (when tags
      ;; Where Org puts a heading's: `org-tags-column'.
      (let* ((string (org-make-tag-string tags))
             (words (current-column))
             (column (if (>= org-tags-column 0)
                         org-tags-column
                       (- (abs org-tags-column) (string-width string)))))
        (insert (make-string (max 1 (- column words)) ?\s) string)))))

(defun org-item-tags-align-all ()
  "Align the tags on every item of the buffer, at `org-tags-column'."
  (org-with-wide-buffer
   (goto-char (point-min))
   (while (re-search-forward org-item-tags--re nil t)
     (let ((bol (save-match-data (org-item-tags--item-start (match-beginning 0)))))
       (when bol
         (org-item-tags--write bol (org-item-tags--local bol))
         (goto-char (line-end-position)))))))

(defun org-item-tags-set (tags &optional pos)
  "Set the tags of the item or heading at POS to TAGS.
TAGS is a list of strings or a string, \"a:b\" or \":a:b:\"; nil or
empty takes them all off.  On a heading, as `org-set-tags' does."
  (let ((bol (org-item-tags--item-start pos))
        (tags (org-item-tags--normalize tags)))
    (if bol
        (org-item-tags--write bol tags)
      (save-excursion
        (when pos (goto-char pos))
        (org-set-tags tags)))))

(defun org-item-tags-toggle (tag &optional onoff pos)
  "Put TAG on the item or heading at POS, or take it off.
ONOFF `on' or `off' forces one; otherwise it is toggled.  Return non-nil
when TAG is on afterwards."
  (let* ((tags (org-item-tags-get pos t))
         (on (pcase onoff
               ('on t)
               ('off nil)
               (_ (not (member tag tags))))))
    (org-item-tags-set (if on
                           (append (remove tag tags) (list tag))
                         (remove tag tags))
                       pos)
    on))

(defun org-item-tags--buffer-tags ()
  "Return the tags on the items of the current buffer, as (TAG) lists."
  (let (tags)
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (re-search-forward org-item-tags--re nil t)
       (let ((found (match-string-no-properties 1)))
         (when (save-match-data (org-item-tags--item-start (match-beginning 0)))
           (dolist (tag (org-split-string found ":"))
             (cl-pushnew (list tag) tags :test #'equal))))))
    (nreverse tags)))

;;;; Org's commands

(defun org-item-tags--set-tags-command (fn &optional arg)
  "Around `org-set-tags-command': set an item's tags on its first line.
FN is the command, ARG its argument.  With \\[universal-argument],
which aligns the tags of every heading, the items' too.  On a heading,
with another prefix argument or a region, FN as it is."
  (let ((bol (and (not arg)
                  (not (org-region-active-p))
                  (org-item-tags--item-start))))
    (cond
     ((equal arg '(4))
      (funcall fn arg)
      (org-item-tags-align-all))
     ((not bol)
      (funcall fn arg))
     (t
      (cl-letf* ((tags (org-item-tags-get bol))
                 ((symbol-function 'org-before-first-heading-p) #'ignore)
                 ((symbol-function 'org-back-to-heading)
                  (lambda (&rest _) (goto-char bol)))
                 ((symbol-function 'org-get-tags)
                  (lambda (&rest _) tags))
                 ((symbol-function 'org-set-tags)
                  (lambda (new) (org-item-tags-set new bol))))
        (funcall fn arg))))))

(defun org-item-tags--toggle-range ()
  "Return the lines `org-toggle-item' and `org-toggle-heading' act on.
As (BEG . END), END a marker after what is put in at it, as a command
that writes a line anew does: the region from its first non-blank line,
or the current line."
  (if (org-region-active-p)
      (cons (save-excursion
              (goto-char (region-beginning))
              (skip-chars-forward " \r\t\n")
              (line-beginning-position))
            (copy-marker (region-end) t))
    (cons (line-beginning-position) (copy-marker (line-end-position) t))))

(defun org-item-tags--toggle-item (fn &rest args)
  "Around `org-toggle-item': a heading made an item keeps its tags.
FN is the command, ARGS its arguments."
  (let* ((range (org-item-tags--toggle-range))
         (beg (car range))
         (end (cdr range))
         (headings (save-excursion (goto-char beg) (org-at-heading-p))))
    (if (not headings)
        (apply fn args)
      ;; Org drops a heading's tags; written so they are no tags, Org
      ;; keeps them as words, and they are written back as an item's.
      (save-excursion
        (goto-char beg)
        (while (< (point) end)
          (when (looking-at org-tag-line-re)
            (let ((tags (match-string 1)))
              (goto-char (match-beginning 1))
              (skip-chars-backward " \t")
              (delete-region (point) (line-end-position))
              (insert " " org-item-tags--hidden tags)))
          (forward-line)))
      (unwind-protect (apply fn args)
        (save-excursion
          (goto-char beg)
          (while (search-forward org-item-tags--hidden end t)
            (delete-char -1)
            (let ((bol (line-beginning-position)))
              (if (org-item-tags--item-start bol)
                  (org-item-tags--write bol (org-item-tags--local bol))
                ;; Still a heading, Org having stopped: the tags are its.
                (when (org-at-heading-p) (org-align-tags))))))
        (set-marker end nil)))))

(defun org-item-tags--toggle-heading (fn &rest args)
  "Around `org-toggle-heading': an item made a heading has its tags aligned.
FN is the command, ARGS its arguments."
  (let* ((range (org-item-tags--toggle-range))
         (beg (car range))
         (end (cdr range)))
    (unwind-protect
        (progn
          (apply fn args)
          (save-excursion
            (goto-char beg)
            (while (< (point) end)
              (when (and (org-at-heading-p) (looking-at org-tag-line-re))
                (org-align-tags))
              (forward-line))))
      (set-marker end nil))))

(defun org-item-tags--buffer-tags-too (tags)
  "After `org-get-buffer-tags': add to TAGS those on the buffer's items."
  (let ((all (copy-sequence tags)))
    (dolist (tag (org-item-tags--buffer-tags))
      (unless (assoc (car tag) all) (setq all (append all (list tag)))))
    all))

;;;; Export

(defun org-item-tags--paragraph-tags (paragraph)
  "Return where PARAGRAPH's first line has tags, as (STRING WORDS TAGS BEFORE).
STRING is the plain text in its contents the tags end; WORDS what is
left of it with the tags taken off; BEFORE the object before STRING when
the blank before the tags is that object's, its post blank, else nil."
  (let ((contents (org-element-contents paragraph))
        before last line-end)
    (while (and contents (not line-end))
      (let ((object (car contents)))
        (if (and (stringp object) (string-search "\n" object))
            (setq line-end object)
          (setq before last last object)))
      (setq contents (cdr contents)))
    (unless line-end
      (setq line-end last last before))
    (let* ((string line-end)
           (newline (and (stringp string) (string-search "\n" string)))
           (head (and (stringp string) (substring string 0 newline))))
      (when (and head
                 (string-match (concat "\\(?:\\`\\|[ \t]+\\)\\(:\\(?:[[:alnum:]_@#%]+:\\)+\\)[ \t]*\\'")
                               head)
                 ;; Tags start the text only where a blank is before it.
                 (or (> (match-beginning 0) 0)
                     (null last)
                     (> (or (org-element-property :post-blank last) 0) 0)))
        (let ((start (match-beginning 0)))
          (list string
                (concat (substring string 0 start)
                        (if newline (substring string newline) ""))
                (org-split-string (match-string 1 head) ":")
                (and (= start 0) last)))))))

(defun org-item-tags--export (tree _backend info)
  "Filter TREE for export with INFO: an item's tags as its heading's are.
An item tagged with one of the exclude tags is left out; with tags:nil,
tags are not written."
  (let ((exclude (plist-get info :exclude-tags))
        (with (plist-get info :with-tags)))
    ;; Each item is changed after all are found: the tree is not changed
    ;; while it is walked.
    (dolist (item (org-element-map tree 'item #'identity info))
      (let* ((paragraph (car (org-element-contents item)))
             (found (and (org-element-type-p paragraph 'paragraph)
                         (org-item-tags--paragraph-tags paragraph))))
        (when found
          (cond ((seq-intersection (nth 2 found) exclude)
                 (org-element-extract item))
                ((not with)
                 (when (nth 3 found)
                   (org-element-put-property (nth 3 found) :post-blank 0))
                 (apply #'org-element-set-contents
                        paragraph
                        (delete ""
                                (mapcar (lambda (object)
                                          (if (eq object (nth 0 found)) (nth 1 found) object))
                                        (org-element-contents paragraph))))))))))
  tree)

;;;; The mode

(defun org-item-tags--fontify (limit)
  "Find the next tags on an item's first line before LIMIT, for font lock."
  (let (found)
    (while (and (not found) (re-search-forward org-item-tags--re limit t))
      (setq found (save-match-data (org-item-tags--item-start (match-beginning 0)))))
    found))

(defconst org-item-tags--keywords
  '((org-item-tags--fontify 1 'org-tag prepend))
  "Font lock keywords showing tags on items.")

(defun org-item-tags--font-lock (on)
  "Show tags on items in the current buffer when ON, else stop."
  (if on
      (font-lock-add-keywords nil org-item-tags--keywords 'append)
    (font-lock-remove-keywords nil org-item-tags--keywords))
  (font-lock-flush))

(defun org-item-tags--font-lock-on ()
  "Show tags on items in the current buffer."
  (org-item-tags--font-lock t))

;;;###autoload
(define-minor-mode org-item-tags-mode
  "Take tags on list items, at the end of an item's first line, as tags.
Org's commands then carry them: \\[org-set-tags-command] on an item's
first line sets the item's tags; \\[org-toggle-item] and
\\[org-toggle-heading] keep them, between a heading and an item; export
leaves out an item tagged with an exclude tag, and writes no item tags
under tags:nil.  They are shown as a heading's are and offered in
completion."
  :global t
  :group 'org-item-tags
  (if org-item-tags-mode
      (progn
        (advice-add 'org-set-tags-command :around #'org-item-tags--set-tags-command)
        (advice-add 'org-toggle-item :around #'org-item-tags--toggle-item)
        (advice-add 'org-toggle-heading :around #'org-item-tags--toggle-heading)
        (advice-add 'org-get-buffer-tags :filter-return #'org-item-tags--buffer-tags-too)
        (add-hook 'org-export-filter-parse-tree-functions #'org-item-tags--export)
        (add-hook 'org-mode-hook #'org-item-tags--font-lock-on))
    (advice-remove 'org-set-tags-command #'org-item-tags--set-tags-command)
    (advice-remove 'org-toggle-item #'org-item-tags--toggle-item)
    (advice-remove 'org-toggle-heading #'org-item-tags--toggle-heading)
    (advice-remove 'org-get-buffer-tags #'org-item-tags--buffer-tags-too)
    (remove-hook 'org-export-filter-parse-tree-functions #'org-item-tags--export)
    (remove-hook 'org-mode-hook #'org-item-tags--font-lock-on))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-mode)
        (org-item-tags--font-lock org-item-tags-mode)))))

(provide 'org-item-tags)

;;; org-item-tags.el ends here
