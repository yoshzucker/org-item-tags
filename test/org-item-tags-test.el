;;; org-item-tags-test.el --- Tests for org-item-tags  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'org-item-tags)
(require 'ox-md)
(require 'org-indent)

(defmacro org-item-tags-test--with (text &rest body)
  "Run BODY in an Org buffer holding TEXT, point at its start."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,text)
     (goto-char (point-min))
     ,@body))

(defmacro org-item-tags-test--on (&rest body)
  "Run BODY with `org-item-tags-mode' on, and off again after."
  `(unwind-protect (progn (org-item-tags-mode 1) ,@body)
     (org-item-tags-mode -1)))

(defun org-item-tags-test--line (text)
  "Move to the start of the line holding TEXT."
  (goto-char (point-min))
  (search-forward text)
  (forward-line 0))

(ert-deftest org-item-tags-test-split ()
  "A line's words and its tags, Org's tag characters only."
  (should (equal (org-item-tags-split "Ask the team :waiting:") '("Ask the team" "waiting")))
  (should (equal (org-item-tags-split "words  :a:b@c:\n") '("words" "a" "b@c")))
  (should (equal (org-item-tags-split "at 10:30") '("at 10:30")))
  (should (equal (org-item-tags-split "a :x: b") '("a :x: b")))
  (should (equal (org-item-tags-split "a:x:") '("a:x:"))))

(ert-deftest org-item-tags-test-get ()
  "An item's own tags, on its first line only; inherited from items
above and the heading, marked so; on a heading, as Org has them."
  (org-item-tags-test--with
      "* H :proj:\n- outer :a:\n  - inner :b:\n    more :c:\n- term :: words :d:\n#+begin_src org\n- in a block :e:\n#+end_src\n"
    (let ((org-use-tag-inheritance t))
      (org-item-tags-test--line "- inner")
      (should (equal (org-item-tags-get nil t) '("b")))
      (let ((all (org-item-tags-get)))
        (should (equal all '("proj" "a" "b")))
        (should (get-text-property 0 'inherited (car all)))
        (should-not (get-text-property 0 'inherited (nth 2 all))))
      (org-item-tags-test--line "more")
      (should (equal (org-item-tags-get nil t) '("proj")))
      (org-item-tags-test--line "- term")
      (should (equal (org-item-tags-get nil t) '("d")))
      (org-item-tags-test--line "- in a block")
      (should-not (org-item-tags--item-start))
      (let ((org-use-tag-inheritance nil))
        (org-item-tags-test--line "- inner")
        (should (equal (org-item-tags-get) '("b")))))))

(ert-deftest org-item-tags-test-set ()
  "Tags written at the end of the first line, replaced, taken off; where,
at `org-tags-column', as a heading's."
  (org-item-tags-test--with "* H\n- Ask the team\n  more\n"
   (let ((org-tags-column 0))
    (org-item-tags-test--line "- Ask")
    (org-item-tags-set '("waiting"))
    (should (equal (buffer-string) "* H\n- Ask the team :waiting:\n  more\n"))
    (org-item-tags-set "a:b")
    (should (equal (buffer-string) "* H\n- Ask the team :a:b:\n  more\n"))
    (should (org-item-tags-toggle "c"))
    (should-not (org-item-tags-toggle "a"))
    (should (equal (buffer-string) "* H\n- Ask the team :b:c:\n  more\n"))
    (org-item-tags-set nil)
    (should (equal (buffer-string) "* H\n- Ask the team\n  more\n"))
    (let ((org-tags-column 20))
      (org-item-tags-set '("x"))
      (should (equal (buffer-string) "* H\n- Ask the team      :x:\n  more\n")))
    (let ((org-tags-column -20))
      (org-item-tags-set '("x"))
      (should (equal (buffer-string) "* H\n- Ask the team   :x:\n  more\n")))
    (goto-char (point-min))
    (org-item-tags-set '("h"))
    (should (equal (org-get-tags nil t) '("h"))))))

(ert-deftest org-item-tags-test-align-all ()
  "C-u C-c C-q aligns the items' tags with the headings'."
  (org-item-tags-test--with "* H :h:\n- item :a:\n  - inner   :b:\n"
    (org-item-tags-test--on
     (let ((org-tags-column -20))
       (org-set-tags-command '(4))
       (should (equal (buffer-string)
                      "* H              :h:\n- item           :a:\n  - inner        :b:\n"))))))

(ert-deftest org-item-tags-test-org-indent ()
  "With `org-indent-mode', an item's tags are seen where its heading's are."
  (org-item-tags-test--with "* H :h:\n- item\n** H2 :h:\n- deeper\n"
    (let ((org-tags-column -20)
          (org-indent-indentation-per-level 1))
      (org-indent-mode 1)
      (org-indent-add-properties (point-min) (point-max))
      (org-align-tags t)
      (org-item-tags-test--line "- item")
      (org-item-tags-set '("a"))
      (org-item-tags-test--line "- deeper")
      (org-item-tags-set '("b"))
      (let ((seen (lambda (tag)
                    (goto-char (point-min))
                    (search-forward tag)
                    (+ (current-column) (org-item-tags--prefix-width (line-beginning-position))))))
        (should (= (funcall seen ":h:") (funcall seen ":a:")))
        (goto-char (point-min))
        (search-forward "** H2")
        (should (= (progn (search-forward ":h:") (+ (current-column) (org-item-tags--prefix-width (line-beginning-position))))
                   (funcall seen ":b:")))))))

(ert-deftest org-item-tags-test-set-tags-command ()
  "C-c C-q on an item's first line sets its tags; elsewhere the heading's."
  (org-item-tags-test--with "* H :h:\n- item\n  more\n"
    (org-item-tags-test--on
     (let ((org-use-fast-tag-selection nil)
           (org-tags-column 0))
       (cl-letf (((symbol-function 'completing-read-multiple)
                  (lambda (_prompt table &rest _)
                    (should (assoc "h" (if (functionp table) org-last-tags-completion-table table)))
                    '("x" "y"))))
         (org-item-tags-test--line "- item")
         (end-of-line)
         (call-interactively #'org-set-tags-command)
         (should (equal (buffer-string) "* H :h:\n- item :x:y:\n  more\n"))
         (org-item-tags-test--line "more")
         (call-interactively #'org-set-tags-command)
         (goto-char (point-min))
         (should (equal (org-get-tags nil t) '("x" "y"))))))))

(ert-deftest org-item-tags-test-buffer-tags ()
  "Completion offers the tags on items too."
  (org-item-tags-test--with "* H :h:\n- item :i:\n"
    (org-item-tags-test--on
     (should (equal (sort (mapcar #'car (org-get-buffer-tags)) #'string<) '("h" "i"))))))

(ert-deftest org-item-tags-test-toggle-item ()
  "C-c - keeps the tags of the headings it makes items."
  (org-item-tags-test--with "* H\n** First   :a:b:\n** Second\n** Third :c:\n"
    (org-item-tags-test--on
     (let ((org-adapt-indentation nil)
           (org-tags-column 0)
           (transient-mark-mode t))
       (org-item-tags-test--line "** First")
       (push-mark (point-max) t t)
       (call-interactively #'org-toggle-item)
       (should (equal (buffer-string) "* H\n- First :a:b:\n- Second\n- Third :c:\n")))))
  (org-item-tags-test--with "* H\n** Alone      :a:\n"
    (org-item-tags-test--on
     (let ((org-adapt-indentation nil)
           (org-tags-column -20))
       (org-item-tags-test--line "** Alone")
       (call-interactively #'org-toggle-item)
       (should (equal (buffer-string) "* H\n- Alone          :a:\n"))))))

(ert-deftest org-item-tags-test-toggle-heading ()
  "C-c * makes an item's tags the heading's, aligned as Org aligns them."
  (org-item-tags-test--with "* H\n- First :a:\n"
    (org-item-tags-test--on
     (let ((org-tags-column 20))
       (org-item-tags-test--line "- First")
       (call-interactively #'org-toggle-heading)
       (should (equal (org-get-tags nil t) '("a")))
       (should (string-match-p "^\\*\\* +First +:a:$" (buffer-string)))
       (should (= (save-excursion (search-forward ":a:") (- (current-column) 3)) 20))))))

(ert-deftest org-item-tags-test-export ()
  "An item with an exclude tag is left out; tags:nil writes no item tags."
  (let ((text "* H\n- keep [[https://e.x][link]] :a:\n  more\n- drop :noexport:\n- plain\n"))
    (org-item-tags-test--on
     (org-item-tags-test--with text
       (let ((out (org-export-as 'md nil nil t '(:with-toc nil :section-numbers nil))))
         (should (string-match-p "keep \\[link\\](https://e.x) :a:" out))
         (should-not (string-match-p "drop" out))
         (should (string-match-p "plain" out))))
     (org-item-tags-test--with (concat "#+options: tags:nil\n" text)
       (let ((out (org-export-as 'md nil nil t '(:with-toc nil :section-numbers nil))))
         (should (string-match-p "keep \\[link\\](https://e.x)\n" out))
         (should-not (string-match-p ":a:" out)))))))

(ert-deftest org-item-tags-test-font-lock ()
  "Tags on an item's first line are shown in `org-tag'; others are not."
  (org-item-tags-test--with "* H\n- item :a:\n  more :b:\n"
    (org-item-tags-test--on
     (org-item-tags--font-lock t)
     (font-lock-ensure)
     (search-forward ":a:")
     (should (memq 'org-tag (ensure-list (get-text-property (1- (point)) 'face))))
     (search-forward ":b:")
     (should-not (memq 'org-tag (ensure-list (get-text-property (1- (point)) 'face)))))))

(ert-deftest org-item-tags-test-mode-off ()
  "With the mode off, Org's commands are as they were."
  (org-item-tags-test--on)
  (org-item-tags-test--with "* H\n** First :a:\n"
    (let ((org-adapt-indentation nil))
      (org-item-tags-test--line "** First")
      (org-toggle-item nil)
      (should (equal (buffer-string) "* H\n- First\n")))))

;;; org-item-tags-test.el ends here
