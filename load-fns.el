;;; load-fns.el --- Library loading functions

;; Copyright (C) 1991, 92, 93, 94, 95, 96, 97, 98, 99, 00, 04, 05, 06, 2007 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: load-fns.el,v 1.8 2007/08/09 07:07:38 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 51 Franklin Street, Fifth Floor; Boston, MA 02110-1301, USA.

;;; Commentary:
;;; Code:

;;;###autoload
(defvar load-offer-compile-dynamic-p nil
  "*If non-nil, `load-offer-compile' will compile for dynamic loading.
This value of this variable is temporarily bound to both
`byte-compile-dynamic' and `byte-compile-dynamic-docstrings' while
compiling.")

;;;###autoload
(defvar load-offer-compile-default-action 'query
  "*Default action to take for files which need compiling.
When the function `load-offer-compile' determines that a bytecode file is
missing or out of date with respect to the source file, it will use the
value of this variable to determine what to do.

If the value is the symbol `query', `ask', or is nil, interactively prompt
 user with a menu of choices.

If the value is `skip', do not load the file at all.

If the value is `compile-and-load', compile the source file and then load
 the compiled program.

If the value is `load-and-compile', load the source file, then compile the
 program.  This is rarely useful.

If the value is `load-source', just load the source file without compiling
 anything.

If the value is `load-compiled', load the already-compiled file \(which may
 be out of date\).  If the compiled file doesn't exist, the source file
 will be loaded but no compilation will be done.")

;;;###autoload
(defvar load-offer-compile-check-directory-function
  'load-offer-compile-check-directory
  "*Function to run to check for missing bytecode directory.
Function takes one argument, a directory name.
If function returns nil, no attempt will be made to compile files.

This hook exists so that the missing directories can be optionally created.")

;;;###autoload
(defvar load-offer-compile-check-directory-mkdir-p 'query
  "*Action to take if bytecode directory does not exist.
If a directory specified to `load-offer-compile-check-directory' does not
exist, this variable is consulted to decide what to do.

If value is `t', create directory.
If value is nil, do not create directory; source files whose bytecode
 files go into that directory cannot be compiled.
For any other value, query user interactively.")

;; This is not a user option.
;; If user is asked to create a bytecode directory and declines, that
;; directory name is added to this list so that they are never asked about
;; it again during this session.
(defvar load-offer-compile-check-directory-declined nil)

;; This is not a user option.
;; This is used by require-offer-compile to change how the file is loaded.
(defvar load-offer-compile-load-action 'load)


;;;###autoload
(defun load-offer-compile (file &rest load-flags)
  "Load FILE, offering \(interactively\) to compile if source file is newer.
Any additional args to this function are passed to `load'.  If called
interactively, completion on library names in `load-path' is available."
  (interactive (list (read-load-library)))
  (and (eq 'require load-offer-compile-load-action)
       (symbolp file)
       (setq file (symbol-name file)))
  (let ((debug-on-error t)
        (match-data (match-data))
        (path load-path)
        (action load-offer-compile-default-action)
        (compile-after-load nil)
        (after-load-forms nil)
        (load-result nil)
        (load-file (if (and (eq 'require load-offer-compile-load-action)
                            (stringp (car load-flags)))
                       (prog1
                           (car load-flags)
                         (setq load-flags (cdr load-flags)))
                     file))
        (load-filec nil))
    ;; Canonicalize file name.
    (cond ((string-match "/" load-file))
          (t
           (setq load-file (concat load-file ".el"))
           (while path
             (if (file-exists-p (concat (car path) "/" load-file))
                 (setq load-file (concat (car path) "/" load-file)
                       path nil)
               (setq path (cdr path))))))
    (cond ((and (file-exists-p load-file)
                (progn
                  (setq load-filec (byte-compile-dest-file load-file))
                  (and (funcall load-offer-compile-check-directory-function
                                (file-name-directory load-filec))
                       (file-newer-than-file-p load-file load-filec))))
           (and (memq action '(query ask nil))
                (setq action (load-offer-compile-choice load-file load-filec)))
           (cond ((eq action 'skip)
                  (setq load-file nil))
                 ((eq action 'compile-and-load)
                  (load-offer-do-compilation load-file)
                  (setq load-file load-filec))
                 ((eq action 'load-and-compile)
                  (setq compile-after-load t))
                 ((eq action 'load-source))
                 ((and (eq action 'load-compiled)
                       (file-exists-p load-filec))
                  (setq load-file load-filec))))
          (t
           (setq load-file file)))

    (cond (load-file
           (cond ((eq load-offer-compile-load-action 'load)
                  (setq load-result (apply 'load load-file load-flags)))
                 ((eq load-offer-compile-load-action 'require)
                  (setq load-result
                        (require (intern file) load-file))))

           ;; We may not have loaded exactly the file name specified to
           ;; this function, and since the match in after-load-alist must
           ;; be exact, try matching non-absolute entries if we in fact
           ;; loaded a file by a different name, then evaluate the forms in
           ;; after-load-alist manually.
           (cond ((eq load-file file));; already evaluated
                 ((not (boundp 'after-load-alist)));; nothing to eval
                 ((and (fboundp 'load-history-regexp)
                        (assoc (load-history-regexp file) after-load-alist)))
                 ((assoc load-file after-load-alist));; already evaluated
                 (t
                  (eval-after-load-forms-for load-file)))))

    (and compile-after-load
         load-file
         (load-offer-do-compilation load-file))

    (store-match-data match-data)
    load-result))

;; Check to see if directory exists and if not, query for its creation.
;; Remember which directories the user has declined to create so we do not
;; repeatedly ask the same questions every time this function is invoked.
(defun load-offer-compile-check-directory (dir)
  (cond ((file-exists-p dir))
        ((eq load-offer-compile-check-directory-mkdir-p t)
         (make-directory dir t))
        ((eq load-offer-compile-check-directory-mkdir-p nil) nil)
        ((member dir load-offer-compile-check-directory-declined) nil)
        ((let ((ans nil)
               (edir dir))
           (while (not (file-exists-p edir))
             (setq edir (file-name-directory (directory-file-name edir))))
           (load-offer-compile-query "*Bytecode directory missing*"
             (format "%s:\n\n\t%s\n\n%s:\n\n\t%s\n\n%s, %s."
                     "Missing bytecode directory"
                     (directory-file-name dir)
                     "Deepest existing directory is"
                     (directory-file-name edir)
                     "If bytecode directory is not created"
                     "some files cannot be compiled")
             "Create missing directories? "
             (lambda (c)
               (cond ((char-equal c ?y) (setq ans t)   nil)
                     ((char-equal c ?n) (setq ans nil) nil)
                     (t 'retry))))
           ans)
         (make-directory dir t)
         t)
        (t
         ;; User said no.  Do not ask about this directory again.
         (setq load-offer-compile-check-directory-declined
               (cons dir load-offer-compile-check-directory-declined))
         nil)))

;; Ask user what to do about a file which needs compiling.
;; Return value is like that of `load-offer-compile-default-action'.
(defun load-offer-compile-choice (load-file load-filec)
  (let* ((abbrev (if (fboundp 'abbreviate-file-name)
                     'abbreviate-file-name
                   'identity))
         (init  (file-name-nondirectory load-file))
         (initc (concat init "c"))
         (init-dir  (funcall abbrev (file-name-directory load-file)))
         (init-dirc (funcall abbrev (file-name-directory load-filec)))
         (result nil))
    (load-offer-compile-query "*Load Init File*"
      (format "%s\n\n%s\n%s\n%s\n%s\n%s"
              (cond ((string= init-dir init-dirc)
                     (format "In directory %s,\n%s." init-dir
                             (if (file-exists-p load-filec)
                                 (format "%s is older than %s" initc init)
                               (format "%s is not byte-compiled" init))))
                    ((file-exists-p load-filec)
                     (format "The file %s%s\nis older than %s%s."
                             init-dirc initc init-dir init))
                    (t (format "%s is not byte-compiled." init)))
              (format "0\tSkip %s entirely." file)
              (format "1\tCompile %s and then load it." initc)
              (format "2\tLoad %s, then compile %s." init initc)
              (format "3\tJust load %s." init)
              (if (file-exists-p load-filec)
                  (format "4\tJust load %s (which is out of date)." initc)
                ""))
      (format "What to do about %s? " file)
      (function
       (lambda (c)
         (cond ((char-equal c ?0) (setq result 'skip)             nil)
               ((char-equal c ?1) (setq result 'compile-and-load) nil)
               ((char-equal c ?2) (setq result 'load-and-compile) nil)
               ((char-equal c ?3) (setq result 'load-source)      nil)
               ((and (char-equal c ?4)
                     (file-exists-p load-filec))
                (setq result 'load-compiled)
                nil)
               (t 'retry)))))
    result))

;; Display a buffer with a temporary message, display an echo-area prompt,
;; read a character of input.  Run action on read character.
;; If function returns t, another character of input is read.
;; inhibit-quit is set so that C-g may be read.
(defun load-offer-compile-query (tmpbuf-name tmpbuf-msg prompt action)
  (let ((winconfig (current-window-configuration))
        (buf (with-output-to-temp-buffer tmpbuf-name
               (princ tmpbuf-msg) standard-output))
        (cursor-in-echo-area t)
        (inhibit-quit t)
        (loop t)
        c)
    (unwind-protect
        (while loop
          (message prompt)
          (setq c (read-char))
          (setq quit-flag nil)
          (setq loop (cond ((char-equal c ?\C-g)
                            (signal 'quit '("Well fuck you then."))
                            nil)
                           ((funcall action c))))
          (cond ((eq loop 'retry)
                 (message "Invalid response.")
                 (sit-for 2))))
      (kill-buffer buf)
      (set-window-configuration winconfig)))
  (if inhibit-quit
      (error "inhibit-quit is set!")))

(put 'load-offer-compile-query 'lisp-indent-function 1)

(defun load-offer-do-compilation (file)
  (let ((byte-compile-dynamic            load-offer-compile-dynamic-p)
        (byte-compile-dynamic-docstrings load-offer-compile-dynamic-p))
    (byte-compile-file file)))


(defun library-loaded-p (lib &optional subfeature)
  (save-match-data
    (cond ((symbolp lib)
           (if subfeature
               (featurep lib subfeature) ; emacs 22 or later
             (featurep lib)))

          ((not (boundp 'load-history)) nil)
          ((null load-history)          nil)
          ((and (fboundp 'load-history-filename-element)
                (load-history-filename-element (load-history-regexp lib)))
           t)
          ((assoc lib load-history)
           t)

          ;; library name has a `/' char: look for any matching entry that
          ;; ends in that path, with or without .el(|c) extension.
          ;; n.b. Emacs 22 always records the whole name of the loaded file.
          ((string-match "/" lib)
           (let* ((re (concat "/" (regexp-quote lib) "\\'"))
                  (lhe (delq nil (mapcar 'car load-history)))
                  (lh  (mapcar 'file-name-sans-extension lhe))
                  (foundp nil))
             (while lhe
               (if (or (string-match re (car lhe))
                       (string-match re (car lh)))
                   (setq foundp t
                         lhe nil)
                 (setq lhe (cdr lhe)
                       lh (cdr lh))))
             foundp))

          ((member lib
            (mapcar (lambda (l)
                      (and (car l)
                           (file-name-sans-extension
                            (file-name-nondirectory (car l)))))
                    load-history))
           t))))

;;;###autoload
(defun add-after-load-alist (key &rest forms)
  "Add all FORMS to KEY in `auto-load-alist' that aren't already present.
If KEY is not in auto-load-alist, it and all FORMS are added.
If KEY is library which is already loaded, immediately evaluate all of the
forms which were not already present."
  (let* ((ala-key (if (and (stringp key)
                           (fboundp 'load-history-regexp))
                      (load-history-regexp key)
                    key))
         (node (assoc ala-key after-load-alist))
         (new nil))
    (cond (node
           (while forms
             (or (member (car forms) (cdr node))
                 (setq new (cons (car forms) new)))
             (setq forms (cdr forms)))
           (cond (new
                  (setq new (nreverse new))
                  (nconc (cdr node) new))))
          (t
           (setq after-load-alist
                 (cons (apply 'list ala-key forms) after-load-alist))
           (setq new forms)))
    ;; Eval new forms now if library was already loaded.
    (if (library-loaded-p key)
        (while new
          (eval (car new))
          (setq new (cdr new))))))

;; indent like `while'.
(put 'add-after-load-alist 'lisp-indent-function 1)

;;;###autoload
(defun add-forms-to-after-load-alist (forms)
  (mapc (lambda (f)
          (apply 'add-after-load-alist f))
    forms))

;; indent like save-excursion
(put 'add-forms-to-after-load-alist 'lisp-indent-function 0)

;;;###autoload
(defun eval-after-load-forms-for (load-file)
  "Evaluate forms in `after-load-alist' for LOAD-FILE.
First search for load-file literally; then try load-file name sans any
directory specification; then try name sans any directory name or
extension.

LOAD-FILE may also be a symbol, in which case the forms for that symbol
will be evaluated.  Such forms are typically executed when a
feature is `provide'd."
  (cond ((not (boundp 'after-load-alist)) nil)
        ((symbolp load-file)
         (eval (cons 'progn (cdr (assq load-file after-load-alist)))))
        ((fboundp 'load-history-regexp)
         (let ((alist after-load-alist)
               alist-elt)
           (while alist
             (setq alist-elt (car alist)
                   alist     (cdr alist))
             (when (and (stringp (car alist-elt))
                        (string-match (car alist-elt) load-file))
               (eval (cons 'progn (cdr alist-elt)))
               (setq alist nil)))))
        ((stringp load-file)
         (eval (cons 'progn
                     (cdr (or (assoc load-file after-load-alist)
                              (assoc (setq load-file
                                           (file-name-nondirectory load-file))
                                     after-load-alist)
                              (assoc (file-name-sans-extension load-file)
                                     after-load-alist))))))))

;;;###autoload
(defun load-libraries-with-debugging-if-exist (&rest libs)
  (let ((__debug-on-error__save__ debug-on-error))
    (unwind-protect
        (while libs
          ;; Make sure debug-on-error is reset for each file, in case one
          ;; of them resets it.
          (setq debug-on-error t)
          (if (stringp (car libs))
              (load (car libs) t)
            (eval (car libs)))
          (setq libs (cdr libs)))
      (setq debug-on-error __debug-on-error__save__))))

(put 'load-libraries-with-debugging-if-exist 'lisp-indent-function 0)

;;;###autoload
(defun override-autoload (fn file &rest autoload-args)
  "Override any previous autoload and re-autoload.
If the function is already defined \(i.e. it is not currently an
autoload\), it is not changed."
  (cond ((or (not (fboundp fn))
             (and (consp (symbol-function fn))
                  (eq 'autoload (car (symbol-function fn)))))
         (fmakunbound fn)
         (apply 'autoload fn file autoload-args))))

;;;###autoload
(defun override-autoloads (alist &rest autoload-args)
  "Override any previous autoloads and re-autoload.
Each argument should be a cons in which the car consists of the function
name, and the cdr is the file to load it from.
If the function is already defined \(i.e. it is not currently an
autoload\), it is not changed."
  (while alist
    (apply 'override-autoload
           (car (car alist)) (cdr (car alist)) autoload-args)
    (setq alist (cdr alist))))

(put 'override-autoloads 'lisp-indent-function 0)

(defun read-load-library (&optional prompt)
  "Read an emacs library file name.
Offer completion via the `fff-elisp' package if available,
or else via `locate-file-completion' if that is available."
  (or prompt
      (setq prompt "Load library: "))
  (cond ((featurep 'fff-elisp)
         (fff-completing-read-emacs-lisp-library prompt))
        ((fboundp 'locate-file-completion)
         (completing-read prompt 'locate-file-completion
                          (cons load-path load-suffixes)))
        (t (read-string prompt))))

;;;###autoload
(defun require-soft (feature &optional file)
  "Try to require FEATURE, but don't signal an error if require fails."
  ;; Check that feature isn't already present, because condition-case is
  ;; too much overhead to bother with unless it's necessary.
  (or (featurep feature)
      (condition-case nil
          (require feature file)
        (error nil))))

;;;###autoload
(defun require-offer-compile (feature &optional filename noerror)
  "Require FEATURE, offering compilation via `load-offer-compile'."
  (if (featurep feature)
      feature
    (cond ((and (boundp 'byte-compile-current-file)
                byte-compile-current-file)
           ;; The byte compiler cannot interrupt the compilation of one
           ;; file to compile another.  So if this form is evaluated during
           ;; compilation, just require the other file using regular
           ;; mechanics.
           (if noerror
               (condition-case nil
                   (require feature filename)
                 (error nil))
             (require feature filename)))
          (t
           (let ((load-offer-compile-load-action 'require))
             (if noerror
                 (condition-case nil
                     (load-offer-compile feature filename)
                   (error nil))
               (load-offer-compile feature filename)))))))

;; This hook enables `require-offer-compile' forms to be treated like
;; `require', i.e. the indicated libraries are loaded at compile time to
;; pick up any macro definitions.
(put 'require-offer-compile 'byte-hunk-handler 'byte-compile-file-form-require-offer-compile)
(defun byte-compile-file-form-require-offer-compile (form)
  (apply 'require-offer-compile (mapcar 'eval (cdr form)))
  (byte-compile-keep-pending form 'byte-compile-normal-call))

;; This is also defined in files.el, but the version here offers completion
;; on available library names.
;;;###autoload
(defadvice load-library (around load-fns:completion (library) activate)
  "Use `read-load-library' in order to offer completion."
  (interactive (list (read-load-library)))
  (load library))

(provide 'load-fns)

;;; load-fns.el ends here.
