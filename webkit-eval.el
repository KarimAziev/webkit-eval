;;; webkit-eval.el --- Evaluate javascript in xwidgets -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/webkit-eval
;; Version: 0.1.0
;; Keywords: lisp
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Evaluate javascript or typescript in xwidgets.

;;; Code:

(require 'xwidget)
(defcustom webkit-eval-default-url "https://www.emacswiki.org"
  "Url to open when there are no active session."
  :type 'string
  :group 'webkit-)

(defcustom webkit-eval-elisp-format 'prettier-elisp-to-string
  "Function to format emacs-lisp-code code."
  :type 'function
  :group 'webkit-)

(defvar webkit-eval-edit-src-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'webkit-eval-compile-and-eval)
    (define-key map (kbd "C-c '") 'webkit-eval-compile-typescript-region-or-buffer)
    map)
  "Keymap for `webkit-eval-edit-src-mode'.")

(defvar webkit-eval-result-buffer-name "*webkit-result*")
(defvar-local webkit-eval-cycle-results-alist nil
  "Alist of major modes and corresponding results from evaluating in webkit.")

(defvar webkit-eval-results-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-.") 'webkit-eval-show-next-result)
    map))

(defvar webkit-eval-js-function-wrapper
  "(function (result) {
  const decycle = (obj, seen = [], path = []) => {
  const isObject = (v) => typeof v === 'object';
  const isString = (v) => typeof v === 'string';
  const isArray = (v) => Array.isArray(v);
  const isNumber = (v) => typeof v === 'number';
  const isBigint = (v) => typeof v === 'bigint';
  const isBoolean = (v) => typeof v === 'boolean';

  if (obj === null) {
    return null;
  }
  if (obj === undefined) {
    return undefined;
  }
  if (isNumber(obj) || isBoolean(obj)) {
    return obj;
  }
  if (isString(obj) || isBigint(obj)) {
    return obj.toString();
  }

  if (seen.includes(obj)) {
    return { ':webkit-Circular': path };
  }

  if (isArray(obj)) {
    return obj.map((item, idx) => decycle(item, seen, [...path, idx]));
  }
  if (isObject(obj)) {
    return Object.keys(obj).reduce((acc, key) => {
      const value = obj[key];
      const objPath = [...path, key];
      acc[key] = decycle(value, [...seen, obj], objPath);
      return acc;
    }, {});
  } else {
    return obj;
  }
};

const stringify = (data, replacer, space) => {
  const result = decycle(data);
  return JSON.stringify(result, replacer, space);
};
  return stringify(result, null, 2)
})(%s);")

(defun webkit-eval-get-next-result ()
  "Return cons of (MODE . RESULT)."
  (let* ((alist webkit-eval-cycle-results-alist)
         (current (assoc major-mode alist)))
    (or (seq-find #'cdr
                  (seq-drop (member current
                                    alist)
                            1))
        (seq-find #'cdr
                  (remove current alist)))))

(defun webkit-eval-show-next-result ()
  "Show result in other mode."
  (interactive)
  (when-let ((alist webkit-eval-cycle-results-alist)
             (next (webkit-eval-get-next-result)))
    (when next
      (delay-mode-hooks
        (let ((inhibit-read-only t))
          (delete-region (point-min)
                         (point-max))
          (funcall (car next))
          (save-excursion
            (insert (cdr next)))))
      (setq webkit-eval-cycle-results-alist alist)
      (webkit-eval-results-mode))))

(defun webkit-eval-fontify (content &optional mode-fn &rest args)
  "Fontify CONTENT according to MODE-FN called with ARGS.
If CONTENT is not a string, instead of MODE-FN emacs-lisp-mode will be used."
  (with-temp-buffer
    (delay-mode-hooks
      (apply (or mode-fn 'emacs-lisp-mode) args)
      (goto-char (point-min))
      (insert (if (or (eq major-mode 'emacs-lisp-mode)
                      (not (stringp content)))
                  (pp-to-string content)
                content))
      (font-lock-ensure)
      (buffer-string))))

(defun webkit-eval-get-region ()
  "Return current active region as string or nil."
  (when (and (region-active-p)
             (use-region-p))
    (string-trim (buffer-substring-no-properties
                  (region-beginning)
                  (region-end)))))

(defun webkit-eval-change-file-ext (file new-ext)
  "Replace extension of FILE with NEW-EXT."
  (concat (file-name-sans-extension file) "." new-ext))

(defun webkit-eval-get-mode-by-file-ext (extension)
  "Return corresponding major mode for files with EXTENSION."
  (with-temp-buffer (let ((buffer-file-name
                           (concat
                            (temporary-file-directory)
                            (concat "script-xwidget." extension))))
                      (delay-mode-hooks (set-auto-mode
                                         t)
                                        major-mode))))


(defun webkit-eval-compile-and-eval ()
  "Execute javascript string STR in current webkit session."
  (interactive)
  (save-buffer)
  (let ((compiled
         (or (webkit-eval-wrap-code
              (or (webkit-eval-get-region)
                  (buffer-substring-no-properties (point-min)
                                                  (point-max)))
              nil)
             (or (webkit-eval-get-region)
                 (buffer-substring-no-properties (point-min)
                                                 (point-max))))))
    (xwidget-webkit-execute-script
     (xwidget-webkit-current-session)
     compiled
     (lambda (result)
       (let ((alist `((,(webkit-eval-get-mode-by-file-ext "json") . ,result)
                      (emacs-lisp-mode
                       .
                       ,
                       (funcall webkit-eval-elisp-format
                                (with-temp-buffer
                                  (erase-buffer)
                                  (insert
                                   (string-trim
                                    (or result "")))
                                  (goto-char
                                   (point-min))
                                  (let ((json-array-type
                                         'list)
                                        (json-object-type
                                         'plist))
                                    (or (ignore-errors (or (json-read)))
                                        result)))))
                      (,(webkit-eval-get-mode-by-file-ext "js") . ,compiled))))
         (with-current-buffer (get-buffer-create
                               webkit-eval-result-buffer-name)
           (with-current-buffer-window
               (get-buffer
                webkit-eval-result-buffer-name)
               (cons (or 'display-buffer-in-direction)
                     '((window-height . window-preserve-size)))
               (lambda (window _value)
                 (with-selected-window window
                   (setq buffer-read-only t)
                   (let ((inhibit-read-only t))
                     (when-let ((next
                                 (car
                                  alist)))
                       (funcall (car next))
                       (setq-local webkit-eval-cycle-results-alist
                                   alist)
                       (save-excursion
                         (when (cdr next)
                           (insert (cdr next)))))
                     (webkit-eval-results-mode)))))
           (select-window (get-buffer-window (current-buffer)))))))))

(defun webkit-eval-wrap-code (code &optional compile)
  "Wrap string CODE with immediately Invoked Function Expression.
Also remove export keyword.
If COMPILE is non nil also compile CODE."
  (let ((normalized
         (format "(() => { %s })()"
                 (replace-regexp-in-string
                  "\\_<\\(export\\)\\_>"
                  "" code))))
    (format
     webkit-eval-js-function-wrapper
     (if compile
         (replace-regexp-in-string ";$" ""
                                   (string-trim
                                    (webkit-eval-compile-typescript-string
                                     normalized)))
       normalized))))

(defun webkit-eval-compile-typescript-string (code &rest tsc-args)
  "Compile typescript CODE string with TSC-ARGS."
  (setq tsc-args (flatten-list tsc-args))
  (let* ((temp-file (concat (temporary-file-directory)
                            (make-temp-name "script") ".ts"))
         (outfile (webkit-eval-change-file-ext temp-file "js"))
         (command (string-join
                   (delq nil (append (list "tsc"
                                           (shell-quote-argument
                                            temp-file))
                                     tsc-args))
                   "\s")))
    (when-let ((buff (get-file-buffer outfile)))
      (with-current-buffer buff
        (set-buffer-modified-p nil))
      (kill-buffer buff))
    (let ((inhibit-message t))
      (write-region code nil temp-file)
      (with-temp-buffer
        (shell-command command (current-buffer)
                       (current-buffer))))
    (when (file-exists-p outfile)
      (with-temp-buffer
        (insert-file-contents outfile)
        (buffer-string)))))

;;;###autoload
(defun webkit-eval-compile-typescript-region-or-buffer ()
  "Compile region or buffer with tsc program."
  (interactive)
  (let* ((buff (current-buffer))
         (result (webkit-eval-wrap-code
                  (or (webkit-eval-get-region)
                      (buffer-substring-no-properties
                       (point-min)
                       (point-max)))
                  t)))
    (with-current-buffer (get-buffer-create (concat (buffer-name buff)
                                                    "-compiled.js"))
      (with-current-buffer-window
          (get-buffer-create (concat (buffer-name buff)
                                     "-compiled.js"))
          (cons (or 'display-buffer-in-direction)
                '((window-height . window-preserve-size)))
          (lambda (window _value)
            (with-selected-window window
              (let ((alist
                     `((,(webkit-eval-get-mode-by-file-ext "ts") .
                        ,(webkit-eval-fontify result
                                              'typescript-mode)))))
                (setq buffer-read-only t)
                (let ((inhibit-read-only t))
                  (when-let ((next
                              (car
                               alist)))
                    (funcall (car next))
                    (setq-local webkit-eval-cycle-results-alist
                                alist)
                    (save-excursion
                      (insert (cdr next))))
                  (webkit-eval-results-mode))))))
      (select-window (get-buffer-window (current-buffer))))
    (message "Couldn't compile")))

(defun webkit-eval-get-or-create-edit-buffer (name &optional code)
  "Get or create buffer and file in temporarly directory with NAME.
If CODE is non-nil, replace contents with CODE."
  (let* ((file (concat (temporary-file-directory) name))
         (buff (get-file-buffer file)))
    (if buff
        (with-current-buffer
            buff
          (setq header-line-format
                (substitute-command-keys
                 "\\<webkit-eval-edit-src-map>\
        Use `\\[webkit-eval-compile-and-eval]' to compile and eval, `\\[webkit-eval-compile-typescript-region-or-buffer]' to compile"))
          (unless (and (boundp 'webkit-eval-edit-src-mode)
                       (symbol-value
                        'webkit-eval-edit-src-mode))
            (when code
              (delete-region (point-min)
                             (point-max))
              (insert code))
            (webkit-eval-edit-src-mode))
          (current-buffer))
      (write-region (or
                     code
                     ""
                     (webkit-eval-get-region)
                     "")
                    nil file
                    nil 0)
      (with-current-buffer (find-file-noselect file)
        (setq header-line-format
              (substitute-command-keys
               "\\<webkit-eval-edit-src-map>\
        Use `\\[webkit-eval-compile-and-eval]' to compile and eval, `\\[webkit-eval-compile-typescript-region-or-buffer]' to compile"))
        (current-buffer)))))

(define-minor-mode webkit-eval-results-mode
  "Minor mode for displaying results in xwidgets.
Results are available in json format and in `emacs-lisp-mode'.

Use command `webkit-eval-show-next-result' to cycle between this formats
and view executed."
  :lighter " webkit-result"
  :keymap webkit-eval-results-map
  :global nil
  (setq header-line-format
        (substitute-command-keys
         (concat "\\<webkit-eval-results-map>\
        Use `\\[webkit-eval-show-next-result]' to show result in "
                 (when-let ((next
                             (car-safe
                              (webkit-eval-get-next-result))))
                   (when (symbolp next)
                     (symbol-name next))))))
  (use-local-map
   (let ((map
          (copy-keymap
           webkit-eval-results-map)))
     (make-composed-keymap (list map)
                           (current-local-map)))))

(define-minor-mode webkit-eval-edit-src-mode
  "Minor mode for editing and evaluating typescript code in webkit session.

It is turned on after command `webkit-eval-src-edit'.

\\<\\<webkit-eval-edit-src-map>\\{webkit-eval-edit-src-map}>."
  :lighter " webkit-edit"
  :keymap webkit-eval-edit-src-map
  :global nil
  (when webkit-eval-edit-src-mode
    (setq buffer-read-only nil)
    (set-buffer-modified-p nil)
    (use-local-map
     (let ((map (copy-keymap
                 webkit-eval-edit-src-map)))
       (use-local-map (make-composed-keymap (list map)
                                            (current-local-map)))))))

;;;###autoload
(defun webkit-eval-src-edit ()
  "Create temporary typescript file for evaluating it in xwidget-webkit session.
\\<webkit-eval-edit-src-map>
When done, exit with `\\[webkit-eval-compile-and-eval]'."
  (interactive)
  (unless (xwidget-webkit-current-session)
    (xwidget-webkit-browse-url (or webkit-eval-default-url
                                   (read-string "Url: " "https://"))))
  (when-let ((buff (webkit-eval-get-or-create-edit-buffer "script-xwidget.ts")))
    (if-let ((window (get-buffer-window buff)))
        (select-window window)
      (pop-to-buffer buff))))

(provide 'webkit-eval)
;;; webkit-eval.el ends here