;; Copyright (C) 2016 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'comint)
(require 'load-relative)

(require-relative-list '("../common/buffer/helper")
		       "realgud-")
(declare-function realgud-get-cmdbuf   'realgud-buffer-helper)

(defun realgud:backend-complete ()
  "Send a command to the command buffer and parse the output.

Use the `comint-redirect-send-command-to-process' function to
send a issue a debugger completion command. We pass back a the
positions in the command buffer for the prefix text along with
prossible completions. For example:

  (881 887 (\"record\" \"refresh\" ...))

The right debugger command to use is given in the regexp-hash of
`realgud-cmdbuf-info' under the key `complete-cmd-template'.
The template should have a %s placeholder for the prefix to complete. The default
value for `complete-cmd-template' is 'complete %s' as used by
gdb and other debuggers.
"
  (interactive)
  (let* ((buffer (current-buffer))
	 (cmdbuf (realgud-get-cmdbuf))
	 (process (get-buffer-process (current-buffer)))
	 (start-pos (save-excursion (comint-goto-process-mark) (point)))
	 (end-pos (point))
	 (len (- end-pos start-pos))
	 (regexp-hash
	  (and (realgud-cmdbuf-info? realgud-cmdbuf-info)
	       (realgud-sget 'cmdbuf-info 'regexp-hash)))
	 (complete-command-template
	  (or (and regexp-hash
		   (gethash "complete-cmd-template" regexp-hash))
	      "complete %s"))
	 )

    ;; get the input string
    (when (> end-pos start-pos)
      (let* ((input-str (buffer-substring-no-properties start-pos end-pos))
             (command-str (format complete-command-template input-str))
             (output-str (with-temp-buffer
                           (comint-redirect-send-command-to-process
                            command-str (current-buffer) process nil t)
                           ;; Wait for the process to complete
                           (with-current-buffer (process-buffer process)

                             (while (null comint-redirect-completed)
                               (accept-process-output nil 0 5))) ;; wait 5ms

                           (buffer-substring (point-min) (1- (point-max)))))
             (output-values (split-string output-str "\n"))
             (prefix (car output-values)))
        (list (- end-pos len) end-pos (cdr output-values))))))

(defun realgud:completion-at-point ()
  (let ((completion-results (realgud:backend-complete)))
    (when completion-results
      (list (nth 0 completion-results)
            (nth 1 completion-results)
            (nth 2 completion-results)
            :exclusive 'yes))))

(provide-me "realgud-")
