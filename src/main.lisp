;; main.lisp

;; Copyright (c) 2023 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :racebox-tools)

(defun main (args)
  (declare (ignorable args))
  (let ((db-name (get-database-filename)))
    (format t "db-name: ~a~%" db-name)
    (sqlite:with-open-database (db db-name)
      (create-db db))
    (unwind-protect
         (handler-case
             (progn
               (connect)
               (loop
                 :do
                    (sqlite:with-open-database (db db-name)
                      (let* ((raw-value (read-current-value))
                             (gps-value (to-gps-message raw-value)))
                        (with-slots (raw-id) gps-value
                          (setf raw-id (insert-message db raw-value))
                          (insert-message db gps-value))))
                    (sleep (/ 1 25))))
           (error (err)
             (format t "Received error: ~a~%Quitting GPS logger.~%~%" err)))
      (disconnect))
    0))
