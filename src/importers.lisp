;; importers.lisp

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

(defun read-csv-stream (stream)
  "Read a stream of CSV data downloaded from the RaceBox service.
Not yet implemented."
  (cl-csv:read-csv stream))

(defun read-csv-file (file-name)
  "Read a CSV data file downloaded from the RaceBox service.
Not yet implemented."
  (with-input-from-file (inf file-name)
    (read-csv-stream inf)))


(defun read-vbo-stream (stream)
  "Read a stream of CSV data downloaded from the RaceBox service.
Not yet implemented."
  stream)

(defun read-vbo-file (file-name)
  "Read a CSV data file downloaded from the RaceBox service.
Not yet implemented."
  (with-input-from-file (inf file-name)
    (read-vbo-stream inf)))


(defun read-gpx-stream (stream)
  "Read a stream of GPX data downloaded from the RaceBox service.
Not yet implemented."
  stream)

(defun read-gpx-file (file-name)
  "Read a GPX data file downloaded from the RaceBox service.
Not yet implemented."
  (with-input-from-file (inf file-name)
    (read-gpx-stream inf)))
