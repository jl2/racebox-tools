;; racebox-tools.asd

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

(asdf:defsystem #:racebox-tools
  :description "Describe racebox-tools here"
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license  "ISC"
  :version "0.0.1"
  :serial t

  :depends-on (
               #:alexandria
               #:babel
               #:binary-types
               #:cl-csv
               #:cl-ppcre
               #:dbus
               #:local-time
               #:local-time-duration
               #:utm
               #:sqlite
               #:dbus-tools
               #:bluetooth-tools
               #:3d-vectors
               #:uiop
               )

  :components ((:module "src"
                        :components
                        ((:file "package")
                         (:file "dbus")
                         (:file "dbus-object")
                         (:file "messages")
                         (:file "importers")
                         (:file "exporters")
                         (:file "database")
                         (:file "main"))))

  :in-order-to ((test-op (test-op racebox-tools.test))))
