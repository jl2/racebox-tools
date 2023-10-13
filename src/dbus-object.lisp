;; dbus-object.lisp

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

(defun publish-example ()
  (handler-case
      (dbus:with-open-bus (bus (dbus:session-server-addresses))
        (format t "Bus connection name: ~A~%" (dbus:bus-name bus))
        (dbus:publish-objects bus))
    (end-of-file ()
      :disconnected-by-bus)))

(dbus:define-dbus-object racebox-service
  (:path "/"))

(dbus:define-dbus-method (racebox-service start-recording) () (:string)
  (:interface "org.jl2.RaceBoxRecorder")
  (format nil "Starting recorder."))

(dbus:define-dbus-method (racebox-service stop-recording) () (:string)
  (:interface "org.jl2.RaceBoxRecorder")
  (format nil "Stopping recorder."))

(dbus:define-dbus-method (racebox-service database-filenames) () (:array :string)
  (:interface "org.jl2.RaceBoxRecorder")
  ;;(mapcar #'file-namestring (uiop:directory-files (asdf:system-relative-pathname :racebox-tools "databases/") "*.db"))
  (format nil "(~{~s~^ ~}" (mapcar #'file-namestring (uiop:directory-files (asdf:system-relative-pathname :racebox-tools "databases/") "*.db")))
  )

(dbus:define-dbus-method (racebox-service introspect) () (:string)
  (:interface "org.freedesktop.DBus.Introspectable")
  (uiop:read-file-string (asdf:system-relative-pathname :racebox-tools "service.xml")))

(defun is-recording ()
  nil)

(defun database-file-count ()
  (length (uiop:directory-files (asdf:system-relative-pathname :racebox-tools "databases/") "*.db")))

(dbus:define-dbus-method (racebox-service get) ((interface :string)
                                                (name :string))
    (:string)
  (:interface "org.freedesktop.DBus.Properties")
  (cond ((and (string= interface "org.jl2.RaceBoxRecorder")
              (string= name "Recording"))
         (if (is-recording) "True" "False"))
        ((and (string= interface "org.jl2.RaceBoxRecorder")
              (string= name "DatabaseFileCount"))
         (format nil "~a" (database-file-count)))))

(dbus:define-dbus-method (racebox-service get) ((interface :string)) (:array :variant)
  (:interface "org.freedesktop.DBus.Properties")
  (when (string= interface "org.jl2.RaceBoxRecorder")
    (list (list "Recording" (if (is-recording) "True" "False"))
          (list "DatabaseFileCount" (format nil "~a" (database-file-count))))))

(dbus:define-dbus-signal-handler (racebox-service on-signal)
    ((s :string))
  (:interface "org.jl2.RaceBoxRecorder")
  (format t "Got signal with arg ~S~%" s))

;; (defparameter *mt* (bt:make-thread #'publish-example))

;; (dbt:inspect-introspected-object :system "org.jl2.RaceboxRecorder" "/org/jl2/RaceboxRecorder")
