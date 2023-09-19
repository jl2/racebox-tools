;; database.lisp

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

(defparameter *database-directory*
  (asdf:system-relative-pathname :racebox-tools
                                 "databases/")
  "Directory where Sqlite database files will be saved.")

(defun create-db (db)
  (loop :for sql-file :in (uiop:directory-files (asdf:system-relative-pathname :racebox-tools "sql/")
                                                "create_table*.sql")
        :for the-sql = (alexandria:read-file-into-string sql-file)
        :do
           (sqlite:execute-non-query db the-sql)))

(defgeneric insert-message (db msg)
  (:documentation "Add msg to db."))

(defmethod insert-message (db (msg racebox-mini-data-message))
  (with-slots (itow
               year
               month
               day

               hour
               minute
               second

               validity-flags
               time-accuraccy
               nanosecond

               fix-status
               fix-status-flags
               date-time-flags
               number-of-svs

               longitude
               latitude

               wgs-altitude
               msl-altitude

               horizontal-accuracy
               vertical-accuracy

               speed
               heading

               speed-accuracy
               heading-accuracy

               pdop

               lat-lon-flags

               battery-status

               g-force-x
               g-force-y
               g-force-z

               rotation-rate-x
               rotation-rate-y
               rotation-rate-z
               ) msg

    (sqlite:execute-non-query
     db
     "INSERT INTO raw_data (
     itow,

     year,
     month,
     day,

     hour,
     minute,
     second,

     validity_flags,
     time_accuraccy,
     nanosecond,

     fix_status,
     fix_status_flags,
     date_time_flags,
     number_of_svs,

     longitude,
     latitude,

     wgs_altitude,
     msl_altitude,

     horizontal_accuracy,
     vertical_accuracy,

     speed,
     heading,

     speed_accuracy,
     heading_accuracy,

     pdop,

     lat_lon_flags,

     battery_status,

     g_force_x,
     g_force_y,
     g_force_z,

     rotation_rate_x,
     rotation_rate_y,
     rotation_rate_z
) values
 (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
     itow

     year
     month
     day

     hour
     minute
     second

     validity-flags
     time-accuraccy
     nanosecond

     fix-status
     fix-status-flags
     date-time-flags
     number-of-svs

     longitude
     latitude

     wgs-altitude
     msl-altitude

     horizontal-accuracy
     vertical-accuracy

     speed
     heading

     speed-accuracy
     heading-accuracy

     pdop

     lat-lon-flags

     battery-status

     g-force-x
     g-force-y
     g-force-z

     rotation-rate-x
     rotation-rate-y
     rotation-rate-z))
  (sqlite:last-insert-rowid db))

(defmethod insert-message (db (msg gps-message))
  (with-slots (timestamp
               longitude
               latitude
               msl-altitude
               wgs-altitude
               speed
               heading
               g-force
               rotation
               raw-id) msg
    (sqlite:execute-non-query
     db
     "INSERT INTO gps_message (
  timestamp,
  longitude,
  latitude,
  msl_altitude,
  wgs_altitude,
  speed,
  heading,
  g_force_x,
  g_force_y,
  g_force_z,
  rotation_x,
  rotation_y,
  rotation_z,
  raw_id
) values (?,?,?,?,?,?,?,?,?,?,?,?,?,?)
"
     (local-time:format-timestring nil timestamp)
     longitude
     latitude
     msl-altitude
     wgs-altitude
     speed
     heading
     (vx g-force)
     (vy g-force)
     (vz g-force)
     (vx rotation)
     (vy rotation)
     (vz rotation)
     raw-id))

  (sqlite:last-insert-rowid db))

(defun get-database-filename ()
  "Return a database filename with a timestamp."
  (make-pathname :defaults *database-directory*
                 :name (format nil "databases/racebox-~a" (local-time:now))
                 :type "db"))
