;; package.lisp

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

(defpackage :racebox-tools

  (:nicknames :rbt)

  (:use #:cl #:alexandria #:3d-vectors #:dbus-tools)
  (:export #:list-racebox-devices
           #:is-racebox-device
           #:first-racebox-device

           #:connect
           #:disconnect

           #:read-metadata

           #:read-current-value

           #:read-raw-value

           #:decode-packet
           #:to-gps-message

           #:rbm-battery-status

           #:rbm-timestamp
           #:rbm-latitude
           #:rbm-longitude


           #:rbm-speed
           #:rbm-heading

           #:rmb-wgs-altitude
           #:rbm-msl-altitude

           #:rbm-g-force-x
           #:rbm-g-force-y
           #:rbm-g-force-z

           #:rbm-rotation-rate-x
           #:rbm-rotation-rate-y
           #:rbm-rotation-rate-z
           
           #:rbm-fix-status
           #:rbm-validity
           #:rbm-accuracy
           #:rbm-status-flags-valid-fix
           #:rbm-status-flags-differential-corrections-applied
           #:rbm-status-flags-power-state
           #:rbm-status-flags-valid-heading
           #:rbm-status-flags-carrier-phase-range-solution

           #:rbm-datetime-flags-available-confirmation-of-date-time
           #:rbm-datetime-flags-confirmed-utc-date-validity
           #:rbm-datetime-flags-confirmed-utc-time-validity

           #:rbm-lat-lon-valid
           #:rbm-lat-lon-correction-age

           #:gps-message
           #:timestamp
           #:longitude
           #:latitude
           #:msl-altitude
           #:wgs-altitude
           #:speed
           #:heading
           #:g-force
           #:rotation

           #:gps-message-timestamp
           #:gps-message-longitude
           #:gps-message-latitude
           #:gps-message-msl-altitude
           #:gps-message-wgs-altitude
           #:gps-message-speed
           #:gps-message-heading
           #:gps-message-g-force
           #:gps-message-rotation

           #:create-db
           #:get-data-base-filename
           #:insert-message
           #:main

           #:*database-directory*
           #:db-to-gpx
           ;; Not yet implemented...
           ;; #:read-csv-stream
           ;; #:read-csv-file

           ;; #:read-vbo-stream
           ;; #:read-vbo-file

           ;; #:read-gpx-stream
           ;; #:read-gpx-file
           ))
