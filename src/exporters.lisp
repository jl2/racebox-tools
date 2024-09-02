;; exporters.lisp

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

(defun format-iso (tm)
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
      (decode-universal-time tm)
    (declare (ignore dow dst-p tz)
             (type fixnum yr mon day hr min sec dow))
    (format nil
            "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            yr     mon    day    hr     min    sec)))

(defun db-to-gpx (db-filename gpx-filename)
  (sqlite:with-open-database (db db-filename)
    (alexandria:with-output-to-file (stream gpx-filename)
      (format stream
              "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<gpx version=\"1.0\"
     creator=\"gpxtools\"
     xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
     xmlns=\"http://www.topografix.com/GPX/1/0\"
     xsi:schemaLocation=\"http://www.topografix.com/GPX/1/0 http://www.topografix.com/GPX/1/0/gpx.xsd\">~%")

      (format stream "  <time>~a</time>~%" (sqlite:execute-single db
                                                                  "select timestamp from gps_message order by timestamp limit 1"))
      (format stream "  <trk>~%")
      (format stream "    <name>~a</name>~%" db-filename)
      (format stream "    <trkseg>~%")

      (loop
        :with statement = (sqlite:prepare-statement db
                                                    "select latitude, longitude, wgs_altitude, timestamp
                                                       from gps_message
                                                      order by timestamp")

        :while (sqlite:step-statement statement)
        :do
           (format stream
                   "      <trkpt lat=\"~,9f\" lon=\"~,9f\">~%"
                   (sqlite:statement-column-value statement 0)
                   (sqlite:statement-column-value statement 1))
           (format stream
                   "        <ele>~,9f</ele>~%"
                   (sqlite:statement-column-value statement 2))
           (format stream
                   "        <time>~a</time>~%"
                   (sqlite:statement-column-value statement 3))
           (format stream
                   "      </trkpt>~%")
        :finally
           (sqlite:finalize-statement statement))

      (format stream "    </trkseg>~%")
      (format stream "  </trk>~%")
      (format stream "</gpx>~%"))))
