;; racebox-tools.lisp

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

(defun first-racebox ()
  "Return the name of the first RaceBox Mini device found."
  (dbt:managed-object-name (first (list-racebox-devices))))

(defun is-racebox-device (object)
  "Check if a bluetooth object is a RaceBox Mini device."
  (and (dbt:is-bluetooth-device object)
       (let* ((attributes (dbt:managed-object-value object))
              (properties (dbt:get-value attributes "org.bluez.Device1"))
              (name (get-value properties "Name")))
         (cl-ppcre:scan "^RaceBox Mini [0-9]+" name))))

(defun list-racebox-devices ()
  "Return a list of all known RaceBox Mini devices."
  (remove-if-not #'is-racebox-device
                 (dbt:list-bluetooth-objects)))

(defun connect (&key (device-name (first-racebox)))
  "Connect to a RaceBox."
  (dbt:invoke-method-simple :system
                            "org.bluez"
                            device-name
                            "org.bluez.Device1"
                            "Connect"))

(defun disconnect (&key (device-name (first-racebox)))
  "Disconnect "
  (dbt:invoke-method-simple :system
                            "org.bluez"
                            device-name
                            "org.bluez.Device1"
                            "Disconnect"))

(defun read-metadata (&key (device-name (dbt:managed-object-name (first (list-racebox-devices)))))
  "Return the type, serial number, firmware version, hardware version, and manufacturer."
  (loop :for key :in '(:type :serial :firmware-version :hardware-version :manufacturer)
        :for uuid :in '("00002a24-0000-1000-8000-00805f9b34fb"
                        "00002a25-0000-1000-8000-00805f9b34fb"
                        "00002a26-0000-1000-8000-00805f9b34fb"
                        "00002a27-0000-1000-8000-00805f9b34fb"
                        "00002a29-0000-1000-8000-00805f9b34fb")
        :collecting (cons key
                          (dbt:to-string
                           (dbt:read-gatt-characteristic-by-uuid device-name
                                                                 uuid)))))

(eval-when
    (:compile-toplevel  ; this top-level form will be executed by the
                                        ;  file compiler

     :load-toplevel     ; this top-level form will be executed at load-time
                                        ;  of the compiled file

     :execute)          ; executed whenever else

  ;; Based on "RaceBox Mini Data Message" section of the protocol documentation
  (binary-types:define-binary-class ubx-header ()
    ((header1 :accessor header1 :binary-type binary-types:u8)
     (header2 :accessor header2 :binary-type binary-types:u8)
     (message-class :accessor message-class :binary-type binary-types:u8)
     (message-id :accessor message-id :binary-type binary-types:u8)
     (payload-length :accessor payload-length :binary-type binary-types:u16)))

  (binary-types:define-binary-class racebox-mini-data-message ()
    (
     ;; milliseconds from GPS week start
     (itow :accessor itow :binary-type binary-types:u32)

     (year :binary-type binary-types:u16)
     (month :binary-type binary-types:u8)
     (day :binary-type binary-types:u8)

     (hour :binary-type binary-types:u8)
     (minute :binary-type binary-types:u8)
     (second :binary-type binary-types:u8)

     (validity-flags :accessor validity-flags :binary-type binary-types:u8)
     (time-accuraccy :accessor time-accuracy :binary-type binary-types:u8)
     (nanosecond :binary-type binary-types:s32)

     (fix-status :accessor fix-status :binary-type binary-types:u8)
     (date-time-flags :accessor date-time-flags :binary-type binary-types:u8)
     (number-of-svs :accessor number-of-svs :binary-type binary-types:u8)

     (longitude :accessor longitude :binary-type binary-types:s32)
     (latitude :accessor latitude :binary-type binary-types:s32)

     (wgs-altitude :accessor wgs-altitude :binary-type binary-types:s32)
     (msl-altitude :accessor msl-altitude :binary-type binary-types:s32)

     (horizontal-accuracy :accessor horizontal-accuracy :binary-type binary-types:u32)
     (vertical-accuracy :accessor vertical-accuracy :binary-type binary-types:u32)

     (speed :accessor rb-speed :binary-type binary-types:s32)
     (heading :accessor heading :binary-type binary-types:s32)
     (speed-accuracy :accessor speed-accuracy :binary-type binary-types:u32)
     (heading-accuracy :accessor heading-accuracy :binary-type binary-types:u32)
     (pdop :accessor pdop :binary-type binary-types:u16)
     (lat-lon-flags :accessor lat-lon-flags :binary-type binary-types:u8)
     (battery-status :accessor battery-status :binary-type binary-types:u8)

     (g-force-x :accessor g-force-x :binary-type binary-types:s16)
     (g-force-y :accessor g-force-y :binary-type binary-types:s16)
     (g-force-z :accessor g-force-z :binary-type binary-types:s16)

     (rotation-rate-x :accessor rotation-rate-x :binary-type binary-types:s16)
     (rotation-rate-y :accessor rotation-rate-y :binary-type binary-types:s16)
     (rotation-rate-z :accessor rotation-rate-z :binary-type binary-types:s16))))

(defun get-timestamp (data-message)
  "Get a `local-time:timestamp` at the time of data-message."
  (with-slots (year month day hour minute second nanosecond) data-message
    (local-time-duration:timestamp-duration+
     ;; RaceBox nanoseconds can be negative, which isn't supported by
     ;; localtime, so add the nanoseconds as a duration.
     (local-time:encode-timestamp 0
                                  second
                                  minute
                                  hour
                                  day
                                  month
                                  year
                                  :timezone local-time:+utc-zone+)
     (local-time-duration:duration :nsec nanosecond))))

(defun read-raw-value (&key (device-name (dbt:managed-object-name (first (list-racebox-devices)))))
  "Read an octet buffer containing the most recent reading from specified device."
  (dbt:read-gatt-characteristic-by-uuid device-name
                                        "6e400003-b5a3-f393-e0a9-e50e24dcca9e"))

(defun read-current-value (&key (device-name (dbt:managed-object-name (first (list-racebox-devices)))))
  "The current sensor value decoded as a (values racebox-mini-data-message ubx-header check1 check2 octet-buffer)"
  (decode-packet
   (read-raw-value :device-name device-name)))


(defun compute-checksum (packet)
  "Compute the checksum used by ubx messages."
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 3))
           (type (simple-array (unsigned-byte 8)) packet))
  (loop :for ck-a fixnum  = 0 :then (mod (+ ck-a (aref packet i)) 256)
        :for ck-b fixnum = 0 :then (mod (+ ck-b ck-a) 256)
        :for i fixnum :from 2 :below (- (length packet) 2)
        :finally (return (values ck-a ck-b))))

(defun decode-packet (raw-data)
  "Decode raw octet buffer from the gatt characteristic into a
(values racebox-mini-data-message ubx-header check1 check2 octet-buffer)"
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 3))
           (type (simple-array (unsigned-byte 8)) raw-data))
  (let ((byte-count (length raw-data)))
    (multiple-value-bind (check-byte1 check-byte2) (compute-checksum raw-data)
      (declare (type (unsigned-byte 8) check-byte1 check-byte2))
      (when (or (/= (aref raw-data (- byte-count 2)) check-byte1)
                (/= (aref raw-data (- byte-count 1)) check-byte2))
        (error "Checksum error! ~a ~a ~a" check-byte1 check-byte2 raw-data))
      (let* ((binary-types:*endian* :little-endian)
             (input-stream (flexi-streams:make-in-memory-input-stream raw-data))
             (header (binary-types:read-binary 'ubx-header input-stream)))
        (cond ((and (= (the (unsigned-byte 8) (message-class header)) #16rff)
                    (= (the (unsigned-byte 8) (message-id header)) #16r1))
               (let ((message (binary-types:read-binary 'racebox-mini-data-message input-stream)))
                 (values message header check-byte1 check-byte2 raw-data)))
              (t
               (values nil header check-byte1 check-byte2 raw-data)))))))

;; TODO Get notifications of incoming messages.
;; (dbus:define-dbus-object racebox-listener-service
;;   (:path "/org/jl2/RaceBoxService"))

;; (dbus:define-dbus-method (racebox-service ) () (:list)
;;   (:interface "org.jl2.RaceBoxService")
;;   (read-current-value))

;; (dbus:define-dbus-signal-handler (my-service on-signal) ((s :string))
;;   (:interface "org.adeht.MyService")
;;   (format t "Got signal with arg ~S~%" s))

;; (defun publish-example ()
;;   (handler-case
;;       (with-open-bus (bus (session-server-addresses))
;;         (format t "Bus connection name: ~A~%" (bus-name bus))
;;         (publish-objects bus))
;;     (end-of-file ()
;;       :disconnected-by-bus)))

;; TODO: Add utility functions for importing downloaded CSV, GPX, and .vbo files.

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


(defun main (args)
  (declare (ignorable args))
  ;; TODO: What should main do?
  0)
