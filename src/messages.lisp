;; messagesto.lisp

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

(eval-when
    (:compile-toplevel
     :load-toplevel
     :execute)

  ;; Based on "RaceBox Mini Data Message" section of the protocol documentation
  (binary-types:define-binary-class ubx-header ()
    ((header1 :accessor header1
              :binary-type binary-types:u8)
     (header2 :accessor header2
              :binary-type binary-types:u8)
     (message-class :accessor message-class
                    :binary-type binary-types:u8)
     (message-id :accessor message-id
                 :binary-type binary-types:u8)
     (payload-length :accessor payload-length
                     :binary-type binary-types:u16)))

  (binary-types:define-binary-class racebox-mini-data-message ()
    (
     ;; milliseconds from GPS week start
     (itow :accessor itow
           :binary-type binary-types:u32)

     (year :binary-type binary-types:u16)
     (month :binary-type binary-types:u8)
     (day :binary-type binary-types:u8)

     (hour :binary-type binary-types:u8)
     (minute :binary-type binary-types:u8)
     (second :binary-type binary-types:u8)

     (validity-flags :accessor validity-flags
                     :binary-type binary-types:u8)
     (time-accuraccy :accessor time-accuracy
                     :binary-type binary-types:u32)
     (nanosecond :binary-type binary-types:s32)

     (fix-status :accessor fix-status
                 :binary-type binary-types:u8)
     (fix-status-flags :accessor fix-status-flags
                       :binary-type binary-types:u8)
     (date-time-flags :accessor date-time-flags
                      :binary-type binary-types:u8)
     (number-of-svs :accessor number-of-svs
                    :binary-type binary-types:u8)

     (longitude :accessor longitude
                :binary-type binary-types:s32)
     (latitude :accessor latitude
               :binary-type binary-types:s32)

     (wgs-altitude :accessor wgs-altitude
                   :binary-type binary-types:s32)
     (msl-altitude :accessor msl-altitude
                   :binary-type binary-types:s32)

     (horizontal-accuracy :accessor horizontal-accuracy
                          :binary-type binary-types:u32)
     (vertical-accuracy :accessor vertical-accuracy
                        :binary-type binary-types:u32)

     (speed :accessor rb-speed
            :binary-type binary-types:s32)
     (heading :accessor heading
              :binary-type binary-types:s32)

     (speed-accuracy :accessor speed-accuracy
                     :binary-type binary-types:u32)
     (heading-accuracy :accessor heading-accuracy
                       :binary-type binary-types:u32)

     (pdop :accessor pdop
           :binary-type binary-types:u16)

     (lat-lon-flags :accessor lat-lon-flags
                    :binary-type binary-types:u8)

     (battery-status :accessor battery-status
                     :binary-type binary-types:u8)

     (g-force-x :accessor g-force-x
                :binary-type binary-types:s16)
     (g-force-y :accessor g-force-y
                :binary-type binary-types:s16)
     (g-force-z :accessor g-force-z
                :binary-type binary-types:s16)

     (rotation-rate-x :accessor rotation-rate-x
                      :binary-type binary-types:s16)
     (rotation-rate-y :accessor rotation-rate-y
                      :binary-type binary-types:s16)
     (rotation-rate-z :accessor rotation-rate-z
                      :binary-type binary-types:s16))))

(defun rbm-fix-status (msg)
  (with-slots (fix-status) msg
    (cond ((= 0 fix-status) :no-fix)
          ((= 2 fix-status) :2d-fix)
          ((= 3 fix-status) :3d-fix)
          (t :unknown))))

(defun rbm-timestamp (data-message)
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

(defun rbm-latitude (data-message)
  (with-slots (latitude) data-message
    (/ latitude
       100000.0)))

(defun rbm-longitude (data-message)
  (with-slots (longitude) data-message
    (/ longitude
       100000.0)))

(defun rbm-battery-status (data-message)
  data-message)


(defun rbm-speed-in-km-per-hour (data-message)
  (with-slots (speed) data-message
    ;; mm/s to km/h
    (/ speed
       (* 1000 1000)
       (* 60 60)
       1.0)))

(defun rbm-heading (data-message)
  (with-slots (heading) data-message
    (/ heading
       10000.0)))

(defun rmb-wgs-altitude (data-message)
  (with-slots (wgs-altitude) data-message
    (/ wgs-altitude
       (* 1000 1000)
       1.0)))

(defun rbm-msl-altitude (data-message)
  (with-slots (msl-altitude) data-message
    (/ msl-altitude
       (* 1000 1000)
       1.0)))

(defun rbm-g-force-x (data-message)
  (with-slots (g-force-x) data-message
    (/ g-force-x
       1000
       1.0)))
(defun rbm-g-force-y (data-message)
  (with-slots (g-force-y) data-message
    (/ g-force-y
       1000
       1.0)))
(defun rbm-g-force-z (data-message)
  (with-slots (g-force-z) data-message
    (/ g-force-z
       1000
       1.0)))

(defun rbm-rotation-rate-x (data-message)
  (with-slots (rotation-rate-x) data-message
    (/ rotation-rate-x
       100
       1.0)))
(defun rbm-rotation-rate-y (data-message)
  (with-slots (rotation-rate-y) data-message
    (/ rotation-rate-y
       100
       1.0)))
(defun rbm-rotation-rate-z (data-message)
  (with-slots (rotation-rate-z) data-message
    (/ rotation-rate-z
       100
       1.0)))

(defstruct gps-message
  (timestamp (local-time:now) :type local-time:timestamp)
  (longitude 0.0 :type real)
  (latitude 0.0 :type real)
  (msl-altitude 0.0 :type real)
  (wgs-altitude 0.0 :type real)
  (speed 0.0 :type real)
  (heading 0.0 :type real)
  (g-force (vec3 0.0 0.0 0.0) :type vec3)
  (rotation (vec3 0.0 0.0 0.0) :type vec3)
  (raw-id 0 :type fixnum))


(defun to-gps-message (data-message)
  (with-slots (year month day hour minute second nanosecond
               longitude latitude msl-altitude wgs-altitude speed heading
               g-force-x g-force-y g-force-z
               rotation-rate-x rotation-rate-y rotation-rate-z) data-message
    (values (make-gps-message :timestamp (rbm-timestamp data-message)
                              :latitude (/ latitude
                                           10000000.0)
                              :longitude (/ longitude
                                            10000000.0)
                              :msl-altitude (/ msl-altitude
                                               (* 1000.0))
                              :wgs-altitude (/ wgs-altitude
                                               (* 1000.0))
                              :speed (/ speed
                                        (* 1000 1000)
                                        (* 60 60)
                                        1.0)
                              :heading (/ heading
                                          100000.0)
                              :g-force (vec3 (/ g-force-x
                                                1000.0)
                                             (/ g-force-y
                                                1000.0)
                                             (/ g-force-z
                                                1000.0))
                              :rotation (vec3 (/ rotation-rate-x
                                                 100.0)
                                              (/ rotation-rate-y
                                                 100.0)
                                              (/ rotation-rate-z
                                                 100.0)))
            data-message)))

(defun compute-checksum (packet)
  "Compute the checksum used by ubx messages."
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 3))
           (type (simple-array (unsigned-byte 8)) packet))
  (loop
    :for ck-a fixnum = 0 :then (mod (+ ck-a (aref packet i))
                                    256)
    :for ck-b fixnum = 0 :then (mod (+ ck-b ck-a)
                                    256)
    :for i fixnum :from 2 :below (- (length packet) 2)
    :finally (return (values ck-a ck-b))))


(defun validate-checksum (raw-data)
  "Validate the checksum of the raw-data from the RaceBox."
  (let ((byte-count (length raw-data)))
    (multiple-value-bind (check-byte1 check-byte2) (compute-checksum raw-data)
      (declare (type (unsigned-byte 8) check-byte1 check-byte2))
      (when (or (/= (aref raw-data (- byte-count 2)) check-byte1)
                (/= (aref raw-data (- byte-count 1)) check-byte2))
        (error "Checksum error! ~a ~a ~a" check-byte1 check-byte2 raw-data))
      (values check-byte1 check-byte2))))

(defun is-racebox-mini-message (header)
  "Check if message has class 0xff and id 0x1"
  (and (= #16rff
          (the (unsigned-byte 8) (message-class header)))
       (= #16r1
          (the (unsigned-byte 8) (message-id header)))))

(defun decode-packet (raw-data)
  "Decode raw octet buffer from the gatt characteristic into a
(values racebox-mini-data-message ubx-header check1 check2 octet-buffer)"

  (declare (optimize (speed 3) (safety 0) (debug 0) (space 3))
           (type (simple-array (unsigned-byte 8)) raw-data))

  (when (zerop (length raw-data))
    (return-from decode-packet nil))

  (multiple-value-bind (check-byte1 check-byte2) (validate-checksum raw-data)
    (let* ((binary-types:*endian* :little-endian)
           (input-stream (flexi-streams:make-in-memory-input-stream raw-data))
           (header (binary-types:read-binary 'ubx-header input-stream)))

      (cond ((is-racebox-mini-message header)
             (values (binary-types:read-binary 'racebox-mini-data-message input-stream)
                     header
                     check-byte1
                     check-byte2
                     raw-data))
            (t
             (values nil
                     header
                     check-byte1
                     check-byte2
                     raw-data))))))

(defun read-current-value (&key (device-name (dbt:managed-object-name (first (list-racebox-devices)))))
  "The current sensor value decoded as a (values racebox-mini-data-message ubx-header check1 check2 octet-buffer)"
  (decode-packet
   (read-raw-value :device-name device-name)))
