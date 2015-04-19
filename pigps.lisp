;; pigps.lisp

;; Copyright (c) 2015, Jeremiah LaRocco <jeremiah.larocco@gmail.com>

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

(in-package #:pigps)

;; This code is intricately tied to the NMEA data format.
;; For more information about NMEA, see this PDF:
;; https://www.sparkfun.com/datasheets/GPS/NMEA%20Reference%20Manual1.pdf
;; Or this website:
;; http://www.gpsinformation.org/dale/nmea.htm

;; For informational purposes, here's what NMEA data looks like from
;; Adafruit's Ultimate GPS unit:

;; Sample data before getting a satellite fix:

;; $GPGGA,000812.800,,,,,0,00,,,M,,M,,*7B
;; $GPGSA,A,1,,,,,,,,,,,,,,,*1E
;; $GPRMC,000812.800,V,,,,,0.00,0.00,060180,,,N*41
;; $GPVTG,0.00,T,,M,0.00,N,0.00,K,N*32

;; Sample data with a satellite fix:

;; GPGGA,034056.000,3959.0498,N,10515.2269,W,2,06,1.21,1648.2,M,-20.6,M,0000,0000*61
;; $GPGSA,A,3,06,02,24,17,12,28,,,,,,,1.51,1.21,0.90*01
;; $GPRMC,034056.000,A,3959.0498,N,10515.2269,W,0.10,106.02,190415,,,D*7D
;; $GPVTG,106.02,T,,M,0.10,N,0.18,K,D*35
;; $GPGGA,034057.000,3959.0498,N,10515.2269,W,2,06,1.21,1648.2,M,-20.6,M,0000,0000*60
;; $GPGSA,A,3,06,02,24,17,12,28,,,,,,,1.51,1.21,0.90*01
;; $GPRMC,034057.000,A,3959.0498,N,10515.2269,W,0.13,103.14,190415,,,D*7D
;; $GPVTG,103.14,T,,M,0.13,N,0.23,K,D*3C
;; $GPGGA,034058.000,3959.0498,N,10515.2269,W,2,06,1.21,1648.2,M,-20.6,M,0000,0000*6F
;; $GPGSA,A,3,06,02,24,17,12,28,,,,,,,1.51,1.21,0.90*01
;; $GPGSV,3,1,11,06,77,078,37,02,51,214,34,51,43,183,30,24,39,262,29*7B
;; $GPGSV,3,2,11,12,38,314,27,17,37,066,16,10,25,134,22,28,13,129,17*72
;; $GPGSV,3,3,11,20,11,169,21,03,09,038,18,25,02,313,*4A
;; $GPRMC,034058.000,A,3959.0498,N,10515.2269,W,0.19,79.78,190415,,,D*4E
;; $GPVTG,79.78,T,,M,0.19,N,0.35,K,D*07
;; $GPGGA,034059.000,3959.0498,N,10515.2268,W,2,06,1.21,1648.2,M,-20.6,M,0000,0000*6F
;; $GPGSA,A,3,06,02,24,17,12,28,,,,,,,1.51,1.21,0.90*01
;; $GPRMC,034059.000,A,3959.0498,N,10515.2268,W,0.32,43.85,190415,,,D*4C

(defun read-nmea-line (fd)
  "Use wpi:serial-getchar to read a line of NMEA data from the serial handle."

  ;; serial-getchar returns numeric bytes, and we write them to stream
  ;; as characters and return the string.
  (with-output-to-string (stream)

    ;; Always start with a "$"
    (format stream "$")

    ;; Gaurd against starting the read half way into a line by throwing out
    ;; characters until the first "$"
    (iterate 
     (for next-char = (wpi:serial-getchar fd))
     (while (not (= next-char (char-code #\$)))))

    ;; Now we're immediately after the $, so read characters until the "*"
    (iterate
     ;; Compute the checksum as we go, initializing to 0
     (with check-sum = 0)

     ;; Get the next character
     (for next-char = (wpi:serial-getchar fd))

     ;; Stop reading after *
     (while (not (= next-char (char-code #\*))))

     ;; Write the character to the stream - this doesn't execute for *
     (format stream "~c" (code-char next-char))

     ;; Update the checksum.  The checksum algorithm is xor of all bytes
     (setf check-sum (logxor check-sum next-char))

     ;; At this point we've read up to "*" and we're immediately after it
     (finally
      ;; Read the next two characters, which make up the checksum
      (let* ((cs0 (wpi:serial-getchar fd))
	     (cs1 (wpi:serial-getchar fd))

	     ;; A little ugly, but convert to an integer by writing
	     ;; the characters to a string and reading it
	     (chk (read-from-string
		   (format nil "#x~c~c" (code-char cs0) (code-char cs1)))))

	;; Check that the checksum we calculate matches the expected
	(if (not (= chk check-sum))
	    ;; If not throw an error, otherwise do nothing and
	    ;; exit the with-output-to-string
	    (error
	     (format nil "Checksum mismatch! Got ~a expected ~a"
		     check-sum chk))))))))

;; Modified from http://cl-cookbook.sourceforge.net/strings.html#reverse
(defun split (line char)
  "Split a line of text on the specified character."
  (loop for i = 0 then (1+ j)
     as j = (position char line :start i)
     collect (subseq line i j)
     while j))

(defstruct gps-datapoint
  "A GPS reading a certain point in time"
  (timestamp (local-time:now) :type local-time:timestamp)
  (latitude 0.0 :type float)
  (longitude 0.0 :type float)
  (elevation 0.0 :type float))
    
(defun parse-time-date (time-str date-str)
  "Parse a HHMMSS.SSS time string and a DDMMYY into a local-time:timestamp"
  (let ((hour (read-from-string (subseq time-str 0 2)))
	(min (read-from-string (subseq time-str 2 4)))
	(sec (read-from-string (subseq time-str 4 6)))
	(fsec (read-from-string (subseq time-str 7 10)))
	(date-day (read-from-string (subseq date-str 0 2)))
	(date-month (read-from-string (subseq date-str 2 4)))
	(date-year (read-from-string (subseq date-str 4))))
    (local-time:encode-timestamp (* 1000000 (/ fsec 1000)) sec min hour
				 date-day date-month date-year
				 :offset 0)))

(defun degrees-minutes (str &key (deg-len 2))
  "Convert a DDMM.MMM... string into decimal degrees.
:deg-len specifys the number of degree digits."
  (let ((degrees (read-from-string (subseq str 0 deg-len)))
	(minutes (read-from-string (subseq str deg-len))))
    (+ degrees (/ minutes 60.0))))
  
(defun parse-rmc (parts)
  "Parse an NMEA RMC message"

  ;; RMC is the "Recommended Minimum Specific GNSS Data"
  ;; It contains Time, date, position, course, and speed data

  ;; Throw an error if the data is invalid
  (if (string= (nth 2 parts) "V")
      (error "Recieved data indivates it is invalid! No satelite fix!"))

  ;; Parse out each subfield we care about
  (let* ((utc-time-str (nth 1 parts))
	 (date-str (nth 9 parts))
	 (tstamp (parse-time-date utc-time-str date-str))

	 (latitude-str (nth 3 parts))
	 (latdec (degrees-minutes latitude-str :deg-len 2))

	 (n-or-s (nth 4 parts))
	 (latitude (if (string= n-or-s "S") (- latdec) latdec))

	 (longitude-str (nth 5 parts))
	 (londec (degrees-minutes longitude-str :deg-len 3))

	 (e-or-w (nth 6 parts))
	 (longitude (if (string= e-or-w "W") (- londec) londec)))

    ;; Return the point
    (make-gps-datapoint :timestamp tstamp :longitude longitude :latitude latitude)))

(defun parse-gga (parts)
  "Parse an NMEA GGA message"

  ;; GGA is the "Global Position System Fixed Data" message
  ;; It contains Time, position, elevation and fix type data

  ;; The position data is more accurate than the RMC message, but
  ;; there is no date field

  ;; Throw an error if there isn't a fix
  (if (string= (nth 6 parts) "0")
      (error "No fix!"))

  ;; Parse out each subfield we care about
  (let* ((latitude-str (nth 2 parts))
	 (latdec (degrees-minutes latitude-str :deg-len 2))

	 (n-or-s (nth 3 parts))
	 (latitude (if (string= n-or-s "S") (- latdec) latdec))

	 (longitude-str (nth 4 parts))
	 (londec (degrees-minutes longitude-str :deg-len 3))

	 (e-or-w (nth 5 parts))
	 (longitude (if (string= e-or-w "W") (- londec) londec))
	 (elevation (read-from-string (nth 9 parts))))

    ;; Return the point
    (make-gps-datapoint :timestamp (local-time:now)  :elevation elevation :longitude longitude :latitude latitude)))


(defun get-next-rmc (fd)
  "Search incoming data for the next RMC message."
  (iterate

    ;; Read NMEA lines
    (for next-parts = (split (read-nmea-line fd) #\, ))

    ;; Until an RMC message is found
    (until (string= "$GPRMC" (car next-parts)))

    ;; Return the parsed RMC data point
    (finally (return (parse-rmc next-parts)))))

(defun get-next-gga (fd)
  "Search incoming data for the next GGA message."
  (iterate

    ;; Read NMEA lines
    (for next-parts = (split (read-nmea-line fd) #\, ))

    ;; Until a GGA message is found
    (until (string= "$GPGGA" (car next-parts)))

    ;; Return the parsed GGA data point
    (finally (return (parse-gga next-parts)))))


(defun current-location ()
  (let ((fd (wpi:serial-open "/dev/ttyAMA0" 9600)))

    ;; Unwind protect so that wpi:serial-close is always called
    (unwind-protect

	 ;; It's lame to read two messages for this, but:
	 ;; The GGA field has higher precision latitude and longitude values,
	 ;; and has elevation data, but doesn't contain a date field.
	 ;; RMC has a date field, but no elevation and is less precise.

	 ;; So this function reads both fields and returns the best of both
	 (let ((gga (get-next-gga fd))
	       (rmc (get-next-rmc fd)))
	   (setf (gps-datapoint-timestamp gga) (gps-datapoint-timestamp rmc))
	   gga)
    
      (wpi:serial-close fd))))

