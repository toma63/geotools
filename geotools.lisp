
(defun latitude2radians (latitute)
  "latitude is specified as a signed decimal in degrees, positive is north."
  (* pi (/ (+ latitute 90.0) 180.0)))

(defun longitude2radians (longitude)
  "longitude is specified as a signed decimal in degrees, positive is east."
  (* pi (/ (+ longitude 180.0) 180.0)))

(defun delta-lat (lat1 lat2)
  "Compute the difference between two latitudes in positive radians"
  (abs (- lat1 lat2)))

(defun delta-long (long1 long2)
  "Compute the difference between two longitudes in positive radians."
  (let ((diff (abs (- long1 long2))))
    (if (> diff pi) (- (* 2 pi) diff)
        diff)))

(defvar EARTH-RADIUS 6371.0)

(defun haversine-a (lat1 long1  lat2 long2)
  "compute the a value for the Haversine formula"
  (let* ((lat1r (latitude2radians lat1))
         (lat2r (latitude2radians lat2))
         (long1r (longitude2radians long1))
         (long2r (longitude2radians long2))
         (lat-delta (delta-lat lat1r lat2r))
         (long-delta (delta-long long1r long2r))
         (lat-delta-half-sin (sin (/ lat-delta 2.0)))
         (lat-sin-sq (* lat-delta-half-sin lat-delta-half-sin))
         (long-delta-half-sin (sin (/ long-delta 2.0)))
         (long-sin-sq (* long-delta-half-sin long-delta-half-sin)))
    (+ lat-sin-sq (* (cos lat1r) (cos lat2r) long-sin-sq))))

(defun haversine-c (a)
  "Compute the central angle in the Haversine formula"
  (* 2.0 (atan (sqrt a) (sqrt (- 1 a)))))

(defun great-circle (lat1 long1 lat2 long2)
  "Compute the great circle distance in km between two latitude/longitude points in signed decimal degrees"
  (* EARTH-RADIUS (haversine-c (haversine-a lat1 long1 lat2 long2))))

(defun mh-char2num (char)
  "takes a single character from a maidenhead grid and converts it to a 0 based integer"
  (let ((raw-code (char-code char)))
    (cond ((and (>= raw-code 65) (< raw-code 89)) (- raw-code 65))
	  ((and (>= raw-code 48) (< raw-code 58 )) (- raw-code 48))
	  (t (error "out of range character")))))

(defvar LONG-FACTORS '(20 2 1/12 1/120))
(defvar LAT-FACTORS '(10 1 1/24 1/240))
;; alternating longitude and latitude factors
(defvar LONG-LAT-OFFSETS '(10 5 1 1/2 1/24 1/48 1/240 1/480))

(defun mh2ll (mh-grid)
  "Handle 2/4/6/8 charcters, default to center for missing lower grids.
    Each pair of characters represents a longitude and latitude."
  (let* ((uc-grid (string-upcase mh-grid))
	 (int-mapped (map 'list #'mh-char2num uc-grid))
	 (long-center-offset 0)
	 (lat-center-offset 0))
    (loop 
       for degrees in int-mapped
       for i from 0
       if (evenp i) 
         collect degrees into longitudes and
	 do (setq long-center-offset (float (nth i LONG-LAT-OFFSETS)))
       else
	 collect degrees into latitudes and
	 do (setq lat-center-offset (float (nth i LONG-LAT-OFFSETS)))
       finally (return (list (- (+ long-center-offset (apply #'+ (mapcar #'* longitudes LONG-FACTORS))) 180.0)
			     (- (+ lat-center-offset (apply #'+ (mapcar #'* latitudes LAT-FACTORS))) 90.0))))))



