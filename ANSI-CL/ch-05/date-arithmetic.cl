(defconstant months
  #(0 31 59 90 120 151 181 212 243 273 304 334 365))

(defconstant year-zero 2000)

(defun leap? (year)
  "Determine if a year is leap."
  (and (zerop (mod year 4))
       (or (zerop (mod year 400))
           (not (zerop (mod year 100))))))

(defun date->num (date month year)
  "Convert a date to a number by summing the number of days from year-zero."
  (+ (- date 1) (month-num month year) (year-num year)))

(defun month-num (month year)
  "Determine the number of days in a year up to the given month."
  (+ (svref months (- month 1))
     (if (and (> month 2) (leap? year)) 1 0)))

(defun year-num (year)
  "Determine the number of days in a given year."
  (let ((days 0))
    (if (>= year year-zero)
        (dotimes (i (- year year-zero) days)
          (incf days (year-days (+ year-zero i))))
        (dotimes (i (- year-zero year) (- days))
          (incf days (year-days (+ year i)))))))

(defun year-days (year)
  (if (leap? year) 366 365))

(defun num->date (number)
  "Convert an integer back to a date."
  (multiple-value-bind (year left) (num-year number)
    (multiple-value-bind (month days) (num-month left year)
      (values days month year))))

(defun num-year (n)
  "Determine the year in the given number."
  (if (< n 0)
      (do* ((y (- year-zero 1) (- y 1))
            (d (- (year-days y)) (- d (year-days y))))
           ((<= d n) (values y (- n d))))
      (do* ((y year-zero (+ y 1))
            (prev 0 d)
            (d (year-days y) (+ d (year-days y))))
           ((> d n) (values y (- n prev))))))

(defun num-month (n y)
  "Extract the month and days of given year from given number."
  (if (leap? y)
      (cond ((= n 59) (values 2 29))
            ((> n 59) (nmon (- n 1)))
            (t (nmon n)))
      (nmon n)))

(defun nmon (n)
  "Determine the position of given month in the months vector."
  (let ((m (position n months :test #'<)))
    (values m (+ 1 (- n (svref months (- m 1)))))))

(defun date+ (d m y n)
  "Add a number of days to a given date."
  (num->date (+ (date->num d m y) n)))
