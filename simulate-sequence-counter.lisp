;;;; simulate-sequence-counter.lisp

(in-package #:simulate-sequence-counter)

(defun random-sequence (sequence-length die-sides)
  (let ((ret (make-array sequence-length)))
    (loop for i from 1 to sequence-length
          do (setf (aref ret (1- i))
                   (1+ (random die-sides))))
    ret))
(defun count-occurence-starting (sequence start to-occur)
  (loop for i from 0 to (1- (length to-occur)) do
    (when (/= (nth i to-occur) (aref sequence (+ i start)))
      (return-from count-occurence-starting 0)))
  1)

(defun count-occurence (sequence to-occur)
  (loop for i from 0 to (- (array-dimension sequence 0) (length to-occur))
        for at-i = (count-occurence-starting sequence i to-occur)
        summing at-i))

(defun single-experiment (sequence-length to-occur)
  (count-occurence (random-sequence sequence-length 7) to-occur))

(defun many-experiments (experiment-count sequence-length to-occur)
  (let ((observed (loop for i from 1 to experiment-count summing (single-experiment sequence-length to-occur))))
    (format t "Received ~a observations~%" observed)
    (/ (float observed) (float experiment-count))))

(defun simulate-twitter-question ()
  (simulate-sequence-counter::many-experiments 1000000 523 '(3 2 5 7 4)))
