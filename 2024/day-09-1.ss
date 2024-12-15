(use-modules (rnrs io ports))

(define (read-disk)
  (with-input-from-file "2024/input/day-09.txt"
    (lambda ()
      (get-line (current-input-port)))))

(define-syntax dotimes
  (syntax-rules ()
    ((_ (var times) body ...)
     (let loop ((var 0))
       (while (< var times)
         body ...
         (set! var (+ var 1)))))))

(define (make-disk diskmap)
  (define (digit-at i)
    (string->number (string (string-ref diskmap i))))

  (define (disk-size)
    (let loop ((i 0)
               (size 0))
      (if (>= i (string-length diskmap)) size
          (loop (+ i 1) (+ size (digit-at i))))))

  (let ((result (make-vector (disk-size))))
    (let loop ((i 0)
               (offset 0)
               (file-id 0))
      (dotimes (j (digit-at i))
        (vector-set! result (+ offset j) file-id))
      (let ((new-i (+ i 2))
            (new-offset (+ offset
                           (digit-at i)
                           (if (>= (+ i 1) (string-length diskmap)) 0
                               (digit-at (+ i 1))))))
        (if (>= new-i (string-length diskmap)) result
            (loop new-i new-offset (+ file-id 1)))))))

(define (compact-disk! disk)
  (define (next-left left)
    (let loop ((new-left left))
      (if (or (>= new-left (vector-length disk))
              (unspecified? (vector-ref disk new-left)))
          new-left
          (loop (+ new-left 1)))))

  (define (next-right right)
    (let loop ((new-right right))
      (if (or (negative? new-right)
              (not (unspecified? (vector-ref disk new-right))))
          new-right
          (loop (- new-right 1)))))

  (define (swap i j)
    (let ((temp (vector-ref disk i)))
      (vector-set! disk i (vector-ref disk j))
      (vector-set! disk j temp)))

  (let loop ((left (next-left 0))
             (right (next-right (- (vector-length disk) 1))))
    (if (<= right left) disk
        (begin (swap left right)
               (loop (next-left left) (next-right right))))))

(define (disk-checksum disk)
  (let loop ((i 0)
             (checksum 0))
    (if (or (>= i (vector-length disk))
            (unspecified? (vector-ref disk i)))
        checksum
        (loop (+ i 1) (+ checksum (* i (vector-ref disk i)))))))

(let ((disk (make-disk (read-disk))))
  (compact-disk! disk)
  (display (disk-checksum disk))
  (newline))
