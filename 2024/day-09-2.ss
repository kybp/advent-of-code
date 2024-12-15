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
  (define (first-file-index)
    (let loop ((file-index (- (vector-length disk) 1)))
      (if (unspecified? (vector-ref disk file-index))
          (loop (- file-index 1))
          file-index)))

  (define (next-file-index file-index)
    (let loop ((new-file-index file-index))
      (if (negative? new-file-index) new-file-index
          (let ((initial (vector-ref disk file-index))
                (element (vector-ref disk new-file-index)))
            (if (or (equal? element initial)
                    (unspecified? element))
                (loop (- new-file-index 1))
                new-file-index)))))

  (define (try-move-file file-index)
    (let* ((width (file-width file-index))
           (block (find-block width))
           (source (+ (- file-index width) 1)))
      (if (and block
               (not (negative? source))
               (> source block))
          (move-file source block width))))

  (define (file-width file-index)
    (let ((initial (vector-ref disk file-index)))
      (let loop ((i file-index)
                 (size 0))
        (if (and (not (negative? i))
                 (equal? (vector-ref disk i) initial))
            (loop (- i 1) (+ size 1))
            size))))

  (define (find-block size)
    (let loop ((i 0)
               (current-size 0))
      (if (>= i (vector-length disk)) #f
          (cond ((equal? size current-size)
                 (- i size))
                ((unspecified? (vector-ref disk i))
                 (loop (+ i 1) (+ current-size 1)))
                (else
                 (loop (+ i 1) 0))))))

  (define unspecified (begin))

  (define (move-file source target length)
    (dotimes (i length)
      (vector-set! disk (+ target i)
                   (vector-ref disk (+ source i)))
      (vector-set! disk (+ source i)
                   unspecified)))

  (let loop ((file-index (first-file-index)))
    (if (negative? file-index) disk
        (begin (try-move-file file-index)
               (loop (next-file-index file-index))))))

(define (disk-checksum disk)
  (let loop ((i 0)
             (checksum 0))
    (cond ((>= i (vector-length disk))
           checksum)
          ((unspecified? (vector-ref disk i))
           (loop (+ i 1) checksum))
          (else
           (loop (+ i 1) (+ checksum (* i (vector-ref disk i))))))))

(let ((disk (make-disk (read-disk))))
  (compact-disk! disk)
  (display (disk-checksum disk))
  (newline))
