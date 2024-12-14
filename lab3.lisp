;Завдання 1

(defun swap-adjacent (list)
  "Обміняйте сусідні елементи в ЛІСТі."
  (cond
    ((null (cdr list)) list)                       ; Якщо список порожній або містить лише один елемент, поверніть його.
    ((> (car list) (cadr list))                    ; Якщо перший елемент більше за другий, обміняйте їх.
     (cons (cadr list) (swap-adjacent (cons (car list) (cddr list)))))
    (t
     (cons (car list) (swap-adjacent (cdr list))))))  ; Інакше, продовжуйте обмінюватися з рештою списку.

(defun bubble-sort-functional (list)
  "Сортуйте ЛІСТ за допомогою функціонального підходу з методом бульбашки."
  (let ((swapped-list (swap-adjacent list)))      ; Застосувати обмін сусідніх елементів.
    (if (equal swapped-list list)                  ; Якщо більше обмінів не потрібні, поверніть відсортований список.
        list
      (bubble-sort-functional swapped-list))))    ; Інакше, рекурсивно відсортуйте змінений список.

(defun run-bubble-sort-functional-tests ()
  "Запустіть тестові випадки для bubble-sort-functional."
  (format t "Тест 1: bubble-sort-functional ~%")
  (format t "~a~%" (equal (bubble-sort-functional '(4 2 5 1 3)) '(1 2 3 4 5)))
  (format t "Тест 2: bubble-sort-functional ~%")
  (format t "~a~%" (equal (bubble-sort-functional '(1 2 3 4 5)) '(1 2 3 4 5)))
  (format t "Тест 3: bubble-sort-functional (порожній список) ~%")
  (format t "~a~%" (equal (bubble-sort-functional '()) '())))

(run-bubble-sort-functional-tests)


;Завдання 2

(defun bubble-sort-imperative (lst)
  "Сортуйте ЛСТ за стандартним імперативним методом бульбашки."
  (let ((sorted-list (copy-list lst)))            ; Скопіюйте список для сортування.
    (dotimes (i (1- (length sorted-list)))       ; Зовнішній цикл над списком.
      (dotimes (j (- (length sorted-list) i 1)) ; Внутрішній цикл для порівнянь.
        (let ((current (nth j sorted-list))
              (next (nth (1+ j) sorted-list)))
          (when (> current next)                 ; Якщо поточний елемент більше наступного, обміняйте їх.
            (setf (nth j sorted-list) next)
            (setf (nth (1+ j) sorted-list) current)))))
  sorted-list)

(defun run-bubble-sort-imperative-tests ()
  "Запустіть тестові випадки для bubble-sort-imperative."
  (format t "Тест 1: bubble-sort-imperative ~%")
  (format t "~a~%" (equal (bubble-sort-imperative '(4 2 5 1 3)) '(1 2 3 4 5)))
  (format t "Тест 2: bubble-sort-imperative ~%")
  (format t "~a~%" (equal (bubble-sort-imperative '(1 2 3 4 5)) '(1 2 3 4 5)))
  (format t "Тест 3: bubble-sort-imperative (порожній список) ~%")
  (format t "~a~%" (equal (bubble-sort-imperative '()) '())))

(run-bubble-sort-imperative-tests)
