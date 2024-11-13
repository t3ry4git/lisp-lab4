<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Терентьєв Іван Дмитрович КВ-11</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
Завдання складається з двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з  [лабораторної роботи 3](https://github.com/t3ry4git/lisp-lab3/tree/main) з такими змінами: 
* використати функції вищого порядку для роботи з послідовностями (де це доречно);
* додати до інтерфейсу функції (та використання в реалізації) два ключових параметра: ```key``` та ```test``` , що працюють аналогічно до того, як працюють параметри з такими назвами в функціях, що працюють з послідовностями. При цьому ```key``` має виконатись мінімальну кількість разів.
2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за варіантом (див. п 4.1.2). Використання псевдо-функцій не забороняється, але, за можливості, має бути мінімізоване.

## Варіант першої частини 6(22)
Алгоритм сортування вставкою №1 (з лінійним пошуком зліва) за незменшенням.
## Лістинг реалізації першої частини завдання
```lisp
(defun insert-sort-recursive (lst &key (key #'identity) (test #'<))
  "Insertion sort, designed for recursive calls."
  ;; Define a helper function to insert an element into a sorted list.
  (labels ((insert (element sorted)
                   ;; If the sorted part is empty, return a list with just the element.
                   (if (null sorted)
                       (list element)
                       (let* ((head (car sorted))
                              (element-key (funcall key element))
                              (head-key (funcall key head)))
                         ;; Compare keys; insert element or continue recursively.
                         (if (funcall test element-key head-key)
                             (cons element sorted)
                             (cons head (insert element (cdr sorted)))))))
           ;; Recursive function to sort the list by inserting elements one by one.
           (sort-recursive (unsorted sorted)
                           ;; If unsorted is empty, return the sorted list.
                           (if (null unsorted)
                               sorted
                               ;; Otherwise, insert the first unsorted element into sorted.
                               (sort-recursive (cdr unsorted)
                                               (insert (car unsorted) sorted)))))
    ;; Initialize sorting with an empty sorted list.
    (sort-recursive lst nil)))
```
### Тестові набори та утиліти першої частини
```lisp
(defun run-recursive-sort-test (input expected-result test-description &key (key #'identity) (test #'<))
  "Test function specifically for recursive insertion sort with key and test parameters."
  (let ((result (insert-sort-recursive input :key key :test test)))
    (if (equal result expected-result)
        (format t "~A: successfully.~%" test-description)
        (format t "~A: failed! ~%Expected: ~A~%Got: ~A~%" test-description expected-result result))))

(defun test-sorting-recursive ()
  "Testing insert-sort-recursive with various cases."
  (format t "Testing recursive insertion sort...~%")
  ;; Standard cases
  (run-recursive-sort-test '(3 1 4 1 5 9 2 6 5 3 5) '(1 1 2 3 3 4 5 5 5 6 9) "Recursive: Standard list")
  (run-recursive-sort-test '(1 2 3 4 5) '(1 2 3 4 5) "Recursive: Already sorted list")
  (run-recursive-sort-test '(5 4 3 2 1) '(1 2 3 4 5) "Recursive: Reverse order list")
  (run-recursive-sort-test '() '() "Recursive: Empty list")
  (run-recursive-sort-test '(1) '(1) "Recursive: Single element list")
  (run-recursive-sort-test '(1 2 2 1) '(1 1 2 2) "Recursive: List with duplicates")

  ;; Key function tests
  (run-recursive-sort-test '(3 -1 -4 1 -5 9 -2 6 -5 3 -5) '(-1 1 -2 3 3 -4 -5 -5 -5 6 9)
                           "Recursive: Sorting with key as abs"
                           :key #'abs)
  (run-recursive-sort-test '(3 -1 -4 1 -5 9 -2 6 -5 3 -5) '(-1 1 -2 3 3 -4 -5 -5 -5 6 9)
                           "Recursive: Sorting with key as abs in ascending order (<)"
                           :key #'abs :test #'<)
  (run-recursive-sort-test '(3 -1 -4 1 -5 9 -2 6 -5 3 -5) '(1 -1 -2 3 3 -4 -5 -5 -5 6 9)
                           "Recursive: Sorting with key as abs in ascending order(<=)"
                           :key #'abs :test #'<=)
  (run-recursive-sort-test '(3 -1 -4 1 -5 9 -2 6 -5 3 -5) '(9 6 -5 -5 -5 -4 3 3 -2 -1 1)
                           "Recursive: Sorting with key as abs in descending order"
                           :key #'abs :test #'>)

  ;; Custom test functions
  (run-recursive-sort-test '(3 2 5 4 1) '(1 2 3 4 5) "Recursive: Default test function (<=)" :test #'<=)
  (run-recursive-sort-test '(3 2 5 4 1) '(5 4 3 2 1) "Recursive: Descending order" :test #'>)
  (run-recursive-sort-test '((2 . 3) (1 . 2) (4 . 5) (3 . 1))
                           '((1 . 2) (2 . 3) (3 . 1) (4 . 5))
                           "Recursive: Custom sorting by car"
                           :key #'car)
  (run-recursive-sort-test '((2 . 3) (1 . 2) (4 . 5) (3 . 1))
                           '((3 . 1) (1 . 2) (2 . 3) (4 . 5))
                           "Recursive: Custom sorting by cdr"
                           :key #'cdr)

  (format t "Testing completed.~%"))
```
### Тестування першої частини
```lisp
cl-user > (test-sorting-recursive)
Testing recursive insertion sort...
Recursive: Standard list: successfully.
Recursive: Already sorted list: successfully.
Recursive: Reverse order list: successfully.
Recursive: Empty list: successfully.
Recursive: Single element list: successfully.
Recursive: List with duplicates: successfully.
Recursive: Sorting with key as abs: successfully.
Recursive: Sorting with key as abs in ascending order (<): successfully.
Recursive: Sorting with key as abs in ascending order(<=): successfully.
Recursive: Sorting with key as abs in descending order: successfully.
Recursive: Default test function (<=): successfully.
Recursive: Descending order: successfully.
Recursive: Custom sorting by car: successfully.
Recursive: Custom sorting by cdr: successfully.
Testing completed.
NIL
```
## Варіант другої частини 10(22)
Написати функцію ```duplicate-elements-reducer```, яка має один основний параметр n та один ключовий параметр — функцію ```duplicate-p```. ```duplicate-elements-reducer``` має повернути функцію, яка при застосуванні в якості першого аргументу reduce робить наступне: при обході списку, кожен елемент списка-аргумента ```reduce```, для якого функція ```duplicate-p``` повертає значення ```t``` (або не ```nil``` ), дублюється n разів. Якщо користувач не передав функцію ```duplicate-p``` у ```duplicate-elements-reducer```, тоді дублюються всі елементи вхідного списку. Обмеження, які накладаються на використання функції-результату ```duplicate-elements-reducer``` при передачі у reduce визначаються розробником (тобто, наприклад, необхідно чітко визначити, якими мають бути значення ключових параметрів функції reduce ```from-end``` та ```initial-value```).

```lisp
CL-USER> (reduce (duplicate-elements-reducer 2)

'(1 2 3)
:from-end ...
:initial-value ...)

(1 1 2 2 3 3)
CL-USER> (reduce (duplicate-elements-reducer 3 :key #'evenp)

'(1 2 3)
:from-end ...
:initial-value ...)

(1 2 2 2 3)
```
## Лістинг реалізації другої частини завдання
```lisp
(defun duplicate-elements-reducer (n &key (duplicate-p (constantly t)))
  "Returns a function for use with REDUCE that duplicates elements of a list based on a given condition.

  This function creates and returns a lambda expression, which can be used with REDUCE to process a list.
  When called, this lambda function checks each element of the input list using the DUPLICATE-P predicate.
  If DUPLICATE-P returns true (T or non-NIL), the element is duplicated N times.

  If DUPLICATE-P is not provided or explicitly passed as NIL, it defaults to a function that always returns true, 
  causing all elements to be duplicated.

  Parameters:
    N (required) — The number of times each element satisfying the condition should be duplicated.
    DUPLICATE-P (optional, keyword argument) — A predicate function that takes an element of the list as input 
    and returns true if the element should be duplicated N times. If not provided, or if passed as NIL, 
    defaults to duplicating all elements.

  Usage examples:
    (reduce (duplicate-elements-reducer 2) '(1 2 3) :initial-value '())
    => (1 1 2 2 3 3)
    
    (reduce (duplicate-elements-reducer 3 :duplicate-p #'evenp) '(1 2 3) :initial-value '())
    => (1 2 2 2 3)

  Constraints:
    - An initial-value must be provided and must be a list.
    - The from-end parameter cannot be used with a value of T.
    - If the input list is empty but initial-value (even '()) is provided, NIL will be returned.

  "
  (let ((effective-duplicate-p (or duplicate-p (constantly t))))
    (lambda (acc item)
      (let ((items (if (funcall effective-duplicate-p item)
                       (make-list n :initial-element item)
                       (list item))))
        (nconc acc items)))))

(defun safe-reduce-duplicate-elements (n lst &key (duplicate-p nil))
  "A safe wrapper for REDUCE with duplicate-elements-reducer."
  (reduce (duplicate-elements-reducer n :duplicate-p duplicate-p) lst
          :initial-value '() :from-end nil))
```
### Тестові набори та утиліти другої частини
```lisp
(defun run-duplicate-elements-test (input n duplicate-p expected-result test-description)
  "Test function for duplicate-elements-reducer with specified parameters."
  (let ((result (safe-reduce-duplicate-elements n input :duplicate-p duplicate-p)))
    (if (equal result expected-result)
        (format t "~A: successfully.~%" test-description)
        (format t "~A: failed! ~%Expected: ~A~%Got: ~A~%" test-description expected-result result))))

(defun test-duplicate-elements-reducer ()
  "Testing duplicate-elements-reducer with various cases."
  (format t "Testing duplicate-elements-reducer...~%")
  ;; Standard cases
  (run-duplicate-elements-test '(1 2 3) 2 nil '(1 1 2 2 3 3) "All elements duplicated twice")
  (run-duplicate-elements-test '(1 2 3) 3 nil '(1 1 1 2 2 2 3 3 3) "All elements duplicated three times")

  ;; Duplicate only even numbers
  (run-duplicate-elements-test '(1 2 3) 2 #'evenp '(1 2 2 3) "Even elements duplicated twice")
  (run-duplicate-elements-test '(1 2 3 4) 3 #'evenp '(1 2 2 2 3 4 4 4) "Even elements duplicated three times")

  ;; Duplicate only odd numbers
  (run-duplicate-elements-test '(1 2 3 4 5) 2 #'oddp '(1 1 2 3 3 4 5 5) "Odd elements duplicated twice")
  (run-duplicate-elements-test '(1 2 3 4 5) 3 #'oddp '(1 1 1 2 3 3 3 4 5 5 5) "Odd elements duplicated three times")

  ;; Edge cases
  (run-duplicate-elements-test '() 2 nil '() "Empty list")
  (run-duplicate-elements-test '(1) 2 nil '(1 1) "Single element duplicated twice")
  (run-duplicate-elements-test '(1 2 3) 0 nil '() "Duplicating with n = 0 (no duplication)")

  (format t "Testing completed.~%"))

```
### Тестування другої частини
```lisp
cl-user > (test-duplicate-elements-reducer)
Testing duplicate-elements-reducer...
All elements duplicated twice: successfully.
All elements duplicated three times: successfully.
Even elements duplicated twice: successfully.
Even elements duplicated three times: successfully.
Odd elements duplicated twice: successfully.
Odd elements duplicated three times: successfully.
Empty list: successfully.
Single element duplicated twice: successfully.
Duplicating with n = 0 (no duplication): successfully.
Testing completed.
NIL
```