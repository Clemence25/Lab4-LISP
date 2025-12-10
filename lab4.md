<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт до лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання""<br/>
дисципліни "Вступ до функціонального програмування"
</p>

<p align="right"> 
<b>Студент</b>: 
 Оваденко Владислава КВ-21</p>

<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання
Завдання складається з двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної
роботи 3 з такими змінами:
використати функції вищого порядку для роботи з послідовностями (де/якщо
це доречно, в разі, якщо функції вищого порядку не були використані при
реалізації л.р. No3); додати до інтерфейсу функції (та використання в реалізації) два ключових
параметра: key та test , що працюють аналогічно до того, як працюють
параметри з такими назвами в функціях, що працюють з послідовностями (р.12). 
При цьому key має виконатись мінімальну кількість разів.
2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за
варіантом (див. п 4.1.2). Використання псевдофункцій не забороняється, але, за
можливості, має бути зменшене до необхідного мінімуму.

## Варіант першої частини 3
Алгоритм сортування обміном №2 (із використанням прапорця) за незменшенням.

## Лістинг реалізації першої частини завдання
```lisp
(defun swap (lst key test)
  (cond
    ((null (rest lst))
     (values lst nil))

    ((funcall test (funcall key (first lst)) (funcall key (second lst)))
     (multiple-value-bind (rest-of-list new-flag) (swap (cons (first lst) (rest (rest lst))) key test)
       (values (cons (second lst) rest-of-list) t)))

    (t (multiple-value-bind (rest-of-list new-flag) (swap (rest lst) key test)
         (values (cons (first lst) rest-of-list) new-flag)))))


(defun sort-func (lst &key (key #'identity) (test #'>))
  (multiple-value-bind (new-list flag) (swap lst key test)
    (if flag
        (sort-func new-list :key key :test test)
        new-list)))
```
### Тестові набори та утиліти першої частини
```lisp
(defun check-first-function (name input expected)
    "Execute `my-reverse' on `input', compare result with `expected' and print
    comparison status"
    (format t "~:[FAILED~;passed~] ~a~%"
        (equal (sort-func input) expected)
        name))

(defun test-first-function ()
    (check-first-function "test 1" '(5 3 4 1 2) '(1 2 3 4 5))  
    (check-first-function "test 2" '(1 2 3 4 5) '(1 2 3 4 5)) 
    (check-first-function "test 3" '(1 1 1 1 1) '(1 1 1 1 1))
    (check-first-function "test 4" '(2 2 3 3 1) '(1 2 2 3 3))
    (check-first-function "test 5" nil nil))
```
### Тестування першої частини
```lisp
CL-USER> (test-first-function)
passed test 1
passed test 2
passed test 3
passed test 4
passed test 5
NIL
```

## Варіант другої частини 1
Написати функцію add-prev-fn , яка має один ключовий параметр — функцію
transform . add-prev-fn має повернути функцію, яка при застосуванні в якості
першого аргументу mapcar разом з одним списком-аргументом робить наступне: кожен
елемент списку перетворюється на точкову пару, де в комірці CAR знаходиться значення
поточного елемента, а в комірці CDR знаходиться значення попереднього елемента
списку. 
Якщо функція transform передана, тоді значення поточного і попереднього
елементів, що потраплять у результат, мають бути змінені згідно transform .
transform має виконатись мінімальну кількість разів.
CL-USER> (mapcar (add-prev-fn) '(1 2 3))
((1 . NIL) (2 . 1) (3 . 2))
CL-USER> (mapcar (add-prev-fn :transform #'1+) '(1 2 3))
((2 . NIL) (3 . 2) (4 . 3))

## Лістинг реалізації другої частини завдання
```lisp
(defun add-prev-fn (&key (transform #'identity))
  (let ((prev-transformed nil)
        (have-prev nil))
    (lambda (current)
      (let ((cur-t (funcall transform current)))
        (prog1
            (cons cur-t (if have-prev prev-transformed nil))
          (setf prev-transformed cur-t
                have-prev t))))))

```

### Тестові набори та утиліти другої частини
```lisp
(defun check-add-prev-fn (name input expected &key (transform #'identity))
  (format t "~:[FAILED~;passed~]... ~a~%" 
          (equal (mapcar (add-prev-fn :transform transform) input) expected)
          name))


(defun test-add-prev-fn ()
  (check-add-prev-fn "test 1" '(1 2 3) '((1 . NIL) (2 . 1) (3 . 2)))
  (check-add-prev-fn "test 2" '(42) '((42 . NIL)))
  (check-add-prev-fn "test 3" '() '())
  (check-add-prev-fn "test 4" '(1 2 3) '((2 . NIL) (3 . 2) (4 . 3)) :transform #'1+)
  (check-add-prev-fn "test 5" '(1 2 3) '((2 . NIL) (4 . 2) (6 . 4)) :transform #'(lambda (x) (* 2 x)))
  (check-add-prev-fn "test 6" '(nil nil nil) '((NIL . NIL) (NIL . NIL) (NIL . NIL)))) 
```

### Тестування другої частини
```lisp
CL-USER> (test-add-prev-fn)
passed... test 1
passed... test 2
passed... test 3
passed... test 4
passed... test 5
passed... test 6
NIL
```