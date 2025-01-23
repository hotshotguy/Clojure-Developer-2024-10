(ns otus-04.homework.magic-square)

;; Оригинальная задача:
;; https://www.codewars.com/kata/570b69d96731d4cf9c001597
;;
;; Подсказка: используйте "Siamese method"
;; https://en.wikipedia.org/wiki/Siamese_method

(defn create-empty-square [n]
  (vec (repeat n (vec (repeat n 0)))))

(defn inc-index [n i]
  (let [new-i (inc i)]
    (if (> new-i (dec n))
      0
      new-i)))

(defn dec-index [n i]
  (let [new-i (dec i)]
    (if (< new-i 0)
     (dec n)
      new-i)))

(defn update-square [s i j value]
  [(update-in s [i] assoc j value) i j])

(defn next-square [square i j value]
  (let [n (count square)
        next-i (dec-index n i)
        next-j (inc-index n j)
        up-i (inc-index n i)]
    (cond
      (zero? (get-in square [next-i next-j])) (update-square square next-i next-j value)
      (zero? (get-in square [up-i j])) (update-square square up-i j value)
      :else [])))


(defn magic-square
  "Функция возвращает вектор векторов целых чисел,
  описывающий магический квадрат размера n*n,
  где n - нечётное натуральное число.

  Магический квадрат должен быть заполнен так, что суммы всех вертикалей,
  горизонталей и диагоналей длиной в n должны быть одинаковы."
  [n]
  {:pre [(odd? n)]}
  (let [init (create-empty-square n)
        init-s (assoc init 0 (assoc (init 0) (quot n 2) 1))]
    (loop [s init-s
           i 0
           j (quot n 2)
           value 2]
      (let [[result next-i next-j] (next-square s i j value)]
        (if (nil? result)
          s
          (recur result next-i next-j (inc value)))))))

(comment
  (magic-square 3))
