(ns otus-04.homework.scramblies)

;; Оригинальная задача:
;; https://www.codewars.com/kata/55c04b4cc56a697bb0000048
(defn is-gte? [coll [key value]]
  (let [coll-value (coll key)]
    (and coll-value (>= coll-value value))))

(defn scramble?
  "Функция возвращает true, если из букв в строке letters
  можно составить слово word."
  [letters word]
  (let [fword (frequencies word)
        fletters (frequencies letters)]
    (every? #(is-gte? fletters %) fword)))

(comment
  (scramble? "rkqodlw" "world")
  )
