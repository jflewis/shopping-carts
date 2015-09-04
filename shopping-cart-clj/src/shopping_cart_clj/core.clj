(ns shopping-cart-clj.core
  (:require [clojure.string :as s])
  (:gen-class))



(def headers->keywords {"Item-name" :item
                        "price" :price})

(def filename "../inventory/inventory.csv")


(defn str->double
  [str]
  (Double. str))

;;CSV conversion
(def conversions {:item identity
                  :price str->double })

(defn parse
  [string]
  (map #(s/split % #",")
       (s/split string #"\n")))

(defn get-inventory
  [rows]
  (let [headers (map #(get headers->keywords %) (first rows))
        unmapped-rows (next rows)]
    (map (fn [unmapped-row]
           (into {}
                 (map (fn [header column]
                        [header ((get conversions header) column)])
                      headers
                      unmapped-row)))
         unmapped-rows)))


(defn String->Number [str]
  (let [n (read-string str)]
       (if (number? n) n nil)))


(defn print-shopping-cart-total 
  [cart]
  (let [ total (reduce + 0 (map :price cart ))]
    (printf "Total cost: $%.2f\n" total)
    (println "---------------------")
    cart))

(defn shopping-cart-total 
  [cart]
  (let [ total (reduce + 0 (map :price cart ))]
    total))



(defn list-items
  [items]
  ;;uses map to create a vector [index, {item}]
  ;;use doseq to go through new vector
  (let [num-items (map vector (iterate inc 1) items)]
    (doseq [[index item] num-items] (printf "%d: Item: %s, Price:$%.2f\n" index (:item item) (:price item))))
  (println "---------------------")
  items)


(defn get-input [prompt]
  (println prompt)
  (read-line))

(defn add-to-inventory [inventory]
  (let [name (get-input "Enter product name")]
    (loop [price (String->Number (get-input "Enter product price"))]
      (if (nil? price)
        (recur (String->Number (get-input "Not a Number, try again:")))
        (conj inventory {:item name :price price})))))


(defn add-to-cart [cart inventory]
  (loop [item (String->Number (get-input "Enter product # to add"))]
    (if (nil? item)
      (recur (String->Number (get-input "Not a Number, try again:")))
      (conj  cart (get inventory (- item 1))))))

(defn remove-index [index collection]
  (loop [iter 0 final-col [] remaining collection]
    (if (= index iter )
      (vec (flatten (cons final-col (rest remaining))))
      (recur (inc iter) (conj final-col (first remaining)) (next remaining)))))
    


(defn remove-index [index collection]
  (loop [iter 0 final-col [] remaining collection]
    (if (= index iter )
      (vec (flatten (cons final-col (rest remaining))))
      (recur (inc iter) (conj final-col (first remaining)) (next remaining))))) 

(defn checkout [cart]
  (println "Thank you for shopping at clojure@Us")
  (println "Your items\n----------")
  (list-items cart)
  (printf "Your total comes to: $%.2f" (shopping-cart-total cart)))

(def items (into [] (get-inventory (parse (slurp filename)))))
(def shopping-cart [])

(defn -main
  "Entry point into the program"
  [& args]
  (loop [inventory items
         cart shopping-cart]
    (println "1.List items")
    (println "2. add item")
    (println "3. remove item")
    (println "4. view cart")
    (println "5. add item to cart")
    (println "6. current cart total")
    (println "7. Checkout")
    (println "---------------------")
    (let [input (read-line)]
      (case input
        "1" (recur (list-items  inventory) cart)
        "2" (recur (add-to-inventory inventory) cart)
        "3" (recur (remove-item inventory) cart)
        "4" (recur inventory (list-items cart))
        "5" (recur inventory (add-to-cart cart inventory))
        "6" (recur inventory (print-shopping-cart-total cart))
        "7" (checkout cart)
        "exit" (println "Okay, cya!")
        (recur inventory cart)))))





    
