(ns px)

(def tick (atom 0))

(defn cancel [order]
    (printf "cancel order : {} " order)
    (cancel-order (:id order)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; pai xu ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- order-price [order]
  (str-to-decimal (:price order)))

(defn- order-amount [order]
  (str-to-decimal (:amount order)))

(defn cancel-low-orders [price-min orders]
  (let [low-orders (filter (comp (> price-min) (order-price)) orders)]
    (map (cancel) low-orders)
    (not= 0 (count low-orders))))

(defn cancel-high-orders [price-max orders]
  (let [high-orders  (filter (comp (< price-max) (order-price)) orders)]
    (map (cancel) high-orders)
    (not= 0 (count high-orders))))

(defn buy-sell-1 [symbol diff amount]
  (let [real-buy-amount (buy-amount 1)
        buy-value (/ (/ real-buy-amount amount) 0.1)
        real-sell-amount (sell-amount 1)
        sell-value (/ (/ real-sell-amount amount) 0.1)]
    (when (or (< buy-value diff) (< sell-value diff))
      ; (printf "buy-1 price = {}, amount = {} {}, diff = {}" *price-buy1, real-amount amount value)
      (buy symbol *price-buy1 amount)
      (sell symbol *price-sell1 amount))))

(defn- acc-buy-amount [total index]
  (+ total (buy-amount index)))

(defn- acc-sell-amount [total index]
  (+ total (sell-amount index)))

(defn buy-sell-2-5 [symbol diff amount]
  (let [real-buy-amount (reduce (acc-buy-amount) 0.0 [2 3 4 5])
        buy-value (/ (/ real-buy-amount amount) 2)
        buy-price (if (= 0 (mod @tick 2)) *price-buy4 *price-buy5)

        real-sell-amount (reduce (acc-sell-amount) 0.0 [2 3 4 5])
        sell-value (/ (/ real-sell-amount amount) 2)
        sell-price (if (= 0 (mod @tick 2)) *price-sell4 *price-sell5)]
      (when (or (< buy-value diff) (< sell-value diff))
        ; (printf "buy-2-5 price = {}, amount = {} {}, diff = {}" *price-buy3 real-amount amount value)
        (buy symbol buy-price amount)
        (sell symbol sell-price amount))))

(defn- buy-6-15 [symbol diff amount]
  (let [real-amount (reduce (acc-buy-amount) 0.0 [6 7 8 9 10 11 12 13 14 15])
        value (/ (/ real-amount amount) 50)
        price (if (= 0 (mod @tick 2)) *price-buy10 *price-buy12)]
      (when (< value diff)
        ; (printf "buy-6-15 price = {}, amount = {} {}, diff = {}" *price-buy10 real-amount amount value)
        (buy symbol price amount))))

(defn- sell-6-15 [symbol diff amount]
  (let [real-amount (reduce (acc-sell-amount) 0.0 [6 7 8 9 10 11 12 13 14 15])
        value (/ (/ real-amount amount) 50)
        price (if (= 0 (mod @tick 2)) *price-sell11 *price-sell12)]
      (when (< value diff)
        ; (printf "sell-6-15 price = {}, amount = {} {}, diff = {}" *price-sell10 real-amount amount value)
        (sell symbol price amount))))
