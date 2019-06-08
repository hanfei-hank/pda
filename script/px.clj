(ns px)

(def symbol (atom "eosusdt"))
(def prec (atom 3))

(def tick (atom 0))

(def bsvusdt "bsvusdt")
(def dashusdt "dashusdt")
(def eoseth "eoseth")
(def eosusdt "eosusdt")
(def zecusdt "zecusdt")

(defn init [api-key api-secret]
  (reset! prec (get {"bsvusdt" 2, "dashusdt" 1, "eosusdt" 3, "eoseth" 5, "eosbtc" 7, "zecusdt" 2} @symbol))
  (printf "symbom = {}, prec = {}" 
     @symbol @prec)
  (set-api api-key api-secret @symbol)
  (sleep 8000))

(defn- cancel [order]
    (printf "cancel order : {} " order)
    (cancel-order (:id order)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; pai xu ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- order-price [order]
  (str-to-decimal (:price order)))

(defn- order-amount [order]
  (str-to-decimal (:amount order)))

(defn- cancel-low-orders [price-min orders]
  (let [low-orders (filter (comp (> price-min) (order-price)) orders)]
    (map (cancel) low-orders)
    (not= 0 (count low-orders))))

(defn- cancel-high-orders [price-max orders]
  (let [high-orders  (filter (comp (< price-max) (order-price)) orders)]
    (map (cancel) high-orders)
    (not= 0 (count high-orders))))

(defn- buy-1 [diff amount]
  (let [real-amount (buy-amount 1)
        value (/ (/ real-amount amount) 0.08)]
    (when (< value diff)
      ; (printf "buy-1 price = {}, amount = {} {}, diff = {}" *price-buy1, real-amount amount value)
      (buy @symbol *price-buy1 amount))))

(defn- sell-1 [diff amount]
  (let [real-amount (sell-amount 1)
        value (/ (/ real-amount amount) 0.08)]
    (when (< value diff)
      ; (printf "sell-1 price = {}, amount = {} {}, diff = {}" *price-sell1 real-amount amount value)
      (sell @symbol *price-sell1 amount))))

(defn- acc-buy-amount [total index]
  (+ total (buy-amount index)))

(defn- acc-sell-amount [total index]
  (+ total (sell-amount index)))

(defn- buy-2-5 [diff amount]
  (let [real-amount (reduce (acc-buy-amount) 0.0 [2 3 4 5])
        value (/ (/ real-amount amount) 3)
        price (if (= 0 (mod @tick 2)) *price-buy4 *price-buy5)]
      (when (< value diff)
        ; (printf "buy-2-5 price = {}, amount = {} {}, diff = {}" *price-buy3 real-amount amount value)
        (buy @symbol price amount))))

(defn- sell-2-5 [diff amount]
  (let [real-amount (reduce (acc-sell-amount) 0.0 [2 3 4 5])
        value (/ (/ real-amount amount) 3)
        price (if (= 0 (mod @tick 2)) *price-sell4 *price-sell5)]
      (when (< value diff)
        ; (printf "sell-2-5 price = {}, amount = {} {}, diff = {}" *price-sell3 real-amount amount value)
        (sell @symbol price amount))))

(defn- buy-6-15 [diff amount]
  (let [real-amount (reduce (acc-buy-amount) 0.0 [6 7 8 9 10 11 12 13 14 15])
        value (/ (/ real-amount amount) 50)
        price (if (= 0 (mod @tick 2)) *price-buy10 *price-buy12)]
      (when (< value diff)
        ; (printf "buy-6-15 price = {}, amount = {} {}, diff = {}" *price-buy10 real-amount amount value)
        (buy @symbol price amount))))

(defn- sell-6-15 [diff amount]
  (let [real-amount (reduce (acc-sell-amount) 0.0 [6 7 8 9 10 11 12 13 14 15])
        value (/ (/ real-amount amount) 50)
        price (if (= 0 (mod @tick 2)) *price-sell11 *price-sell12)]
      (when (< value diff)
        ; (printf "sell-6-15 price = {}, amount = {} {}, diff = {}" *price-sell10 real-amount amount value)
        (sell @symbol price amount))))

;; sell 1  buy 1
(defn handle-px-1 [diff amount cycle]
  (reset! tick (+ 1 @tick))
  (let [n (mod @tick cycle)]
    (when (= 0 n)
      (buy-1 diff amount)
      (buy-2-5 diff amount)
      (buy-6-15 diff amount))
    (when (= (/ cycle 2) n)
      (sell-1 diff amount)
      (sell-2-5 diff amount)
      (sell-6-15 diff amount))
    (when (= 5 (mod @tick (* 5 cycle)))
      (get-orders @symbol)
      (when *sell-orders
        (cancel-high-orders *price-sell19 *sell-orders))
      (when *buy-orders
        (cancel-low-orders *price-buy19 *buy-orders)))))
