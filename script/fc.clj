(ns fc)

(def api-key (atom ""))
(def api-secret (atom ""))
(def symbol (atom "eosusdt"))
(def prec (atom 3))

;; 间隔时间，毫秒
(def interval (atom 9000))

(def sell-amount (atom 40.0))

(def sell-levels [1.035, 1.05, 1.065, 1.08, 1.095, 1.11, 1.125, 1.14, 1.155])
(def min-sell-level 1.03)
(def max-sell-level 1.16)

(defn set-symbol [s]
  (reset! symbol s)
  (reset! prec (get {"eosusdt" 3, "eosbtc" 7} s)))

(defn- init []
  (set-api @api-key @api-secret @symbol)
  (sleep 4000))

(defn- cancel [order]
    (printf "cancel order : {} " order)
    (cancel-order (:id order)))

(defn- compute-price [price factor]
    (ceiling (* price factor) @prec))

(defn cancel-all []
  (get-orders @symbol)
  (when *sell-orders
      (printf "cancel {} sell orders" (count *sell-orders))
      (map (cancel) *sell-orders)))

(defn submit-sell [amount factor]
    (let [sell-price (compute-price *price-sell1 factor)]
        (sell @symbol sell-price amount)
        (printf "submit sell amount = {}, price = {}" amount, sell-price)))

(defn submit-all []
  (get-market @symbol)
  (map (submit-sell @sell-amount) sell-levels)
  (printf "submit ok"))

(defn- cancel-submit [order sell-price]
    (cancel order)
    (sell @symbol sell-price @sell-amount)
    (printf "recreat order {} -> {}" (:price order) sell-price))

(defn check-min-sell-order [base-price price-min level-index order]
    ; (printf "check sell order {} {} {}" price-min level-index order)
    (if (> price-min (str-to-decimal (:price order)))
      (let [sell-price (compute-price base-price (get sell-levels level-index))]
        (cancel-submit order sell-price)
        (- level-index 1))
      level-index))

(defn check-max-sell-order [base-price price-max level-index order]
    ; (printf "check max sell order {} {} {}" price-max level-index order)
    (if (< price-max (str-to-decimal (:price order)))
      (let [sell-price (compute-price base-price (get sell-levels level-index))]
          (cancel-submit order sell-price)
          (+ level-index 1))
      level-index))

; order process
(defn handle-sell-orders [base-price orders]
    ; (when debug (printf "handle sell orders {}" base-price))
    (let [sorted-orders (sort [:price] orders)
          price-min (compute-price base-price min-sell-level)
          price-max (compute-price base-price max-sell-level)]
        (reduce (check-min-sell-order base-price price-min) (- (count sell-levels) 1) sorted-orders)
        (reduce (check-max-sell-order base-price price-max) 0 sorted-orders)))

(defn gua-dan []
    (init)
    (cancel-all)
    (sleep 1000)
    (submit-all)
    (sleep 1000)
    (while true 
        (sleep @interval)
        (get-orders @symbol)
        (get-market @symbol)
        (handle-sell-orders *price-sell1 *sell-orders)))

;;  test code
(def test-orders 
  [{:id "1" :price "10.111" :amount "10.0"}
   {:id "2" :price "10.312" :amount "10.0"}
   {:id "3" :price "12.823" :amount "10.0"}
   {:id "4" :price "10.613" :amount "10.0"}])

(defn test []
    (printf "test call")
    (handle-sell-orders  10.0 test-orders))
