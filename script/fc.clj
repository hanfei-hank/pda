(ns fc)

(def api-key (atom ""))
(def api-secret (atom ""))
(def symbol (atom "eosusdt"))
(def prec (atom 3))

(def tick (atom 0))

;; 间隔时间，毫秒
(def interval (atom 9000))

(def bsvusdt "bsvusdt")
(def dashusdt "dashusdt")
(def eoseth "eoseth")
(def eosusdt "eosusdt")
(def zecusdt "zecusdt")

(def sell-amount (atom 1.0))    ;; 每笔卖单的数量
(def sell-levels [1.018, 1.023, 1.028, 1.033, 1.038, 1.05, 1.065, 1.08, 1.095, 1.11])
(def min-sell-level 1.015)
(def max-sell-level 1.115)

(def buy-amount (atom 1.0))    ;; 每笔买单的数量
(def buy-levels [0.97, 0.955, 0.94, 0.925, 0.91, 0.895, 0.88, 0.865, 0.85])
(def min-buy-level 0.845)
(def max-buy-level 0.975)

(defn init []
  (reset! prec (get {"bsvusdt" 2, "dashusdt" 1, "eosusdt" 3, "eoseth" 5, "eosbtc" 7, "zecusdt" 2} @symbol))
  (printf "symbom = {}, prec = {}, sell-amount = {}, buy-amount = {}, interval = {}" 
     @symbol @prec @sell-amount @buy-amount @interval)
  (set-api @api-key @api-secret @symbol)
  (sleep 8000))

(defn- cancel [order]
    (printf "cancel order : {} " order)
    (cancel-order (:id order)))

(defn- compute-price [price factor]
    (ceiling (* price factor) @prec))

(defn submit-sell [amount factor]
    (let [sell-price (compute-price *price-sell1 factor)]
        (sell @symbol sell-price amount)
        (printf "submit sell amount = {}, price = {}" amount, sell-price)))

(defn- cancel-submit-sell [order sell-price]
    (cancel order)
    (sleep 500)   ;; 有时候取消订单没有及时完成，造成下面的挂单失败，所以暂停一会
    (sell @symbol sell-price @sell-amount)
    (printf "recreat order {} -> {}" (:price order) sell-price))

(defn check-min-sell-order [base-price price-min level-index order]
    ; (printf "check sell order {} {} {}" price-min level-index order)
    (if (> price-min (str-to-decimal (:price order)))
      (let [sell-price (compute-price base-price (get sell-levels level-index))]
        (cancel-submit-sell order sell-price)
        (- level-index 1))
      level-index))

(defn check-max-sell-order [base-price price-max level-index order]
    ; (printf "check max sell order {} {} {}" price-max level-index order)
    (if (< price-max (str-to-decimal (:price order)))
      (let [sell-price (compute-price base-price (get sell-levels level-index))]
          (cancel-submit-sell order sell-price)
          (+ level-index 1))
      level-index))

; order process
(defn handle-gd-sell [base-price]
    ; (when debug (printf "handle sell orders {}" base-price))
    (let [sorted-orders (sort [:price] *sell-orders)
          price-min (compute-price base-price min-sell-level)
          price-max (compute-price base-price max-sell-level)
          ret1 (reduce (check-min-sell-order base-price price-min) (- (count sell-levels) 1) sorted-orders)
          ret2 (reduce (check-max-sell-order base-price price-max) 0 sorted-orders)]
        (or (not= ret1 (- (count sell-levels) 1)) (not= ret2 0))))

(defn reset-gd-sell []
    (get-orders @symbol)
    (when *sell-orders
      (printf "cancel {} sell orders" (count *sell-orders))
      (map (cancel) *sell-orders))

    (sleep 1000)
    (get-market @symbol)
    (map (submit-sell @sell-amount) sell-levels)
    (printf "submit all sell order ok")

    (sleep 5000))

;; buy functions
(defn submit-buy [amount factor]
    (let [buy-price (compute-price *price-buy1 factor)]
        (buy @symbol buy-price amount)
        (printf "submit buy amount = {}, price = {}" amount, buy-price)))

(defn- cancel-submit-buy [order buy-price]
    (cancel order)
    (sleep 500)   ;; 有时候取消订单没有及时完成，造成下面的挂单失败，所以暂停一会
    (buy @symbol buy-price @buy-amount)
    (printf "recreat buy order {} -> {}" (:price order) buy-price))

(defn check-min-buy-order [base-price price-min level-index order]
    ; (printf "check buy order {} {} {}" price-min level-index order)
    (if (> price-min (str-to-decimal (:price order)))
      (let [buy-price (compute-price base-price (get buy-levels level-index))]
        (cancel-submit-buy order buy-price)
        (+ level-index 1))
      level-index))

(defn check-max-buy-order [base-price price-max level-index order]
    ; (printf "check max buy order {} {} {}" price-max level-index order)
    (if (< price-max (str-to-decimal (:price order)))
      (let [buy-price (compute-price base-price (get buy-levels level-index))]
          (cancel-submit-buy order buy-price)
          (- level-index 1))
      level-index))

; order process
(defn handle-buy-orders [base-price orders]
    ; (when debug (printf "handle buy orders {}" base-price))
    (let [sorted-orders (sort [:price] orders)
          price-min (compute-price base-price min-buy-level)
          price-max (compute-price base-price max-buy-level)
          ret1 (reduce (check-max-buy-order base-price price-max) (- (count buy-levels) 1) sorted-orders)
          ret2 (reduce (check-min-buy-order base-price price-min) 0 sorted-orders)]
        (when (or (not= ret1 (- (count buy-levels) 1)) (not= ret2 0))
          (printf "order changed, query again!")
          (get-orders @symbol))))

(defn buy-gua-dan []
    (init)

    (get-orders @symbol)
    (when *buy-orders
      (printf "cancel {} buy orders" (count *buy-orders))
      (map (cancel) *buy-orders))

    (sleep 1000)
    (get-market @symbol)
    (map (submit-buy @buy-amount) buy-levels)
    (printf "submit all buy order ok")

    (sleep 5000)
    (get-orders @symbol)
    (while true 
        (sleep @interval)
        (get-market @symbol)
        (handle-buy-orders *price-buy1 *buy-orders)))
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; pai xu ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- order-price [order]
  (str-to-decimal (:price order)))

(defn- order-amount [order]
  (str-to-decimal (:amount order)))

(defn- valid-orders [orders]
  (filter (comp (< 0.009) (order-amount)) orders))

(defn- cancel-low-orders [price-min orders]
  (let [low-orders (filter (comp (> price-min) (order-price)) orders)]
    (map (cancel) low-orders)
    (not= 0 (count low-orders))))

(defn- cancel-high-orders [price-max orders]
  (let [high-orders  (filter (comp (< price-max) (order-price)) orders)]
    (map (cancel) high-orders)
    (not= 0 (count high-orders))))
