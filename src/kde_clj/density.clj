(in-ns 'kde-clj.core)

(load "utility")

;;(defn kernel-density-factory-u
;;  [kernel data bandwidth]
;;  (let [n (count data)]
;;    (fn [x] (/ (apply + (map #(kernel (/ (- x %1) bandwidth)) data))
;;               (* n bandwidth)))))

(defn kernel-density-factory
  [kernel data bandwidth]
  (let [n (count data)]
    (fn [x] (/ (apply + (map #(kernel (/ x bandwidth) (/ %1 bandwidth))
                             data))
               (* n bandwidth)))))

(defn compute-density
  ([kernel data]
   (compute-density kernel data (pick-bandwidth data)))
  ([kernel data bandwidth]
   (let [kde (kernel-density-factory kernel data bandwidth)
         x (pick-space data)]
   (pmap kde x))))

(defn build-density-plot
  ([kernel data]
   (build-density-plot kernel data (pick-bandwidth data)))
  ([kernel data bandwidth]
   (let [kde (kernel-density-factory kernel data bandwidth)
         x (pick-space data)
         density (pmap kde x)]
     (c/scatter-plot x density))))

;;(defn integrate-analytical
;;  ([kernel data a b]
;;   (integrate-analytical kernel data a b (pick-bandwidth data)))
;;  ([kernel data a b bandwidth]
;;   (let [kde-integral (kernel-density-factory kernel data bandwidth)]
;;     (- (kde-integral b)
;;        (kde-integral a)))))


(defn build-integrable-density
  "Computes the density of a given set of `data` and returns it in a
  format to be numerically integrated with step size `dx`."
  ([kernel data dx]
   (build-integrable-density kernel data dx (pick-bandwidth data)))
  ([kernel data dx bandwidth]
   (let [kde (kernel-density-factory kernel data bandwidth)
         x (range (apply min data) (apply max data) dx)]
     {:dx dx
      :x x
      :d (pmap kde x)})))

(defn plot-integrable-density
  [integrable]
  (c/scatter-plot (:x integrable) (:d integrable)))

(defn integrate-integrable-density
  "Numerically integrates an 'integrable' density as returned by
  `build-integrable-density`."
  [integrable from to]
  (let [dx (:dx integrable)
        ;; off-by-one errors on numerical inaccuracies?
        from-index (int (quot (- from (first (:x integrable))) dx))
        to-index (int (quot (- to (first (:x integrable))) dx))
        n (- to-index from-index)
        n-from (- (count (:x integrable)) from-index)
        values (take n (take-last n-from (:d integrable)))]
    (apply + (map #(* dx %) values))))
