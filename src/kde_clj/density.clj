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
   (map kde x))))

(defn build-density-plot
  ([kernel data]
   (build-density-plot kernel data (pick-bandwidth data)))
  ([kernel data bandwidth]
   (let [kde (kernel-density-factory kernel data bandwidth)
         x (pick-space data)
         density (map kde x)]
     (c/scatter-plot x density))))

(defn integrate-analytical
  ([kernel data a b]
   (integrate-kde kernel data a b (pick-bandwidth data)))
  ([kernel data a b bandwidth]
   (let [kde-integral (kernel-density-factory kernel data bandwidth)]
     (- (kde-integral b)
        (kde-integral a)))))

