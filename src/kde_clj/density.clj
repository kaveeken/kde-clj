(in-ns 'kde-clj.core)

(load-file "src/kde_clj/utility.clj")

(defn kernel-density-factory
  [kernel data bandwidth]
  (let [n (count data)]
    (fn [x] (/ (apply + (map #(kernel (/ (- x %1) bandwidth)) data))
               (* n bandwidth)))))

(defn kernel-density-deriv-factory
  [kernel-deriv data bandwitdth]
  (let [n (count data)]
    (fn [x] (/ (apply + (map #(kernel-deriv (/ x bandwitdth) (/ %1 bandwitdth))
                             data))
               (* n bandwitdth)))))

(defn compute-density
  ([kernel data]
   (compute-density kernel data (pick-bandwidth data)))
  ([kernel data bandwidth]
   (let [kde (kernel-density-deriv-factory kernel data bandwidth)
         x (pick-space data)]
   (map kde x))))

(defn build-density-plot
  ([kernel data]
   (build-density-plot kernel data (pick-bandwidth data)))
  ([kernel data bandwidth]
   (let [kde (kernel-density-deriv-factory kernel data bandwidth)
         x (pick-space data)
         density (map kde x)]
     (c/scatter-plot x density))))
