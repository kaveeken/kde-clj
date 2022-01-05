(in-ns 'kde-clj.core)

(defn normalize
  [vector]
  (let [vecmin (apply min vector)
        zeroed (map #(- % vecmin) vector)
        zvecmax (apply max zeroed)]
    (mapv #(/ %1 zvecmax) zeroed)))

(defn un-normalize
  [x original]
  (let [vecmin (apply min vector)
        zeroed (map #(- % vecmin) vector)
        zvecmax (apply max zeroed)]
    (+ (* x zvecmax) vecmin)))

(defn linspace
  [start end n]
  (let [distance (- end start)
        stepsize (/ distance n)]
    (range start (+ end stepsize) stepsize)))

(defn show [chart]
  "Renders a chart and saves the result in a temp file"
  (i/save chart "/tmp/chart.png" :width 700 :height 500))

(defn plot-timeseries
  [y]
  (let [x (range (count y))]
    (c/scatter-plot x y)))

(defn pick-bandwidth
  ([data]
   (pick-bandwidth data 40))
  ([data target]
   (/ (- (apply max data)
         (apply min data))
      target)))

(defn pick-space
  ([data]
   (pick-space data 1000))
  ([data n]
   (linspace (apply min data)
             (apply max data)
             n)))
