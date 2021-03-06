(in-ns 'kde-clj.core)

(defn kernel-gauss
  ([u]
   (/ (Math/exp (* -1/2 u u))
      (Math/sqrt (* 2 Math/PI))))
  ([x xi]  ; to keep same format as derivatives below
   (kernel-gauss (- x xi))))

;; would be nice to translate in terms of `u`
(defn kernel-gauss-deriv
  [x xi]
  (* (kernel-gauss (- x xi))
     (- xi x)))

(defn kernel-gauss-deriv2
  [x xi]
  (* (kernel-gauss x xi)
     (- (* (- x xi) (- x xi)) 1))) ;; product rule gives ((x - xi)^2 - 1)K(x)

(defn kernel-gauss-deriv3 ;; uhh don't actually need this one
  [x xi]
  (* (kernel-gauss x xi)
     (+ (* (- xi x) (- xi x) (- xi x))
        (- x xi)
        (* -2 (- xi x))))) ;; (xi-x)^3 + (x-xi) -2(xi-x)

(defn error-function-approx
  [x]
  (let [a1 0.278393
        a2 0.230389
        a3 0.000972
        a4 0.078108
        sum (+ 1
               (* a1 x)
               (* a2 x x)
               (* a3 x x x)
               (* a4 x x x x))
        denom (* sum sum sum sum)]
    (- 1 (/ 1 denom))))

(defn error-function-approx-2
  [x]
  (let [p 0.3275911
        a1 0.254829592
        a2 -0.284496736
        a3 1.421413741
        a4 -1.453152027
        a5 1.061405429
        t (/ 1 (+ 1 (* p x)))
        sum (+ (* a1 t)
               (* a2 t t)
               (* a3 t t t)
               (* a4 t t t t)
               (* a5 t t t t t))]
        (- 1 (* sum (Math/exp (- (* x x)))))))

(defn kernel-gauss-integral
  [x xi]
  (* 1/2
     (error-function-approx-2 (/ (- x xi) (Math/sqrt 2)))))

;; these seem to turn everything into a unimodal distribution
(defn kernel-parabolic
  [u]
  (* 3/4 (- 1 (* u u))))

(defn kernel-parabolic-deriv
  [x xi]
  (* 3/2
     (+ (* -2 x)
        (* 2 xi))))

(defn kernel-triweight
  [u]
  (* 35/32
     (Math/pow (- 1 (* u u)) 3)))

(defn kernel-triangular
  [u]
  (- 1 (Math/abs u)))
