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
