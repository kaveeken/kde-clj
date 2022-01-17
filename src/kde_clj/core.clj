(ns kde-clj.core
  (:require [clojure.string :as str]
            [incanter.stats :as s]
            [incanter.charts :as c]
            [incanter.core :as i]))

(load "density")
(load "kernels")

(defn gauss-kde
  ([data]
   (gauss-kde data (pick-bandwidth data)))
  ([data bandwidth]
   (kernel-density-factory kernel-gauss data bandwidth)))

(defn compute-gauss-density
  ([data] compute-gauss-density data (pick-bandwidth data))
  ([data bandwidth]
   (let [kde (gauss-kde data bandwidth)
         x (pick-space data)]
     (map kde x))))

(defn integrate-gauss
  ([data a b dx] (integrate-gauss data a b dx (pick-bandwidth data)))
  ([data a b dx bandwidth]
   (let [kde (gauss-kde data bandwidth)]
     (integrate kde a b dx))))


;;(def dummy (reduce conj (linspace 1 1.2 1000) (linspace 1 1.3 1000)))
;;(time (integrate
;;       (kernel-density-deriv-factory kernel-gauss dummy (pick-bandwidth dummy))
;;       1.2 1.35 0.01))

