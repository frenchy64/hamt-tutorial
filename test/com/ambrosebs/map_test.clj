(ns com.ambrosebs.map-test
  (:require [clojure.test :refer :all]
            [collection-check :refer [assert-map-like]]
            [clojure.test.check.generators :as gen]
            [com.ambrosebs.map :as map]))

(deftest a-test
  (is (assert-map-like (map/create {}) gen/int gen/int)))
