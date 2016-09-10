(ns restruct.core-test
  (:require [clojure.test :refer :all]
            [com.billpiel.restruct.core :as rs]))

(deftest simple-tests
  (testing "simple stuff"
    (is (= (rs/restruct [0 1]
                        '[a b]
                        '[b a])
           [1 0]))

    (is (= (rs/restruct {:a {:b :c}}
                        '{x {y z}}
                        '{y {x z}})
           {:b {:a :c}}))))

(deftest trickier-tests
  (testing "trickier stuff"

    (is (= (rs/restruct {:a 1 :b [2]}
                        '{w x y [z]}
                        '[w x y z])
           [:a 1 :b 2]))

    (is (= (rs/restruct {1 {2 [3 4]
                            12 [10 11]}
                         5 {6 [7 8]
                            9 [10 11]}}
                        '{w {x [y z]}}
                        '{z {y [w x y z]}})
           {4 {3 [1 2 3 4]}
            11 {10 [1 12 10 11 5 9 10 11]}
            8 {7 [5 6 7 8]}}))))

(deftest trickiest-tests
  (testing "trickiest stuff"

    (is (= (rs/restruct {:a {:aa 1
                             :bb 2}
                         :b {:aa 3
                             :bb 4
                             :cc 11}
                         :c {:aa 5
                             :bb 6}
                         :x {:aa 7
                             :bb 8}}
                        '{z {zk zv}
                          :x ^::nil-kv {xk xv}}
                        '{zk {z zv :x xv}}
                        (fn [m]
                          (or (nil?  (m 'xk))
                              (= (m 'zk) (m 'xk)))))
           {:aa {:x 7
                 :a 1
                 :b 3
                 :c 5}
            :bb {:x 8
                 :a 2
                 :b 4
                 :c 6}}))

    (is (= (rs/restruct {1 {:a [2 3] :b 4 :c 5}
                         6 {:a [7 8] :b 9 :c 10 :d 11}}
                        '{z {:a [y x] w v}}
                        '{w [v x y z]})
           {:b [4 3 2 1 9 8 7 6]
            :c [5 3 2 1 10 8 7 6]
            :d [11 8 7 6]}))))
