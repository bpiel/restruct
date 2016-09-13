(ns com.billpiel.restruct.core-test
  (:require [clojure.test :refer :all]
            [com.billpiel.restruct.core :as rs]
            [clojure.data :as data]))

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

(deftest cycle-tests
  (testing "rest test"
    (is (= (rs/restruct [1 2]
                        '[a ::rs/& b]
                        '[a b])
           [1 2]))
    (is (= (rs/restruct [1 2 3 4]
                        '[a ::rs/& b]
                        '[a b])
           [1 [2 3 4]]))
    (is (= (rs/restruct [1 2 3 4]
                        '[a ::rs/& b]
                        '[a ^:first b])
           [1 2]))
    (is (= (rs/restruct [1 2 3 4]
                        '[a ::rs/& b]
                        '[a ^:last b])
           [1 4]))
    (is (= (rs/restruct [1 2 3 4]
                        '[a ::rs/& b]
                        '[a ^:vec b])
           [1 [2 3 4]]))
    (is (= (rs/restruct [1 2 3 4]
                        '[a ::rs/& b c]
                        '[a b c])
           [1 [2 4] 3]))))

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
                        '{zk {z zv :x ^:first xv}}
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

(comment 

  (def test-input2
    {:measures
     {:plans
      [{"customer count"
        {:query 111
         :render 222},
        ":post" {}}],
      :rjql
      [{:src-alias nil, :expr ":value", :output :__m1, :top-level? true}]},
     :rows
     {:plans
      [{"customer count"
        {:query 333
         :render 444},
        ":post" {}}],
      :rjql
      [{:src-alias nil, :expr "313", :output :__r1, :top-level? true}]},
     :columns
     {:plans
      [{"customer count"
        {:xfrms 555
         :query 666
         :render 777
         :ranges 888},
        ":post" {:ranges 999}}],
      :rjql
      [{:time ":time",
        :interval "Quarter",
        :src-alias nil,
        :output :__c1,
        :top-level? true}]},
     :sources
     {"customer count" 1212
      ":post" 1313}}) 

  (def test-input1
    {:measures
     {:plans
      [{"customer count"
        {:query
         [{:expr :%count.310,
           :command-id 1,
           :output :__m1}]
         :render
         {:type [:metric :standard],
          :default-value 0,
          :command-id 1,
          :metric-header "customer count",
          :output :__m1}},
        ":post" {}}],
      :rjql
      [{:src-alias nil, :expr ":value", :output :__m1, :top-level? true}]},
     :rows
     {:plans
      [{"customer count"
        {:query
         [{:expr :313,
           :output :__r1}]
         :render
         {:type [:metric :standard],
          :metric-header "customer count",
          :output :__r1}},
        ":post" {}}],
      :rjql
      [{:src-alias nil, :expr "313", :output :__r1, :top-level? true}]},
     :columns
     {:plans
      [{"customer count"
        {:xfrms
         [{:type :time,
           :inputs {:__c1_year :year, :__c1_quarter :quarter},
           :interval :quarter,
           :format nil,
           :output :__c1}]
         :query
         [{:output :__c1_year,
           :expr {:s "extract(year from \"311\")"}}
          {:output :__c1_quarter,
           :expr {:s "extract(quarter from \"311\")"}}]
         :render
         {:type [:metric :time],
          :metric-header "customer count",
          :output :__c1},
         :ranges
         {:start nil,
          :end
          {:day 12,
           :minute 0},
          :interval "Quarter",
          :output :__c1}},
        ":post" {:ranges {:interval "Quarter", :output :__c1}}}],
      :rjql
      [{:time ":time",
        :interval "Quarter",
        :src-alias nil,
        :output :__c1,
        :top-level? true}]},
     :sources
     {"customer count"
      {:metric 43,
       :alias "customer count",
       :timerange
       [{:field ":time",
         :start nil,
         :end
         {:day 12,
          :minute 0}}],
       :type :metric,
       :query
       {:type :metric,
        :where
        [[:and
          [:>= :311 #inst "1970-01-01T00:00:00.000000000-00:00"]
          [:< :311 #inst "2016-09-13T00:00:00.000000000-00:00"]]]
        :order [],
        :limit nil}}
      ":post" {:type :post}}})

  (def test-output1
    {:query
     {"customer count"
      {:measures
       [{:expr :%count.310,
         :command-id 1,
         :output :__m1}],
       :rows
       [{:expr :313,
         :output :__r1}],
       :columns
       [{:output :__c1_year,
         :expr {:s "extract(year from \"311\")"}}
        {:output :__c1_quarter,
         :expr {:s "extract(quarter from \"311\")"}}],
       :source
       {:type :metric,
        :where
        [[:and
          [:>= :311 #inst "1970-01-01T00:00:00.000000000-00:00"]
          [:< :311 #inst "2016-09-13T00:00:00.000000000-00:00"]]]
        :order [],
        :limit nil,}},
      ":post" {:measures [], :rows [], :columns [], :source nil}},
     :transform
     {"customer count"
      {:measures [],
       :rows [],
       :columns
       [{:type :time,
         :inputs {:__c1_year :year, :__c1_quarter :quarter},
         :interval :quarter,
         :format nil,
         :output :__c1}],
       :source nil},
      ":post" {:measures [], :rows [], :columns [], :source nil}},
     :ranges
     {:rows {nil [nil nil]},
      :columns
      {:__c1
       [{:start nil,
         :end
         {:day 12,
          :minute 0},
         :interval "Quarter",
         :output :__c1}
        {:interval "Quarter", :output :__c1}]}},
     :render
     {:measures
      {:__m1
       [{:type [:metric :standard],
         :default-value 0,
         :command-id 1,
         :metric-header "customer count",
         :output :__m1}],
       nil [nil]},
      :rows
      {:__r1
       [{:type [:metric :standard],
         :metric-header "customer count",
         :output :__r1}],
       nil [nil]},
      :columns
      {:__c1
       [{:type [:metric :time],
         :metric-header "customer count",
         :output :__c1}],
       nil [nil]},
      :sources
      {"customer count"
       {:metric 43,
        :alias "customer count",
        :timerange
        [{:field ":time",
          :start nil,
          :end
          {:day 12,
           :minute 0}}],
        :type :metric,
        :query
        {:type :metric,
         :where
         [[:and
           [:>= :311 #inst "1970-01-01T00:00:00.000000000-00:00"]
           [:< :311 #inst "2016-09-13T00:00:00.000000000-00:00"]]]
         :order [],
         :limit nil,}},
       ":post" {:type :post}}},
     :parsed
     {:measures
      [{:src-alias nil, :expr ":value", :output :__m1, :top-level? true}],
      :rows
      [{:src-alias nil, :expr "313", :output :__r1, :top-level? true}],
      :columns
      [{:time ":time",
        :interval "Quarter",
        :src-alias nil,
        :output :__c1,
        :top-level? true}],
      :sources nil}})



  (deftest beyond-tests
    (testing "beyond everything"

      (is (= (rs/restruct test-input1
                          '{dim {:plans [{src {step x}}] :rjql rjql}
                            :sources {src2 y}}
                          '{step {src {dim [x] :source y}}}
                          (fn [m]
                            (= (m 'src) (m 'src2))))))))


  (clojure.pprint/pprint (data/diff (rs/restruct test-input1
                                                 '{mrc {:plans [{src {step sv
                                                                      :ranges {rak rav :ouput raout}
                                                                      :render {rek rev :ouput reout}
                                                                      :xfrms x}}]
                                                        :rjql rjql}
                                                   :sources {src2 srcv}}
                                                 '{step {src {mrc [sv]
                                                              :source srcv}}
                                                   :transform {src {mrc [x]
                                                                    :source srcv}}
                                                   :ranges {mrc {raout [{rak rav :output raout}]}}
                                                   :render {mrc {reout [{rek rev :output reout}]}}
                                                   :parsed {mrc rjql}}
                                                 (fn [m]
                                                   (= (m 'src) (m 'src2))))
                                    test-output1))


  (rs/restruct [{:k 1 :b 2 :c 3}]
               '[{:k k ks vs}{:k k ks vs}]
               '{k {ks vs}})

  (comment))
