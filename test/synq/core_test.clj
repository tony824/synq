(ns synq.core-test
  (:require [clojure.test :refer :all]
            [synq.core :refer :all])
  (:import (clojure.lang ExceptionInfo)))

(deftest test-change-coins
  (testing "Should change 1 cent"
    (is (= {1 1}
           (change-coins 1))))

  (testing "Should change 3 cents"
    (is (= {1 3}
           (change-coins 3))))

  (testing "Should change 5 cents"
    (is (= {5 1}
           (change-coins 5))))

  (testing "Should change 7 cents"
    (is (= {5 1 1 2}
           (change-coins 7))))

  (testing "Should change 11 cents"
    (is (= {10 1 1 1}
           (change-coins 11))))

  (testing "Should change 13 cents"
    (is (= {10 1 1 3}
           (change-coins 13)))))

(deftest test-build-coin-map

  (testing "Start with an empty list and amount"
    (let [amount 27
          m (build-coin-map amount)]
      (is (= amount
             (:amount m)))
      (is (= []
             (:coins m)))))

  (testing "Throw exception"
    (let [amount "amount"]
      (is (thrown-with-msg? ExceptionInfo #"Not supported string" (build-coin-map amount))))
    (let [amount 12.3343434]
      (is (thrown-with-msg? ExceptionInfo #"Only" (build-coin-map amount))))))

(deftest test-update-coins

  (testing "Should process 1 cent correctly"
    (let [cc (build-coin-map 1)]
      (is (= {:amount 0
              :coins [1]}
             (update-coins cc 1)))))

  (testing "Should precess 3 cents"
    (let [cc (build-coin-map 3)]
      (is (= {:amount 3
              :coins []}
             (update-coins cc 5)))))

  (testing "Should remain 2 cents when making change for 7 cents by 5"
    (let [cc (build-coin-map 7)]
      (is (= {:amount 2
              :coins [5]}
             (update-coins cc 5))))))
