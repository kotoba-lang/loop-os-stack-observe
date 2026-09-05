(ns loop-os-stack-observe.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [loop-os-stack-observe.core :as loop]))

(def valid-nvme-descriptor
  {:format loop/format-id
   :machine/id "reference-x86-64-nvme"
   :machine/provenance :vendor-declared
   :machine/source "NVM Express 2.0 §3.1.2 logical block 4KiB; §4.6 queue pairs"
   :page {:base-bytes 4096 :huge [2097152]}
   :storage [{:id :nvme0 :kind :block :block-bytes 4096 :queue-depth 64
              :seek-cost :none :max-transfer-bytes 131072}]})

(deftest transcription-checker-passes-valid-descriptor
  (is (empty? (loop/validation-errors valid-nvme-descriptor))))

(deftest transcription-checker-catches-real-errors
  (testing "format id must be :kotoba.machine/v1"
    (is (contains? (set (loop/validation-errors (assoc valid-nvme-descriptor :format :v2)))
                   :invalid-format)))
  (testing "block-bytes must be a power of two (2304B is the classic mistake)"
    (is (contains? (set (loop/validation-errors
                         (assoc-in valid-nvme-descriptor [:storage 0 :block-bytes] 2304)))
                   :invalid-storage-block-bytes)))
  (testing "seek-cost is closed over #{:none :low :high} — :fixed is a typo class"
    (is (contains? (set (loop/validation-errors
                         (assoc-in valid-nvme-descriptor [:storage 0 :seek-cost] :fixed)))
                   :invalid-storage-seek-cost)))
  (testing "max-transfer below block cannot be planned against"
    (is (contains? (set (loop/validation-errors
                         (assoc-in valid-nvme-descriptor [:storage 0 :max-transfer-bytes] 512)))
                   :storage-transfer-smaller-than-block))))

(deftest gap-ledger-seeds-match-measured-state
  (let [obs (loop/observe-gaps "resources/gaps/gap-ledger.edn")]
    (testing "2 closed, 4 open as measured 2026-09-05"
      (is (= 2 (count (:closed obs))))
      (is (= 4 (count (:open obs)))))
    (testing "dependency order: first open gap is wayland"
      (is (= :wayland-protocol-corpus (:gap/id (first (:open obs))))))))

(deftest next-tranche-picks-first-open-gap
  (let [t (loop/next-tranche (loop/observe-gaps "resources/gaps/gap-ledger.edn"))]
    (is (= :wayland-protocol-corpus (:gap t)))
    (is (seq (:action t)))))

(deftest run-cycle-appends-evidence
  (let [tmp (doto (java.io.File/createTempFile "osstack" ".edn") .delete)
        r (loop/run-cycle! {:root "." :ledger-path (.getPath tmp)})]
    (is (= :os-stack-observe-cycle (:event/type (:entry r))))
    (is (= 4 (count (:gaps-open-list (:entry r)))))
    (is (.exists (java.io.File. (.getPath tmp))))
    ;; append-only: run again, ledger grows
    (loop/run-cycle! {:root "." :ledger-path (.getPath tmp)})
    (is (= 2 (count (filter seq (str/split (slurp (.getPath tmp)) #"\n")))))))
