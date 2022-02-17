(ns lambdaisland.trikl1.telnet-test
  (:require [clojure.test :refer :all]
            [lambdaisland.trikl1.telnet :as telnet])
  (:import (java.nio ByteBuffer CharBuffer)))

(def NUL (unchecked-byte 0))

(defn bytes->string
  "Turn a byte array into a string, truncates at the first NULL byte."
  [bs]
  (String.
   (if (some #{NUL} bs)
     (into-array Byte/TYPE (take (.indexOf (seq bs) NUL) bs))
     bs)))

(defn bb->str
  "Turn a bytebuffer into a string, truncates at the first NULL byte."
  [^ByteBuffer bb]
  (bytes->string (.array bb)))

(defn byte-buffer
  "Create a byte-buffer from a string or with a size"
  [s-or-n]
  (if (string? s-or-n)
    (ByteBuffer/wrap (.getBytes ^String s-or-n))
    (ByteBuffer/allocate s-or-n)))

(defn bb-put
  "Write strings or telnet commands to a byte-buffer"
  [bb & args]
  (doseq [a args]
    (if (string? a)
      (run! #(.put bb %) (.getBytes ^String a))
      (run! #(.put bb %) (telnet/telnet-command-bytes [a])))))

(deftest filter-commands-test
  (testing "plain ascii characters pass right through"
    (let [src (byte-buffer "abc")
          dest (byte-buffer 5)]
      (is (= {:commands []}
             (telnet/filter-commands {:src src :dest dest})))
      (is (= "abc" (bb->str dest)))))

  (testing "filters out telnet commands"
    (let [src (ByteBuffer/allocate 20)
          dest (ByteBuffer/allocate 20)]
      (.mark src)
      (bb-put src "abc" :IAC :DO :LINEMODE "def")
      (.reset src)

      (is (= {:commands [[:IAC :DO :LINEMODE]]}
             (telnet/filter-commands {:src src :dest dest})))
      (is (= "abcdef" (bb->str dest))))))
