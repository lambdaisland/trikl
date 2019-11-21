(ns lambdaisland.trikl1.io
  (:import java.io.InputStream
           [java.nio ByteBuffer CharBuffer]
           [java.nio.charset Charset CharsetDecoder CoderResult CodingErrorAction]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* #_true :warn-on-boxed)

(defn ^CharsetDecoder charset-decoder
  ([]
   (charset-decoder "UTF-8"))
  ([encoding]
   (.. (Charset/forName encoding)
       (newDecoder)
       (onMalformedInput CodingErrorAction/IGNORE)
       (onUnmappableCharacter CodingErrorAction/IGNORE))))

(defn charset-decode [^CharsetDecoder decoder ^ByteBuffer byte-buf]
  (.decode decoder byte-buf))

#_
(defn charset-decode [^CharsetDecoder decoder ^ByteBuffer byte-buf ^CharBuffer char-buf]
  (when (< 0 (.remaining byte-buf))
    (let [end-of-input     false
          ^CoderResult res (.decode decoder byte-buf char-buf end-of-input)]
      (when (.isUnderflow res)
        (.flush decoder char-buf)))
    (.flip char-buf))
  nil)

(defn read-to-byte-buffer [^InputStream in ^ByteBuffer bb]
  (let [limit (.read in (.array bb) 0 (.capacity bb))]
    (if (< 0 limit)
      (doto bb
        (.rewind)
        (.limit limit))
      (doto bb
        (.rewind)
        (.limit 0)))))


;; (def bb (ByteBuffer/allocate 8192))

;; (.get bb)

;; (.rewind bb)

;; (.limit bb 1000)
;; (.position bb 0)

;; (clojure.reflect/reflect CharsetDecoder)

;; (clojure.reflect/reflect java.io.InputStream)

;; (.array bb)
#_
(defn char-reader [^InputStream in]
  (let [buf-size 8192
        buffer   (byte-array buf-size)
        buffer'  (byte-array buf-size)
        decoder  (charset-decoder)]
    (fn []
      (let [end (.read in buffer 0 buf-size)
            {:keys [dest-end commands]} (filter-commands {:src buffer :dest buffer' :end end})]
        [commands (charset-decode decoder buffer' dest-end)]))))
