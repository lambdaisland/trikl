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

(defn charset-decode ^CharBuffer [^CharsetDecoder decoder ^ByteBuffer byte-buf]
  (.decode decoder byte-buf))

(.limit
 (.limit
  (charset-decode
   (charset-decoder)
   (ByteBuffer/wrap (into-array Byte/TYPE [1 2 3])))
  0))

(defn read-to-byte-buffer [^InputStream in ^ByteBuffer bb]
  (let [limit (.read in (.array bb) 0 (.capacity bb))]
    (println {::read-to-byte-buffer {:read limit
                                     :bytes (take limit (String. (.array bb)))}})
    (cond
      (= -1 limit)
      (throw (java.io.IOException. "End of stream reading input-stream"))

      (< 0 limit)
      (doto bb
        (.rewind)
        (.limit limit))

      :else
      (doto bb
        (.rewind)
        (.limit 0)))))
