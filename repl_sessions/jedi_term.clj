(ns jedi-term
  (:require [trikl.screen :as screen])
  (:import (com.jediterm.terminal.emulator JediEmulator)
           (com.jediterm.terminal ArrayTerminalDataStream
                                  TerminalColor
                                  TerminalOutputStream
                                  TerminalDisplay
                                  TerminalDataStream
                                  TerminalDataStream$EOF)
           (com.jediterm.terminal.model JediTerminal
                                        StyleState
                                        TerminalTextBuffer
                                        TerminalLine)))

(defprotocol IColor
  (to-rgb [_]))

(extend-protocol IColor
  java.awt.Color
  (to-rgb [c]
    [(.getRed c) (.getGreen c) (.getBlue c)])
  TerminalColor
  (to-rgb [tc]
    (to-rgb (.toAwtColor tc)))
  nil
  (to-rgb [_]))

(defn terminal-display [^TerminalTextBuffer text-buffer]
  (reify TerminalDisplay
    (getRowCount [this]
      (.getHeight text-buffer))
    (getColumnCount [this]
      (.getWidth text-buffer))
    (setCursor [this x y])
    (setCursorShape [this shape])
    (beep [this])
    (requestResize [this pendingResize origin cursorX cursorY resizeHandler]
      (.resize text-buffer pendingResize origin cursorX cursorY resizeHandler))
    (scrollArea [this scrollRegionTop scrollRegionSize dy])
    (setCursorVisible [this shouldDrawCursor])
    (setScrollingEnabled [this enabled])
    (setBlinkingCursor [this enabled])
    (setWindowTitle [this name])
    (setCurrentPath [this path])
    (terminalMouseModeSet [this mode])
    (ambiguousCharsAreDoubleWidth [this] false)))

(defn queue-data-stream [queue]
  (reify TerminalDataStream
    (^char getChar [this]
     (let [ch (.poll queue)]
       (if ch
         ch
         (throw (TerminalDataStream$EOF.)))))
    (^void pushChar [this ^char ch]
     (.addFirst queue ch))
    (readNonControlCharacters [this max-chars]
      (loop [max-chars max-chars
             chars []]
        (if (< 0 max-chars)
          (let [next (.poll queue)]
            (if (and (< 0 max-chars) next (<= (long \space) (long next)))
              (recur (dec max-chars) (conj chars next))
              (apply str chars)))
          (apply str chars))))
    (pushBackBuffer [this chars length]
      (doseq [ch (take length chars)]
        (.addLast queue ch)))))

(defn deque ^java.util.Deque []
  (java.util.concurrent.LinkedBlockingDeque.))

(defn emulator [width height]
  (let [state (StyleState.)
        text-buffer (TerminalTextBuffer. width height state)
        terminal (JediTerminal. (terminal-display text-buffer) text-buffer state)
        queue (deque)

        data-stream (queue-data-stream queue)]
    {:state state
     :text-buffer text-buffer
     :terminal terminal
     :queue queue
     :emulator (JediEmulator.
                data-stream
                ;; TODO: hook this up so we can get stuff like resize events
                (reify TerminalOutputStream
                  (^void sendBytes [this ^bytes response])
                  (^void sendString [this ^String string]))
                terminal)}))

(defn write-to-emulator [{:keys [emulator queue]} string]
  (doseq [ch string]
    (.addLast queue ch))
  (.resetEof emulator)
  (while (.hasNext emulator)
    (.next emulator)))

(defn text-buffer->charels [text-buffer]
  (vec (for [y (range (.getWidth text-buffer))]
         (vec (for [x (range (.getHeight text-buffer))
                    :let [^TerminalLine line (.getLine text-buffer y)]
                    :when line
                    :let [ch (.charAt line x)
                          style (.getStyleAt line x)]]
                (screen/->Charel ch
                                 (when style (to-rgb (.getForeground style)))
                                 (when style (to-rgb (.getBackground style)))))))))

(comment
  (def emu (emulator 10 10))

  (.getScreenLines (:text-buffer emu))

  (write-to-emulator emu "[38;2;10;20;30mxxxxx")

  (def ss (text-buffer->charels (:text-buffer emu)))

  (= ss (:charels (screen/new-screen 10 10)))

  (let [s1 ss #_(screen/new-screen 10 10)
        s2 (-> (screen/new-screen 10 10)
               (assoc-in [:charels 0 0 :fg] [10 20 30])
               (assoc-in [:charels 0 0 :char] \y)
               (assoc-in [:charels 1 2 :char] \z)
               :charels)
        sb (StringBuilder.)]
    (screen/diff sb {:fg (to-rgb (.getForeground (.getCurrent (:state emu))))
                     :bg (to-rgb (.getBackground (.getCurrent (:state emu))))}
                 s1
                 s2)
    (write-to-emulator emu (str sb)))




  )
