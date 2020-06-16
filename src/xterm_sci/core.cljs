(ns xterm-sci.core
  (:require ["xterm" :as xterm]
            #_["local-echo" :as local-echo]
            [clojure.tools.reader.reader-types :as r]
            [clojure.string :as str]
            [sci.core :as sci]
            [sci.impl.interpreter :as sci-impl]
            [sci.impl.vars :as vars]
            [sci.impl.parser :as p]
            [sci.impl.utils :as utils]
            [goog.crypt :as c]))

;; causing compilation issues
#_local-echo/LocalEchoController


(defn string->byte-seq [s]
  (c/stringToUtf8ByteArray s))

(defonce term (doto (xterm/Terminal.)
                (.open (js/document.getElementById "app"))
                (.write "user=> ")))

(def unconsumed-input (atom ""))
(def env (atom {}))
(def ctx (sci/init {:env env}))

(defn read-form [s]
  (try
    (let [string-reader (r/string-reader s)
          buf-len 1
          pushback-reader (r/PushbackReader. string-reader (object-array buf-len) buf-len buf-len)
          reader (r/indexing-push-back-reader pushback-reader)
          res (p/parse-next ctx reader)]
      (if (= :edamame.impl.parser/eof res)
        ::eof
        [res (subs s (.-s-pos string-reader))]))
    (catch :default e
      (when (str/includes? (.-message e) "EOF while reading")
        ::eof))))

(defn try-eval! []
  (vars/with-bindings {vars/current-ns @vars/current-ns}
    (loop [ret ::none]
      (let [form (read-form @unconsumed-input)]
        (if (= ::eof form)
          (when-not (= ::none ret)
            (.write term (str (pr-str ret) "\r\n" @vars/current-ns "=> ")))
          (let [[expr remainder] form
                ret (sci-impl/eval-form ctx expr)]
            (reset! unconsumed-input remainder)
            (recur ret)))))))


(defonce handler
  (.onData term (fn [data]
                  (prn (string->byte-seq data))
                  (if (= (char 127) data)
                    (swap! unconsumed-input
                           (fn [input]
                             (let [lines (str/split input #"\r")]
                               (if (seq (last lines))
                                 (do
                                   (.write term "\u001b[1D \u001b[1D")
                                   (subs input 0 (dec (count input))))
                                 input))))
                    (do
                      (swap! unconsumed-input + data)
                      (.write term data)
                      (when (str/includes? data "\r")
                        (.write term "\r\n")
                        (try-eval!)))))))
