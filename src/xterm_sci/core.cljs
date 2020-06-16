(ns xterm-sci.core
  (:require ["xterm" :as xterm]
            ["local-echo" :as local-echo :default local-echo-controller]
            [clojure.tools.reader.reader-types :as r]
            [clojure.string :as str]
            [sci.core :as sci]
            [sci.impl.interpreter :as sci-impl]
            [sci.impl.vars :as vars]
            [sci.impl.parser :as p]
            [sci.impl.utils :as utils]
            [goog.crypt :as c]))

(defonce term (doto (xterm/Terminal.)
                (.open (js/document.getElementById "app"))))

(defonce line-discipline (local-echo-controller. term))

(def unconsumed-input (atom ""))
(def env (atom {}))
(def ctx (sci/init {:env env
                    :realize-max 1000
                    :profile :termination-safe
                    :classes {'js js/window}}))

;; (let [e* (sci/new-var ...), ctx (sci/init {:namespaces .... {'e* e*})] .... (sci/alter-var-root *e ...))

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
            (binding [*print-length* 20]
              (let [printed (try
                              (pr-str ret)
                              (catch :default e
                                (str "Error while printing: " (pr-str e))))]
                (.write term (str printed "\r\n" )))))
          (let [[expr remainder] form
                ret (try
                      (sci-impl/eval-form ctx expr)
                      (catch :default e
                        (swap! env assoc-in [:namespaces (vars/current-ns-name) '*e] (vars/SciVar. e '*e {}))
                        (.write term (pr-str e))
                        (.write term "\r\n")
                        ::none))]
            (reset! unconsumed-input remainder)
            (recur ret)))))))

(defn input-loop []
  (.then (.read line-discipline (str @vars/current-ns "=> "))
         (fn [line]
           (swap! unconsumed-input + line)
           (try-eval!)
           (input-loop))))

(defonce i
  (input-loop))
