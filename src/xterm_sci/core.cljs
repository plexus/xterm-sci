(ns xterm-sci.core
  (:require ["local-echo" :as local-echo :default local-echo-controller]
            ["xterm" :as xterm]
            [clojure.string :as str]
            [sci.core :as sci]))

(defonce term (doto (xterm/Terminal.)
                (.open (js/document.getElementById "app"))))
(defonce line-discipline (local-echo-controller. term))
(defonce unconsumed-input (atom ""))
(defonce await-more-input (atom false))
(defonce last-ns (atom @sci/ns))
(defonce last-error (sci/new-dynamic-var '*e nil))
(defonce ctx (sci/init {:realize-max 1000
                        :profile :termination-safe
                        :classes {'js js/window}
                        :namespaces {'clojure.core {'*e last-error}}}))

;; (let [e* (sci/new-var ...), ctx (sci/init {:namespaces .... {'e* e*})] .... (sci/alter-var-root *e ...))

(defn chop [s line col]
  (subs (str/join (drop (dec line) (str/split-lines s))) col))

(defn skip-handled-input [reader]
  (swap! unconsumed-input chop
         (sci/get-line-number reader)
         (sci/get-column-number reader)))

(defn handle-error [last-error e]
  (sci/alter-var-root last-error (constantly e))
  (.write term (ex-message e))
  (.write term "\r\n"))

(defn read-form [reader]
  (try
    (let [form (sci/parse-next ctx reader)]
      (skip-handled-input reader)
      form)
    (catch :default e
      (if (str/includes? (.-message e) "EOF while reading")
        ::eof-while-reading
        (do
          (handle-error last-error e)
          (skip-handled-input reader)
          ;;we're done handling this input
          ::sci/eof)))))

(defn print-val [v]
  (binding [*print-length* 20]
    (let [printed (try
                    (pr-str v)
                    (catch :default e
                      (str "Error while printing: " (pr-str e))))]
      (.write term (str printed "\r\n")))))

(defn eval! []
  (sci/with-bindings {sci/ns @last-ns
                      last-error @last-error}
    (loop []
      (let [reader (sci/reader @unconsumed-input)
            form (read-form reader)]
        (cond (= ::sci/eof form) (reset! await-more-input false)
              (= ::eof-while-reading form) (reset! await-more-input true)
              :else
              (let [ret (try
                          (sci/eval-form ctx form)
                          (catch :default e
                            (handle-error last-error e)
                            ::err))]
                (when-not (= ::err ret) ;; do nothing, continue in input-loop
                  (print-val ret)
                  (reset! last-ns @sci/ns)
                  (recur))))))))

(defn prompt []
  (if @await-more-input "> "
      (str @last-ns "=> ")))

(defn input-loop []
  (.then (.read line-discipline (prompt))
         (fn [line]
           (swap! unconsumed-input (fn [input]
                                     (if-not (str/blank? input)
                                       (str input " " line)
                                       line)))
           (eval!)
           (input-loop))))

(defonce i ;; don't start another input loop on hot-reload
  (input-loop))
