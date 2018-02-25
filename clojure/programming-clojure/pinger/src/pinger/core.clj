(ns pinger.core
  (:require [pinger.scheduler :as scheduler])
  (:import [java.net URL HttpURLConnection]))

(defn response-code [address]
  (let [conn ^HttpURLConnection (.openConnection (URL. address))
        code (.getResponseCode conn)]
    (when (< code 400)
      (-> conn .getInputStream .close))
    code))

(defn available? [address]
  (= 200 (response-code address)))

(defn check []
  (let [addresses ["https://google.com"
                   "https://clojure.org"
                   "https://google.com/invalid-url"]]
    (while true
      (doseq [address addresses]
        (println address ": " (available? address))))))

(def immediately 0)
(def every-minute (* 1000 60))

(defn start [e]
  "REPL helper. Start pinger on executor e."
  (scheduler/periodically e check immediately every-minute))

(defn stop [e]
  "REPL helper. Stop executor e."
  (scheduler/shutdown-executor e))

(defn -main []
  (start (scheduler/scheduled-executor 1)))
