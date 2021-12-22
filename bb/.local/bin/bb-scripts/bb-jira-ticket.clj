#!/usr/bin/env bb

(require
 '[clojure.string :as str])
(require
 '[clojure.java.shell :as shell])
(require '[cheshire.core :as json])
(require '[babashka.curl :as curl])

(def config
  {:url
   "https://singularitygroup.atlassian.net"
   :username
   "benjamin.schwerdtner@gmail.com"
   :token
   (str/trim
    (:out
     (shell/sh
      "pass" "jira-api-token")))})

(defn req [url]
  {:url (str (:url config) url)
   :headers {"Content-Type" "application/json"}
   :basic-auth [(:username config) (:token config)]})

(defn
  ticket
  [id]
  (let [opts (req
              (str "/rest/api/3/issue/" id))]
    (->
     (curl/get (:url opts) opts)
     :body
     (json/decode keyword))))


(when (= *file* (System/getProperty "babashka.file"))
  (let [tkt (ticket
             (str/upper-case
              (first *command-line-args*)))]
    (if-let
        [out
         (second *command-line-args*)]
        (spit out (prn-str tkt))
        (clojure.pprint/pprint tkt))))
