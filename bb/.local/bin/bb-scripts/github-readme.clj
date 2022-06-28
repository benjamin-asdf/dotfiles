#!/usr/bin/env bb

(defn parse-github-url [s]
  (let [[_ owner repo] (re-find #"https://github.com/(.+?)/(.+?)$" s)]
    {:owner owner :repo repo}))

(defn main [url]
  (let
      [{:keys [owner repo]} (parse-github-url url)
       file (str "/tmp/githubreadme" (random-uuid) ".md")]
      (spit
       file
       (->
        (curl/get (str "https://api.github.com/repos/" owner "/" repo "/readme")
                  {:accept "application/vnd.github.v3.raw+json"})
        :body))
      (println file)))

(main (first *command-line-args*))
