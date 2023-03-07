(ns org-emitter-test
  (:require [clojure.test :refer [deftest are is run-tests]]
            [org]
            [clojure.string :as str]))

(deftest emit-org-syntax
  (are [segments expected] (= expected (org/emit segments))
    [[:text/plain "this is text"]]
    "this is text"

    [[:text/header "heading" 1]]
    "* heading"

    [[:text/header "heading" 2]]
    "** heading"

    [[:text/header "heading" 3]]
    "*** heading"
    
    [[:hyperlink "http://www.example.com" "an example"]]
    "[[http://www.example.com][an example]]"

    [[:text/plain "this is "] [:text/bold "bolded"] [:text/plain " text"]]
    "this is *bolded* text"

    [[:text/italic "some stuff"] [:text/plain " "] [:text/bold "other stuff"] [:text/plain " "] [:inline-code "more stuff"]]
    "/some stuff/ *other stuff* ~more stuff~"

    [[:internal-link "that node" "aaaa-bbbb-ddddeeeeffffgggg"]]
    "[[id:aaaa-bbbb-ddddeeeeffffgggg][that node]]"

    [[:tag "TagHeadacheYes"]]
    "[[id:TagHeadacheYes][TagHeadacheYes]]"

    [[:attr "TagHeadache"]]
    "[[id:TagHeadache][TagHeadache]]"
    
    [[:source "console.log(\"hello\");" "javascript"]]
    (str/join \newline [""
                        "#+begin_src javascript"
                        "console.log(\"hello\");"
                        "#+end_src\n"])

    [[:text/plain "We have some header"] [:source "(println \"here\")" "clojure"] [:text/plain "Followed by more text"]]
    (str/join \newline ["We have some header"
                        "#+begin_src clojure"
                        "(println \"here\")"
                        "#+end_src"
                        "Followed by more text"])))

(defn- daily-entity [mmddyyyy human]
  {:block/uid mmddyyyy
   :block/title human})



(deftest org-node-paths
  (is (= "dest/daily/2023-03-06.org" (org/daily-path "dest" (daily-entity "03-06-2023" "March 6th, 2023"))))
  
  )

(run-tests)
