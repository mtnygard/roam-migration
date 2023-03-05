(ns org-emitter-test
  (:require [clojure.test :refer [deftest are run-tests]]
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

    [[:text/plain "this is"] [:text/bold "bolded"] [:text/plain "text"]]
    "this is *bolded* text"

    [[:text/italic "some stuff"] [:text/bold "other stuff"] [:inline-code "more stuff"]]
    "/some stuff/ *other stuff* ~more stuff~"

    [[:internal-link "that node" "aaaa-bbbb-ddddeeeeffffgggg"]]
    "[[id:aaaa-bbbb-ddddeeeeffffgggg][that node]]"

    [[:tag "TagHeadacheYes"]]
    "[[id:TagHeadacheYes][TagHeadacheYes]]"

    [[:attr "TagHeadache"]]
    "[[id:TagHeadache][TagHeadache]]"
    
    [[:block-code "console.log(\"hello\");" "javascript"]]
    (str/join \newline ["#+begin_src javascript"
                        "console.log(\"hello\");"
                        "#+end_src"])

    [[:text/plain "We have some header"] [:block-code "(println \"here\")" "clojure"] [:text/plain "Followed by more text"]]
    (str/join \newline ["We have some header"
                        "#+begin_src clojure"
                        "(println \"here\")"
                        "#+end_src"
                        "Followed by more text"])))

(run-tests)
