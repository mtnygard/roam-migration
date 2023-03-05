(ns roam-parser-test
  (:require
   [clojure.test :refer [are deftest run-tests]]
   [roam :refer [parse]]))

(deftest roam-markdown-parsing
  (are [in expected] (= expected (parse in))
    ""
    []

    "this is plain text. it should be unchanged"
    [[:text/plain "this is plain text. it should be unchanged"]]

    "**bold**"
    [[:text/bold "bold"]]
    
    "this has **bold** text"
    [[:text/plain "this has "] [:text/bold "bold"] [:text/plain " text"]]
    
    "this is __emphatic text__"
    [[:text/plain "this is "] [:text/italics "emphatic text"]]

    "this has `inline code` with stuff after"
    [[:text/plain "this has "] [:inline-code "inline code"] [:text/plain " with stuff after"]]
    
    "Attribute:: looks-like-this"
    [[:attr "Attribute"] [:text/plain " looks-like-this"]]

    "This is # not a tag"
    [[:text/plain "This is # not a tag"]]
    
    "This is a #OneWordTag"
    [[:text/plain "This is a "] [:tag "OneWordTag"]]

    "#Tag/Alt"
    [[:tag "Tag/Alt"]]

    "This is #atag# not a word enclosed in #"
    [[:text/plain "This is "] [:tag "atag"] [:text/plain "# not a word enclosed in #"]]

    "This is a #[[Three Word Link written as a tag]]"
    [[:text/plain "This is a "] [:internal-link "Three Word Link written as a tag"]]

    "This is a [[roam link]]"
    [[:text/plain "This is a "] [:internal-link "roam link"]]

    "This text contains\n```javascript\nconsole.log(\"Some code\");\n```\nwith some junk after"
    [[:text/plain "This text contains\n"] [:source "console.log(\"Some code\");\n" "javascript"] [:text/plain "\nwith some junk after"]]

    "Attribute:: #WithTag [[and link]]"
    [[:attr "Attribute"] [:text/plain " "] [:tag "WithTag"] [:text/plain " "] [:internal-link "and link"]]

    "This is a [markdown link](https://daringfireball.net/projects/markdown/)"
    [[:text/plain "This is a "] [:hyperlink "https://daringfireball.net/projects/markdown/" "markdown link"]]

    "This is an [incomplete link](https://example.com"
    [[:text/plain "This is an [incomplete link](https://example.com"]]

    "This has an empty inline code segment: ``"
    [[:text/plain "This has an empty inline code segment: "] [:inline-code ""]]
    ))


(run-tests)
