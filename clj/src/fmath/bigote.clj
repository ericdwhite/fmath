(ns fmath.bigote)

(defn mustache-compile [template]
  (let [mc (new com.sampullara.mustache.MustacheCompiler
              (new java.io.File "template"))]
    (. mc setOutputDirectory "classes")
    (. mc parseFile template)))

;;
;; Some ugly IO wrapping
(defn- mustache-string []
  " Wraps the underlying Java IO classes into
    an array of two parts. [0] the underlying
    string buffer, and [1] the output stream
    used in Mustache.execute"
  (let [sw (new java.io.StringWriter)]
    [sw (new com.sampullara.util.FutureWriter sw)]))

(defn mustache-execute [mustache data]
  "Executes a pre compiled mustache created with
   mustach-compile.  The data must have keys as
   strings, for example {\"name\" \"eric\"}."
  (let [resp (mustache-string)]
    (do
      (. mustache execute (resp 1) (new com.sampullara.mustache.Scope data))
      (. (resp 1) flush)
      (. (resp 0) toString))))
