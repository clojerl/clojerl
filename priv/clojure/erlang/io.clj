;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns
  ^{:author "Stuart Sierra, Chas Emerick, Stuart Halloway",
     :doc "This file defines polymorphic I/O utility functions for Clojure."}
    clojure.erlang.io
    (:require clojure.string)
    (:import
     (erlang.io IReader IWriter File
                StringReader Closeable)
     #_(java.net URI URL MalformedURLException Socket URLDecoder URLEncoder)))


(defprotocol ^{:added "1.2"} Coercions
  "Coerce between various 'resource-namish' things."
  (^{:tag erlang.io.File, :added "1.2"} as-file [x] "Coerce argument to a file.")
  (^{:tag clojerl.String, :added "1.2"} as-url [x] "Coerce argument to a URL."))

(defn- url-encode [uri]
  (http_uri/encode.e (erlang/binary_to_list.e uri)))

(defn- url-decode [encoded-uri]
  (http_uri/decode.e (erlang/binary_to_list.e encoded-uri)))

(defn- escaped-utf8-urlstring->str [s]
  (-> (clojure.string/replace s "+" (url-encode "+"))
      url-decode))

(extend-protocol Coercions
  nil
  (as-file [_] nil)
  (as-url [_] nil)

  clojerl.String
  (as-file [s] (erlang.io.File/open.e s))
  (as-url [s] s)

  erlang.io.File
  (as-file [f] f)
  (as-url [f] (erlang.io.File/path.e f))

  #_ (URL
       (as-url [u] u)
       (as-file [u]
                (if (re-find #"file://" u)
                  (as-file (escaped-utf8-urlstring->str
                            (clojure.string/replace u \/ (erlang.io.File/separator_char.e))))
                  (throw (str "Not a file: " u))))))

(defprotocol ^{:added "1.2"} IOFactory
  "Factory functions that create ready-to-use, versions of
   the various I/O types, on top of anything that can
   be unequivocally converted to the requested kind of stream.

   Common options include

     :append    true to open stream in append mode
     :encoding  string name of encoding to use, e.g. \"UTF-8\".

   Callers should generally prefer the higher level API provided by
   reader, writer, input-stream, and output-stream."
  (^{:added "1.2"} make-reader [x opts] "Creates a BufferedReader. See also IOFactory docs.")
  (^{:added "1.2"} make-writer [x opts] "Creates a BufferedWriter. See also IOFactory docs."))

(defn ^erlang.io.IReader reader
  "Attempts to coerce its argument into an open java.io.Reader.
   Default implementations always return a java.io.BufferedReader.

   Default implementations are provided for Reader, BufferedReader,
   InputStream, File, URI, URL, Socket, byte arrays, character arrays,
   and String.

   If argument is a String, it tries to resolve it first as a URI, then
   as a local file name.  URIs with a 'file' protocol are converted to
   local file names.

   Should be used inside with-open to ensure the Reader is properly
   closed."
  {:added "1.2"}
  [x & opts]
  (make-reader x (when opts (apply hash-map opts))))

(defn ^erlang.io.IWriter writer
  "Attempts to coerce its argument into an open java.io.Writer.
   Default implementations always return a java.io.BufferedWriter.

   Default implementations are provided for Writer, BufferedWriter,
   OutputStream, File, URI, URL, Socket, and String.

   If the argument is a String, it tries to resolve it first as a URI, then
   as a local file name.  URIs with a 'file' protocol are converted to
   local file names.

   Should be used inside with-open to ensure the Writer is properly
   closed."
  {:added "1.2"}
  [x & opts]
  (make-writer x (when opts (apply hash-map opts))))

(defn- ^clojerl.Boolean append? [opts]
  (boolean (:append opts)))

(defn- ^clojerl.String encoding [opts]
  (or (:encoding opts) "UTF-8"))

(defn- buffer-size [opts]
  (or (:buffer-size opts) 1024))

(defn file-encoding [opts]
  #erl [:encoding
        (if (= "latin1" (:encoding opts)) :latin1 :utf8)])

(defn file-open [path modes]
  (erlang.io.File/open.e path (clj_core/to_list.1 modes)))

(defn file-path [file]
  (erlang.io.File/path.e file))

(extend-type clojerl.String
  IOFactory
  (make-reader [this opts]
    (let [enc  (file-encoding opts)]
      (file-open this [:read enc])))
  (make-writer [this opts]
    (let [mode (if (:append opts) :append :write)
          enc  (file-encoding opts)]
      (file-open this [mode enc]))))

(extend-type erlang.io.File
  IOFactory
  (make-reader [this opts]
    (let [enc  (file-encoding opts)]
      (file-open (file-path this) [:read enc])))
  (make-writer [this opts]
    (let [mode (if (:append opts) :append :write)
          enc  (file-encoding opts)]
      (file-open (file-path this) [mode enc]))))

(extend-type nil
  IOFactory
  (make-reader [x opts]
    (throw (str "Cannot open <" (pr-str x) "> as a Reader.")))
  (make-writer [x opts]
    (throw (str "Cannot open <" (pr-str x) "> as a Writer."))))

(defmulti
  ^{:doc "Internal helper for copy"
     :private true
     :arglists '([input output opts])}
  do-copy
  (fn [input output opts] [(type input) (type output)]))

(defmethod do-copy [erlang.io.File erlang.io.StringWriter]
  [input output opts]
  (loop []
    (let [line (erlang.io.File/read_line.e input)]
      ;; (erlang/display.e line)
      (when (not= line :eof)
        (do (erlang.io.StringWriter/write.e output line)
            (recur))))))

(defn copy
  "Copies input to output.  Returns nil or throws IOException.
  Input may be an InputStream, Reader, File, byte[], or String.
  Output may be an OutputStream, Writer, or File.

  Options are key/value pairs and may be one of

    :buffer-size  buffer size to use, default is 1024.
    :encoding     encoding to use if converting between
                  byte and char streams.

  Does not close any streams except those it opens itself
  (on a File)."
  {:added "1.2"}
  [input output & opts]
  (do-copy input output (when opts (apply hash-map opts))))

(defn ^clojerl.String as-relative-path
  "Take an as-file-able thing and return a string if it is
   a relative path, else IllegalArgumentException."
  {:added "1.2"}
  [x]
  (let [f (as-file x)
        path (erlang.io.File/path.e f)]
    (if (= :absolute (filename/pathtype.e path))
      (throw (str f " is not a relative path"))
      path)))

(defn ^File file
  "Returns a erlang.io.File, passing each arg to as-file.  Multiple-arg
   versions treat the first argument as parent and subsequent args as
   children relative to the parent."
  {:added "1.2"}
  ([arg]
     (as-file arg))
  ([parent child]
   (let [path (-> [parent (as-relative-path child)]
                  clj_core/to_list.1
                  filename/join.1)]
     (file path)))
  ([parent child & more]
   (let [path (-> (map str (list* parent child more))
                  clj_core/to_list.1
                  filename/join.1)]
     (file path))))

(defn delete-file
  "Delete file f. Raise an exception if it fails unless silently is true."
  {:added "1.2"}
  [f & [silently]]
  (or (erlang.io.File/delete.e (as-file f))
      silently
      (throw (str "Couldn't delete " f))))

(defn make-parents
  "Given the same arg(s) as for file, creates all parent directories of
   the file they represent."
  {:added "1.2"}
  [f & more]
  (with-open [f (apply file f more)]
    (filelib/ensure_dir.1 (file-path f))))
