(ns text-processor.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :as cli])
  (:import (clojure.lang ExceptionInfo)
           (java.io FileNotFoundException
                    UnsupportedEncodingException)))

(def ^:const single-space " ")

(defn- write-to-path
  "Writes the specified lines to the specified path."
  [lines-to-write output-path]
  (when (.exists (io/as-file output-path))
    (io/delete-file output-path))
  (with-open [w (io/writer output-path)]
    (doseq [line lines-to-write]
      (.write w line)
      (.newLine w))))

(defn- build-padding-string
  "Returns a string of spaces of the specified width."
  [width]
  (apply str (repeat width single-space)))

(defn- get-max-row-count
  "Returns the row count of the column with the largest number of rows."
  [columns]
  (->>
    (map count columns)
    (apply max)))

(defn- build-rows-to-write
  "Returns a vector of rows of the specified columns aligned side-by-side."
  [columns column-width separator-width]
  (let [max-row-count (get-max-row-count columns)
        blank-column-row (build-padding-string column-width)
        column-separator (build-padding-string separator-width)
        rows-to-write (atom [])
        current-row (atom "")]
    (loop [row-idx 0]
      (when (< row-idx max-row-count)
        (loop [col-idx 0]
          (when (< col-idx (count columns))
            (let [column (nth columns col-idx)
                  have-row (< row-idx (count column)) ; cols can have different lengths
                  row-has-text (and have-row (not= (nth column row-idx) ""))]
              ; append text or padding
              (if row-has-text
                (swap! current-row str (nth column row-idx))
                (swap! current-row str blank-column-row))
              ; when not last col add col separator
              (when (< col-idx (- (count columns) 1))
                (swap! current-row str column-separator))
              (recur (inc col-idx)))))
        (swap! rows-to-write conj @current-row)
        (reset! current-row "")
        (recur (inc row-idx))))
    @rows-to-write))

(defn- pad-to-width
  "Returns the specified text with trailing spaces to the specified width."
  [width text]
  (->>
    (- width (count text))
    (build-padding-string)
    (str text)))

(defn- build-padded-words
  "Returns the specified words padded up to spaces-to-add from left to right."
  [words count-word-gaps count-spaces-to-add]
  (loop [idx   0
         added 0
         words words]
    (if (< added count-spaces-to-add)
      (recur (if (< idx (- count-word-gaps 1))
               (inc idx)
               0) ; back to first word gap
             (inc added)
             (assoc words idx (str (nth words idx) single-space)))
      words)))

(defn- justify-text
  "Returns the specified text justified to column width from left to right."
  [text column-width]
  (let [words (str/split text #"\s+")
        count-words (count words)
        count-word-gaps (max (- count-words 1) 1) ; max (0 1) handles single word
        count-spaces-removed-by-split (if (> count-words 1) count-word-gaps 0)
        count-spaces-to-add (+ count-spaces-removed-by-split (- column-width (count text)))]
    (->>
      (build-padded-words words count-word-gaps count-spaces-to-add)
      (str/join)
      (pad-to-width column-width))))

(defn- build-processed-text
  "Returns a map of text-buffer justified up to column-width, and the remaining text."
  [text-buffer column-width]
  (let [remainder-words (atom ()) ; use list b/c conj adds to front
        text-to-justify (atom text-buffer)]
    (while (> (count @text-to-justify) column-width)
      (let [idx-of-last-space (str/last-index-of @text-to-justify single-space)]
        (when (= idx-of-last-space nil)
          (throw (ex-info (format "Text contained a word longer than specified column width (%d). Please try a larger width value." column-width) {:type :column-width-too-low})))
        ; take last word and place at front of remainder
        (swap! remainder-words conj (str/trim (subs @text-to-justify idx-of-last-space)))
        ; remove word from end of text
        (reset! text-to-justify (subs @text-to-justify 0 idx-of-last-space))))
    {:justified-text (justify-text @text-to-justify column-width)
     :remainder-text (str/join single-space @remainder-words)}))

(defn- space-needed?
  "Returns true if a space needs to be added between text-buffer and input-line."
  [text-buffer input-line]
  (and (not= text-buffer "")
       (not= input-line "")
       (not (str/ends-with? text-buffer single-space))
       (not (str/starts-with? input-line single-space))))

(defn- build-justified-column
  "Returns all lines of the specified document as a vector of justified strings."
  [document column-width justify-last-line?]
  (let [output-lines (atom [])
        text-buffer (atom "")]
    (doseq [line document]
      (let [trimmed-line (str/trim line)]
        (if (space-needed? @text-buffer trimmed-line)
          (swap! text-buffer str single-space))
        (swap! text-buffer str trimmed-line)
        (cond
          (= trimmed-line "") ; line between paragraphs
          (do
            (when (> (count(str/trim @text-buffer)) 0)
              (when justify-last-line?
                (reset! text-buffer (:justified-text (build-processed-text @text-buffer column-width))))
              (swap! output-lines conj (pad-to-width column-width @text-buffer)))
            (reset! text-buffer "")
            (swap! output-lines conj (build-padding-string column-width))) ; preserve paragraphs
          :else
          (while (> (count @text-buffer) column-width)
            (let [processed-text (build-processed-text @text-buffer column-width)]
              (swap! output-lines conj (:justified-text processed-text))
              (reset! text-buffer (:remainder-text processed-text)))))))
    (when (> (count(str/trim @text-buffer)) 0) ; last line of document
      (when justify-last-line?
        (reset! text-buffer (:justified-text (build-processed-text @text-buffer column-width))))
      (swap! output-lines conj (pad-to-width column-width @text-buffer)))
    @output-lines))

(defn- build-justified-columns
  "Returns all lines of all documents as a vector of vectors justified strings."
  [input-documents column-width justify-last-line?]
  (let [justified-columns (atom [])]
    (doseq [input-document input-documents]
      (swap! justified-columns conj (build-justified-column input-document column-width justify-last-line?)))
    @justified-columns))

(defn- read-from-path
  "Returns all lines of the specified file as a lazy sequence of strings."
  [path encoding]
  (try
    (with-open [r (io/reader path :encoding encoding)]
      (doall (line-seq r)))
    (catch FileNotFoundException e
      (throw (ex-info (format "The file '%s' was not found. Please check that the file exists and try again." path) {:type :file-not-found})))
    (catch UnsupportedEncodingException e
        (throw (ex-info (format "The encoding '%s' is not supported. For a list of supported ecodings please see http://docs.oracle.com/javase/7/docs/technotes/guides/intl/encoding.doc.html." encoding) {:type :unsupported-encoding})))))

(defn- read-input-documents
  "Returns all lines of the specified files as a vector of vectors."
  [input-paths encoding]
  (let [input-documents (atom [])]
    (doseq [input-path input-paths]
      (swap! input-documents conj (read-from-path input-path encoding)))
    @input-documents))

(defn- remove-trailing-slashes
  "Returns the input text with any trailing slashes removed."
  [text]
  (loop [text text]
    (if-not (str/ends-with? text "/")
      text
      (recur (subs text 0 (- (count text) 1))))))

(defn- get-input-paths
  "Returns a list of all file names in the specified directory."
  [input-path]
  (let [clean-path (remove-trailing-slashes input-path)
        fseq (file-seq (io/file clean-path))
        files (filter #(.isFile %) fseq)]
    (when (empty? files)
      (throw (ex-info (format "The path '%s' was empty or not found. Please check that the path exists and try again." clean-path) {:type :path-empty-or-not-found})))
    (->>
      (map #(.getName %) files)
      (map #(str clean-path "/" %)))))

(defn- error-msg
  "Returns an error message formatted using the specified errors."
  [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (str/join \newline errors)))

(defn- usage
  "Returns a usage message using the specified options."
  [options-summary]
  (->> ["Takes all files in the specified input directory and prints them in justified columns to the specified output file."
        ""
        "Usage: text-processor [options]"
        ""
        "Options:"
        options-summary
        ""
        "Please refer to https://github.com/jasonracey/text-processor for more information."]
       (str/join \newline)))

(defn- exit
  "Prints a message and returns an exit status to caller."
  [status msg]
  (println msg)
  (System/exit status))

(defn- get-required-opts-error
  "Returns an error message if a required opt is missing."
  [options]
  (let [input (:input options)
        output (:output options)
        column-width (:column-width options)]
    (cond
      (nil? input) {:error "Input path is required."}
      (nil? output) {:error "Output path is required."}
      (nil? column-width) {:error "Column width is required."}
      :else nil)))

(def ^:private cli-options
  "Returns a vector of supported command-line args and associated validation rules."
  [["-i" "--input PATH" "Required. Path to directory containing files to process."
    :validate [#(not (str/blank? %)) "Must not be null or empty"]]

   ["-o" "--output PATH" "Required. Path and name of file to write to."
    :validate [#(not (str/blank? %)) "Must not be null or empty"]]

   ["-c" "--column-width NUMBER" "Required. Width of columns to write. Must be >= the longest word in the files in input."
    :parse-fn #(Integer/parseInt %)
    :validate [#(> % 0) "Must be greater than 0"]]

   ["-s" "--separator-width NUMBER" "Optional. Width of column separator. Must be >= 0."
    :default 4
    :parse-fn #(Integer/parseInt %)
    :validate [#(>= % 0) "Must be greater than or equal to 0"]]

   ["-e" "--encoding STRING" "Optional. Encoding of files to process."
    :default "utf-8"
    :validate [#(not (str/blank? %)) "Must not be null or empty"]]

   ["-j" "--justify-last true|false" "Optional. Justify last line of paragraphs (otherwise aligns left)"
    :default true
    :parse-fn #(Boolean/parseBoolean %)]

   ["-h" "--help"]])

(defn -main
  "Takes all files in the specified input directory and prints them in justified columns to the specified output file."
  [& args]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)]
    (cond
      (:help options) (exit 0 (usage summary))
      errors (exit 1 (error-msg errors)))
    (when-let [message (:error (get-required-opts-error options))]
      (exit 1 (str "Error: " message \newline \newline (usage summary))))
    (try
      (let [input-path (:input options)
            encoding (:encoding options)
            column-width (:column-width options)
            separator-width (:separator-width options)
            justify-last-line? (:justify-last options)
            output-path (:output options)]
        (-> (get-input-paths input-path)
            (read-input-documents encoding)
            (build-justified-columns column-width justify-last-line?)
            (build-rows-to-write column-width separator-width)
            (write-to-path output-path))
        (exit 0 (format "Success: output written to %s" output-path)))
      (catch ExceptionInfo e
        (exit 1 (format "Error: %s" (.getMessage e)))))))
