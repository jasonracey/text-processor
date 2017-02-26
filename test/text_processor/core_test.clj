(ns text-processor.core-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [text-processor.core :as t])
  (:import (clojure.lang ExceptionInfo)))

(def ^:const output-path "test.txt")

(defn- delete-output-file []
  (when (.exists (io/as-file output-path))
    (io/delete-file output-path)))

(defn- main-fixture [f]
  (f)
  (delete-output-file))

(use-fixtures :each main-fixture)

(deftest test-write-to-path
  (let [lines1 ["a" "b" "c"]
        lines2 ["d" "e" "f" "g"]]
    (#'t/write-to-path lines1 output-path)
    ; write again to make sure we're not appending
    (#'t/write-to-path lines2 output-path)
    (let [written (vec (#'t/read-from-path output-path "utf-8"))]
      (testing "write-to-path deletes existing file"
        (is (= (count written) (count lines2))))
      (testing "write-to-path writes specified lines"
        (is (= (compare written lines2) 0))))))

(deftest test-build-padding-string
  (testing "returns empty string when width <= 0"
    (is (= (#'t/build-padding-string -1) ""))
    (is (= (#'t/build-padding-string  0) "")))
  (testing "returns requested width string when > 0"
    (is (= (#'t/build-padding-string  1) " "))
    (is (= (#'t/build-padding-string  2) "  "))))

(deftest test-get-max-row-count
  (testing "returns 0 when vectors are empty"
    (is (= (#'t/get-max-row-count [[]]) 0))
    (is (= (#'t/get-max-row-count [[][]]) 0)))
  (testing "returns count of largest vector"
    (is (= (#'t/get-max-row-count [[][1]]) 1))
    (is (= (#'t/get-max-row-count [[1][1 2 3][1 2]]) 3))))

(deftest test-build-rows-to-write
  (testing "combines vector elements at matching indices into a strings"
    (let [columns [["a1" "a2"] ["b1" "b2"]]
          result (#'t/build-rows-to-write columns 2 1)]
      (is (= (compare result ["a1 b1" "a2 b2"]) 0))))
  (testing "places column-spacing number of spaces between columns"
    (let [columns [["a1" "a2"] ["b1" "b2"]]
          result (#'t/build-rows-to-write columns 2 2)]
      (is (= (compare result ["a1  b1" "a2  b2"]) 0))))
  (testing "fills empty column lines with spaces to preserve alignment"
    (let [columns [["a1"] ["b1" "b2"]]
          result (#'t/build-rows-to-write columns 2 1)]
      (is (= (compare result ["a1 b1" "   b2"]) 0)))))

(deftest test-pad-to-width
  (testing "returns specified string when width is <= string length"
    (is (= (#'t/pad-to-width -1 "a") "a"))
    (is (= (#'t/pad-to-width 1 "a") "a")))
  (testing "returns specified string padded to specified width"
    (is (= (#'t/pad-to-width 2 "a") "a "))
    (is (= (#'t/pad-to-width 3 "a") "a  "))))

(deftest test-build-padded-words
  (testing "adds specified number of spaces"
    (is (= (#'t/build-padded-words ["a" "b" "c"] 2 2) ["a " "b " "c"])))
  (testing "spaces are added left to right"
    (is (= (#'t/build-padded-words ["a" "b" "c"] 2 3) ["a  " "b " "c"])))
  (testing "does not pad last word"
    (is (= (#'t/build-padded-words ["a" "b" "c"] 2 4) ["a  " "b  " "c"]))))

(deftest test-justify-text
  (testing "width less than number of characters returns collapsed text"
    (is (= (#'t/justify-text "a b c" 2) "abc")))
  (testing "width less than text length collapses text right to left"
    (is (= (#'t/justify-text "a b c" 3) "abc"))
    (is (= (#'t/justify-text "a b c" 4) "a bc")))
  (testing "width equal to text length returns text unchanged"
    (is (= (#'t/justify-text "a b c" 5) "a b c")))
  (testing "width greater than text length expands text left to right"
    (is (= (#'t/justify-text "a b c" 6) "a  b c")))
  (testing "single word is padded on the right"
    (is (= (#'t/justify-text "a" 2) "a "))))

(deftest test-build-processed-text
  (testing "Throws an exception when word longer than column-width."
    (is (thrown? ExceptionInfo (#'t/build-processed-text "ab" 1 ))))
  (testing "Takes whole words up to column-width, left to right."
    (is (= (#'t/build-processed-text "first second third" 5)
           {:justified-text "first" :remainder-text "second third"}))
    (is (= (#'t/build-processed-text "first second third" 12)
           {:justified-text "first second" :remainder-text "third"}))
    (is (= (#'t/build-processed-text "first second third" 18)
           {:justified-text "first second third" :remainder-text ""})))
  (testing "Pads space as needed in justified-text, left to right."
    (is (= (#'t/build-processed-text "first second third" 6)
           {:justified-text "first " :remainder-text "second third"}))
    (is (= (#'t/build-processed-text "first second third" 13)
           {:justified-text "first  second" :remainder-text "third"}))
    (is (= (#'t/build-processed-text "first second third" 19)
           {:justified-text "first  second third" :remainder-text ""}))
    (is (= (#'t/build-processed-text "first second third" 20)
           {:justified-text "first  second  third" :remainder-text ""}))))

(deftest test-space-needed?
  (testing "Returns true only if there is text and no existing space."
    (is (= (#'t/space-needed? "a" "b") true))
    (is (= (#'t/space-needed? "a " "b") false))
    (is (= (#'t/space-needed? "a" " b") false))
    (is (= (#'t/space-needed? "" "b") false))
    (is (= (#'t/space-needed? "a" "") false))
    (is (= (#'t/space-needed? "" "") false))
    (is (= (#'t/space-needed? " " "") false))
    (is (= (#'t/space-needed? "" " ") false))
    (is (= (#'t/space-needed? " " " ") false))))

(deftest test-build-justified-column
  (testing "Justifies last line when requested."
    (let [document ["1234" "1234" "1234" "1234"]
          result (#'t/build-justified-column document 12 true)]
      (is (= (compare result ["1234    1234" "1234    1234"]) 0))))
  (testing "Does not justify last line when not requested."
    (let [document ["1234" "1234" "1234" "1234"]
          result (#'t/build-justified-column document 12 false)]
      (is (= (compare result ["1234    1234" "1234 1234   "]) 0))))
  (testing "Preserves paragraphs."
    (let [document ["1234" "" "1234"]
          result (#'t/build-justified-column document 4 false)]
      (is (= (compare result ["1234" "    " "1234"]) 0))))
  (testing "Preserves multiple lines between paragraphs."
    (let [document ["1234" "" "" "1234"]
          result (#'t/build-justified-column document 4 false)]
      (is (= (compare result ["1234" "    " "    " "1234"]) 0)))))

(deftest test-build-justified-columns
  (testing "Builds justified columns for one document."
    (let [documents [["1234" "5678"]]
          result (#'t/build-justified-columns documents 12 true)]
      (is (= (compare result [["1234    5678"]]) 0))))
  (testing "Builds justified columns for multiple documents."
    (let [documents [["1234" "5678"]
                     ["abcd" "efgh"]]
          result (#'t/build-justified-columns documents 12 true)]
      (is (= (compare result [["1234    5678"] ["abcd    efgh"]]) 0)))))

(def ^:const expected-text
    ["Alice was beginning to get very tired of sitting by her sister on the"
     "bank, and of having nothing to do: once or twice she had peeped into the"
     "book her sister was reading, but it had no pictures or conversations in"
     "it, ‘and what is the use of a book,’ thought Alice ‘without pictures or"
     "conversations?’"
     ""
     "So she was considering in her own mind (as well as she could, for the"
     "hot day made her feel very sleepy and stupid), whether the pleasure"
     "of making a daisy-chain would be worth the trouble of getting up and"
     "picking the daisies, when suddenly a White Rabbit with pink eyes ran"
     "close by her."])

(deftest test-read-from-path
  (testing "Throws an exception when the specified encoding is not supported."
    (is (thrown? ExceptionInfo (#'t/read-from-path "./resources/txt/utf8/utf-8.txt" "bad-encoding"))))
  (testing "Throws an exception when the file is not found."
    (is (thrown? ExceptionInfo (#'t/read-from-path "./resources/txt/utf8/bad-file-name.txt" "utf-8"))))
  (testing "Uses the specified encoding to read the file."
    (let [result (vec (#'t/read-from-path "./resources/txt/utf8/utf-8.txt" "utf-8"))]
      (is (= (compare result expected-text) 0)))
    (let [result (vec (#'t/read-from-path "./resources/txt/windows1252/windows-1252.txt" "windows-1252"))]
      (is (= (compare result expected-text) 0)))))

(deftest test-read-input-documents
  (testing "Reads all of the specified documents and stores individuaally."
    (let [documents ["./resources/txt/small/test1.txt"
                     "./resources/txt/small/test2.txt"
                     "./resources/txt/small/test3.txt"]
                     result (#'t/read-input-documents documents "utf-8")]
      (is (= (count result) (count documents))))))

(deftest test-remove-trailing-slashes
  (testing "Removes all trailing slashes."
    (is (= (#'t/remove-trailing-slashes "") ""))
    (is (= (#'t/remove-trailing-slashes "/") ""))
    (is (= (#'t/remove-trailing-slashes "//") ""))
    (is (= (#'t/remove-trailing-slashes "a") "a"))
    (is (= (#'t/remove-trailing-slashes "a/") "a"))
    (is (= (#'t/remove-trailing-slashes "a//") "a"))))

(deftest test-get-input-paths
  (testing "Throws an exception when the specified path is empty or not found."
    (is (thrown? ExceptionInfo (#'t/get-input-paths "")))
    (is (thrown? ExceptionInfo (#'t/get-input-paths "./resources/txt/bad-path"))))
  (testing "Forms full path to file."
    (is (= (#'t/get-input-paths "./resources/txt/utf8") '("./resources/txt/utf8/utf-8.txt"))))
  (testing "Returns path of all files in directory."
    (is (= (#'t/get-input-paths "./resources/txt/small") '("./resources/txt/small/test1.txt"
                                                           "./resources/txt/small/test2.txt"
                                                           "./resources/txt/small/test3.txt")))))

(deftest test-error-msg
  (testing "Formats errors onto new lines."
    (is (= (#'t/error-msg ["a"]) "The following errors occurred while parsing your command:\n\na"))
    (is (= (#'t/error-msg ["a" "b"]) "The following errors occurred while parsing your command:\n\na\nb"))))

(deftest test-get-required-opts-error
  (testing "Input is required."
    (let [result (#'t/get-required-opts-error {:input nil :output "output.txt" :column-width 50})]
      (is (= result {:error "Input path is required."}))))
  (testing "Output is required."
    (let [result (#'t/get-required-opts-error {:input "./input" :output nil :column-width 50})]
      (is (= result {:error "Output path is required."}))))
  (testing "Column width is required."
    (let [result (#'t/get-required-opts-error {:input "./input" :output "output.txt" :column-width nil})]
      (is (= result {:error "Column width is required."})))))
