# text-processor

Takes all files in the specified input directory and prints them in justified columns to the specified output file.

## Prerequisites

    $ brew install leiningen

## Installation

    $ git clone https://github.com/jasonracey/text-processor.git

## Usage

    $ lein run text-processor [args]

## Options

```
-i, --input PATH                      Required. Path to directory containing files to process.
-o, --output PATH                     Required. Path and name of file to write to.
-c, --column-width NUMBER             Required. Width of columns to write. Must be >= the longest word in the files in input.
-s, --separator-width NUMBER   4      Optional. Width of column separator. Must be >= 0.
-e, --encoding STRING          utf-8  Optional. Encoding of files to process.
-j, --justify-last true|false  true   Optional. Justify last line of paragraphs (otherwise aligns left)
-h, --help
```

## Examples

The input path, output path, and column-width args are required:

    $ lein run text-processor -i ./dir-containing-files -o output.txt -c 50

You can customize the spacing between written columns (default is 4 spaces):

    $ lein run text-processor -i ./dir-containing-files -o output.txt -c 50 -s 3

You can specify the encoding of the files you are processing (default is utf-8):

    $ lein run text-processor -i ./dir-containing-files -o output.txt -c 50 -e utf-16

You can indicate whether or not you want to justify the last line of each paragraph (default is true):

    $ lein run text-processor -i ./dir-containing-files -o output.txt -c 50 -j false

## License

Copyright © 2017 Jason Racey

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
