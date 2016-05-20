# with-filters

A simple way of nesting complex code. E.g. Setup / Teardown code in
a web server.

## Requirements

Has no dependencies other than Common Lisp, so should run anywhere.

## Usage

Clone repo into quicklisp ```local-projects```` directory and
quickload :with-filters and you should be good to go. (You may
need to ```quicklisp:register-local-projects```.

## Documentation

(clear-filters)

(list-filters)

(deffilter)

(with-filter-chain)

## Important performance note!



## Example


(See ```src/demo.lisp``` for a runnable example)

## License / Copyright

Released under the MIT license, so you can use this in your own
proprietory projects if you'd like just not claim it as your own
and/or sell it.

By Ivan Kocienski (c) 2016