# with-filters

A simple way of nesting complex code. E.g. Setup / Teardown code in
a web server.

## Requirements

Has no dependencies other than Common Lisp, so should run anywhere.

## Usage

Clone repo into quicklisp ```local-projects```` directory and
quickload :with-filters and you should be good to go. (You may
need to ```quicklisp:register-local-projects```).

## Documentation

    (clear-filters)

Reset all defined filters

    (list-filters)

List defined filters alphabetically

    (deffilter (NAME DATA-VAR) &body)

Define a filter. ```NAME``` is a keyword used to identify filter in database.
```DATA-VAR``` is the symbol name you want to use to pass data into the filter.

    (with-filter-chain (DATA-VAR :A :B :C ...) &body)

Encapsulate some code in a filter chain stack. ```DATA-VAR``` is the symbol of the
variable to be passed into the chain. (The demo makes this clearer). ```:A :B :C ...```
is the chain of filters that will be called like you just had.

    (:A (lambda ()
          (:B (lambda ()
	        (:C (lambda ()
		       &body))))))

Nothing will stop you from calling the same callback more than once or
detecting/preventing cyclical callbacks, so be careful.

    (yield DATA)

Invoke the next step of the chain. ```DATA``` is the value you want to pass down
the chain. You don't have to call ```yield``` if you want the filter to halt the
call chain.

## Important performance note!

The ```with-filter-chain``` calls an internal method to lookup each stage of the
chain with each invokation of the chain. It does not pull the callbacks out
of the database and create a nested set of ```funcall```'s. This is slower than
just having ```funcall``` but it does mean filters can be redefined without needing
to re-compile all the ```with-filter-chain``` call points.


## Example

    (deffilter (:callback value)
      (format t "filter start (value=~a) ~%" value)
      (yield (* value 2)) ;; <-- must call this!
      (format t "filter  end (value=~a)~%" value))

    (defun test-filter ()
      (let ((value 2))
        (with-filter-chain (value :filter)
          (format t "CODE  value=~a~%" value))))
      
(See ```src/demo.lisp``` for a runnable example)

## License / Copyright

Released under the MIT license, so you can use this in your own
proprietory projects if you'd like just not claim it as your own
and/or sell it.

By Ivan Kocienski (c) 2016