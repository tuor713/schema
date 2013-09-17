# schema

A weekend project of writing a small schema validation library for Clojure (or EDN) datastructures. 

Not anything near as sophisticated as [Schema](https://github.com/prismatic/schema). But remarkably similar despite completely independent evolution.

## Usage 

```clojure
(require '[uwh.schema :as s])

(s/valid? (s/vector-of {:symbol s/string :close s/double :change s/double})
		       [{:symbol "IBM" :close 192.17 :change 0.75}
		        {:symbol "ORCL" :close 32.46 :change 1.01}])
=> true

(s/validate (s/vector-of {:symbol s/string :close s/double :change s/double})
		         [{:symbol "IBM" :close 192.17 :change 0.75}
		          {:symbol :ORCL :close 32.46 :change 1.01}])
=> (ValidationFailure{Failure at '1 > :symbol': Expected string but got :ORCL})

```

## License

Copyright © 2013 Valentin Mahrwald

Distributed under the Eclipse Public License, the same as Clojure.
