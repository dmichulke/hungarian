# hungarian

A Clojure library designed to solve the assignment problem using the hungarian algorithm. 

https://en.wikipedia.org/wiki/Hungarian_algorithm

It's advantages are 

- plain clojure
- kind of didactic (general algorithm according to wikipedia article, zero assignment follows https://community.topcoder.com/tc?module=Static&d1=tutorials&d2=maxFlow 

It's disadvantages are:

- it's alpha (first shot, just carved out of one of my projects)
- no testing
- it's not the fastest (O(N^5) or something)
- there is no clojar available right now

## Usage

    ;> (def m (->> (repeatedly 16 #(rand-int 100))
		     (partition 4)
		     (mapv vec)))
    ;; #'hungarian.core/m
    ;> (minimize m)
    ;; {:cost 107, :assignments {0 0, 1 1, 2 3, 3 2}}
    ;; Your mileage may vary

There are predefined test matrices named t1 .. t5 in core.clj

You can use minimize-rectangle for non (NxN) matrices. 

## License

Copyright © 2014 Daniel Michulke, All rights reserved.

Distributed under the Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at the root of this distribution.

By using this software in any fashion, you are agreeing to be bound by the terms of this license.
You must not remove this notice, or any other, from this software.
