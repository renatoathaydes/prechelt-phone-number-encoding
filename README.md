## Prechelt Phone Number Encoding

This project implements the phone number encoding described by [Lutz Prechelt](http://wwwipd.ira.uka.de/~prechelt/)
in his article [Comparing Java vs. C/C++ Efficiency Differences to Interpersonal Differences](https://www.ebhakt.info/dl/Comparejavaandc_D9F7/compare_java_c.pdf),
and later used by [Ron Garret](https://flownet.com/ron/) in his own shor paper, [Lisp as an alternative to Java](https://flownet.com/ron/papers/lisp-java/lisp-java.pdf).

The instructions given to participants of the studies (which I used to implement my own solution in this repo)
can be found at [flownet.com](https://flownet.com/ron/papers/lisp-java/instructions.html).

## Motivation

I decided to "participate" in the study by implementing the algorithm in Java in order to see if the conclusions in that
paper were reasonable and stand, for Java at least, all these years later.

Basically, I wanted to see if a modern Java programmer like me could implement the algorithm in Java
faster than the Java programmers did back then. I specially wanted to see if I could approach the times taken
by Lisp developers!

I was extremely impressed by [Peter Norvig](https://norvig.com/)'s [Lisp implementation](http://www.norvig.com/java-lisp.html),
which he claims took him only **2 hours (including documentation) and 45 lines of code (!!)**.

## Conclusion

I managed to write all the boilerplate code in just about 1 hour, then it took me another hour or so to write a basic solution
to the problem with a few tests... but actually finishing the algorithm off, unfortunately, took me some time (I don't write algorithms like
that every day), specially because of a small bug that I couldn't easily figure out how to fix (I was trying to
_inject_ a digit when I could not find a word on any position, instead of only at the beginning of a potential word...
once the reason was found, it was trivial to fix).

All in all, I estimate **my total time was around 3.5 hours**. That's almost twice as much as Norvig, but at least I can take
some comfort in knowing I managed to do quite well when compared with other participants in the study:
**2 to 8.5 hours for Lisp, 3 to 25 hours for C/C++, 4 to 63 hours for Java**.

My solution required **196 lines of code** using Java 16 (without records, which would have eliminated at least 20 lines
from the `Item` POJO I used in my solution), compared with a range of **51 to 182 for Lisp**, and
**107 to 614 for the other languages**.
