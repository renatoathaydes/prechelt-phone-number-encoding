## Prechelt Phone Number Encoding

`This project implements the phone number encoding described by [Lutz Prechelt](https://twitter.com/prechelt)
in his article for the COMMUNICATIONS OF THE ACM (October 1999/Vol. 42, No. 10), [Comparing Java vs. C/C++ Efficiency Differences to Interpersonal Differences](https://www.ebhakt.info/dl/Comparejavaandc_D9F7/compare_java_c.pdf),
later re-used for a more extended paper, [An empirical comparisonofC, C++, Java,Perl, Python, Rexx, and Tclfor asearch/string-processing program](http://page.mi.fu-berlin.de/prechelt/Biblio/jccpprtTR.pdf)
from March, 2000.

The same problem was later used by [Ron Garret](https://flownet.com/ron/) (aka Erann Gat) on
[Lisp as an alternative to Java](https://flownet.com/ron/papers/lisp-java/lisp-java.pdf), also from the year 2000.

The instructions given to participants of the latter study (which was slightly adapted from the original paper,
and used to implement my own solution in this repository) can be found at
[flownet.com](https://flownet.com/ron/papers/lisp-java/instructions.html).

Unfortunately, the authors of the papers do not appear to have made the solutions used in their studies public, but
after searching through the Internet, I was able to find a few.

[Peter Norvig](https://norvig.com/), after finding out about the Lisp paper, wrote his own
[Lisp program](http://www.norvig.com/java-lisp.html) to solve the problem.

There are also [several solutions in Haskell](https://wiki.haskell.org/Phone_number),
[one in Python](https://www.dr-josiah.com/2010/12/python-as-alternative-to-lisp-or-java.html) and
[one in Lua](https://gist.github.com/ayourtch/752460).`

## Motivation

I decided to "participate" in the study by implementing the algorithm in Java in order to see if the conclusions in that
paper were reasonable and stand, for Java at least, all these years later.

Basically, I wanted to see if a modern Java programmer like me could implement the algorithm in Java
faster than the Java programmers did back then. I specially wanted to see if I could approach the times taken
by Lisp developers!

I was extremely impressed by [Peter Norvig](https://norvig.com/)'s [Lisp implementation](http://www.norvig.com/java-lisp.html),
which he claims took him only **2 hours (including documentation) and 45 lines of code (!!)**.

## Results

I managed to write all the boilerplate code in just about 1 hour, then it took me another hour or so to write a basic solution
to the problem with a few tests... but actually finishing the algorithm off, unfortunately, took me some time (I don't write algorithms like
that every day), specially because of a small bug that I couldn't easily figure out how to fix (I was trying to
_inject_ a digit when I could not find a word on any position, instead of only at the beginning of a potential word...
once the reason was found, it was trivial to fix).

### Time

All in all, I estimate **my total time was around 3.5 hours**. That's almost twice as much as Norvig, but at least I can take
some comfort in knowing I managed to do quite well when compared with other participants in the study:
**2 to 8.5 hours for Lisp, 3 to 25 hours for C/C++, 4 to 63 hours for Java**.

### Lines of code

My solution required **196 lines of code** using Java 16 (without records, which would have eliminated at least 20 lines
from the `Item` POJO I used in my solution), compared with a range of **51 to 182 for Lisp**, and
**107 to 614 for the other languages**.

### Performance

To measure performance, I downloaded a [list of German words](https://gist.githubusercontent.com/MarvinJWendt/2f4f4154b8ae218600eb091a5706b5f4/raw/36b70dd6be330aa61cd4d4cdfda6234dcb0b8784/wordlist-german.txt)
I found on GitHub because I couldn't find the original document used in the study. Neither the original study nor the
follow-up Lisp study seemed to have bothered making their test data available, let alone the solutions submitted by the
participants, so I had to content myself with using what I could get.

I used the [CleanupWords.java](src/java/CleanupWords.java) code to turn the UTF-8 German words into ASCII, so the program
input was similar to what had been used in the original study, hopefully. I also took only the first `75_000` words
(out of a total of `1_908_815`) as this was a _Quantitative Requirement_ of the original study.

I downloaded and installed the [SBCL](http://www.sbcl.org/manual/) Common Lisp compiler to run Norvig's solution and
compare that against mine.

### Compiling and running

## Setup

1. Before running the programs, please download the German words list from the following GitHub Gist
(notice that any file containing a word per line should do!):

* [MarvinJWendt/wordlist-german.txt](https://gist.githubusercontent.com/MarvinJWendt/2f4f4154b8ae218600eb091a5706b5f4/raw/36b70dd6be330aa61cd4d4cdfda6234dcb0b8784/wordlist-german.txt)

2. Compile the Java sources in this repo.

Besides the [Main](src/java/Main.java) program, there are a few utilities we will use in the next few steps to setup the data
for running everything.

Run this command to compile all Java sources:

```
javac src/* -d build
```

3. To follow the study guidelines, pass the downloaded file through a _cleaner_ so that it only contains ASCII characters.

I wrote a small [Java program](src/java/CleanupWords.java) that does that for the German dictionary.
You can run it as follows:

```
java -cp build CleanupWords german-words.txt # or the name of the file you downloaded
```

> This command accepts a second argument to set the maximum number of words to include. By default, it only uses
 75_000 words as that's the limit mentioned in the study's `Quantitative requirements`, but given how fast computers are
 nowadays, to let the programs run for a good amount of time, you may use a much higher number of words, as we'll see
 in the Performance Analysis later.

4. Generate a list of phone numbers to encode.

Again, I wrote a Java [phone number generator](src/java/GeneratePhoneNumbers.java) to do that.

It will generate phone numbers that include not just digits, but also `/` and `-` characters, as per the original study.
Phone numbers are limited to between 1 and 50 characters with at least one of them being a digit.

The program prints the phone numbers to stdout, so to save them into a file, pipe the output to the file:

```
java -cp build GeneratePhoneNumbers > phones.txt
```

> By default, 1000 phone numbers are generated. You can pass another count as an argument if desired.

5. If you want to run the Lisp program, you must have a Common Lisp compiler available.

I've used the [SBCL](https://lisp-lang.org/learn/getting-started/) compiler in my tests. Follow the link for
instructions on installing it on your platform.

## Running

With the input files prepared, we're ready to run the Lisp and Java programs.

#### Running the Lisp program

To run the Lisp program with the small test files, simply run:

```
sbcl --script lisp/main.lisp
```

To use the _real_ german dictionary and large, generated phone numbers file, pass them as arguments:

> The output is huge, so we pipe the output to a file `sbcl_out.txt` which we can inspect later,
> and redirect stderr to stdout, so we can see errors.

```
sbcl --script lisp/main.lisp german-words.ascii.txt phones.txt > sbcl_out.txt 2>&1 
```

#### Running the Java program

Compile the Java sources by running:

```
javac -cp src/* -d build
```

This puts all compiled class files into the `build` directory.

To run the Java program with the small test files, simply run:

```
java -cp build Main
```

To use the _real_ german dictionary and large, generated phone numbers file, pass them as arguments:

> The output is huge, so we pipe the output to a file `java_out.txt` which we can inspect later,
> and redirect stderr to stdout, so we can see errors.

```
java -cp build Main german-words.ascii.txt phones.txt \
    > java_out.txt 2>&1 
```

## Performance Analysis

To analyse the performance of the programs, I am just running them using my shell's `time` utility with
[a few tweaks](https://superuser.com/questions/480928/is-there-any-command-like-time-but-for-memory-usage)
to also show memory usage.

Here's an example run of both programs, with the timing results (using 75_000 words and 1_000 phone numbers):

```
▶ time sbcl --script lisp/main.lisp german-words.txt.ascii phones.txt > sbcl_out.txt 2>&1 

sbcl --script lisp/main.lisp german-words.txt.ascii phones.txt > sbcl_out.txt   0.09s  user 0.03s system 94% cpu 0.119 total
avg shared (code):         0 KB
avg unshared (data/stack): 0 KB
total (sum):               0 KB
max memory:                38864 KB
page faults from disk:     0
other page faults:         10372

programming/projects/prechelt-phone-number-encoding  master ✗                                                                                          3d15h ⚑ ◒  
▶ time java -cp build/ Main german-words.txt.ascii phones.txt > java_out.txt 2>&1                                     
java -cp build/ Main german-words.txt.ascii phones.txt > java_out.txt 2>&1   0.60s  user 0.09s system 238% cpu 0.291 total
avg shared (code):         0 KB
avg unshared (data/stack): 0 KB
total (sum):               0 KB
max memory:                101064 KB
page faults from disk:     1
other page faults:         31615
```

Both programs run very fast, but the Lisp program seems twice as fast, while requiring half as much memory!

We can check that both programs found the same solutions by running this command:

```
diff <(sort java_out.txt) <(sort sbcl_out.txt)
```

All seems well!

To know for sure which program scales better, we should try again with more words and more inputs.

To try with 750_000 words and 10_000 phone numbers, we can generate the input data first:

```
java -cp build CleanupWords german-words.txt 750000
java -cp build GeneratePhoneNumbers 10000 > phones.txt
```

Double check that the files have changed as expected:

```
▶ wc -l german-words.txt.ascii 
  750000 german-words.txt.ascii

▶ wc -l phones.txt            
   10000 phones.txt
```

```
▶ time sbcl --script lisp/main.lisp german-words.txt.ascii phones.txt > sbcl_out.txt 2>&1 

sbcl --script lisp/main.lisp german-words.txt.ascii phones.txt > sbcl_out.txt   2.15s  user 0.95s system 91% cpu 3.406 total
avg shared (code):         0 KB
avg unshared (data/stack): 0 KB
total (sum):               0 KB
max memory:                213408 KB
page faults from disk:     4192
other page faults:         69647

▶ time java -cp build/ Main german-words.txt.ascii phones.txt > java_out.txt 2>&1         
java -cp build/ Main german-words.txt.ascii phones.txt > java_out.txt 2>&1   7.76s  user 2.11s system 170% cpu 5.794 total
avg shared (code):         0 KB
avg unshared (data/stack): 0 KB
total (sum):               0 KB
max memory:                1009644 KB
page faults from disk:     5193
other page faults:         283666
```

This seems to confirm the Lisp program is twice as fast, and this time it used less than a quarter of the RAM as the
Java program did!

However, there's something fishy going on...

```
▶ wc -l java_out.txt          
  420226 java_out.txt

▶ wc -l sbcl_out.txt          
  274399 sbcl_out.txt
```

The Java program finds a lot more solutions than the Lisp program. Could it be a bug in the Java program?

To investigate that, I wrote a [solution checker program](src/java/OutputChecker.java) that attempts to identify invalid
solutions.

Doing that is much easier than finding the solutions in the first place, so I am fairly confident the checker works.

To run the output checker, run the following commands:

```
▶ java -cp build OutputChecker german-words.txt.ascii java_out.txt 
OK

▶ java -cp build OutputChecker german-words.txt.ascii sbcl_out.txt 
OK
```

Hm, both outputs contain only valid solutions. But why is the Java program finding so many more solutions?

Using the `diff` tool on the Terminal didn't help much as the diff is huge, but looking at it from my IDE might shed
some light on what's going on:

![Diff between Java output and Lisp output](images/java_out_sbcl_out_diff.png)

The red circle shows a phone number for which the Lisp program did not find any solutions at all. That's weird because
the solutions found by the Java program seem pretty straightforward.

My output checker program already validated that the Java output only contains valid solutions. If we don't trust it,
we can manually check that at least _some_ of the solutions that the Java program found - but the Lisp program didn't -
are indeed valid solutions!

For example, take this solution from the diff above:

```
/61167813: CNN 6 Bond

6 1 1 6 7 8 1 3 
C N N 6 B o n d
```

Checking the encoding is correct is easy:

![Diff between Java output and Lisp output](images/solution-check.svg)

The other valid solutions for this phone number are all valid as well and none of them seem to break any rules that I
can think of.

It looks like Norvig's Lisp solution must have a bug: it can't find all valid solutions!

The pattern of missed solutions by the Lisp program are not clear to me after looking at many other missed solutions,
so the only way to find out why some solutions are being missed is to try to understand how the Lisp program works and
maybe even fix it!

### Comparing the Lisp program by Norvig with my Java program

Here's the body of the main part of Norvig's Lisp program:

```lisp
(defun print-translations (num digits &optional (start 0) (words nil))
  (if (>= start (length digits))
      (format t "~a:~{ ~a~}~%" num (reverse words))
      (let ((found-word nil)
            (n 1)) ; leading zero problem
        (loop for i from start below (length digits) do
              (setf n (+ (* 10 n) (nth-digit digits i)))
              (loop for word in (gethash n *dict*) do
                 (setf found-word t)
                 (print-translations num digits (+ 1 i) (cons word words))))
        (when (and (not found-word) (not (numberp (first words))))
           (print-translations num digits (+ start 1)
                               (cons (nth-digit digits start) words))))))
```

All other functions are merely helpers for this main function.

When I first looked at this, I was amazed how such a short program could solve the problem at hand.
Have a look at my final [Java program](src/java/Main.java) and contrast that with Norvig's [small Lisp implementation](src/lisp/main.lisp).

Besides being almost 4 times longer, my Java solution is broken up into quite a lot of classes:

> Keep in mind that according to the instructions in the study, the program should be written
 professionally and with a focus on correctness, but at the same time it should consist of a single file.

* `Main` exists just to be the entry-point of the program.
* `PhoneNumberEncoder` represents an object that wraps a dictionary and can be used to encode phone numbers.
* `Trie` is an implementation of the [trie](https://en.wikipedia.org/wiki/Trie) data structure created to hold the dictionary in memory for efficient lookups. 
* `Trie#Node` is a poor man implementation of a union type (used to hold each partial solution part): `digit | (clean_word, word)`.
* `Item` is a poor man's Tuple `[original, result]` (for remembering both the original _item_ and its cleaned-up form).
* `InputParser` parses a `File` into a `Stream<Item>` using a given _cleaner_ function to process each line.
* `WordsInputCleaner` is the `clean` function for words (can be passed into `InputParser`).
* `PhoneNumberCleaner` is the `clean` function for phone numbers (can be passed into `InputParser`).

Some Java developers may choose a very different approach, but as an experienced Java developer, I would say that this
is a fairly standard approach to use in Java.

To be honest, I think it looks quite elegant if you ignore the annoying data classes that could've been simple tuples or unions if Java supported those!
This might be Stockholm syndrome talking though.

The Java program does look like extremely over-engineered when compared with the Lisp one, which required just a few short functions
to do the same thing!

Or does it really do the same thing??

Let's see.

```lisp
  (if (>= start (length digits))
      (format t "~a:~{ ~a~}~%" num (reverse words))
```

The first two lines of the Lisp function shown earlier are easy to understand.

It checks if the current index, `start`, is `>=` than the length of the `digits` array, and if it is,
it prints the solution, `words`. It seems to need to reverse the words because it accumulates then back-to-front later
(when it finds a good word, it calls `(cons word words)`).

The next lines are the `else` clause, where it tries to find words matching the phone number's digits.

```lisp
      (let ((found-word nil)
            (n 1)) ; leading zero problem
```

These lines define the variables `found-word` and `n` with initial values `nil` and `1`, respectively.

```lisp
         (loop for i from start below (length digits) do
              (setf n (+ (* 10 n) (nth-digit digits i)))
              (loop for word in (gethash n *dict*) do
                 (setf found-word t)
                 (print-translations num digits (+ 1 i) (cons word words))))
```

Here we have interesting stuff going on. It loops from indexes `start` to the length of `digits` (exclusive).

At each step, it sets `n` to an integer that's equivalent to the phone number's digits visited so far.
For example, when `i` is `0` and the phone number is `3456`, we should expect `n` to be `10*n + 3`. As `n` was initialized
to `1`, that's `13`. In the next step, `n` becomes `134` and so on. That means the phone number builds up to an integer
value with `1` prepended to it (that avoids dropping `0` values within the phone number).

For each value of `n` the program then tries to find all words in the dictionary that match it by calling
`(gethash n *dict*)`.

To understand that, we need to understand how the dictionary is represented. The `load-dictionary`function creates the
dictionary as follows:

```lisp
(defun load-dictionary (file size)
  (let ((table (make-hash-table :test #'eql :size size)))
    (with-open-file (in file)
      (loop for word = (read-line in nil) while word do
        (push word (gethash (word->number word) table))))
    table))
```

> This part of the code uses the [extended loop construct](http://www.lispworks.com/documentation/HyperSpec/Body/m_loop.htm#loop)
> of Common Lisp, which seems pretty complex to me! I still don't fully understand how `push` above is actually
> modifying the `table`, as `table` does not seem to be explicitly used except to read a hash from.

`word->number` is a function that _translates a word (string) into a phone number, according to the rules_.
It also makes sure to prepend `1` to the value it returns, so that it matches the values of `n` that will be used later.
Hence, at the end of the loop, `table` should contain a mapping from each phone number to the list of words representing
that phone number.

Now we can see that once the loop finds a word, it sets `found-word` to `t` (true) and then it recurses,
starting from the next position of the input and including the found word in the `words` argument.

After it's all done, it will have printed all matching solutions NOT including digit replacements for positions
where no word was found... so it continues:

```lisp
        (when (and (not found-word) (not (numberp (first words))))
           (print-translations num digits (+ start 1)
                               (cons (nth-digit digits start) words)))
```

The above snippet attempts to insert a digit from the input in the current position as long as no word
had been found, and the previous word had not been a digit.

Very smart algorithm.

But why can't it find certain solutions?

Let's try the algorithm using the counter-example found by my Java program:

```
/61167813
```

When it sees `611` it finds a word, `CNN`, then recurses starting from index `3`.
At index `3`, it finds two words that could form a solution, but none of them end up with a full valid solution.
But `found-word` is still set to `true`, so the program does not attempt to insert a digit at index `3`.

This might seem to be following the instructions:

```
If and only if at a particular point no word at all from
the dictionary can be inserted, a single digit from the phone number can
be copied to the encoding instead.
```

However, that only seems to be a useful limitation in case the word(s) found at that position actually produced a solution!
After all, it's better to find a solution with a digit in it than no solution at all?!

We can modify the program to interpret this instruction as I had done, so that more valid, useful solutions can be found
(which seems to be the ultimate objective).

[Here's my change to Norvig's program](https://github.com/renatoathaydes/prechelt-phone-number-encoding/commit/37031964c3395b0b510fd150109cdca3e1da0f6e)
so that it behaves identically to my Java program.

All I had to do was return a `boolean` when recursing, so that instead of setting the variable `found-word` to `true`
when a word was fond at a position, we set it to `true` only if the word produced a solution to the problem.
It only added a couple of lines to the program and did not significantly change its complexity.

With my change, the performance of the Java and Lisp programs became much more similar (and the outputs were identical):

```
▶ time sbcl --script lisp/main.lisp german-words.txt.ascii phones.txt > sbcl_out.txt 2>&1 

sbcl --script lisp/main.lisp german-words.txt.ascii phones.txt > sbcl_out.txt   5.59s  user 1.33s system 99% cpu 6.965 total
avg shared (code):         0 KB
avg unshared (data/stack): 0 KB
total (sum):               0 KB
max memory:                213264 KB
page faults from disk:     13
other page faults:         74049

▶ time java -cp build/ Main german-words.txt.ascii phones.txt > java_out.txt 2>&1                
java -cp build/ Main german-words.txt.ascii phones.txt > java_out.txt 2>&1   6.56s  user 1.88s system 165% cpu 5.110 total
avg shared (code):         0 KB
avg unshared (data/stack): 0 KB
total (sum):               0 KB
max memory:                910860 KB
page faults from disk:     305
other page faults:         303290

programming/projects/prechelt-phone-number-encoding  master ✗                                                                                           3h7m ⚑ ◒  
▶ wc -l sbcl_out.txt                                                             
  420226 sbcl_out.txt

programming/projects/prechelt-phone-number-encoding  master ✗                                                                                           3h7m ⚑ ◒  
▶ wc -l java_out.txt
  420226 java_out.txt

programming/projects/prechelt-phone-number-encoding  master ✗                                                                                           3h7m ⚑ ◒  
▶ diff --speed-large-files <(sort java_out.txt) <(sort sbcl_out.txt)
```

As you can see, things reversed and the Java program ran faster by nearly two seconds (`5.11` VS `6.96` seconds),
but Lisp still won by far in memory consumption. However, the JVM seems to be a bit too greedy allocating memory,
and when I tell the JVM to limit memory allocation, I was able to get the maximum memory down to under `480MB` for
the inputs I was trying:

```
▶ time java -Xmx400m -cp build/ Main german-words.txt.ascii phones.txt > java_out.txt 2>&1
java -Xmx400m -cp build/ Main german-words.txt.ascii phones.txt > java_out.tx   17.57s  user 2.06s system 317% cpu 6.176 total
avg shared (code):         0 KB
avg unshared (data/stack): 0 KB
total (sum):               0 KB
max memory:                479200 KB
page faults from disk:     90
other page faults:         150125
```

This did slow it down, though. It still ran just a little bit faster than the Lisp program, albeit requiring twice as
much RAM.

### Analysis of my Java program

My Java program has the `completeSolution` of the `Trie` class at its heart:

```java
private boolean completeSolution( List<Node> solution,
                                  char[] chars,
                                  int index,
                                  boolean allowInsertDigit,
                                  Consumer<List<Node>> onSolution ) {
    var solutionFound = false;
    if ( index < chars.length ) {
        var digit = chars[ index ] - 48;
        var trie = items[ digit ];
        if ( trie != null ) {
            solutionFound = trie.completeSolution( solution, chars, index + 1, false, onSolution );
            var atEndOfInput = index + 1 == chars.length;

            for ( Item word : trie.values ) {
                var nextSolution = append( solution, new Node( word ) );
                if ( atEndOfInput ) {
                    onSolution.accept( nextSolution );
                    solutionFound = true;
                } else {
                    solutionFound |= root.completeSolution( nextSolution, chars, index + 1, true, onSolution );
                }
            }
        }

        if ( !solutionFound && allowInsertDigit ) {
            solutionFound = tryInjectDigit( solution, chars, index, onSolution );
        }
    }
    return solutionFound;
}
```

It check if it's already at the end, in which case it has nothing to do... otherwise, it looks at the next digit:

```java
var digit = chars[ index ] - 48;
var trie = items[ digit ];
```

The first line converts a `char` to the corresponding integer value of the telephone number, then looks it up in the
current `Trie`'s `items` (which is an array of fixed size `10`, as digits can only range from `0` to `9`).

If there's a sub-Trie, it recurses:

```java
solutionFound = trie.completeSolution( solution, chars, index + 1, false, onSolution );
```

This method returns a `boolean` so that the caller can know if this branch of the `Trie` has been successful so far.

Next, the algorithm looks at each of the `values` in the current `Trie`, which are the words that can fit a solution
at the current position. Hence, each of the possible words are appended to a solution and then we recurse again:

```java
for ( Item word : trie.values ) {
    var nextSolution = append( solution, new Node( word ) );
    // accept solution if we're at the end
    if ( atEndOfInput ) {
        onSolution.accept( nextSolution );
        solutionFound = true;
    } else {
        solutionFound |= root.completeSolution( nextSolution, chars, index + 1, true, onSolution );
    }
}
```

Notice that an immutable `List` is used to represent each solution because the same partial solution might be used in
several solutions, so we need to copy the current, partial solution before proceeding so we don't lose the original...
for example, we might have solutions `[ABC, DEF]` and `[ABC, FGH]` (ignoring the encoding for a moment).
When we're at `ABC` we don't want to add `DEF` to the current partial solution, of course, otherwise the next solution
would become `[ABC, DEF, FGH]`.

If we're at the end of the input, we don't recurse and simply accept the current solution, otherwise we recurse starting
from the root `Trie` (all sub-`Trie`s keep a reference to the root to make this efficient), which means we start looking
at the beginning of the words again (but from the next `index` of the phone number).

```java
if ( !solutionFound && allowInsertDigit ) {
    solutionFound = tryInjectDigit( solution, chars, index, onSolution );
}
```

Finally, if no solution was found at the current position, and we're allowed to insert a digit (the caller knows if we're
allowed because it has just inserted either a digit - in which case we are - or a word - in which case, we're not), we
try to do just that.

`tryInjectDigit` appends the current digit into the solution and recurse into `completeSolution` from the root
of the `Trie`.

#### Changing the Java program to behave like the original Norvig's program

Just like we were able to modify Norvig's program to behave exactly like the Java program, we can also try to modify the
Java program to behave like Norvig's original program.

This can be useful to check how hard it is to modify the Java program and compare it to changing the Lisp program, which
was very easy even for me, someone who knows almost nothing about Common Lisp at all (I know a little basic Scheme,
but CL seems quite different).

Well, it did take me some effort to change my solution... here's the [changeset](https://github.com/renatoathaydes/prechelt-phone-number-encoding/commit/1237643ab2d6d2a445598441e9295c9260fac6d7)...
it took me the best part of an hour to figure out why my simple
modification, which I thought would work, didn't (I had trouble figuring out the best location to check whether a
recursion call would find a full word in it). To modify the Lisp program took much less effort (most of which looking
up documentation for functions I am unfamiliar with that I needed to use), probably less than 15 minutes. In both cases,
however, the change was pretty minimal.

After this change, my Java program started running almost exactly as fast as the original Norvig program, but still with
roughly twice as much memory required.

Other solutions:


Full Paper:

http://page.mi.fu-berlin.de/prechelt/Biblio/jccpprtTR.pdf

data
https://flownet.com/ron/papers/lisp-java/