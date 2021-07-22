## Benchmark Runner

Simple Rust program to run benchmarks.

It collects the memory allocated by the program and then prints it, together with the
total time taken by the program to execute to completion.

It checks whether the process has completed every 5ms (so it can sample memory usage at the same time), hence this is
the lowest time resolution achievable.

### Compiling

```
cargo build --release
```

### Running

```
target/release/benchmark_runner <program> <args>
```

Example:

```
target/release/benchmark_runner java -cp build/java Main
```

### Testing

Compile the [alloc.c](alloc.c) file with make:

```
make alloc
```

`alloc` can allocate the given number of bytes and sleep for the given amount of seconds.

Example:

```
$ ./alloc 10000 2
Allocating 10000 bytes
Memory successfully allocated. Sleeping for 2 seconds....
Done
```

This allows us to test the benchmark_runner:

```
target/release/benchmark_runner ./alloc 1024000 1
Proc,Memory(bytes),Time(ms)
./alloc,1785856,1012
```

It reports the `alloc` program allocated up to 1,785,856 bytes (1,744kb) and ran for 2014 ms.

The bytes allocated reflect the fact that there's a minimum overhead running a C program:

```
target/release/benchmark_runner ./alloc 1 1
Proc,Memory(bytes),Time(ms)
./alloc,749568,1011
```

We can see that the overhead is around 732kb (on my machine, at least).

Hence, `benchmark_runner` is telling us that in the previous run, approximately `1012kb` were allocated
(1,744kb - 732kb), which is pretty close to the expected `1Mb`.
