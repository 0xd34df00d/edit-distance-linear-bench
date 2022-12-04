# edit-distance-linear-bench

This is the benchmark for the [edit-distance-linear](https://github.com/0xd34df00d/edit-distance-linear) library.
See also [this post](https://0xd34df00d.me/posts/2020/01/fast-edit-distance.html) for details.

This project uses `stack`. So, to build:
```shell
stack build
```

To run criterion benchmarks:
```shell
stack bench
```
Or, to generate a fancy HTML report:
```shell
stack bench --benchmark-arguments '--output=report.html'
```

Another (really sloppy, but quick) way of benchmarking a given implementation is running it once or twice on some test data and dumping the RTS execution stats via
```shell
stack exec -- edit-distance-linear-bench-exe <implementation> +RTS -sstderr
```
where implementation is an identifier like `3SL` (refer to [app/Main.hs](app/Main.hs) for the list of possible identifiers).
