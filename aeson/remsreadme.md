build: 
> stack init
> -- add include-dir to stack.yaml
> stack build

run: 
stack exec aeson-benchmark-aeson-parse 65536 700 benchmarks/json-data/buffer-builder.json

bins: 
aeson-benchmark-aeson-encode
aeson-benchmark-aeson-parse
aeson-benchmark-compare
aeson-benchmark-compare-with-json
aeson-benchmark-dates
aeson-benchmark-json-parse
aeson-benchmark-micro
aeson-benchmark-typed
aeson-example-generic
aeson-example-simplest
aeson-example-th
