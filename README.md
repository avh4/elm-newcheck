
## Examples

```sh
(cd examples && elm-test)
```

(You will see test failures like the following because `CircularBuffer.elm` has bugs in it!)

```sh
elm-test
--------

Running 1 test. To reproduce these results, run: elm-test --fuzz 100 --seed 650553919

↓ CircularBufferSpec
✗ runAll

Given [{ name = "put 0", pre = <function>, go = <function> },{ name = "get", pre = <function>, go = <function> },{ name = "put 0", pre = <function>, go = <function> },{ name = "get", pre = <function>, go = <function> },{ name = "put 0", pre = <function>, go = <function> },{ name = "put 0", pre = <function>, go = <function> },{ name = "get", pre = <function>, go = <function> },{ name = "get", pre = <function>, go = <function> }]

    <init>  -->  []
    get  -->  [0]
    put 0  -->  [0,0]
    put 0  -->  [0]
    get  -->  []
    put 0  -->  [0]
    get  -->  []
    put 0  -->  [0]
    get  ==>  FAILED

        expected Just 0, but got: Nothing
```
