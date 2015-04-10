# OCaml Interpreter (simple)

# English Manual

## Contents
This is simple OCaml interpreter written by OCaml.

## Compilation
```
make
```

## Execution Sample

```
MyCaml> 1;;
1
MyCaml> true;;
true
MyCaml> fun x -> x;;
function
MyCaml> let succ n = n + 1;;
let succ = function
MyCaml> let a = 10;;
let a = 10
MyCaml> succ a;;
11
MyCaml> let rec fact n = if n < 1 then 1 else n * fact (n - 1);;
let fact = recursive function
MyCaml> fact 10;;
3628800
```

## Cleanup
```
make clean
```

# 日本語版手引 (Japanese Manual)

OCaml で記述された簡単な OCaml の処理系です。

## コンパイル方法
English Manual の Compilation を参照

## 実行例
English Manual の Execution Sample を参照

## 掃除
English Manual の Cleanup を参照
