# OCaml Interpreter (simple)

# English Manual

## Contents

This is simple OCaml interpreter written by OCaml.

### data type

* integer
* boolean
* list
* pair
* triple

### implementation

* variable
* functional abstraction
* recursive function
* pattern match
* type inference (including let polymorphism)

## Compilation
```
make
```

## Execution Sample

```
shell$ ./my_caml
MyCaml> 1;;
- : int = 1
MyCaml> true;;
- : bool = true
MyCaml> [];;
- : (type1 list) = []
MyCaml> (1, true);;
- : (int * bool) = (1, true)
MyCaml> let id x = x;;
val id : (type1 -> type1) = function
MyCaml> if id true then id 1 else 0;;
- : int = 1
MyCaml> let succ n = n + 1;;
val succ : (int -> int) = function
MyCaml> let a = 10;;
val a : int = 10
MyCaml> succ a;;
- : int = 11
MyCaml> let pred n = n - 1 in pred 100;;
- : int = 99
MyCaml> let rec fact n = if n < 1 then 1 else n * fact (n - 1);;
val fact : (int -> int) = recursive function
MyCaml> fact 10;;
- : int = 3628800
MyCaml> let rec zeros n = if n <= 0 then [] else 0 :: zeros (n - 1);;
val zeros : (int -> (int list)) = recursive function
MyCaml> zeros 10;;
- : (int list) = [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
MyCaml> let rec sum l = match l with | [] -> 0 | x :: xs -> x + sum xs;;
val sum : ((int list) -> int) = recursive function
MyCaml> sum [1;2;3;4;5];;
- : int = 15
MyCaml> let rec length l = match l with | [] -> 0 | x :: xs -> 1 + length xs;;
val length : ((type4 list) -> int) = recursive function
MyCaml> length [1;2;3] + length [true;false] + length [length;length;length];;
- : int = 8
MyCaml> let fst x = match x with | (x, y) -> x;;
val fst : ((type3 * type4) -> type3) = function
MyCaml> fst (1, [1]);;
- : int = 1
```

## Cleanup
```
make clean
```

# 日本語版手引 (Japanese Manual)

## 内容

OCaml で記述された簡単な OCaml の処理系です。

### データ型

* 整数
* 真偽値
* リスト
* 組
* 3つ組

### 実装内容

* 変数
* 関数抽象
* 再帰関数
* パターンマッチ
* let多相付きの型推論

## コンパイル方法

English Manual の Compilation を参照

## 実行例

English Manual の Execution Sample を参照

## クリーンアップ方法

English Manual の Cleanup を参照
