# dynamic-witness
Implementation of http://eric.seidel.io/pub/nanomaly-icfp16.pdf.

### Pre-requisites
1. Oracle JDK 8
2. Scala 2.11.8 or higher
3. [sbt](http://www.scala-sbt.org/) (scala build tool)

### Running
1. From the directory containing `src`, run `sbt`.
2. Run `compile` and then `test`.
3. Use the command `run witness` to generate witnesses for a ill-typed program.
4. Use the command `run eval` to evaluate a program.

### Syntax
The BNF for the toy language implemeneted for this prototype is the following:
```ocaml
b ::= true | false
n ::= ... | -1 | 0 | 1 | ...
op ::= + | - | * | / | % | > | < | =
lst ::= expr :: lst | []

expr ::=  n
        | b
        | (e, e)
        | fst e
        | snd e
        | e or e
        | e and e
        | e op e
        | fun id -> e
        | fix id -> expr
        | let id = e in e
        | let id (id)+ = e in e
        | let rec id (id)+ = e in e
        | let (e, e) = e in e
        | if e then e else e
        | match e with 
          | [] -> e
          | hd :: tl -> e
        | (e)
```

### Desugaring
A lot of the syntax given is sugar for a much simpler underlying syntax. The following rules are used by the desugarer:
```ocaml
let id = e1 in e2         ==> (fun id -> e2) e1
e1 or e2                  ==> if e1 then e1 else e2
e1 and e2                 ==> if e1 then e2 else e1
let fn ids = fb in b      ==> let fn = fun id1 -> fun id2 -> ... -> fb in b               where ids = [id1, id2, ..., idn]
let rec fn ids = fb in b  ==> let fn = fix fn -> fun id1 -> fun id2 -> ... -> fb in b     where ids = [id1, id2, ..., idn]
let (e1, e2) = e in b     ==> Underlying(ECaseOfProduct(e, List(e1, e2), b)
fst e                     ==> Underlying(ECaseOfProduct(e, List(hd, tl), hd)
snd e                     ==> Underlying(ECaseOfProduct(e, List(hd, tl), tl)
```
while this desugaring is convinient for my implementation, in an actual system, one would want all of theses constructs as primitive to support better error messages and debugging for the user.

### Comparison to the actual artifact
- [NanoMaLy](https://github.com/ucsd-progsys/nanomaly/tree/master/src/NanoML), the implementation for the artifact described in the paper support a larger subset of OCaml than this implementation. The artifact implements exceptions, unit types, and records.

- It also implements functions from the OCaml standarf library such has List.map and List.filter.  Adding these to the prototype is trivial since it already supports all the primitives required to implement these.

- The artifact also implements a debugger which allows the user to see the program trace that is generated when the program is run with the witness generated. Implementing the debugger is fairly straight-forward (but a lot of engineering). Since the evalulator for this implementation uses a CEK machine, one can record all the reductions that are done and then display it as an interactive graph. Once the witness is generated, the evaluator can be run with it to generate the trace of all the reductions upto the erroneous state which is then put into a graph.
