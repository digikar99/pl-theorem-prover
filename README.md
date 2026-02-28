# pl-theorem-prover - Propositional Logic Theorem Prover

Basic theorem prover for propositional logic that supports `and or not` operators. Currently, this has no dependencies.

This uses the [Method of analytic tableaux](https://en.wikipedia.org/wiki/Method_of_analytic_tableaux), but users can customize the theorem prover.

## Example

```lisp
CL-USER> (defpackage :pltp-user
           (:use :cl)
           (:local-nicknames (:pl :pl-theorem-prover)))
#<PACKAGE "PLTP-USER">

CL-USER> (in-package :pltp-user)
#<PACKAGE "PLTP-USER">

PLTP-USER> (set-pprint-dispatch 'pl:node 'pl:pprint-node)
NIL

PLTP-USER> (pl:well-formed-formula-p '(not))
; Evaluation aborted on #<SIMPLE-ERROR "~S is not a well-formed-(sub)formula" {12063EEF43}>.

PLTP-USER> (pl:well-formed-formula-p '(not a b))
; Evaluation aborted on #<SIMPLE-ERROR "~S is not a well-formed-(sub)formula" {120642E623}>.

PLTP-USER> (pl:well-formed-formula-p '(and a b))
(AND A B)

PLTP-USER> (pl:well-formed-formula-p '(not a))
(NOT A)

PLTP-USER> (pl:prove* a (not a))
Unprovable.
NIL
#<PL::TABLEAUX {120BA4A0D3}>

PLTP-USER> (pl:prove* a (or b (not a)))
Provable.

T: B
  T: A
    T: (OR B (NOT A))
#<PL::TABLEAUX {120BBC20D3}>

PLTP-USER> (pl:prove* (not b) (or b (not a)))
Provable.

NIL: B
  T: (NOT A)
    T: (NOT B)
      T: (OR B (NOT A))
#<PL::TABLEAUX {1202D720D3}>

PLTP-USER> (pl:prove* a (not b) (or b (not a)))
Unprovable.
NIL
#<PL::TABLEAUX {1202D72333}>

PLTP-USER> (pl:prove* (and a c)
                      (or (not a) b))
Provable.

T: (NOT A)
  T: A
    T: C
      T: (AND A C)
        T: (OR (NOT A) B)
#<PL::TABLEAUX {1205E42123}>
```

## Notes

- `prove*` is a macro. It calls the function `prove`
- Theorem proving algorithm can be changed using the variable `*prover*`. Its default value is `'semantic-tableau-prover`.

## References

- https://philosophy.stackexchange.com/questions/10785/semantic-vs-syntactic-consequence
- https://oeis.org/wiki/Propositional_calculus
- Brute force: https://en.wikipedia.org/wiki/British_Museum_algorithm
- https://en.wikipedia.org/wiki/Method_of_analytic_tableaux
