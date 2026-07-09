# Automated Theorem Proving Language

## Syntax

### Formulae

Formulae can contain the following symbols:
- `0` and `1` corresponding to true and false values respectively,
- variable names consisting of capital letters and underscores only, e.g. `A`, `VAR`, `MY_VAR`,
- logical symbols: `~` (negation), `/\` (conjunction), `\/` (alternative), `=>` (implication), `<=>` (equivalence),
- parentheses for operation order enforcement, e.g. `A /\ (B \/ C)`,
- any amount of spaces.

The list of valid formulae is listed below.

1. A variable is a formula.
2. If `p` is a formula, ` p` and `p ` is also a formula.
3. If `p` is a formula, then `(p)` is also a formula.
4. If `p` is a formula, `~p` is a formula.
5. If `p` and `q` are formulae, then `p/\q`, `p\/q`, `p->q`, `p<=>q` are also formulae.

### Axioms

The axiom is a statement assumed to be true. The syntax is presented below:

```
axiom [name]: [formula];
```

`[name]` should consist of lowercase letters and underscores only.

### Theorems

A theorem consists of a keyword `theorem` followed by its name and formula. The theorem formula should contain an outermost implication which determines the assumption and the conclusion. Then, the proof follows.

The proof is a list consisting of an assumption, deduction steps and a conclusion. The assumption and the conclusion should match their equivalents in the theorem formula.

The detailed syntax is shown below.

```
theorem [name]: [formula]
proof
    intro [name]: [formula];
    have [name]: [formula] by [property_name facts...];
    exact [name];
qed
```

If `intro` or `exact` line is ended with a `by [property_name facts...]` clause, it will not throw an error - instead, the clause is ignored.