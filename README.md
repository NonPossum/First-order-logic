# First-order-logic in Rust



https://en.wikipedia.org/wiki/First-order_logic

https://pl.wikipedia.org/wiki/Rachunek_predykat%C3%B3w_pierwszego_rz%C4%99du



$$∃x ((Kx ∧ ∀y (Ky → y = x)) ∧ Bx)$$


```rust
    //∃x ((Kx ∧ ∀y (Ky → y = x)) ∧ Bx)
    let formula_king_bald = exists("x", and(
        and(
            predicate!("King", var!("x")),
            forall("y", implies(predicate!("King", var!("y")), predicate!("Equals", var!("y"), var!("x"))))
        ),
        predicate!("Bald", var!("x"))
    ));

    model.print_evaluation("The current King of France is bald", &formula_king_bald);
```
