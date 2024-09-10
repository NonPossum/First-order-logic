# First-order-logic in Rust



https://en.wikipedia.org/wiki/First-order_logic




$$∃x ((Kx ∧ ∀y (Ky → y = x)) ∧ Bx)$$


```rust
    let formula_king_bald = exists("x", and(
        and(
            predicate!("King", var!("x")),
            forall("y", implies(predicate!("King", var!("y")), predicate!("Equals", var!("y"), var!("x"))))
        ),
        predicate!("Bald", var!("x"))
    ));

    model.print_evaluation("The current King of France is bald", &formula_king_bald);
```
