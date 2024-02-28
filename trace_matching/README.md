# Trace Verification

Trace parser and SMT-LIB generator.

## Web Invariants

Web invariants are defined in the `trace_matching.smt` file in SMT-LIB format.

## Trace parser and SMT-LIB generator.

The `parse_trace.pl` program allows to parse a JSON file obtained by executing our browser extension and generate a SMT-LIB file which includes the invariants defined in `trace_matching.smt` and that will check if every invariant holds on the traces present in the input JSON.

The `trace_matching_macroexpand.el` script allows to expand simple macros (e.g., `all!` to check that a predicate is valid for all elements of a list) to valid SMT-LIB. The `smtlib_compat.pl` file allows to convert a Z3-specific smt file to standard SMT-LIB, compatible with both Z3 and cvc5.

The `z3_wrapper.el` program wraps the Z3 prover and allows for parallel execution of the solver when the input SMT-LIB file contains scopes (push/pop).
