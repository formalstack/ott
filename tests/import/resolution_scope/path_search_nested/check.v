Require Lib.
Require Main.

Local Definition check_expr : Set := Main.expr.
Local Definition check_expr_imported_val : Lib.value -> Main.expr := Main.Expr_imported_val.
