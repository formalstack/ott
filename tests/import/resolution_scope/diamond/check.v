Require Dep_a.
Require Dep_b.
Require Diamond.

Local Definition check_expr : Set := Diamond.expr.
Local Definition check_expr_term : Dep_a.tm -> Diamond.expr := Diamond.Expr_term.
Local Definition check_expr_annot : Dep_b.pty -> Diamond.expr -> Diamond.expr := Diamond.Expr_annot.
