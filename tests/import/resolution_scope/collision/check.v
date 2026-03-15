Require Collision.
Require Collision_lib.

Local Definition check_expr : Set := Collision.expr.
Local Definition check_expr_cont : Collision_lib.container -> Collision.expr := Collision.Expr_cont.
Local Definition check_expr_valid : Collision.expr -> Prop := Collision.expr_valid.
