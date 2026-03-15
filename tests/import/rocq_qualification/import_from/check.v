From Foo.Bar Require Dep.
From Foo.Bar Require Main.

Local Definition check_ctor : Dep.ty -> Main.expr := Main.Expr_imported_ty.
