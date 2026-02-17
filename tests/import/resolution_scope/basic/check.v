Require Base.

Local Definition check_ty : Set := Base.ty.
Local Definition check_ty_var : Base.typevar -> Base.ty := Base.Ty_var.
Local Definition check_ty_arrow : Base.ty -> Base.ty -> Base.ty := Base.Ty_arrow.
