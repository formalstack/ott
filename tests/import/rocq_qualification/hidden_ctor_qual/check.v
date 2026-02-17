Require Lib.
Require Main.

Local Definition check_uuse : Lib.pointer -> Prop := Main.UUse.
Local Definition check_uuse_one :
  forall pointer : Lib.pointer,
    Lib.JPack pointer (Lib.val_ptr pointer) -> Main.UUse pointer :=
  Main.UUseOne.
