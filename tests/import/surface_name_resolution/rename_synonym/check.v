Require Lib.
Require Main.

Local Definition check_use : Prop := Main.Use.
Local Definition check_use_ok :
  forall value_list : list Lib.value, Lib.Store value_list -> Main.Use :=
  Main.Use_ok.
