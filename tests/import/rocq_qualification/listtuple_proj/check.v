Require Lib.
Require Main.

Local Definition check_use : list Main.value -> Prop := Main.Use.
Fail Local Definition check_use_bad : list Lib.value -> Prop := Main.Use.
