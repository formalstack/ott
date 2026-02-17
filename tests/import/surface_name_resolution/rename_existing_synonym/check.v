Require Lib.
Require Main.

Local Definition check_use : Set := Main.use.
Local Definition check_use_from_lib : Lib.ext -> Main.use := Main.use_from_lib.
Local Definition check_ext_local_unit : Main.ext := Main.ext_local_unit.
Fail Local Definition check_use_from_lib_bad : Main.ext -> Main.use := Main.use_from_lib.
