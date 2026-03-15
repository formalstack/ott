Require LibA.
Require LibB.
Require MultiInputMerged.

Local Definition check_a_tm : Set := MultiInputMerged.a_tm.
Local Definition check_b_tm : Set := MultiInputMerged.b_tm.
Local Definition check_imported_a : LibA.tm -> MultiInputMerged.a_tm := MultiInputMerged.ATm_imported_a.
Local Definition check_imported_b : LibB.tm -> MultiInputMerged.b_tm := MultiInputMerged.BTm_imported_b.
