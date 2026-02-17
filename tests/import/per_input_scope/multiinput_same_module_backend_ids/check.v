Require LibA.
Require LibB.
Require MultiInput.

Local Definition check_a_tm : Set := MultiInput.a_tm.
Local Definition check_b_tm : Set := MultiInput.b_tm.
Local Definition check_imported_a : LibA.tm -> MultiInput.a_tm := MultiInput.ATm_imported_a.
Local Definition check_imported_b : LibB.tm -> MultiInput.b_tm := MultiInput.BTm_imported_b.
