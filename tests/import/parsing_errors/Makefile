include ../common.mk

.PHONY: all clean

all:
	$(call assert_command_fails,$(OTT) -tex_wrap false -o Transitive.tex transitive_main.ott,.transitive.out,Transitive.tex)
	$(call assert_contains,.transitive.out,"Atom_box")
	$(call assert_contains,.transitive.out,"not in scope")
	$(call assert_command_fails,$(OTT) -tex_wrap false -o Ambiguous.tex ambiguous_main.ott,.ambiguous.out,Ambiguous.tex)
	$(call assert_contains,.ambiguous.out,"Atom_box")
	$(call assert_contains,.ambiguous.out,"ambiguous")
	$(call assert_command_fails,$(OTT) -tex_wrap false -o RootRenameBad.tex root_rename_bad.ott,.root_rename_bad.out,RootRenameBad.tex)
	$(call assert_contains,.root_rename_bad.out,"Imported_result_Fold")
	$(call assert_contains,.root_rename_bad.out,"not in scope")
	$(call assert_command_fails,$(OTT) -tex_wrap false -o ClassRenameBad.tex class_rename_bad.ott,.class_rename_bad.out,ClassRenameBad.tex)
	$(call assert_contains,.class_rename_bad.out,"Imported_corePack")
	$(call assert_contains,.class_rename_bad.out,"not in scope")

clean:
	rm -f Transitive.tex Ambiguous.tex RootRenameBad.tex ClassRenameBad.tex \
	  .transitive.out .ambiguous.out .root_rename_bad.out .class_rename_bad.out
