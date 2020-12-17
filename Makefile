mlkit: make_parser.sml
	mlkit modal.mlb

mlton: make_parser.sml
	mlton -default-ann 'warnUnused true' modal.mlb

make_parser.sml: make_parser.cmyacc
	cmyacc -o make_parser.sml make_parser.cmyacc

.PHONY: mlkit mlton
