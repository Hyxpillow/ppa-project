test: *.sml *.mlb lib-local/*.sml lib-local/*.mlb
	mpl -default-type int64 -default-type word64 test.mlb