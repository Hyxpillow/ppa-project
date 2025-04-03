main: *.sml *.mlb lib-local/*.sml lib-local/*.mlb
	mpl -default-type int64 -default-type word64 main.mlb

push:
	git add .
	git commit -m "."
	git push --force

