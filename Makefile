main: *.sml *.mlb lib-local/*.sml lib-local/*.mlb
	mpl -default-type int64 -default-type word64 main.mlb

push:
	git add .
	git commit -m "."
	git push --force


send:
	scp test-graph/* yh5047@access.cims.nyu.edu:~/ppa/ppa-project/test-graph/

recv:
	scp yh5047@access.cims.nyu.edu:~/ppa/ppa-project/output/* ./output/
