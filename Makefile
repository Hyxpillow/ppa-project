NETID = yh5047

main: *.sml *.mlb lib-local/*.sml lib-local/*.mlb
	mpl -default-type int64 -default-type word64 main.mlb

push:
	git add .
	git commit -m "."
	git push

send:
	scp test-graph/* $(NETID)@access.cims.nyu.edu:~/ppa/ppa-project/test-graph/

recv:
	scp $(NETID)@access.cims.nyu.edu:~/ppa/ppa-project/test-graph-output/* ./test-graph-output/
