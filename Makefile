NETID = yh5047
PROJECT_DIR = ~/ppa/ppa-project

main: *.sml *.mlb lib-local/*.sml lib-local/*.mlb
	mpl -default-type int64 -default-type word64 main.mlb

push:
	git add .
	git commit -m "."
	git push

send:
	scp test-graph/* $(NETID)@access.cims.nyu.edu:$(PROJECT_DIR)/test-graph/

recv:
	scp $(NETID)@access.cims.nyu.edu:$(PROJECT_DIR)/test-graph-output/* ./test-graph-output/
