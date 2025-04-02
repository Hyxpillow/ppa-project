main: *.sml *.mlb lib-local/*.sml lib-local/*.mlb
	mpl -default-type int64 -default-type word64 main.mlb

push:
	git add .
	git commit -m "."
	git push --force

send:
	scp meGraph.txt yh5047@access.cims.nyu.edu:~/ppa/ppa-project/

recv:
	scp yh5047@access.cims.nyu.edu:~/ppa/ppa-project/comm.txt .

