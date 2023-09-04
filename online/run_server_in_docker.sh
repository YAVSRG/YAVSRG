docker compose up --build --detach || $SHELL
#winpty docker exec --privileged -it redis sh -c 'echo "vm.overcommit_memory=1" >> /etc/sysctl.conf'
#winpty docker exec --privileged -it redis sh -c 'sysctl vm.overcommit_memory=1'

#$SHELL