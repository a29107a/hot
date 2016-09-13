#!/bin/sh
Dir=sdzs@172.16.225.109:~/apple_data_backup/
scp $1 $Dir
rm -f $1
echo "backup to  "${Dir}"   done!"

