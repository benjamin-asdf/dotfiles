#!/bin/sh
# Log out the person that made the most commits in the last 100 on a file
git log -n100 --no-merges --pretty=format:%an | sort | uniq -c | awk '{print $2}'| head -n1
