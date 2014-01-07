#!/bin/bash
cat repos |
(
while read line
do
cd ~/$line
git status -s
git fetch -v
done
)