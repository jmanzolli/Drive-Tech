#!/bin/bash

source .env

printf "\n\t\tDEPLOYING\n\n"

zip -r drive . -x $pem .git/\* .vscode/\* dev/\* renv/\* 
scp -i $pem -r drive.zip $server:/home/ubuntu/
sudo rm -rf drive.zip
ssh -i $pem $server 'unzip -o drive.zip -d drive'
ssh -i $pem $server 'cd drive; bash build.sh'