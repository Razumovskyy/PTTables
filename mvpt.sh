#!/bin/bash 

set -e

[ $# -lt 2 ] && { echo "Bad arguments list. Provide two arguments: extension name and description text for the obtained PT-tables"; exit 1; }

ext=$1
text=$2

for file in ./PT_CALC/*; do
    filename=$(basename -- "$file")
    extension="${filename##*.}"
    if [ $extension = $ext ]; then
        mv $file /srv/PT_TABLES/
    fi 
done

echo -e $text > "/srv/PT_TABLES/$ext.txt"
