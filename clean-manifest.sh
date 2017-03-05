#!/usr/local/bin/zsh

cp $1 $1.bak
sed "s/'/\"/g" $1 > $1.new
perl -pe 's/(\$[a-zA-Z0-9_\-]*)(?=(?:(?:[^\"\(]*+[\"\(]+){2})*+[^\"\)]*+\z)/\"$1\"/mg' \
    <<< $(cat $1.new) | tee $1.updated > /dev/null
sed "s/\"/'/g" $1.updated > $1
rm $1.new $1.updated
