#!/usr/local/bin/zsh

cp $1 $1.bak
sed "s/'/\"/g" $1 > $1.new
perl -pe 's/(\$[a-zA-Z0-9_\-]*)(?=(?:(?:[^\"\(]*+[\"\(]+){2})*+[^\"\)]*+\z)/\"$1\"/mg' \
    <<< $(cat $1.new) | tee $1 > /dev/null
rm $1.new
