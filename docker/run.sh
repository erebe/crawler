#!/bin/sh

cd /data
python3 -m http.server ${PORT:-8082} &
cd -

while true
do
  awk '/###DATA###/{system("./crawler");next}1' index_template.html > index_full.html
  mv index_full.html /data/index.html
  sleep ${SLEEP_TIME_SEC}
done
