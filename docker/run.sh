#!/bin/sh

while true
do
  awk '/###DATA###/{system("./crawler");next}1' index_template.html > index_full.html
  mv index_full.html homepage/index.html
  sleep ${SLEEP_TIME_SEC}
done
