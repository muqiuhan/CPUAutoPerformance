#!/bin/sh

while :
do
    racket cpu_auto_performance.rkt > /dev/null
    sleep 60
done &
