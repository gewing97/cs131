#!/bin/bash

./test_me_daddy.sh 10 100 25 | tee $1_100_25.csv
./test_me_daddy.sh 10 100 500 | tee $1_100_500.csv
./test_me_daddy.sh 10 100 2500 | tee $1_100_2500.csv

./test_me_daddy.sh 10 30 25 | tee $1_30_25.csv
./test_me_daddy.sh 10 30 500 | tee $1_30_500.csv
./test_me_daddy.sh 10 30 2500 | tee $1_30_2500.csv

