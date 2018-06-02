#!/bin/bash


#states=("NullState" "Synchronized" "BetterSafe" "Unsynchronized" "GetNSet")
#states=("NullState" "Synchronized" "BetterSafe" "GetNSet" )
#states=("BetterSafe" "Synchronized" )
states=("Unsynchronized")


iters=("1000" "100000" "10000000")

threads=("16")

array=($2)

for i in `seq 2 $3`
do
    array+=($(($RANDOM % $2)))
done

#echo "${array[@]}"

sum=0
echo Model,Threads,Transitions,array_size,ns_per_trans
for thread in "${threads[@]}"
do
    for iter in "${iters[@]}"
    do
        for state in "${states[@]}"
        do
            for i in `seq 1 $1`
            do
                s=$(>&1 java UnsafeMemory $state $thread $iter "${array[@]}")
                echo -n $state,$thread,$iter,$3, 
                echo $(echo $s | cut -d " " -f 3)$(echo $s | sed 's/[0-9a-zA-Z\.\ \/]*//g' | sed "s/(.*/,mismatch/g" | tr -s "\ " "," | sed 's/,$//g')
            done
        done
    done
done
