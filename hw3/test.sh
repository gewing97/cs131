#!/bin/bash


#states=("NullState" "Synchronized" "BetterSafe" "Unsynchronized" "GetNSet")
#states=("NullState" "Synchronized" "BetterSafe" "GetNSet" )
states=("BetterSafe" "Synchronized" )



n1=101
n2=15
n3=20
n4=25
n5=40
n6=45
n7=60
n8=35

echo Model,Threads,Transitions,ns_per_trans
for state in "${states[@]}"
do
    sum=0
    declare -f sum
    for i in `seq 1 $1`
    do
	    read s <<< $(java UnsafeMemory $state $2 $3 \
                                                    $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5\
                                                    $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5\
                                                    $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5\
                                                    $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5\
                                                    $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5\
                                                    $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5\
                                                    $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5\
                                                    $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5\
                                                    $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5\
                                                    $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5\
                                                    $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5\
                                                    $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5\
                                                    $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5\
                                                    $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5\
                                                    $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5\
                                                    $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5\
                                                    $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5\
                                                    $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5\
                                                    $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5\
                                                    $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5\
                                                    $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5\
                                                    $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5\
                                                    $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5\
                                                    $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5\
                                                    $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5 $n1 $n2 $n3 $n4 $n5\
                                            )
        #sum="$sum + $(echo $s | cut -d " " -f 3)"
        echo -n $state,$2,$3, 
        echo $(echo $s | cut -d " " -f 3)$(echo $s | sed 's/[0-9a-zA-Z\.\ \/]*//g' | sed "s/(.*/,mismatch/g" | tr -s "\ " "," | sed 's/,$//g')
    done
    #sum=$(echo $sum | bc)
    #echo "                                "$(echo $sum/$1 | bc)
done

