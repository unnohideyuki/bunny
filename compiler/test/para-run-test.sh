trap 'jobs -p | xargs kill' SIGINT

rm -f results/*.txt

./sub-run-para.sh 4 0 &
./sub-run-para.sh 4 1 &
./sub-run-para.sh 4 2 &
./sub-run-para.sh 4 3 &
wait

i0=`cat total_4_0.txt`
i1=`cat total_4_1.txt`
i2=`cat total_4_2.txt`
i3=`cat total_4_3.txt`
nng0=`cat nerr_4_0.txt`
nng1=`cat nerr_4_1.txt`
nng2=`cat nerr_4_2.txt`
nng3=`cat nerr_4_3.txt`

i=`expr $i0 \+ $i1 \+ $i2 \+ $i3`
nng=`expr $nng0 \+ $nng1 \+ $nng2 \+ $nng3`

echo ""

if [ $nng -eq 0 ]; then
    echo "==================="
    echo "Finished $i tests."
    echo "==================="
else
    echo "======================="
    echo " $nng/$i tests failed."
    echo "======================="
fi

exit $nng
