heap=4000000

for style in cps direct
do

echo benchmarking $style
out=bench/bench-$style.log
echo "scheme48 -h $heap" > $out
cat bench/bench-$style.script bench/bench-pgg.script | scheme48 -h $heap >> $out

done
