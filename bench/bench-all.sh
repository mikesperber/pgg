heap=4000000
prefix=$1
shift

for style in $*
do

echo benchmarking $style
out=bench/bench$prefix-$style.log
echo "scheme48 -h $heap" > $out
cat bench/bench-$style.script bench/bench-pgg.script | scheme48 -h $heap >> $out

done
