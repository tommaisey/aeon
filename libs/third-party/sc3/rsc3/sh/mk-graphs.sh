#!/bin/sh

g=md/graphs.md

echo "## graphs" > $g
echo "" >> $g
for i in help/graph/*.lisp help/graph/*.scm
do
    echo "- [$(basename $i)](?t=rsc3&e=$i)" >> $g
done
