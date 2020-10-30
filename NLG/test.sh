#/usr/bin/bash

gf --run < test/basic.gfs | diff -u - test/basic.GOLD

gf --run < test/negation.gfs | diff -u - test/negation.GOLD

gf --run < test/safe.gfs | diff -u - test/safe.GOLD

echo "If this is the first line you see, it means success!"

if [ $# -eq 0 ]
then
    echo ""
else
    echo "Short pieces"
    gf --run < test/basic.gfs

    echo ""
    echo "------"
    echo ""
    echo "Negations"
    gf --run < test/negation.gfs
    echo ""
    echo "------"
    echo ""
    echo "SAFE"
    gf --run < test/safe.gfs
fi
