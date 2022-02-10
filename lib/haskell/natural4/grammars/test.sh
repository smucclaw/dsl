echo "basic tests"
cat tests/basic.conllu | gf-ud ud2gf UDApp Eng UDS at lin no-backups | diff -u - tests/basic.conllu.gold

echo ""
echo "more complex tests"
cat tests/succeeds.conllu | gf-ud ud2gf UDApp Eng UDS at lin no-backups | diff -u - tests/succeeds.conllu.gold
