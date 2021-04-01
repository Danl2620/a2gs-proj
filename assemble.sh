#!/usr/bin/env bash
##ls src/*.S | xargs ./bin/cadius INDENTFILE

##NAME=main
NAME=shrhello

./bin/merlin -V ./src/macro ./src/${NAME}.s

if [ $? -ne 0 ]; then
    exit $?
fi

sed -ie 's/Type(00)/Type(B3)/' src/_FileInformation.txt

./bin/cadius DELETEFILE blankdisk.po NEW.DISK/${NAME}
./bin/cadius ADDFILE blankdisk.po NEW.DISK src/${NAME}
/Applications/GSPlus.app/Contents/MacOS/gsplus
