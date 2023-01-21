#!/usr/bin/env bash
##ls src/*.S | xargs ./bin/cadius INDENTFILE

PRODOS_IMAGE=images/ProDOS_2_4_2.dsk

NAME=main
##NAME=shrhello

##DISKNAME=blankdisk
DISKNAME=projdisk
VOLUMENAME=PROJ
FOLDERNAME=PROJ

./bin/merlin -V ./src/macro ./src/${NAME}.s

if [ $? -ne 0 ]; then
    exit $?
fi

sed -ie 's/Type(00)/Type(B3)/' src/_FileInformation.txt

./ac.sh -g ${PRODOS_IMAGE} PRODOS > files/PRODOS
./ac.sh -g ${PRODOS_IMAGE} BASIC.SYSTEM > files/BASIC.SYSTEM

## create the boot disk
rm ${DISKNAME}.po
./bin/cadius CREATEVOLUME ${DISKNAME}.po ${VOLUMENAME} 800kb
./bin/cadius CREATEFOLDER ${DISKNAME}.po ${FOLDERNAME}
##./bin/cadius ADDFILE ${DISKNAME}.po ${FOLDERNAME} src/${NAME}
./bin/cadius ADDFILE ${DISKNAME}.po ${FOLDERNAME} files/PRODOS
./bin/cadius ADDFILE ${DISKNAME}.po ${FOLDERNAME} files/BASIC.SYSTEM

## add the built file to the disk
./ac.sh -p ${DISKNAME}.po ${NAME} bin 0x2000 < src/${NAME}

/Applications/GSPlus.app/Contents/MacOS/gsplus
