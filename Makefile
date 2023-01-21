
.PHONY: all

ASM := ./bin/merlin -V ./src/macro
AC := ./ac.sh
CADIUS := ./bin/cadius

PRODOS_IMAGE:=images/ProDOS_2_4_2.dsk
BOOT_IMAGE:=images/bootdisk.po
PROJ_IMAGE:=images/projdisk.po
VOLUMENAME:=PROJ
FOLDERNAME:=PROJ

SRCS :=
SRCS += src/main.s
SRCS += src/shrhello.s

BINS := $(SRCS:.s=.bin)

all: $(PROJ_IMAGE) 

%.bin: %.s
	$(ASM) $<
	mv $(<:.s=) $@

## create the boot disk
$(BOOT_IMAGE): $(PRODOS_IMAGE)
	$(AC) -g $(PRODOS_IMAGE) PRODOS > files/PRODOS
	$(AC) -g $(PRODOS_IMAGE) BASIC.SYSTEM > files/BASIC.SYSTEM
	rm -f $(BOOT_IMAGE)
	$(CADIUS) CREATEVOLUME $(BOOT_IMAGE) $(VOLUMENAME) 800kb
	$(CADIUS) CREATEFOLDER $(BOOT_IMAGE) $(FOLDERNAME)
	##./bin/cadius ADDFILE $(BOOT_IMAGE) $(FOLDERNAME) src/$(NAME)
	$(CADIUS) ADDFILE $(BOOT_IMAGE) $(FOLDERNAME) files/PRODOS
	$(CADIUS) ADDFILE $(BOOT_IMAGE) $(FOLDERNAME) files/BASIC.SYSTEM

$(PROJ_IMAGE): $(BOOT_IMAGE) $(BINS)
	cp $(BOOT_IMAGE) $(PROJ_IMAGE)
	for NAME in $(BINS) ; do $(AC) -p $(PROJ_IMAGE) $$NAME bin 0x2000 < $$NAME; done
