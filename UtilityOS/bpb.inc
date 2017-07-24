BITS 16

SECTION .data:
;;;;;;;;;;;;;;;;;;;;;;;;;;BIOS PARAMETER BLOCK;;;;;;;;;;;;;;;;;;;;;;;;;;

bpbOEM						db "MICHAEL",0
bpbBytesPerSector		dw 512
bpbSectorsPerCluster	db 1
bpbReservedSectors		dw 1
bpbNumberOfFATs		db 2
bpbRootEntries				dw 224 ;<- Normal / Power-ISO: 256
bpbTotalSectors				dw 2880
bpbMedia						db 0xF0 ;0xF8 for partitionable media
bpbSectorsPerFAT		dw 9 ;<- Normal / Power-ISO: 12
bpbSectorsPerTrack		dw 18
bpbHeadsPerCylinder	dw 2
bpbHiddenSectors			dd 0
bpbTotalSectorsBig		dd 0
bsDriveNumber				db 0
bsUnused						db 0
bsExtBootSignature		db 0x29
bsSerialNumber				dd 0x00000000
bsVolumeLabel				db "MOS FLOPPY "
bsFileSystem					db "FAT12   "

;;;;;;;;;;;;;;;;;;;;;;;;;;BIOS PARAMETER BLOCK;;;;;;;;;;;;;;;;;;;;;;;;;;

track			db 0x00
head			db 0x00
sector		db 0x00

datasector	dw 0x00
cluster		db 0x00