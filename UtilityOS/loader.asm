BITS 16
ORG 0x7C00
jmp start

section .TEXT
start:
	cli
	;STACK
	;mov ax, 0x0100
	;mov [sgx], ax
	mov ax, 0x07C0
	;mov ds, ax
	mov es, ax
	;mov fs, ax
	;mov gs, ax
	
	mov ax, 0x0
	mov ss, ax
	mov sp, 0xFFFF
	sti
	
	;mov si, boot
	;call print
	;call newline
	mov si, time
	call print
	;call newline
	
	;Load stage 2
	;mov si, resm
	;call print
	
	;Just read  Read what
	;Amount of sectors to read 32*224/512 = 14 sectors
	xor cx, cx
	xor dx, dx
	mov ax, 0x0020						;Directory entry size in bytes
	mul word [bpbRootEntries]		; x count
	div word [bpbBytesPerSector]	; get the amount of sectors used by root entries
	xchg ax, cx ;=14 sectors for root entries
	
	;where to read from, jump over FAT and this bootloader to root entries
	mov al, byte [bpbNumberOfFATs]
	mul word [bpbSectorsPerFAT]
	add ax, word [bpbReservedSectors] ;=19 sectors
	;Calculate the base data sector, FAT sectors + boot sector + root entries sectors
	mov word [datasector], ax

	add word [datasector], cx ;This value should compute to sector 33 ([33*512] ADDR: 0x4200)
	mov bx, 0x0200
	
	call ReadSectors
	call newline
	jmp _root
	
_root:
	;Find stage two in the root directory
	mov cx, word [bpbRootEntries]
	mov di, 0x0200 ;ES:DI -> root entries
.loop:
	mov si, dbm
	call print

	push cx
	mov cx, 0x000B
	mov si, sysimg
	push di
	rep cmpsb ;compare bytes of si and di
	pop di
	je _FAT
	pop cx
	add di, 0x0020
	loop .loop
	jmp hang
	
_FAT:
	;Compute and store the location of the image
	xor dx, dx
	mov dx, word [es:di + 0x001A] ;At this adr. should be the value 0x0002
	mov word [cluster], dx

	call newline
	
	;Read FAT to 07C0:0200
	;Number of clusters to read in cx
	xor ax, ax
	mov al, byte [bpbNumberOfFATs]
	mul word [bpbSectorsPerFAT]
	mov cx, ax
	;location in ax
	mov ax, word [bpbReservedSectors] ;Just the bootsector
	;read to 7C00:0200 again (over-writing root entries on RAM)
	mov bx, 0x0200
	;Read all 18 sectors of the FAT
	call ReadSectors
	
	;Read stage 2 in to 0100:0000
	mov ax, 0x07C0
	xchg [sgx], ax
	mov es, ax
	;Read stage 2 after FAT 25*512
	mov bx, 0x0000
	push bx
	jmp _image

_image:
	call newline
	;read cluster
	mov ax, word [cluster]
	pop bx
	call ClustertoLBA
	xor cx, cx
	mov cl, byte [bpbSectorsPerCluster]
	call ReadSectors
	push bx
	;jmp done ;Stop early

	;Compute next cluster
	;FAT12 has 12-bit entries so where cluster * 1 would be enough for 8-bit entries
	;we must do cluster*1.5 to find its actual place
	mov ax, word [cluster]
	mov cx, ax
	mov dx, ax
	shr dx, 0x1
	add cx, dx
	mov bx, 0x0200
	add bx, cx
	
	push es
	push 0x07C0
	pop es
	mov dx, word [es:bx]
	pop es
		
	;cmp dx, 0xF003
	;je hang
	
	test cx, 0x0001
	jnz .odd

;The following two functions are actually incorrect they need to
;Read 1 word from the 24 per 2 clusters, then the upper (2nd)
;Or lower (1st) byte from the middle word	
.odd:
	mov si, dbn
	call print
	and dx, 0000111111111111b
	jmp .entry
	
.even:
	mov si, dbm
	call print
	shr dx, 0x0008
	jmp .entry
	
.entry:
	mov word [cluster], dx
	mov si, err
	call print
	cmp dx, 0xFF0
	jl _image

done:
	mov si, time
	call print
	jmp 0x0000:0x1000
	jmp hang

hang:
	mov ah, 0Eh
	mov al, '-'
	int 10h
	cli
	hlt
	
newline:
	pusha
	mov si, newln
	call print
	popa
	ret
	
print:
	pusha
	mov ah, 0Eh
.loop:
	lodsb
	cmp al, 0
	je .end
	int 10h
	jmp .loop
.end:
	popa
	ret
	
;Convert LBA to CHS
;C = LBA/(SPT*HPC)
;H = (LBA/SPT)%HPC
;S = (LBA%SPT) + 1
LBAtoCHS:
	;S
	xor dx, dx
	div word [bpbSectorsPerTrack]
	inc dl
	mov byte [sector], dl ;Let it be know that im really stupid, this at one point was [track]... doh!
	xor dx, dx
	div word [bpbHeadsPerCylinder]
	;H, using H calculation get the modulus
	mov byte [head], dl
	;C using H calculation get the divisor
	mov byte [track], al
	ret

;LBA = (Cluster - 2) * SPC
ClustertoLBA:
	sub ax, 0x0002
	xor cx, cx
	mov cl, [bpbSectorsPerCluster]
	mul cx
	add ax, word [datasector]
	ret

;Read Sectors
;AX - Starting block in LBA
;CX - Amount to read
;ES:BX - Buffer
ReadSectors:
.main:
	mov di, 0x03
.loop:
	push ax										;Move params to stack
	push bx
	push cx
	call LBAtoCHS								;Get location from AX
	
	mov ah, 0x02								;Processor directive
	mov al, 0x01 								;Number of sectors to read
	mov ch, byte [track]						;Track
	mov cl, byte [sector]						;Sector number (Starts at one in CHS)
	mov dh, byte [head]						;Head Number
	mov dl, byte[bsDriveNumber]		;Drive Number
	int 13h											;Read disk
	
	jnc .done
	
	mov si, err
	call print
	xor ax, ax
	int 13h											;Reset disk
	pop cx
	pop bx
	pop ax
	dec di
	jnz .loop
	jmp hang											;Exit
.done:
	mov si, lod
	call print
	pop cx											;Get params and operate
	pop bx
	pop ax
	add bx, word [bpbBytesPerSector]
	inc ax
LOOP .main
	ret
	
section .DATA
;1.44MB Floppy 3.5in. Normal settings for 1440KB floppy, fk'd up Power-ISO differences listed as well just incase
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
;boot	db "BOOTSTRAP",0
time	db "04JUN17 - 00:05",0 ;Good to be back baby

newln	db 13,10,0
;resm	db "Loading",0
lod	db ".",0
dbm	db "?",0
dbn	db "^",0
err	db ",",0

;err_read	db "Could not read from device.",0
;err_rset		db "Could not reset drive.",0
;err_dne		db "Halted. Unsucessful boot.",0

track			db 0x00
head			db 0x00
sector		db 0x00

datasector	dw 0x00
cluster		db 0x00

sysimg		db "SYSMAUTLSYS",20,0 ;Chainloader, Boot Image File
sgx		dw 0x0100

times 110 - ($-$$) db 0
dw 0xaa55
