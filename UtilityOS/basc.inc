BITS 16
%include "bpb.inc"
SECTION .text:

screen:
	pusha

	mov ah, 0x00
	mov al, 0x03
	int 0x10
	
	popa
	ret

clear:
	pusha

	call top
	mov ah, 0x09
	mov al, 0x20
	mov bh, 0x0
	mov bl, 00001111b
	mov cx, 0x7E8
	int 0x10
	popa
	ret

top:
	pusha

	mov ah, 0x02
	mov dx, 0x0
	mov bh, 0x0
	int 0x10

	popa
	ret

print:
	pusha
	mov ah, 0x0E
.loop:
	lodsb
	cmp al, 0
	je .done
	int 0x10
	jmp .loop
.done:
	popa
	ret

newline:
	pusha
	lea si, [nl]
	call print
	popa
	ret

;0x2600
getRoot:
	pusha
	xor cx, cx
	xor dx, dx
	mov ax, 0x0020						;Directory entry size in bytes
	mul word [bpbRootEntries]		; x count
	div word [bpbBytesPerSector]	; get the amount of sectors used by root entries
	xchg ax, cx
	
	mov al, byte [bpbNumberOfFATs]
	mul word [bpbSectorsPerFAT]
	add ax, word [bpbReservedSectors] ;=19 sectors
	;mov word [datasector], ax

	;add word [datasector], cx ;This value should compute to sector 33 ([33*512] ADDR: 0x4200)
	mov bx, 0xA000
	
	call ReadSectors

	popa	
	ret

readRoot:
	pusha
	mov cx, 0x04
	mov bx, 0xA000
	call newline
.loop:
	;mov si, err		;Well well well...
	;call print
	push bx
	lea si, [bx]		;A LITERAL SHIT.SHOW.
	call print
	pop bx
	add bx, 0x0020
	call newline
LOOP .loop

	popa
	ret

;Convert LBA to CHS
;C = LBA/(SPT*HPC)
;H = (LBaA/SPT)%HPC
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
	ret											;Exit
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

lod		db '.',0
err		db ',',0
nl		db 13,10,0