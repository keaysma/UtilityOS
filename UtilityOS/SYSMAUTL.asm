;Stage 2
;TODO - Make a version of this JUST for loading the GDT, no bulk items, see if they're causing some unforeseen problem
[bits 16]
ORG 0x1000
jmp protectedMode

;%include "Gdt.inc"
%include "basc.inc"
;%include "bpb.inc"


protectedMode:
.msg	db 'EPM',0

	cli
	push cs
	pop ds
	sti

	mov si, .msg
	call print

	;call installGDT ;Implemented GDT register in bochs says 0x0a <- Installation is faulty

	;Coffee
	;push ds
	;pop es
	;mov bx, 0x600


	push ds
	pop es
	call getRoot
	mov bx, 0xA000

	lea di, [str_img]
	mov si, 0xA000
	mov cx, 0x04
.LOOP:
	push cx
	push di
	push si
	mov cx, 0x0B
	REP CMPSB
	je .LOCATED
	pop si
	pop di
	pop cx
	add si, 0x20
LOOP .LOOP
.NOT_LOCATED:
	mov si, err
	call print
	jmp EPMGATE
.LOCATED:
	lea si, [str_img]
	call print
	pop si
	mov cx, [si+0x1A]
	mov [cluster], cx

EPMGATE:
	call coffee
	call screenPhase

	cli
	lgdt [toc]
	mov eax, cr0
	or eax, 1
	mov cr0, eax

	jmp 0x08:Stage3

screenPhase:
	pusha
	mov ah, 0x0
	mov al, 0x12	;New and improved VGA space!!!
	int 0x10
	popa
	ret

coffee:
	pusha
	
	mov ax, 0x24	;AX - Starting block in LBA, ADDRESS: 0x4800
	mov cx, 0x15	;CX - Amount to read, CLUSTERS: [0x4800 to and including 0x7200]
	
	push 0x0
	pop es
	mov bx, 0xB000	;ES:BX - Buffer, READ TO [DS:0xB000]

	call ReadSectors

	popa
	ret


hang:
	jmp hang

installGDT:
	cli
	lgdt [ds:toc]
	sti
	ret


;*******************************************
; Global Descriptor Table (GDT)
;*******************************************
 
gdtb: 
	dd 0x0 				; null descriptor
	dd 0x0 
 
gdt_code:				; code descriptor
	dw 0xFFFF 			; limit low
	dw 0 				; base low
	db 0 				; base middle
	db 0x9A 			; access
	db 11001111b 		; granularity
	db 0 				; base high
 
gdt_data:				; data descriptor
	dw 0xFFFF 			; limit low (Same as code)
	dw 0 				; base low
	db 0 				; base middle
	db 0x92 			; access
	db 11001111b 		; granularity
	db 0				; base high
 
end_of_gdt:


toc:
	dw end_of_gdt - gdtb - 1 	; limit (Size of GDT)
	dd gdtb 			; base of GDT


[bits 32]
ORG 0x1000

Stage3:
	cli

	mov ax, 0x10
	mov ds, ax
	mov es, ax
	mov fs, ax

	mov gs, ax
	mov ss, ax
	mov es, ax
	mov esp, 0x10000

	mov al, 0xDD		;A20
	out 0x64, al		;A20

	mov edi, 0xB8000	;VIDEO MEMORY

	call ClearScreen

	mov esi, start_msg
	mov AH, 0x3F
	call Puts		

	mov word [pX], 0x00
	mov word [pY], 0x01

	mov esi, ver_msg
	mov AH, 0x3F
	call Puts

	mov word [pX], 0x0E
	mov word [pY], 0x01
	call MoveCursor


	;0xAB A - color (presec. no known list), B - Corralative intensity 
;	mov edi, 0xA0000
;	mov ecx, 0xFA00
;.TLP:
;	mov word [edi], 0x0000
;	add edi, 0x02
;LOOP .TLP
;	jmp GetInput

	mov edi, 0xA0000	;MEMORY!
	mov ecx, 0x32
	mov si, 0xB000
	mov ah, 0x1F
.LOOP:
	push ecx
	mov ecx, 0x32
	.I_LOOP:
		lodsb
		and al, 0x1F
		mov byte [edi], al
		add edi, 0x02
	LOOP .I_LOOP
	pop ecx
	add edi, 0xDC
LOOP .LOOP

	jmp GetInput

MoveCursor:
	pusha
	
	xor eax, eax
	mov ax, [pY]
	mul byte [pWidth]
	add ax, [pX]
	;mul byte [attrib]
	mov ebx, eax

	;Low
	mov al, 0x0F
	mov dx, 0x03D4
	out dx, al

	mov al, bl
	mov dx, 0x03D5
	out dx, al

	;High
	mov al, 0x0E
	mov dx, 0x03D4
	out dx, al

	mov al, bh
	mov dx, 0x03D5
	out dx, al

	popa
	ret

;ESI - String, AH - Attrib
Puts:
	pusha
	push eax

	xor eax, eax
	mov edi, 0xB8000
	mov ax, [pY]
	mul word [pWidth]
	add ax, [pX]
	mul word [attrib]
	add edi, eax

	pop eax

.LOOP:
	lodsb
	;mov ah, 0xF0
	cmp al, 0
	je .done
	mov word [edi], ax
	add edi, 2
	jmp .LOOP

.done:
	popa
	ret

ClearScreen:
	pusha

	mov ecx, 0x7D0
	mov edi, 0xB8000
	mov dh, 0xBB
	mov dl, ' '
.LOOP:
	mov word [edi], dx
	add edi, 2
	loop .LOOP

	popa
	ret

GetInput:
	;mov al, 0xD0
	in al, 0x64

	test al, 0x01
	jnz Out
	jmp GetInput 

Out:
	;mov al, 0xC0
	;out 0x64, al
	

	in al, 0x60
	mov dl, al
	add edi, 0x02

	mov word [edi], dx
	jmp GetInput	

End:
	mov dh, 0x0F
	mov dl, '!'

	mov word [edi+2], dx
	jmp STOP

STOP:
	cli
	hlt

start_msg	db 'Utility Operating System',0
ver_msg		db 'Version: 0.0.1', 0
str_img		db 'COFFEE',0x20,0x20,'IMG',0x20,0x0
pX			dw 0
pY			dw 0
pWidth		dw 0x50
attrib		dw 0x02