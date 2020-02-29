TITLE Project 4 - HexDump Perf

; hexdump.asm
; Alma Thompson
; CS240

INCLUDE CS240.inc
.8086

EXITDOS = 4C00h
CALLDOS = 21h

.code
GetProgramSegmentPrefix PROC
	; accepts:	nothing
	; returns:	BX = DOS Program Segment Prefix

	push	AX
	
	DOSPSP = 62h		; DOS command to get segment prefix
	
	mov	ah, DOSPSP	; fetch DOS program segment prefix
	int	CALLDOS		; returned in BX
	
	pop AX
	ret
GetProgramSegmentPrefix ENDP

GetCommandTail PROC
	; accepts:	DX = offset of filename variable
	; returns:	DX = buffer to write command tail

	pushf
	push	AX
	push	BX
	push	CX
	push	DX
	push	SI
	push	DI
	push	ES

	DOSCTSIZE = 80h			; location of command tail length
	DOSCTTAIL = 81h			; location of command tail characters

	call 	GetProgramSegmentPrefix	; PSP returned in BX
	mov	ES, BX			; ES used to reference segment

	mov	SI, DOSCTTAIL		; load start offset into SI
	mov	BX, DX			; load end offset into BX
	mov	DI, 0
	mov	CX, 0
	mov	CL, ES:[DOSCTSIZE]	; load the count into CX

	jmp	check

top:
	mov	DL, ES:[SI]		; get a character
	cmp	DI, 0			; checks if its a leading space
	jne	saveChar
	cmp	DL, ' '			; checks if its a space
	je skipChar

saveChar:
	mov 	[BX][DI], DL		; save character
	inc	DI

skipChar:
	inc	SI
	dec	CX	

check:
	cmp	CX, 0			; check if all the chars are copied
	jg	top

	mov	BYTE PTR [BX][DI], 0	; NUL-terminates the string	

	pop	ES
	pop	DI
	pop	SI
	pop	DX
	pop	CX
	pop	BX
	pop	AX
	popf
	ret
GetCommandTail ENDP

ReadFile PROC
	DOSREADFILE = 3Fh

	FILESIZE = 4
	FILEBUFFER = 6
	FILEHANDLE = 8

	push	BP
	mov	BP, SP
	pushf
	push	BX
	push	CX
	push	DX

	mov	BX, [bp + FILEHANDLE]
	mov	CX, [bp + FILESIZE]
	mov	DX, [bp + FILEBUFFER]
	mov	AH, DOSREADFILE
	int	CALLDOS
	jc error

	pop	DX
	pop	CX
	pop	BX
	popf	
	pop	BP
	ret

error:
	mov	AX, EXITDOS
	int	CALLDOS
ReadFile ENDP

OpenFile PROC
	; accepts:	DX = filename
	; returns:	AX = file handle
	;		exits on error

	DOSOPENFILE = 3Dh
	FILENAMEDIS = 4
	
	;	+--------------+ <- SP
	;	| registers    |
	;	| old BP       | <- BP
	;	| ret. address |
	;	| filename arg |
	;	+--------------+

	push	BP
	mov	BP, SP
	pushf
	push dx

	mov	DX, [BP + FILENAMEDIS]		; move filename to DX
	mov	AL, 00h				; indicate read only access
	mov	AH, DOSOPENFILE			; open file command
	int	CALLDOS				; move file handle to AX
	jc	error

	pop	DX
	popf
	pop	BP
	ret

error:
	mov	ax, EXITDOS
	int	CALLDOS
OpenFile ENDP

HexOut PROC
	PUSHF
	PUSH dx
	PUSH si
	PUSH ax	
	
	MOV si, 0
	CMP cx, 0
	JE superdone

	MOV dl, 32
	MOV ah, 02h
	INT 21h	

	MOV dl, 32
	MOV ah, 02h
	INT 21h	

	JMP top

top:
	PUSH bx
	MOV al, [bx + si]
	PUSH ax
	PUSH cx
	MOV cl, 4
	SHR al, cl
	POP cx
	AND al, 0FH
	MOV bx, 0
	MOV bl, al
	PUSH si
	MOV si, OFFSET hexdigits
	MOV dl, [si + bx] 
	POP si
	MOV ah, 02h
	int 21h

	POP ax
	AND al, 0Fh
	MOV bx, 0
	MOV bl, al
	PUSH si
	MOV si, OFFSET hexdigits
	MOV dl, [si + bx]
	POP si
	MOV ah, 02h
	int 21h
	POP bx

	MOV dl, 32
	MOV ah, 02h
	INT 21h	

	INC si
	JMP checkspace

fill:
	MOV dl, 32
	MOV ah, 02h
	INT 21h	

	MOV dl, 32
	MOV ah, 02h
	INT 21h	

	MOV dl, 32
	MOV ah, 02h
	INT 21h	

	inc si
	jmp checkspace

space:
	MOV dl, 32
	MOV ah, 02h
	INT 21h	
	jmp continue

checkspace:
	CMP si, 8
	JE space

continue:
	CMP si, cx
	JL top
	CMP si, 16
	JL fill

Done:
	MOV dl, 32
	MOV ah, 02h
	INT 21h	

superdone:
	POP ax
	POP si
	POP dx
	POPF
	ret

HexOut ENDP

TextOut PROC
	; accepts: 	offset in BX, number of chars in CX
	; returns: 	prints out the characters

	pushf
	push dx
	push si
	push ax

	mov dl, "|"
	mov ah, 02h
	int 21h

	cmp CX, 0
	JE done
	mov SI, 0

top:
	mov dl, [bx + si]
	CMP dl, 32
	JL null
	CMP dl, 126
	JG null
	JMP print

null:
	mov dl, "."

print:
	mov ah, 02h
	int 21h

	inc SI
	cmp si, cx
	JL top

done:
	mov dl, "|"
	mov ah, 02h
	int 21h

	pop ax
	pop si
	pop dx
	popf

	ret
TextOUt ENDP

ChangeOffset PROC
	push si
	push ax
	push bx
	pushf 

	mov si, 3

	cmp ch, 16
	JL canAddStart

start:
	mov ah, [bx + si]
	cmp ah, 0F0h
	jne	canAddStart
	mov BYTE PTR [bx][si], 0
	dec si
top:
	mov ah, [bx + si]
	cmp ah, 0FFh
	jne	canAdd
	mov BYTE PTR [bx][si], 0
	dec si
	jmp top

canAddStart:
	mov ah, [bx + si]
	add ah, ch
	mov [bx][si], ah
	jmp done

 canAdd:
	add ah, 1
	mov [bx][si], ah

done:
	popf
	pop bx
	pop ax
	pop si
	ret
ChangeOffset ENDP

OffsetOut PROC
	.data
	hexdigits BYTE "0123456789abcdef",0

	.code
	PUSHF
	PUSH dx
	PUSH si
	PUSH ax	
	
	MOV si, 0

top:
	PUSH bx
	MOV al, [bx + si]
	PUSH ax
	PUSH cx
	MOV cl, 4
	SHR al, cl
	POP cx
	AND al, 0FH
	MOV bx, 0
	MOV bl, al
	PUSH si
	MOV si, OFFSET hexdigits
	MOV dl, [si + bx] 
	POP si
	MOV ah, 02h
	int 21h

	POP ax
	AND al, 0Fh
	MOV bx, 0
	MOV bl, al
	PUSH si
	MOV si, OFFSET hexdigits
	MOV dl, [si + bx]
	POP si
	MOV ah, 02h
	int 21h
	POP bx

	INC si
	CMP si, 4
	JL top

	POP ax
	POP si
	POP dx
	POPF
	ret

OffsetOut ENDP

CopyArrays PROC
	LBUFF = 4
	BUFF = 6

	push bp
	MOV bp, sp

mov si, 0
	mov di, 0

top:
	MOV BX, [bp + 6]
	MOV al, [bx + si]

	MOV BX, [bp + 4]
	MOV [bx][di], al

	inc si
	inc di
	CMP si, 17
	JL top

	pop bp
	ret

CopyArrays ENDP

CheckRepeat PROC

	LBUFF = 4
	BUFF = 6
	IFSAME = 8
	LENBUFF = 10

	push bp
	MOV bp, sp

	; check if end of output
	MOV AX, [bp + LENBUFF]
	CMP AX, 16
	JE samecheck
	MOV AX, [bp + IFSAME]
	CMP AX, 1
	MOV AX, 0
	JE almostdone
	JMP done

	; check if they are the same
samecheck:
	MOV AX, [bp + IFSAME]
	push AX
	mov si, 0
top1:
	MOV BX, [bp + 6]
	MOV al, [bx + si]

	MOV BX, [bp + 4]
	MOV cl, [bx + si]

	CMP al, cl
	JNE notsame
	inc si
	CMP si, 17
	JL top1

	POP AX
	CMP ax, 1
	JE done

	MOV ax, 1
	JMP copythem

notsame:
	POP AX
	CMP ax, 0
	JE copythem

	MOV dl, "*"
	MOV ah, 02h
	int 21h

	MOV dl, 13
	MOV ah, 02h
	int 21h

	MOV dl, 10
	MOV ah, 02h
	int 21h

	MOV ax, 0
	; copies new into old
copythem:
	push AX
	
	MOV dx, [bp + BUFF]
	push dx
	MOV dx, [bp + LBUFF]
	push dx
	CALL CopyArrays
	add sp, 4

	pop AX
	jmp done

almostdone:
	MOV dl, "*"
	MOV ah, 02h
	int 21h

	MOV dl, 13
	MOV ah, 02h
	int 21h

	MOV dl, 10
	MOV ah, 02h
	int 21h

done:
	pop bp
	ret

CheckRepeat ENDP

main PROC
	.data
	filename	BYTE "file.txt", 0, 100 dup(0)
	buffer		BYTE 17 dup(0)
	handle		WORD ?	
	theoffset	BYTE 4 dup(0)
	lastbuffer	BYTE 17 dup(0)
	same		WORD ?
	first		WORD ?

	.code
	mov	AX, @data
	mov	DS, AX

	mov	dx, OFFSET filename
	call	GetCommandTail

	push	DX
	call	OpenFile
	add	SP, 2
	mov	handle, AX

	mov same, 0
	mov first, 0

top:
	mov	DX, handle
	push	DX
	mov	DX, OFFSET buffer
	push	DX
	mov	DX, LENGTHOF buffer - 1
	push	DX
	call	ReadFile
	add	SP, 6

	cmp	AX, 0
	je	done

	cmp first, 0
	JNE check
	push AX
	mov DX, OFFSET buffer
	push DX
	MOV DX, OFFSET lastbuffer
	push dx
	CALL CopyArrays
	add sp, 4
	pop AX
	JMP printout

check:
	push AX
	MOV AX, same
	push AX
	mov DX, OFFSET buffer
	push DX
	mov DX, OFFSET lastbuffer
	PUSH DX
	CALL CheckRepeat
	MOV same, AX
	add sp, 6
	POP AX

	CMP same, 1
	JE skip

printout:
	mov first, 1

	push BX
	mov BX, OFFSET theoffset
	call OffsetOut
	pop BX

	push AX
	push AX
	mov	BX, AX
	mov	buffer[bx], 0

	mov	BX, OFFSET buffer
	pop 	AX
	push 	BX
	mov 	CX, AX
	push 	CX
	call	HexOut
	pop 	CX
	pop  	BX
	call 	TextOut

	MOV dl, 13
	MOV ah, 02h
	int 21h

	MOV dl, 10
	MOV ah, 02h
	int 21h

	pop AX

skip:
	push AX
	push bx
	mov CH, Al
	mov BX, OFFSET theoffset
	call ChangeOffset
	pop bx
	POP AX

	jmp top

done:
	CMP first, 0
	JE donedone

	push BX
	mov BX, OFFSET theoffset
	call OffsetOut
	pop BX

	MOV dl, 13
	MOV ah, 02h
	int 21h

	MOV dl, 10
	MOV ah, 02h
	int 21h

donedone:
	mov	AX, EXITDOS
	int	CALLDOS
	ret
main ENDP

END main