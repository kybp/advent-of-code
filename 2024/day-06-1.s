section .data
        sys_read equ 0
        sys_write equ 1
        sys_open equ 2
        sys_close equ 3
        sys_exit equ 60

        stdout equ 1

	lf equ 0xa

        height equ 130
        width equ 131  ; the last character is the newline
        infile db "2024/input/day-06.txt", 0

section .bss
        grid resb height * width
	visited resb (height * width + 7) / 8
	g_x resb 1
	g_y resb 1
	g_vx resb 1
	g_vy resb 1
        buffer resb 16

section .text
global _start
_start:
	call read_grid
	call init_position
	call walk_guard
	call count_visited
	call print_int

	mov rdi, 0
	mov rax, sys_exit
	syscall

read_grid:
        mov rdi, infile
        xor rsi, rsi      ; 0 for read-only
        mov rax, sys_open
        syscall

        mov rdi, rax            ; transfer the input file descriptor to rdi
        mov rsi, grid           ; use grid as the output buffer
        mov rdx, height * width ; number of bytes to read
        mov rax, sys_read
        syscall

        ret

init_position:
        xor rdx, rdx ; Initialise y-coordinate to 0
        dec rdx      ; Start at -1 because it's incremented at the top of the loop
.next_row:
        inc rdx
        cmp rdx, height
        jge .not_found

        xor rax, rax       ; set x-coordinate to 0 at the start of each line
.next_char:
        cmp rax, width - 1
        jge .next_row

        mov rcx, rdx       ; start with the y-offset
        imul rcx, width    ; scale the y-offset by the number of rows
        add rcx, rax       ; add the x-offset into the given row
        mov bl, [grid+rcx] ; load al with the `rax`th byte of `grid`

        cmp bl, '^'
        je .found
        inc rax            ; move to next character
        jmp .next_char

.found:
	;; Initialise position to located spot with direction facing up
	mov byte [g_vx], 0
	mov byte [g_vy], -1
        mov [g_x], al
        mov [g_y], dl
        xor rax, rax
        ret

.not_found:
        mov rax, 1
        ret

print_int:
    	mov rdi, buffer
        xor rcx, rcx       ; initialise character count to 0
        mov rbx, 10        ; the base to divide by
        dec rdi            ; allocate space for trailing newline
        inc rcx            ; increment character count
        mov byte [rdi], lf ; store trailing newline
.loop:  xor rdx, rdx
        div rbx            ; divide rax by 10, leaving quotient in rax and remainder in rdx
        add dl, '0'        ; convert the remainder to ASCII
        dec rdi            ; move buffer pointer backwards
        mov [rdi], dl      ; store character in the buffer
        inc rcx            ; increment character count
        test rax, rax      ; check if quotient is 0
        jnz .loop          ; if not, continue the loop

        mov rdx, rcx       ; leave string length in rdx for sys_write
        mov rsi, rdi       ; starting address of string
        mov rdi, stdout    ; the file descriptor to write to
        mov rax, sys_write
        syscall

        ret

walk_guard:
.loop:
	movzx rax, byte [g_x]
	movzx rdx, byte [g_y]
	call visit

	add al, [g_vx]
	cmp al, width - 2
	ja .done

	add dl, [g_vy]
	cmp dl, height - 1
	ja .done

	mov rcx, rdx
	imul rcx, width
	add rcx, rax
	mov bl, [grid+rcx]

	cmp bl, '#'
	je .turn
	mov [g_x], al
	mov [g_y], dl
	jmp .loop
.turn:	call turn_guard
	jmp .loop
.done:
	ret

turn_guard:
	cmp byte [g_vx], 0
	je .from_vertical

	cmp byte [g_vx], -1 ; facing left
	je .left_to_up
	mov byte [g_vx], 0  ; we were facing right, so face down
	mov byte [g_vy], 1
	ret
.left_to_up:
	mov byte [g_vx], 0  ; we were facing left, so face up
	mov byte [g_vy], -1
	ret
.from_vertical:
	cmp byte [g_vy], -1 ; facing up
	je .up_to_right
	mov byte [g_vx], -1 ; we were facing down, so face left
	mov byte [g_vy], 0
	ret
.up_to_right:
	mov byte [g_vx], 1  ; we were facing up, so face right
	mov byte [g_vy], 0
	ret

visit:
	mov r9, rdx
	imul r9, width
	add r9, rax
	mov rcx, r9
	shr r9, 3            ; Divide index by 8 to get the byte offset
	and cl, 7            ; Get the bit position within the byte
	mov r8b, 1
	shl r8b, cl          ; Shift 1 by the bit position
	or [visited+r9], r8b ; Set the bit
	ret

count_visited:
	xor rax, rax
	mov rcx, (height * width + 7) / 8
.loop:  movzx r8, byte [visited+rcx-1]
	popcnt r9, r8
	add rax, r9
	dec rcx
	jz .done
	jmp .loop
.done:  ret
