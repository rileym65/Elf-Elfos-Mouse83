; *******************************************************************
; *** This software is copyright 2004 by Michael H Riley          ***
; *** You have permission to use, modify, copy, and distribute    ***
; *** this software so long as this copyright notice is retained. ***
; *** This software may not be used in commercial applications    ***
; *** without express written permission from the author.         ***
; *******************************************************************

; R7 - PC
; R8 - Mouse stack
; R9 - Stack frame
; RA - Flags

include    bios.inc
include    kernel.inc

           org     8000h
           lbr     0ff00h
           db      'mouse',0
           dw      9000h
           dw      endrom+7000h
           dw      2000h
           dw      endrom-2000h
           dw      2000h
           db      0
 
           org     2000h
           br      start

include    date.inc
include    build.inc
           db      'Written by Michael H. Riley',0

progend:   dw      0

start:     call    o_inmsg             ; display header
           db      'Rc/Mouse-83 V0.9.0',10,13,0
           lda     ra                  ; move past any spaces
           smi     ' '
           lbz     start
           dec     ra                  ; move back to non-space character
           ghi     ra                  ; copy argument address to rf
           phi     rf
           glo     ra
           plo     rf
loop1:     lda     rf                  ; look for first less <= space
           smi     33
           lbdf    loop1
           dec     rf                  ; backup to char
           ldi     0                   ; need proper termination
           str     rf
           ghi     ra                  ; back to beginning of name
           phi     rf
           glo     ra
           plo     rf
           ldn     rf                  ; get byte from argument
           lbnz    good                ; jump if filename given
           call    o_inmsg             ; otherwise display usage message
           db      'Usage: type filename',10,13,0
           ret                         ; and return to os
good:      ldi     high fildes         ; get file descriptor
           phi     rd
           ldi     low fildes
           plo     rd
           ldi     0                   ; flags for open
           plo     r7
           call    o_open              ; attempt to open file
           lbnf    load                ; jump if file was opened
           ldi     high errmsg         ; get error message
           phi     rf
           ldi     low errmsg
           plo     rf
           call    o_msg               ; display it
           lbr     o_wrmboot           ; and return to os
load:      mov     r7,program          ; where to place program
loadlp:    mov     rc,128              ; read next 128 bytes
           mov     rf,buffer           ; buffer to retrieve data
           call    o_read              ; read the header
           glo     rc                  ; check for zero bytes read
           lbz     done                ; jump if so
           mov     rf,buffer           ; buffer to retrieve data
linelp:    glo     rc                  ; see if done with block
           lbz     loadlp              ; read next block
           dec     rc                  ; decrement count
           lda     rf                  ; get next byte
           str     r7                  ; store into program space
           inc     r7
           lbr     linelp              ; continue processing block
done:      ldi     '$'                 ; write end of program command
           str     r7
           inc     r7
           ldi     0                   ; also write a zero
           str     r7
           inc     r7
           mov     rf,progend          ; write where program ends
           ghi     r7
           str     rf
           inc     rf
           glo     r7
           str     rf
           mov     r9,r7               ; set stack frame pointer
           call    o_close             ; close input file
           mov     r7,program          ; set program counter for macro search
msrch:     lda     r7                  ; get next byte from program
           lbz     mdone               ; jump if done with macro search
           smi     '$'                 ; check for character starting macro
           lbnz    msrch               ; jump if not leading character
           ldn     r7                  ; get following character
           call    is_uc               ; check for letter
           lbdf    mac_uc              ; jump if so
           call    is_lc               ; check for lowercase
           lbnf    msrch               ; jump if not
           smi     'a'                 ; convert to binary
macstr:    shl                         ; addresses are 2 bytes
           adi     macros.0            ; add in macros address
           plo     rf
           ldi     macros.1
           adci    0
           phi     rf                  ; rf now points to this macros address
           inc     r7                  ; move past letter
           ghi     r7                  ; store address in macro table
           str     rf
           inc     rf
           glo     r7
           str     rf
           lbr     msrch               ; keep looking for macros
mac_uc:    smi     'A'                 ; convert to numeric value
           lbr     macstr              ; and store macro address
mdone:     mov     rf,himem            ; point to high memory pointer
           lda     rf                  ; retrieve it
           phi     r8                  ; and set mouse stack
           ldn     rf
           plo     r8
           mov     ra,0                ; clear flags
           mov     r7,program          ; set program counter to start
runlp:     lda     r7                  ; get next program byte
           smi     ' '                 ; ignore spaces
           lbz     runlp
           dec     r7
           glo     ra                  ; see if tracing is active
           shr
           lbnf    go                  ; jump if not
           ldi     123                 ; display open brace
           call    o_type
           ldn     r7                  ; read next program byte
           call    o_type              ; and display it
           ldi     125                 ; display close brace
           call    o_type
go:        lda r7
           call    is_number           ; check for number
           lbdf    cmd_num             ; jump if it is a number
           call    is_lc               ; check for variable name
           lbdf    cmd_lc
           call    is_uc               ; check for variable name
           lbdf    cmd_uc
           smi     33                  ; check for ! command
           lbz     cmd_ex
           smi     1                   ; check for quote
           lbz     cmd_qt              ; jump if so
           smi     1                   ; check for # command
           lbz     cmd_mc
           smi     1                   ; check for $ command
           lbz     o_wrmboot           ; return Elf/OS
           smi     1                   ; check for % command
           lbz     cmd_pr
           smi     2                   ; check for '
           lbz     cmd_tk              ; jump if so
           smi     1                   ; check for ( command
           lbz     runlp               ; no need to do anything
           smi     1                   ; check for ) command
           lbz     cmd_cp
           smi     1                   ; check for * command
           lbz     cmd_ml
           smi     1                   ; check for + command
           lbz     cmd_ad
           smi     1                   ; check for , command
           lbz     cmd_me
           smi     1                   ; check for - command
           lbz     cmd_sb
           smi     1                   ; check for . command
           lbz     cmd_gv
           smi     1                   ; check for / command
           lbz     cmd_dv
           smi     11                  ; check for : command
           lbz     cmd_sv
           smi     1                   ; check for ; command
           lbz     cmd_me
           smi     1                   ; check for < command
           lbz     cmd_lt
           smi     1                   ; check for = command
           lbz     cmd_eq
           smi     1                   ; check for > command
           lbz     cmd_gt
           smi     1                   ; check for ? command
           lbz     cmd_in
           smi     1                   ; check for @ command
           lbz     cmd_rt
           smi     27                  ; check for [ command
           lbz     cmd_sq
           smi     1                   ; check for \ command
           lbz     cmd_md              ; jump if so
           smi     1                   ; check for ] command
           lbz     runlp               ; no need to do anything
           smi     1                   ; check for ^ command
           lbz     cmd_el
           smi     29                  ; check for { command
           lbz     cmd_tron
           smi     1                   ; check for | command
           lbz     cmd_es
           smi     1                   ; check for } command
           lbz     cmd_troff
           smi     1                   ; check for ~ command
           lbz     cmd_rm
           lbr     runlp               ; ignore unknown command

cmd_sq:    inc     r8                  ; retrieve value from stack
           lda     r8
           plo     re                  ; keep a copy here too
           str     r2
           shl                         ; check for negative number
           lbdf    cmd_sq_1            ; skip conditional if negative
           ldn     r8
           or
           lbnz    runlp               ; do nothing if non-zero
cmd_sq_1:  mov     rc,1                ; need to find ending ]
cmd_sq_lp: lda     r7                  ; get next byte from program
           plo     re                  ; save a copy
           smi     '|'                 ; check for else
           lbz     cmd_sq_el           ; jump if so
           glo     re
           smi     '['                 ; check for open
           lbz     cmd_sq_op           ; jump if open
           glo     re                  ; recover byte
           smi     ']'                 ; check for end
           lbnz    cmd_sq_lp           ; jump if not
           dec     rc                  ; decrement parens count
           glo     rc                  ; see if done
           lbz     runlp               ; jump if so
           lbr     cmd_sq_lp           ; otherwise keep looking
cmd_sq_op: inc     rc                  ; increment bracket count
           lbr     cmd_sq_lp           ; and keep looking
cmd_sq_el: glo     rc                  ; must be at 1 parens count
           smi     1
           lbz     runlp               ; back to main loop if so
           lbr     cmd_sq_lp           ; otherwise keep lookin

cmd_ad:    sex     r8                  ; recover arguments
           irx
           ldxa
           phi     rd
           ldxa
           plo     rd
           ldxa
           phi     rc
           ldx
           plo     rc
           sex     r2                  ; point x back to machine stack
           glo     rd                  ; add the two numbers together
           str     r2
           glo     rc
           add
           plo     rf
           ghi     rd
           str     r2
           ghi     rc
           adc
           phi     rf
           lbr     push                ; push result

cmd_cp:    dec     r7                  ; move back before )
           dec     r7
           mov     rc,1                ; clear parens count
cmd_cp_lp: ldn     r7                  ; get byte from program
           smi     '('                 ; is it open parens
           lbz     cmd_cp_op           ; jump if so
           ldn     r7               
           smi     ')'                 ; check for close parens
           lbz     cmd_cp_cp
cmd_cp_1:  dec     r7                  ; move to prior byte
           lbr     cmd_cp_lp           ; check next character
cmd_cp_cp: inc     rc                  ; increment parens count
           lbr     cmd_cp_1            ; and keep looking
cmd_cp_op: dec     rc                  ; decrement count
           glo     rc                  ; check parens count
           lbz     runlp               ; jump if not nested
           lbr     cmd_cp_1            ; and keep looking

cmd_dv:    sex     r8                  ; recover arguments
           irx
           ldxa
           phi     rd
           ldxa
           plo     rd
           ldxa
           phi     rc
           ldx
           plo     rc
           sex     r2                  ; point x back to machine stack
           call    div16               ; Perform division
           lbr     pushrc

cmd_el:    inc     r8                  ; retrieve value from stack
           lda     r8
           str     r2
           ldn     r8
           or
           lbnz    runlp               ; do nothing if zero
           mov     rc,1                ; need to count parens
cmd_el_lp: lda     r7                  ; get next byte from program
           plo     re                  ; save a copy
           smi     '('                 ; is it open parens
           lbz     cmd_el_op           ; jump if so
           glo     re                  ; recover byte
           smi     ')'                 ; check for close parens
           lbnz    cmd_el_lp           ; jump if not
           dec     rc                  ; decrement close count
           glo     rc                  ; see if done
           lbnz    cmd_el_lp           ; jump if not
           lbr     runlp               ; otherwise continue program
cmd_el_op: inc     rc                  ; increment parens count
           lbr     cmd_el_lp           ; and keep looking

cmd_es:    mov     rc,1                ; need to count parens
cmd_es_lp: lda     r7                  ; get next byte from program
           plo     re                  ; save a copy
           smi     '['                 ; is it open parens
           lbz     cmd_es_op           ; jump if so
           glo     re                  ; recover byte
           smi     ']'                 ; check for close parens
           lbnz    cmd_es_lp           ; jump if not
           dec     rc                  ; decrement close count
           glo     rc                  ; see if done
           lbnz    cmd_es_lp           ; jump if not
           lbr     runlp               ; otherwise continue program
cmd_es_op: inc     rc                  ; increment parens count
           lbr     cmd_es_lp           ; and keep looking

cmd_eq:    sex     r8                  ; recover arguments
           irx
           ldxa
           phi     rd
           ldxa
           plo     rd
           ldxa
           phi     rc
           ldx
           plo     rc
           sex     r2                  ; point x back to machine stack
           glo     rd                  ; add the two numbers together
           str     r2
           glo     rc
           sm
           plo     rf
           ghi     rd
           str     r2
           ghi     rc
           smb
           phi     rf
           glo     rf
           str     r2
           ghi     rf
           or
           lbz     logic_1
           lbr     logic_0

cmd_lt:    sex     r8                  ; recover arguments
           irx
           ldxa
           phi     rd
           ldxa
           plo     rd
           ldxa
           phi     rc
           ldx
           plo     rc
           sex     r2                  ; point x back to machine stack
           glo     rd                  ; add the two numbers together
           str     r2
           glo     rc
           sm
           plo     rf
           ghi     rd
           str     r2
           ghi     rc
           smb
           phi     rf
           glo     rf
           str     r2
           ghi     rf
           or
           lbz     logic_0
           ghi     rf
           shl
           lbdf    logic_1
           lbr     logic_0

cmd_gt:    sex     r8                  ; recover arguments
           irx
           ldxa
           phi     rd
           ldxa
           plo     rd
           ldxa
           phi     rc
           ldx
           plo     rc
           sex     r2                  ; point x back to machine stack
           glo     rd                  ; add the two numbers together
           str     r2
           glo     rc
           sd
           plo     rf
           ghi     rd
           str     r2
           ghi     rc
           sdb
           phi     rf
           glo     rf
           str     r2
           ghi     rf
           or
           lbz     logic_0
           ghi     rf
           shl
           lbdf    logic_1
           lbr     logic_0

logic_0:   mov     rf,0
           lbr     push
logic_1:   mov     rf,1
           lbr     push


cmd_ex:    sex     r8                  ; point to mouse stack
           irx                         ; recover word
           ldxa
           phi     rf
           ldx
           plo     rf
           sex     r2                  ; restore machine stack
           ldn     r7                  ; get next byte
           smi     39                  ; check for single quote
           lbz     cmd_ex_c            ; jump if so
           call    itoa                ; display number
           lbr     runlp               ; then back to command loop
cmd_ex_c:  inc     r7                  ; move past tick mark
           glo     rf                  ; get low of value
           call    o_type              ; and display it
           lbr     runlp               ; then back to loop

cmd_gv:    mov     rc,variables        ; point to variable storage
           inc     r8                  ; point to variable on stack
           lda     r8
           phi     rd
           ldn     r8                  ; get variable number
           shl                         ; 2 bytes per variable
           plo     rd
           ghi     rd
           shlc
           phi     rd
           glo     rc                  ; rc += rd
           str     r2
           glo     rd
           add
           plo     rc
           ghi     rc
           str     r2
           ghi     rd
           adc
           phi     rc
           lda     rc                  ; recover variable value
           phi     rf
           ldn     rc
           plo     rf
           lbr     push                ; push onto stack

cmd_in:    ldi     '?'                 ; display prompt
           call    o_type
           mov     rf,buffer           ; point to buffer space
           call    o_input             ; get input
           ldn     r7                  ; get next byte
           smi     39                  ; check for single quote
           lbz     cmd_in_c            ; jump if so
           push    r7                  ; save program position
           mov     r7,buffer           ; point to input
           call    atoi                ; convert to binary
           pop     r7                  ; recover program position
           lbr     push                ; push number to stack
cmd_in_c:  mov     rf,buffer           ; point to buffer
           ldn     rf                  ; get first character
           plo     rf                  ; and place into rf
           ldi     0
           phi     rf                  ; clear high byte
           inc     r7                  ; move past tick mark
           lbr     push                ; save to stack

cmd_md:    sex     r8                  ; recover arguments
           irx
           ldxa
           phi     rd
           ldxa
           plo     rd
           ldxa
           phi     rc
           ldx
           plo     rc
           sex     r2                  ; point x back to machine stack
           call    mod16               ; get remainder
           lbr     pushrc

cmd_ml:    sex     r8                  ; recover arguments
           irx
           ldxa
           phi     rd
           ldxa
           plo     rd
           ldxa
           phi     rc
           ldx
           plo     rc
           sex     r2                  ; point x back to machine stack
           call    mul16               ; multiply numbers
pushrc:    sex     r8                  ; push result to mouse stack
           glo     rc
           stxd
           ghi     rc
           stxd
           lbr     runlp               ; back to main loop

cmd_num:   dec     r7                  ; move back to number
           call    atoi                ; call atoi
push:      sex     r8                  ; push number onto mouse stack
           glo     rf
           stxd
           ghi     rf
           stxd
           sex     r2                  ; restore x
           lbr     runlp               ; back to command loop

cmd_qt:    lda     r7                  ; get next byte
           plo     re                  ; keep a copy
           smi     34                  ; check for quote
           lbz     runlp               ; done with command
           glo     re                  ; recover byte
           smi     '!'                 ; check for ! mark
           lbz     cmd_qt_1            ; jump if so
           glo     re                  ; recover character
           call    o_type              ; and display it
           lbr     cmd_qt              ; keep processing characters
cmd_qt_1:  call    o_inmsg             ; output cr/lf
           db      10,13,0
           lbr     cmd_qt              ; then keep processing

cmd_rm:    lda     r7                  ; read next program byte
           plo     re                  ; save a copy
           smi     '$'                 ; check for end of program
           lbz     o_wrmboot           ; exit if so
           glo     re                  ; recover byte
           ani     0e0h                ; check for control codes
           lbnz    cmd_rm              ; keep looking until non-printable
           lbr     runlp               ; then back to main loop

cmd_sb:    sex     r8                  ; recover arguments
           irx
           ldxa
           phi     rd
           ldxa
           plo     rd
           ldxa
           phi     rc
           ldx
           plo     rc
           sex     r2                  ; point x back to machine stack
           glo     rd                  ; add the two numbers together
           str     r2
           glo     rc
           sm
           plo     rf
           ghi     rd
           str     r2
           ghi     rc
           smb
           phi     rf
           lbr     push                ; push result

cmd_sv:    mov     rc,variables        ; point to variable storage
           inc     r8                  ; point to variable on stack
           lda     r8
           phi     rd
           lda     r8                  ; get variable number
           shl                         ; 2 bytes per variable
           plo     rd
           ghi     rd
           shlc
           phi     rd
           glo     rc                  ; rc += rd
           str     r2
           glo     rd
           add
           plo     rc
           ghi     rc
           str     r2
           ghi     rd
           adc
           phi     rc
           lda     r8                  ; get high byte from stack
           str     rc                  ; store into variable
           inc     rc
           ldn     r8                  ; get lsb
           str     rc                  ; store into variable
           lbr     runlp               ; back to command loop
           
cmd_tk:    lda     r7                  ; get next byte
           plo     rf                  ; place into rf
           ldi     0                   ; clear hight byte
           phi     rf
           lbr     push                ; push character to stack

cmd_lc:    smi     'a'                 ; convert to 0-25
           plo     rf
           ldi     0
           phi     rf
           lbr     push

cmd_uc:    smi     'A'                 ; convert to 0-25
           plo     rf
           ldi     0
           phi     rf
           lbr     push

cmd_tron:  mov     ra,1
           lbr     runlp

cmd_troff: mov     ra,0
           lbr     runlp

cmd_rt:    lda     r9                  ; retrieve return address
           phi     r7
           ldn     r9
           plo     r7
           dec     r9
           glo     r9                  ; move pointer back to previous entry
           smi     20
           plo     r9
           ghi     r9
           smbi    0
           phi     r9
           lbr     runlp               ; back to main loop

cmd_mc:    lda     r7                  ; get next byte
           call    is_uc               ; check for uppercase letter
           lbdf    cmd_mc_u            ; jump if so
           call    is_lc               ; check for lowercase letter
           lbdf    cmd_mc_l            ; jump if so
           call    o_inmsg             ; display error message
           db      'Invalid macro.',10,13,0
           lbr     o_wrmboot
cmd_mc_u:  smi     'A'                 ; convert to 0-25
           lbr     cmd_mc_1
cmd_mc_l:  smi     'a'                 ; convert to 0-25
cmd_mc_1:  shl                         ; macro addresses are two bytes
           adi     macros.0            ; add in address of macro table
           plo     rc
           ldi     macros.1
           adci    0
           phi     rc                  ; RC now points to macro address
           glo     r9                  ; point R9 to next call frame
           adi     20
           plo     r9
           ghi     r9
           adci    0
           phi     r9                  ; R9 now pointing at new call frame
           mov     rf,r9               ; set RF to first parameter address
           inc     rf
           inc     rf
cmd_mc_lp: lda     r7                  ; get next program byte
           lbz     o_wrmboot           ; jump if end of program found
           plo     re                  ; save a copy
           smi     ';'                 ; check for end of macro call
           lbz     cmd_mc_go           ; jump if found
           glo     re                  ; recover it
           smi     ','                 ; check for comma
           lbnz    cmd_mc_lp           ; jump if not
           ghi     r7                  ; store address of parameter to call frame
           str     rf
           inc     rf
           glo     r7
           str     rf
           inc     rf
           lbr     cmd_mc_lp           ; keep looking for end of macro call
cmd_mc_go: ghi     r7                  ; store return address
           str     r9                  ; in call frame
           inc     r9
           glo     r7
           str     r9
           dec     r9                  ; restore frame address
           lda     rc                  ; retrieve macro address
           phi     r7                  ; into r7
           lda     rc
           plo     r7
           lbr     runlp               ; and then back to main loop

cmd_me:    pop     r7                  ; recover original program counter
           glo     r9                  ; move call frame one up
           adi     20
           plo     r9
           ghi     r9
           adci    0
           phi     r9
           lbr     runlp               ; and back to run loop

cmd_pr:    inc     r8                  ; retrieve parameter from mouse stack
           inc     r8
           ldn     r8
           shl                         ; parameter addresses are 2 bytes
           str     r2                  ; store for add
           glo     r9                  ; add in current call frame address
           add
           plo     rc
           ghi     r9
           adci    0
           phi     rc                  ; RC points to parameter address
           push    r7                  ; save current program pointer
           lda     rc                  ; retrive parameter address into PC
           phi     r7
           lda     rc
           plo     r7
           glo     r9                  ; move call frame 1 call earlier
           smi     20
           plo     r9
           ghi     r9
           smbi    0
           phi     r9
           lbr     runlp               ; and back to main loop

; ************************************
; *** make both arguments positive ***
; *** Arg1 RC                      ***
; *** Arg2 RD                      ***
; *** Returns D=0 - signs same     ***
; ***         D=1 - signs difer    ***
; ************************************
mdnorm:    ghi     rc                  ; get high byte if divisor
           str     r2                  ; store for sign check
           ghi     rd                  ; get high byte of dividend
           xor                         ; compare
           shl                         ; shift into df
           ldi     0                   ; convert to 0 or 1
           shlc                        ; shift into D
           plo     re                  ; store into sign flag
           ghi     rc                  ; need to see if RC is negative
           shl                         ; shift high byte to df
           lbnf    mdnorm2             ; jump if not
           ghi     rc                  ; 2s compliment on RC
           xri     0ffh
           phi     rc
           glo     rc
           xri     0ffh
           plo     rc
           inc     rc
mdnorm2:   ghi     rd                  ; now check rD for negative
           shl                         ; shift sign bit into df
           lbnf    mdnorm3             ; jump if not
           ghi     rd                  ; 2 compliment on RD
           xri     0ffh
           phi     rd
           glo     rd
           xri     0ffh
           plo     rd
           inc     rd
mdnorm3:   glo     re                  ; recover sign flag
           ret                         ; and return to caller
; *********************************************
; *** Function to multiply 2 16 bit numbers ***
; *** RC *= RD                              ***
; *********************************************
mul16:     push    rf                  ; save consumed register
           call    mdnorm              ; normalize numbers
           plo     re                  ; save for later
           ldi     0                   ; zero out total
           phi     rf
           plo     rf
mulloop:   glo     rd                  ; get low of multiplier
           lbnz    mulcont             ; continue multiplying if nonzero
           ghi     rd                  ; check hi byte as well
           lbnz    mulcont
           mov     rc,rf               ; transfer answer
           glo     re                  ; get sign comparison
           shr                         ; shift into DF
           lbnf    mulexit             ; jump if signs were the same
           glo     rc                  ; 2s compliment answer
           xri     0ffh
           adi     1
           plo     rc
           ghi     rc
           xri     0ffh
           adci    0
           phi     rc
mulexit:   pop     rf                  ; recover consumed registers
           ret                         ; return to caller
mulcont:   ghi     rd                  ; shift multiplier
           shr
           phi     rd
           glo     rd
           shrc
           plo     rd
           lbnf    mulcont2            ; loop if no addition needed
           glo     rc                  ; add RC to RF
           str     r2
           glo     rf
           add
           plo     rf
           ghi     rc
           str     r2
           ghi     rf
           adc
           phi     rf
mulcont2:  glo     rc                  ; shift first number
           shl
           plo     rc
           ghi     rc
           shlc
           phi     rc
           lbr     mulloop             ; loop until done

; *********************************************
; *** Function to divide 2 16 bit numbers   ***
; *** RC /= RD                              ***
; *********************************************
div16:     call    mdnorm              ; normalize numbers
           plo     re                  ; save sign comparison
           glo     rd                  ; check for divide by zero
           lbnz    div16_1
           ghi     rd
           lbnz    div16_1
           mov     rc,0                ; return 0 as div/0
           ret                         ; and return to caller
div16_1:   push    rf                  ; save consumed registers
           push    r9
           push    r8
           ldi     0                   ; clear answer
           phi     rf
           plo     rf
           phi     r8                  ; set additive
           plo     r8
           inc     r8
d16lp1:    ghi     rd                  ; get high byte from rd
           ani     128                 ; check high bit
           lbnz    divst               ; jump if set
           glo     rd                  ; lo byte of divisor
           shl                         ; multiply by 2
           plo     rd                  ; and put back
           ghi     rd                  ; get high byte of divisor
           shlc                        ; continue multiply by 2
           phi     rd                  ; and put back
           glo     r8                  ; multiply additive by 2
           shl
           plo     r8
           ghi     r8
           shlc
           phi     r8
           lbr     d16lp1              ; loop until high bit set in divisor
divst:     glo     rd                  ; get low of divisor
           lbnz    divgo               ; jump if still nonzero
           ghi     rd                  ; check hi byte too
           lbnz    divgo
           glo     re                  ; get sign flag
           shr                         ; move to df
           lbnf    divret              ; jump if signs were the same
           ghi     rf                  ; perform 2s compliment on answer
           xri     0ffh
           phi     rf
           glo     rf
           xri     0ffh
           plo     rf
           inc     rf
divret:    mov     rc,rf               ; move answer to rc
           pop     r8                  ; recover consumed registers
           pop     r9
           pop     rf
           ret                         ; jump if done
divgo:     mov     r9,rc               ; copy dividend
           glo     rd                  ; get lo of divisor
           str     r2                  ; store for subtract
           glo     rc                  ; get low byte of dividend
           sm                          ; subtract
           plo     rc                  ; put back into r6
           ghi     rd                  ; get hi of divisor
           str     r2                  ; store for subtract
           ghi     rc                  ; get hi of dividend
           smb                         ; subtract
           phi     rc                  ; and put back
           lbdf    divyes              ; branch if no borrow happened
           mov     rc,r9               ; recover copy
           lbr     divno               ; jump to next iteration
divyes:    glo     r8                  ; get lo of additive
           str     r2                  ; store for add
           glo     rf                  ; get lo of answer
           add                         ; and add
           plo     rf                  ; put back
           ghi     r8                  ; get hi of additive
           str     r2                  ; store for add
           ghi     rf                  ; get hi byte of answer
           adc                         ; and continue addition
           phi     rf                  ; put back
divno:     ghi     rd                  ; get hi of divisor
           shr                         ; divide by 2
           phi     rd                  ; put back
           glo     rd                  ; get lo of divisor
           shrc                        ; continue divide by 2
           plo     rd
           ghi     r8                  ; get hi of divisor
           shr                         ; divide by 2
           phi     r8                  ; put back
           glo     r8                  ; get lo of divisor
           shrc                        ; continue divide by 2
           plo     r8
           lbr     divst               ; next iteration

; *********************************************
; *** Modulo function of 2 16 bit numbers   ***
; *** RC = RC % RD                          ***
; *********************************************
mod16:     push    rc                  ; will need both original numbers later
           push    rd
           call    div16               ; perform division
           pop     rd                  ; multiply by original RD
           call    mul16
           pop     rd                  ; recover original RC
           glo     rc                  ; RC = RD - RC
           str     r2
           glo     rd
           sm
           plo     rc
           ghi     rc
           str     r2
           ghi     rd
           smb
           phi     rc
           ret                         ; return to caller

          
; ****************************************
; ***** Convert ASCII to integer     *****
; ***** R7 - Pointer to ASCII number *****
; ***** Returns: RF - 16-bit integer *****
; ****************************************
atoi:      ldi     0                   ; clear total
           plo     rf
           phi     rf
atoi_0_1:  lda     r7                  ; get next character
           call    is_number           ; is it a number
           lbnf    atoi_0_2            ; jump if not
           smi     '0'                 ; convert it to binary
           plo     re                  ; and set it aside for now
           glo     rf                  ; multiply total by 2
           shl
           plo     rf
           plo     rc                  ; keep a copy here too
           ghi     rf
           shlc
           phi     rf
           phi     rc
           glo     rc                  ; multiply rc by 2
           shl
           plo     rc
           ghi     rc
           shlc
           phi     rc
           glo     rc                  ; multiply rc by 4
           shl
           plo     rc
           ghi     rc
           shlc
           phi     rc
           glo     rc                  ; rf += rc
           str     r2
           glo     rf
           add
           plo     rf
           ghi     rc
           str     r2
           ghi     rf
           adc
           phi     rf
           glo     re                  ; rf += new number
           str     r2
           glo     rf
           add
           plo     rf
           ghi     rf
           adci    0
           phi     rf
           lbr     atoi_0_1            ; loop back for more numerals
atoi_0_2:  dec     r7                  ; move back to non-numeral character
           ret                         ; and return to caller

; **************************************
; ***** Convert RF to bcd in M[RD] *****
; **************************************
tobcd:     push    rd           ; save address
           ldi     5            ; 5 bytes to clear
           plo     re
tobcdlp1:  ldi     0
           str     rd           ; store into answer
           inc     rd
           dec     re           ; decrement count
           glo     re           ; get count
           lbnz    tobcdlp1     ; loop until done
           pop     rd           ; recover address
           ldi     16           ; 16 bits to process
           plo     r9
tobcdlp2:  ldi     5            ; need to process 5 cells
           plo     re           ; put into count
           push    rd           ; save address
tobcdlp3:  ldn     rd           ; get byte
           smi     5            ; need to see if 5 or greater
           lbnf    tobcdlp3a    ; jump if not
           adi     8            ; add 3 to original number
           str     rd           ; and put it back
tobcdlp3a: inc     rd           ; point to next cell
           dec     re           ; decrement cell count
           glo     re           ; retrieve count
           lbnz    tobcdlp3     ; loop back if not done
           glo     rf           ; start by shifting number to convert
           shl
           plo     rf
           ghi     rf
           shlc
           phi     rf
           shlc                 ; now shift result to bit 3
           shl
           shl
           shl
           str     rd
           pop     rd           ; recover address
           push    rd           ; save address again
           ldi     5            ; 5 cells to process
           plo     re
tobcdlp4:  lda     rd           ; get current cell
           str     r2           ; save it
           ldn     rd           ; get next cell
           shr                  ; shift bit 3 into df
           shr
           shr
           shr
           ldn     r2           ; recover value for current cell
           shlc                 ; shift with new bit
           ani     0fh          ; keep only bottom 4 bits
           dec     rd           ; point back
           str     rd           ; store value
           inc     rd           ; and move to next cell
           dec     re           ; decrement count
           glo     re           ; see if done
           lbnz    tobcdlp4     ; jump if not
           pop     rd           ; recover address
           dec     r9           ; decrement bit count
           glo     r9           ; see if done
           lbnz    tobcdlp2     ; loop until done
           ret                  ; return to caller
; ***************************************************
; ***** Output 16-bit integer                   *****
; ***** RF - 16-bit integer                     *****
; ***************************************************
itoa:      push    rf           ; save consumed registers
           push    r9
           push    r8
           push    r7
           glo     r2           ; make room on stack for buffer
           smi     6
           plo     r2
           ghi     r2
           smbi    0
           phi     r2
           mov     rd,r2        ; RD is output buffer
           inc     rd
           ghi     rf           ; get high byte
           shl                  ; shift bit to DF
           lbdf    itoan        ; negative number
itoa1:     call    tobcd        ; convert to bcd
           mov     rd,r2
           inc     rd
           ldi     5
           plo     r8
           ldi     4            ; max 4 leading zeros
           phi     r8
itoalp1:   lda     rd
           lbz     itoaz        ; check leading zeros
           str     r2           ; save for a moment
           ldi     0            ; signal no more leading zeros
           phi     r8
           ldn     r2           ; recover character
itoa2:     adi     030h
           call    o_type       ; display it
itoa3:     dec     r8
           glo     r8
           lbnz    itoalp1
           glo     r2           ; pop work buffer off stack
           adi     6
           plo     r2
           ghi     r2
           adci    0
           phi     r2
           pop     r7
           pop     r8           ; recover consumed registers
           pop     r9
           pop     rf
           ldi     0            ; terminate string
           str     rb
           ret                  ; return to caller
itoaz:     ghi     r8           ; see if leading have been used up
           lbz     itoa2        ; jump if so
           smi     1            ; decrement count
           phi     r8
           lbr     itoa3        ; and loop for next character
itoan:     ldi     '-'          ; show negative
           call    o_type       ; display it
           glo     rf           ; 2s compliment
           xri     0ffh
           adi     1
           plo     rf
           ghi     rf
           xri     0ffh
           adci    0
           phi     rf
           lbr     itoa1        ; now convert/show number

; **************************************
; ***** Check D for 0-9            *****
; ***** Returns: DF=1 - is number  *****
; *****          DF=0 - not number *****
; **************************************
is_number: plo     re                  ; save original value
           smi     '0'                 ; check for below numerals
           lbnf    not_chr             ; jump if not in range
           smi     10                  ; check high of range
           lbdf    not_chr             ; jump if not in range
           lbr     is_chr              ; otherwise singal in range
not_chr:   ldi     0                   ; signal not in range
           shr
           glo     re                  ; recover original value
           ret                         ; and return
is_chr:    ldi     1                   ; sginal is in range
           shr
           glo     re
           ret         

; **************************************
; ***** Check D for a-z            *****
; ***** Returns: DF=1 - is number  *****
; *****          DF=0 - not number *****
; **************************************
is_lc:     plo     re                  ; save original value
           smi     'a'                 ; check for below range
           lbnf    not_chr             ; jump if not in range
           smi     26                  ; check high of range
           lbdf    not_chr             ; jump if above range
           lbr     is_chr              ; otherwise mark in range

; **************************************
; ***** Check D for A-Z            *****
; ***** Returns: DF=1 - is number  *****
; *****          DF=0 - not number *****
; **************************************
is_uc:     plo     re                  ; save original value
           smi     'A'                 ; check for below range
           lbnf    not_chr             ; jump if not in range
           smi     26                  ; check high of range
           lbdf    not_chr             ; jump if above range
           lbr     is_chr              ; otherwise mark in range



errmsg:    db      'File not found',10,13,0
fildes:    db      0,0,0,0
           dw      dta
           db      0,0
           db      0
           db      0,0,0,0
           dw      0,0
           db      0,0,0,0

endrom:    equ     $

buffer:    ds      130
cbuffer:   ds      80
dta:       ds      512
macros:    ds      52
variables: ds      1024
program:   dw      1

