
.data
limit  NUM  100

.code
main:
; A: Leading digit
; B: Second digit
; H: Temporal reg

init:       SET  A, 1  ; A = 1
            SET  A, 1  ; B = 0

loop:       ADD  H, A, B       ; temp = A + B
            SET  B, A          ; B = A
            SET  A, H          ; A = temp

            GT   H, A, limit   ; If A > limit, end
            JT   H, end

; Initialize registers for to_digits
; C: Temporal A register
; D: Number of digits stored in the stack
; H: Temporal register

_to_digits:
            SET  C, A
            SET  D, 0

;
; Split number into individual digits:
;

to_digits:  MOD  H, C, 10        ; Push last digit
            PUSH H
            ADD  D, D, 1         ; D++

find_div:   SET  E, 0            ; Set E as divisor
            JMP find_mult

div_found:  SET  C, E
            GT   H, 100, C       ; If C < 100 go print
            JT   H, last_two
            JMP  to_digits

find_mult:  ADD  E, E, 1         ; E++
            MULT H, E, 10        ; H = E*10

            EQ   H, H, C         ; if H == C (original number)
            JF   H, div_found    ; Go back
            JMP  find_mult       ; Else loop

last_two:
; 2345
; 2345 % 10 = 5:

print_d:    POP   H             ; Pop and print D numbers from the stack
            ADD   H, H, 48      ; Convert to ascii
            OUT   H

            ADD   D, D, -1      ; If it has printed all numbers, go back to main loop
            JF    D, loop       ;
            JMP   print_d
            JMP loop
       
end:        HALT


