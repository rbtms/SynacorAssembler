.data
.code
main:

; Input
; Registers:
;   A: Input len
;   B: Memory pointer
;   G: Temporal register
;   H: Temporal register
loop:     IN   H
          SET  G, H

          EQ   G, G, 10    ; if input == newline
          JT   G, _reverse ;  jump to end

          PUSH H           ; else
          ADD  A, A, 1     ;   push H
          JMP  loop        ;   len++

_reverse: SET  B, 0xFFF    ; mem_ptr = 4095
          SET  G, A        ; Temporal len backup
 reverse: POP  H
          ADD  G, G, -1    ; len--

          WMEM B, H        ; Write H into memory
          ADD  B, B,  1    ; mem_ptr++

          JT   G, reverse  ; Continue while there are elements on the stack

_out_mem: ADD B, B, -1
 out_mem: RMEM H, B        ; Output memory address
          OUT  H           ;

          ADD  B, B, -1    ; mem_ptr--
          ADD  A, A, -1    ; len--
          JT   A, out_mem
          HALT

