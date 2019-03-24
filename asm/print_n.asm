.data
n  NUM  23

.code

; Substraction
; Arguments:
;   E: Argument a (n)
;   F: Argument b (m to substract)
; Other registers:
;   G: Temporal register
;   H: Accumulator
; Returns: H
_sub:   PUSH G
        SET  H, -1
 sub:   ADD  H,  H,  1    ; acc++
        ADD  G,  H,  F    ; temp = acc + m

        EQ   G,  G,  E    ; if temp == n
        JF   G,  sub      ;   Return
        POP  G            ;
        RET               ;

; Output the ASCII value of a number
; Arguments:
;   H: Number
out_ascii: PUSH H
           ADD  H, H, 48
           OUT  H
           POP  H
           RET

; Find the (len-i) digit of a number 
; Arguments:
;   E: n
;   F: (len-i-1)
; Other registers:
;   G: Temporal register
;   H: Accumulator
; Returns: H
_find_n: PUSH D
         PUSH E
         PUSH F
         PUSH G
         SET  H, 0       ; acc   = -1
         MULT D, F, 10
 find_n: ADD  H, H, 1     ; acc++
         MULT G, H, F     ; temp  = acc*i
         ADD  G, G, E     ; temp += n 
         MOD  G, G, D     ; temp %= i

         JT   G, find_n   ; If temp == 0
         SET  E, 10       ;   return sub(10, acc)
         SET  F, H        ;
         CALL _sub        ;
         POP  G           ;
         POP  F           ;
         POP  E           ;
         POP  D           ;
         RET              ;

main:    SET  A, n         ; n
         SET  B, 0         ; i
         SET  C, 1         ; mult
  
loop:    SET  E, A         ; find_n(A, C)
         SET  F, C         ;
         CALL _find_n      ;
         PUSH H            ; Push result to the stack

         SET  E, A         ; A = sub(A, H)
         MULT F, C, H
         CALL _sub         ;
         SET  A, H         ;

         ADD  B, B, 1      ; B++
         JF   A, print_n   ; if a == 0
                           ;   print numbers
         MULT C, C, 10     ; else
         JMP  loop         ;   C *= 10

print_n:
         POP  H
         CALL out_ascii
         ADD  B, B, -1
         
         JT   B, print_n
         HALT

