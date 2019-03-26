.data
input  NUM  30000
times  NUM  1000

.code
; Factorial
; Arguments:
;   E: Input
; Registers:
;   H: Accumulator
;   B: input-n
_sum:   PUSH  B
        SET   H, 0
        SET   B, E
        
 sum:   ADD   H, H, B
        ADD   B, B, -1
        JT    B, sum
        POP   B
        RET

main:   SET   E, input
        CALL  _sum
        
        ADD   A, A, 1
        EQ    C, A, times
        JF    C, main
        HALT

