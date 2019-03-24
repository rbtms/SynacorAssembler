.data
.code
main:
init:       SET   A,  1
oscilator:  JT    A,  to_zero
            SET   A,  1
            CALL  print_n
            JMP   oscilator
to_zero:    SET   A,  0
            CALL  print_n
            JMP   oscilator
print_n:    ADD   B,  A, 48
            OUT   B
            RET

