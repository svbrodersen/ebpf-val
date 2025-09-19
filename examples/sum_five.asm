mov r0, 0
mov r3, 0
mov r1, 5
mov r2, 2
stb [r1], 10
ja +5
mov r4, r1
add r4, r3
add r3, 1
ldxb r5, [r4]
add r0, r5
jlt r3, r2, +-6
exit
