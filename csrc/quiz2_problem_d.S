.global _start
_start:
    li t0, 13             # Load the first number (num1) into t0
    li t1, 7              # Load the second number (num2) into t1
    li t2, 0              # Initialize the result (t2) to 0

loop:
    andi t3, t0, 1        # Check if the least significant bit of t0 is 1 (i.e., if t0 is odd)
    beq t3, x0, skip_add  # If t0 is even, skip the addition step

    # If t0 is odd, add t1 (num2) to the current result in t2
    add t2, t2, t1        # Accumulate the value of t1 in t2

skip_add:
    li t3, 2              # Load immediate value 2 into t3
    mul t1, t1, t3        # Multiply t1 by 2 (t1 = t1 * 2)
    div t0, t0, t3        # Divide t0 by 2 (t0 = t0 / 2)

    bnez t0, loop         # If t0 is not zero, continue the loop

end:
    nop                   # Placeholder for any further operations

