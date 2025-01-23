.global _start

_start:
    li a1, 9                 # Load multiplier value
    li a3, 7                 # Load multiplicand value

    mul t0, a1, a3           # Perform multiplication (t0 = a1 * a3)

loop:
    j loop                    # Infinite loop to halt execution
