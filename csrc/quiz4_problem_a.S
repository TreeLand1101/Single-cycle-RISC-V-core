.global _start

_start:
    li a0, 13             # Load the number (13) into register a0

    # Square the number directly in a0
    mul a0, a0, a0        # a0 = a0 * a0 (square the number using M extension)

end:
    nop                    # Placeholder for any further operations

