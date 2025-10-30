.data
    "Hola Mundo": .word 0
    3: .word 0
    4: .word 0
    contador: .word 0
    mensaje: .word 0
    resultado: .word 0
    t1: .word 0
    t2: .word 0
    t3: .word 0
    newline: .asciiz "\n"

.text
.globl main
main:

# Cuádruplo: (+, 3, 4, t1)
    lw $t0, 3        # Carga '3' en $t0
    lw $t1, 4        # Carga '4' en $t1
    add $t2, $t0, $t1   # $t2 = $t0 + $t1
    sw $t2, t1         # Guarda el resultado en la variable temporal 't1'

# Fin del programa
    li $v0, 10          # Servicio 10 (exit)
    syscall

# Cuádruplo: (/, t1, 3, t2)
    lw $t0, t1        # Carga 't1' en $t0
    lw $t1, 3        # Carga '3' en $t1
    div $t2, $t0, $t1   # $t2 = $t0 / $t1
    sw $t2, t2         # Guarda el resultado en la variable temporal 't2'

# Fin del programa
    li $v0, 10          # Servicio 10 (exit)
    syscall

# Cuádruplo: (:=, t2, None, contador)

# Fin del programa
    li $v0, 10          # Servicio 10 (exit)
    syscall

# Cuádruplo: (:=, "Hola Mundo", None, mensaje)

# Fin del programa
    li $v0, 10          # Servicio 10 (exit)
    syscall

# Cuádruplo: (IMPCAD, mensaje, None, None)

# Fin del programa
    li $v0, 10          # Servicio 10 (exit)
    syscall

# Cuádruplo: (LEERDIG, None, None, resultado)

# Fin del programa
    li $v0, 10          # Servicio 10 (exit)
    syscall

# Cuádruplo: (+, resultado, contador, t3)
    lw $t0, resultado        # Carga 'resultado' en $t0
    lw $t1, contador        # Carga 'contador' en $t1
    add $t2, $t0, $t1   # $t2 = $t0 + $t1
    sw $t2, t3         # Guarda el resultado en la variable temporal 't3'

# Fin del programa
    li $v0, 10          # Servicio 10 (exit)
    syscall

# Cuádruplo: (IMPDIG, t3, None, None)
    lw $a0, t3        # Carga el valor a imprimir en el registro de argumento $a0
    li $v0, 1           # Carga el servicio de sistema 1 (print_int)
    syscall             # Ejecuta la llamada al sistema
    li $v0, 4           # Servicio 4 (print_string)
    la $a0, newline     # Carga la dirección de 'newline'
    syscall

# Fin del programa
    li $v0, 10          # Servicio 10 (exit)
    syscall