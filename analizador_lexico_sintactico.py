import enum
import collections
import re
from ply import yacc

# --- 1. Definición de Token (NamedTuple) ---
Token = collections.namedtuple('Token', ['tipo', 'valor', 'linea', 'col'])

# --- 2. Definición de Tipos de Token (Enum Completo) ---
class TipoToken(enum.Enum):

    # Palabras Reservadas
    PROG = 'PROG'         
    DECL = 'DECL'         # decl
    INICIO = 'INICIO'     # inicio
    FIN = 'FIN'           # fin, end
    IMPDIG = 'IMPDIG'     # impdig
    IMPCAD = 'IMPCAD'     # impcad
    LEERDIG = 'LEERDIG'   # leerdig
    LEERCAD = 'LEERCAD'   # leercad 
    
    
    
    # Operadores y Puntuación

    PC = 'PC'             # ;
    ASIG = 'ASIG'         # :=
    MAS = 'MAS'           # +
    MENOS = 'MENOS'       # -
    DIV = 'DIV'           # /
    MUL = 'MUL'           # *
    PAREN = 'PAREN'       # (
    TESIS = 'TESIS'       # )
    IGUAL = 'IGUAL'       # =
    COMA = 'COMA'         # ,

    # Literales e Identificadores
    TYPE = 'TYPE'         # Para int, cad, booleano
    ID = 'ID'
    CINT = 'CINT'         # Constante Entera
    CADENA_LITERAL = 'CADENA_LITERAL'
    BOOL= 'BOOL'

    # Especiales y Errores
    ERROR = 'ERROR'
    IDERR = 'IDERR'       # Identificador con error (con @)
    # Categorías Internas para Ignorar
    COMENTARIO = 'COMENTARIO'
    ESPACIO = 'ESPACIO'
    SALTO_LINEA = 'SALTO_LINEA'

# --- 3. Diccionario de Palabras Reservadas ---

palabrasReservadas = {
    'pf2025': TipoToken.PROG,       
    'decl': TipoToken.DECL,
    'inicio': TipoToken.INICIO,
    'fin': TipoToken.FIN,
    'impcad': TipoToken.IMPCAD,
    'impdig': TipoToken.IMPDIG,
    'leerdig': TipoToken.LEERDIG,
    'leercad': TipoToken.LEERCAD,     
    'int': TipoToken.TYPE,
    'cad': TipoToken.TYPE,          
    'booleano': TipoToken.TYPE,     
   
}
aritmeticos={
    '+': TipoToken.MAS,
    '-': TipoToken.MENOS,
    '*': TipoToken.MUL,
    '/': TipoToken.DIV,
}
agrupacion={
    '(': TipoToken.PAREN,
    ')': TipoToken.TESIS,
}

simbolos={
    ':=': TipoToken.ASIG,
    '=': TipoToken.IGUAL,
    ';': TipoToken.PC,
    ',': TipoToken.COMA,
}

identificadores={
    'id': TipoToken.ID,
}



# --- 4. Lista de Patrones Regex ---
patrones_lexer = [
    # Ignorar (primero):
    r'(?P<COMENTARIO>#.*)',         # Comentarios estilo '#'
    r'(?P<SALTO_LINEA>\n)',         # Salto de línea
    r'(?P<ESPACIO>[ \t]+)',          # Espacios y Tabs

    # Tokens Significativos:
    r'(?P<ASIG>:=)',                # Asignación :=
    r'(?P<MAS>\+)',                 # Operadores (+ escapado)
    r'(?P<MENOS>-)',                # operadores (- escapado)
    r'(?P<MUL>\*)',                 # Operadores (* escapado)
    r'(?P<DIV>/)',                  # Operadores (/ escapado)
    r'(?P<IGUAL>=)',                # Operador =

    r'(?P<PAREN>\()',               # Puntuación ( ( escapado)
    r'(?P<TESIS>\))',               # Puntuación ) ( escapado)
    r'(?P<PC>;)',                   # Puntuación ;
    r'(?P<COMA>,)',                 # Puntuación ,
    r'(?P<CADENA_LITERAL>"[^"]*")', # Literal de Cadena (simple)
    r'(?P<CINT>\d+)',                # Constante Entera

    r'(?P<IDERR>[a-zA-Z0-9]*[^a-zA-Z0-9,;# \n]+[a-zA-Z0-9]*[^a-zA-Z0-9,;# \n]*)', # Identificador con error

    r'(?P<ID>[a-zA-Z][a-zA-Z0-9]*)', # Identificador 

    # Error (al final):
    r'(?P<ERROR>.)',                # Cualquier otro carácter
]

# Cambiar la generación de tokens para almacenarlos en una lista
class AnalizadorLexico:
    def __init__(self, texto_fuente, patrones_tokens, palabras_reservadas):
        self.texto_fuente = texto_fuente
        self.expresion_regular = re.compile('|'.join(patrones_tokens))
        self.palabras_reservadas = palabras_reservadas
        self.posicion_actual = 0
        self.numero_linea = 1
        self.inicio_linea = 0

    def generar_tokens(self):
        tokens = []  
        while self.posicion_actual < len(self.texto_fuente):
            coincidencia = self.expresion_regular.match(self.texto_fuente, self.posicion_actual)
            if coincidencia:
                nombre_tipo = coincidencia.lastgroup
                valor = coincidencia.group()
                columna = coincidencia.start() - self.inicio_linea + 1
                try:
                    tipo_token = TipoToken[nombre_tipo]
                except KeyError:
                    raise ValueError(f"Nombre de grupo desconocido: {nombre_tipo}")
 
                if tipo_token == TipoToken.SALTO_LINEA:
                    self.numero_linea += 1
                    self.inicio_linea = self.posicion_actual + len(valor)
                elif tipo_token in (TipoToken.ESPACIO, TipoToken.COMENTARIO):
                    pass
                

                elif tipo_token == TipoToken.ID:
                    tipo_token = self.palabras_reservadas.get(valor, TipoToken.ID)
                    tokens.append(Token(tipo_token, valor, self.numero_linea, columna))

                elif tipo_token == TipoToken.IDERR:
                    tokens.append(Token(tipo_token, valor, self.numero_linea, columna))

                elif tipo_token == TipoToken.ERROR:
                    tokens.append(Token(tipo_token, valor, self.numero_linea, columna))
                    
                else:
                    tokens.append(Token(tipo_token, valor, self.numero_linea, columna))

                self.posicion_actual = coincidencia.end()


        return tokens

##Analizador Sintactico 

# --- Configuración para PLY ---
tokens = [token.name for token in TipoToken if token.name not in ['COMENTARIO', 'ESPACIO', 'SALTO_LINEA']]

# Precedencia de operadores
precedence = (
    ('rigth', 'ASIG'),
    ('left', 'MAS', 'MENOS'),
    ('left', 'MUL', 'DIV'),
    ('left', 'PAREN', 'TESIS'),
)

# --- Gramática ---
def p_programa(p):
    'programa : PROG ID PC DECL declaraciones INICIO sentencias FIN'
    hijos = [
        ('PROG', p[1]), ('ID', p[2]), ('PC', p[3]), ('DECL', p[4]),
        p[5],  # nodo de declaraciones
        ('INICIO', p[6]),
        p[7],  # nodo de sentencias
        ('FIN', p[8])
    ]
    p[0] = ('programa', hijos)
    print("Sintaxis válida!")

def p_declaraciones(p):
    '''declaraciones : declaracion declaraciones
                     | empty'''
    rule_name = p.slice[0].type # 'declaraciones'
    if len(p) == 3:  # declaracion declaraciones
        p[0] = (rule_name, [p[1], p[2]])
    else:  # empty (len(p) == 2, p[1] es el resultado de p_empty)
        p[0] = (rule_name, [p[1]])

def p_declaracion(p):
    '''declaracion : TYPE ID decVarias PC'''
    hijos = [('TYPE', p[1]), ('ID', p[2]), p[3], ('PC', p[4])] # p[3] es el nodo de decVarias
    p[0] = ('declaracion', hijos)

def p_decVarias(p):
    '''decVarias : COMA ID decVarias
            | empty'''
    rule_name = p.slice[0].type # 'decVarias'
    if len(p) == 4:  # COMA ID decVarias
        hijos = [('COMA', p[1]), ('ID', p[2]), p[3]] # p[3] es el nodo recursivo
        p[0] = (rule_name, hijos)
    else:  # empty
        p[0] = (rule_name, [p[1]]) # p[1] es el resultado de p_empty

def p_sentencias(p):
    '''sentencias : sentencia sentencias
                  | empty'''
    rule_name = p.slice[0].type # 'sentencias'
    if len(p) == 3:  # sentencia sentencias
        p[0] = (rule_name, [p[1], p[2]])
    else:  # empty
        p[0] = (rule_name, [p[1]]) # p[1] es el resultado de p_empty

def p_sentencia(p):
    '''sentencia : impdig_sent
                 | impcad_sent
                 | asignacion
                 | lectura
                 | declaracion'''
    # p[1] ya es un nodo completo de la sub-regla (e.g., ('impdig_sent', [...]))
    p[0] = ('sentencia', [p[1]])

def p_impdig_sent(p):
    '''impdig_sent : IMPDIG expresion PC'''
    hijos = [('IMPDIG', p[1]), p[2], ('PC', p[3])] # p[2] es el nodo de expresion
    p[0] = ('impdig_sent', hijos)

def p_impcad_sent(p):
    '''impcad_sent : IMPCAD CADENA_LITERAL PC
                   | IMPCAD ID PC'''
    rule_name_lhs = p.slice[0].type # 'impcad_sent'
    keyword_token = p.slice[1].type # IMPCAD
    third_token_type = p.slice[2].type # CADENA_LITERAL o ID

    if third_token_type == 'CADENA_LITERAL':
        hijos = [(keyword_token, p[1]), ('CADENA_LITERAL', p[2]), ('PC', p[3])]
        p[0] = (rule_name_lhs + "_literal", hijos)
    else:  # ID
        hijos = [(keyword_token, p[1]), ('ID', p[2]), ('PC', p[3])]
        p[0] = (rule_name_lhs + "_id", hijos)

def p_asignacion(p):
    'asignacion : ID ASIG expresion PC'
    hijos = [('ID', p[1]), ('ASIG', p[2]), p[3], ('PC', p[4])] # p[3] es el nodo de expresion
    p[0] = ('asignacion', hijos)

def p_lectura(p):
    '''lectura : LEERDIG ID PC
               | LEERCAD ID PC'''
    rule_name_lhs = p.slice[0].type # 'lectura'
    keyword_token_type = p.slice[1].type # LEERDIG o LEERCAD
    hijos = [(keyword_token_type, p[1]), ('ID', p[2]), ('PC', p[3])]
    p[0] = (f"{rule_name_lhs}_{keyword_token_type.lower()}", hijos)
                

def p_expresion(p):
    '''expresion : expresion MAS expresion
                 | expresion MENOS expresion
                 | expresion MUL expresion
                 | expresion DIV expresion
                 | PAREN expresion TESIS
                 | CADENA_LITERAL
                 | CINT
                 | ID'''
    rule_name_lhs = p.slice[0].type # 'expresion'
    if len(p) == 2:  # Producciones terminales: CADENA_LITERAL, CINT, ID
        terminal_token_info = p.slice[1]
        p[0] = (rule_name_lhs, [(terminal_token_info.type, p[1])])
    elif p.slice[1].type == 'PAREN':  # PAREN expresion TESIS
        # p[1] es '(', p[2] es nodo expresion, p[3] es ')'
        hijos = [('PAREN', p[1]), p[2], ('TESIS', p[3])]
        p[0] = (rule_name_lhs, hijos)
    else:  # expresion OP expresion
        # p[1] es nodo expr izq, p[2] es valor de OP, p[3] es nodo expr der
        op_token_info = p.slice[2] # LexToken para MAS, MENOS, etc.
        hijos = [p[1], (op_token_info.type, p[2]), p[3]]
        p[0] = (rule_name_lhs, hijos)

def p_empty(p):
    'empty :'
    p[0] = ('empty_production', [])

# --- Manejo de errores ---
def p_error(p):
    if p:
        print(f"Error de sintaxis en línea {p.lineno}, token '{p.value}'")
    else:
        print("Error de sintaxis al final del archivo")

# --- Integración con el analizador léxico ---
class PlyLexerWrapper:
    def __init__(self, texto):
        self.lexer = AnalizadorLexico(texto, patrones_lexer, palabrasReservadas)
        self.tokens_generados = self.lexer.generar_tokens()
        self.pos = 0

    def token(self):
        if self.pos < len(self.tokens_generados):
            token = self.tokens_generados[self.pos]
            self.pos += 1
            return self.adaptar_token(token)
        return None

    def adaptar_token(self, token):
        return type(
            'LexToken',
            (object,),
            {
                'type': token.tipo.name,
                'value': token.valor,
                'lineno': token.linea,
                'lexpos': token.col
            }
        )

# --- Construir el parser ---
parser = yacc.yacc()


# --- Función para imprimir el árbol ---
def print_parse_tree(node, prefix="", is_last=True):
    """
    Imprime el árbol de análisis sintáctico con un formato visual de árbol.
    node: La tupla del nodo actual ('nombre_nodo', contenido).
          El contenido es una lista de hijos para no terminales, o el valor para terminales.
    prefix: El prefijo de caracteres de línea (e.g., "│   ", "    ") para la indentación.
    is_last: Booleano que indica si este nodo es el último entre sus hermanos.
    """
    if not isinstance(node, tuple) or len(node) != 2:
        # Manejo de nodos que no siguen el formato esperado (nombre, contenido)
        # Podría ser un token simple si la gramática lo produce directamente en una lista de hijos.
        if isinstance(node, str): # Caso simple de un token como string (si ocurre)
             print(f"{prefix}{'└── ' if is_last else '├── '}{node}")
        else:
             print(f"{prefix}{'└── ' if is_last else '├── '}Nodo malformado: {node}")
        return

    node_name, content = node
    
    # Determina el conector para el nodo actual
    connector = "└── " if is_last else "├── "
    
    # Imprime el nombre del nodo actual
    print(f"{prefix}{connector}{node_name}", end="")

    if isinstance(content, list):  # Es un nodo de regla (no terminal) o producción vacía
        print()  # Nueva línea después del nombre de la regla
        # Prepara el prefijo para los hijos de este nodo
        child_prefix = prefix + ("    " if is_last else "│   ")
        num_hijos = len(content)
        for i, child_node in enumerate(content):
            print_parse_tree(child_node, child_prefix, i == num_hijos - 1)
    else:  # Es un nodo terminal (ej: ('ID', 'variable'))
        # Imprime el valor del token en la misma línea
        print(f" : {repr(content)}")

def extraer_simbolos_con_tipos(parse_tree_root):
    simbolos = {}  # {identificador: tipo}
    def recorrer_declaraciones(nodo):
        if not isinstance(nodo, tuple):
            return
        nombre, hijos = nodo
        if nombre == 'declaracion':
            tipo = hijos[0][1]  # 'int', 'cad', 'booleano'
            id_principal = hijos[1][1]
            #if id_principal in simbolos:
                #print(f"Advertencia: Variable '{id_principal}' redeclarada.")
            simbolos[id_principal] = tipo
            # Buscar variables adicionales en decVarias
            dec_varias = hijos[2]
            def extraer_ids_decvarias(nodo):
                if not isinstance(nodo, tuple):
                    return []
                nombre, hijos = nodo
                if nombre == 'decVarias':
                    if len(hijos) == 3:
                        return [hijos[1][1]] + extraer_ids_decvarias(hijos[2])
                    elif len(hijos) == 1:
                        return []
                return []
            for id_extra in extraer_ids_decvarias(dec_varias):
                #if id_extra in simbolos:
                    #print(f"Advertencia: Variable '{id_extra}' redeclarada.")
                simbolos[id_extra] = tipo
        # Recorrer hijos
        if isinstance(hijos, list):
            for h in hijos:
                recorrer_declaraciones(h)
        elif isinstance(hijos, tuple):
            recorrer_declaraciones(hijos)
    # Buscar nodo de declaraciones en el árbol
    declaraciones = None
    for hijo in parse_tree_root[1]:
        if isinstance(hijo, tuple) and hijo[0] == 'declaraciones':
            declaraciones = hijo
            break
    if declaraciones:
        for nodo in declaraciones[1]:
            recorrer_declaraciones(nodo)
    return simbolos

# --- Ejecución del Analizador Léxico y Sintáctico ---
if __name__ == "__main__":
    archivo_fuente = "codigo_fuente.txt"  # Nombre del archivo de entrada
    try:
        with open(archivo_fuente, "r", encoding="utf-8") as archivo:
            codigo_ejemplo = archivo.read()
            
            # Crear el lexer adaptado
            lexer_wrapper = PlyLexerWrapper(codigo_ejemplo)
            
            # Ejecutar el parser
            parse_tree_root = parser.parse(lexer=lexer_wrapper)

            if parse_tree_root:
                print("Analisis sintactico correcto")
                # print("\n--- Árbol de Análisis Sintáctico ---")
                # # Llamada inicial a la función de impresión del árbol
                # print_parse_tree(parse_tree_root, prefix="", is_last=True)
            else:
                print("\nNo se pudo generar el árbol de análisis (error de sintaxis o entrada vacía).")

    except FileNotFoundError:
        print(f"Error: No se encontró el archivo {archivo_fuente}")
        exit(1)

    analizador = AnalizadorLexico(codigo_ejemplo, patrones_lexer, palabrasReservadas)

    try:
        tokens = analizador.generar_tokens()
        # for token in tokens:
        #     print(token)
        archivo_salida_tok = archivo_fuente.replace(".txt", ".tok")
        with open(archivo_salida_tok, "w", encoding="utf-8") as archivo_salida:
            archivo_salida.write(f"{'Tipo de Token':<20}{'Valor':<20} {'Linea':<10} {'Columna':<10}\n")
            archivo_salida.write("-" * 100 + "\n")
            for token in tokens:

                archivo_salida.write(f"{token.tipo.value:<20} {token.valor:<20} Linea:{token.linea:<10} Columna:{token.col:<10}\n")
        
        archivo_salida_dep = archivo_fuente.replace(".txt", ".dep")
        with open(archivo_salida_dep, "w", encoding="utf-8") as archivo_salida2:
            for token in tokens:
                archivo_salida2.write(f"{token.valor}")

        
        
        # --- Generación de Tabla de Símbolos ---
        tabla_simbolos = []
        simbolos_vistos = set() # Para evitar duplicados
        simbolo_id_actual = 1

        for token in tokens:
            # Incluir todos los tipos de token (excepto EOF y los ignorados si se generaran)
            # y evitar duplicados basados en el lexema.
            #if token.tipo != token.valor not in simbolos_vistos:
            if token.valor not in simbolos_vistos:
                 # No incluir tokens vacíos si los hubiera (EOF tiene valor vacío)
                if token.valor:
                    tabla_simbolos.append((simbolo_id_actual, token.valor, token.tipo.value))
                    simbolos_vistos.add(token.valor)
                    simbolo_id_actual += 1

        # --- Extraer tipos desde el arbol sintactico  ---
        tipos_identificadores = {}
        if parse_tree_root:
            tipos_identificadores = extraer_simbolos_con_tipos(parse_tree_root)

        # --- Guardar la tabla de símbolos en un archivo .tab
        archivo_salida_tab = archivo_fuente.replace(".txt", ".tab") 
        with open(archivo_salida_tab, "w", encoding="utf-8") as archivo_tab:
            archivo_tab.write(f"{'ID':<5} {'Lexema':<15} {'Token':<15}{'Tipo en Diccionario':<15} {'Tipo Declarado'}\n")
            archivo_tab.write("-" * 55 + "\n")
            for id_simbolo, lexema,tipo_token in tabla_simbolos:
                tipo_decl = tipos_identificadores.get(lexema, "")
                if lexema in palabrasReservadas:
                    archivo_tab.write(f"{id_simbolo:<5} {lexema:<15} {tipo_token:<15} Palabra reservada\n")
                elif lexema in aritmeticos:
                    archivo_tab.write(f"{id_simbolo:<5} {lexema:<15} {tipo_token:<15} Operador aritmético\n")
                elif lexema in agrupacion:
                    archivo_tab.write(f"{id_simbolo:<5} {lexema:<15} {tipo_token:<15} Agrupación\n")
                elif tipo_token == 'ID':
                    archivo_tab.write(f"{id_simbolo:<5} {lexema:<15} {tipo_token:<15} Identificador{'':<8}{tipo_decl}\n")

        def obtener_tipo_expresion(nodo, simbolos, errores):
            if not isinstance(nodo, tuple):
                return None
            nombre, hijos = nodo
            if nombre == 'expresion':
                if len(hijos) == 1:
                    tipo, valor = hijos[0]
                    if tipo == 'CINT':
                        return 'int'
                    elif tipo == 'CADENA_LITERAL':
                        return 'cad'
                    elif tipo == 'ID':
                        tipo_id = simbolos.get(valor, None)
                        if tipo_id is None:
                            errores.append(f"Variable '{valor}' no declarada.")
                        return tipo_id
                elif len(hijos) == 3:
                    # Manejo de paréntesis: ('PAREN', '('), expresion, ('TESIS', ')')
                    if hijos[0][0] == 'PAREN' and hijos[2][0] == 'TESIS':
                        return obtener_tipo_expresion(hijos[1], simbolos, errores)
                    # Operación binaria
                    tipo_izq = obtener_tipo_expresion(hijos[0], simbolos, errores)
                    op = hijos[1][0]
                    tipo_der = obtener_tipo_expresion(hijos[2], simbolos, errores)
                    if op in ['MAS', 'MENOS', 'MUL', 'DIV']:
                        if tipo_izq != tipo_der:
                            errores.append(f"Operación '{op}' entre tipos incompatibles: '{tipo_izq}' y '{tipo_der}'.")
                            return None
                        if tipo_izq != 'int':
                            errores.append(f"Operación aritmética '{op}' solo permitida entre enteros, no '{tipo_izq}'.")
                            return None
                        return tipo_izq
                    return tipo_izq
            elif nombre == 'sentencia' and isinstance(hijos, list):
                return obtener_tipo_expresion(hijos[0], simbolos, errores)
            return None

        def validar_tipos_en_arbol(nodo, simbolos, errores, en_sentencias=False, declarados=None):
            if declarados is None:
                declarados = set()
            if not isinstance(nodo, tuple):
                return
            nombre, hijos = nodo
            # Detecta declaración en cualquier parte
            if nombre == 'declaracion':
                tipo = hijos[0][1]
                id_principal = hijos[1][1]
                # Duplicados
                if id_principal in declarados:
                    errores.append(f"Variable '{id_principal}' redeclarada.")
                declarados.add(id_principal)
                # Declaración fuera de zona de declaraciones
                if en_sentencias:
                    errores.append(f"Declaración de variable '{id_principal}' de tipo '{tipo}' fuera de la zona de declaraciones.")
                # Variables adicionales
                dec_varias = hijos[2]
                def extraer_ids_decvarias(nodo):
                    if not isinstance(nodo, tuple):
                        return []
                    nombre, hijos = nodo
                    if nombre == 'decVarias':
                        if len(hijos) == 3:
                            return [hijos[1][1]] + extraer_ids_decvarias(hijos[2])
                        elif len(hijos) == 1:
                            return []
                    return []
                for id_extra in extraer_ids_decvarias(dec_varias):
                    if id_extra in declarados:
                        errores.append(f"Variable '{id_extra}' redeclarada.")
                    declarados.add(id_extra)
                    if en_sentencias:
                        errores.append(f"Declaración de variable '{id_extra}' de tipo '{tipo}' fuera de la zona de declaraciones.")
            if nombre == 'asignacion':
                id_var = hijos[0][1]
                tipo_var = simbolos.get(id_var, None)
                tipo_expr = obtener_tipo_expresion(hijos[2], simbolos, errores)
                if tipo_var is None:
                    errores.append(f"Variable '{id_var}' no declarada.")
                elif tipo_expr is None:
                    errores.append(f"No se pudo determinar el tipo de la expresión para '{id_var}'.")
                elif tipo_var != tipo_expr:
                    errores.append(f"Tipo incompatible en asignación a '{id_var}': se esperaba '{tipo_var}', se obtuvo '{tipo_expr}'.")
            # Recorrer hijos
            if isinstance(hijos, list):
                for h in hijos:
                    # Si estamos en sentencias, pasa el flag en_sentencias=True
                    validar_tipos_en_arbol(h, simbolos, errores, en_sentencias=(nombre == 'sentencias' or en_sentencias), declarados=declarados)
            elif isinstance(hijos, tuple):
                validar_tipos_en_arbol(hijos, simbolos, errores, en_sentencias=en_sentencias, declarados=declarados)

        errores_semanticos = []
        # ...existing code...
        #if parse_tree_root:
        #    simbolos_tabla = extraer_simbolos_con_tipos(parse_tree_root)
        #    print("Diccionario de tipos extraído:", simbolos_tabla)  # <-- Agrega esto
        #    validar_tipos_en_arbol(parse_tree_root, simbolos_tabla, errores_semanticos)
        # ...existing code...


        # --- Análisis semántico con tipos y coherencia de operaciones ---
        
        if parse_tree_root:
            simbolos_tabla = extraer_simbolos_con_tipos(parse_tree_root)
            validar_tipos_en_arbol(parse_tree_root, simbolos_tabla, errores_semanticos)
            if errores_semanticos:
                print("\nErrores semánticos encontrados:")
                for err in errores_semanticos:
                    print(" -", err)
            else:
                print("\nAnálisis semántico correcto.")

        #Generación de cuádruplos
        def generar_cuadruplos(nodo, temporales=None, cuadruplos=None):
            if temporales is None:
                temporales = []
            if cuadruplos is None:
                cuadruplos = []
            if not isinstance(nodo, tuple):
                return None
            nombre, hijos = nodo

            if nombre == 'asignacion':
                id_var = hijos[0][1]
                expr = hijos[2]
                temp = generar_cuadruplos(expr, temporales, cuadruplos)
                cuadruplos.append((':=', temp, None, id_var))
                return id_var

            elif nombre == 'expresion':
                if len(hijos) == 1:
                    tipo, valor = hijos[0]
                    return valor
                elif len(hijos) == 3:
                    if hijos[0][0] == 'PAREN' and hijos[2][0] == 'TESIS':
                        return generar_cuadruplos(hijos[1], temporales, cuadruplos)
                    izq = generar_cuadruplos(hijos[0], temporales, cuadruplos)
                    op = hijos[1][1]
                    der = generar_cuadruplos(hijos[2], temporales, cuadruplos)
                    temp = f"t{len(temporales)+1}"
                    temporales.append(temp)
                    cuadruplos.append((op, izq, der, temp))
                    return temp

            elif nombre == 'sentencia' and isinstance(hijos, list):
                for h in hijos:
                    generar_cuadruplos(h, temporales, cuadruplos)

            elif nombre == 'sentencias' and isinstance(hijos, list):
                for h in hijos:
                    generar_cuadruplos(h, temporales, cuadruplos)

            # impcad con ID
            elif nombre == 'impcad_sent_id':
                id_var = hijos[1][1]
                cuadruplos.append(('IMPCAD', id_var, None, None))

            # impcad con literal
            elif nombre == 'impcad_sent_literal':
                literal = hijos[1][1]
                cuadruplos.append(('IMPCAD', literal, None, None))

            # impdig
            elif nombre == 'impdig_sent':
                expr = hijos[1]
                temp = generar_cuadruplos(expr, temporales, cuadruplos)
                cuadruplos.append(('IMPDIG', temp, None, None))

            # leerdig
            elif nombre == 'lectura_leerdig':
                id_var = hijos[1][1]
                cuadruplos.append(('LEERDIG', None, None, id_var))

            # leercad
            elif nombre == 'lectura_leercad':
                id_var = hijos[1][1]
                cuadruplos.append(('LEERCAD', None, None, id_var))

            return None
        
        #Generacion de codigo ensamblador
        def generarCodigoMips(cuadrupos):
            ensamblador = []

            #Seccion de datos
            ensamblador.append(".data")
            variables = set()
            for _, arg1, arg2, res in cuadruplos:
                if isinstance(arg1, str): variables.add(arg1)
                if isinstance(arg2, str): variables.add(arg2)
                if isinstance(res, str): variables.add(res)
            
            for var in sorted(list(variables)):
                ensamblador.append(f"    {var}: .word 0")
            ensamblador.append('    newline: .asciiz "\\n"') # Para imprimir saltos de línea

            # --- Sección de Código (.text) ---
            ensamblador.append("\n.text")
            ensamblador.append(".globl main")
            ensamblador.append("main:")
            
            for op, arg1, arg2, res in cuadruplos:
                ensamblador.append(f"\n# Cuádruplo: ({op}, {arg1}, {arg2}, {res})")
                
                if op == '=':
                    if isinstance(arg1, int): # Asignación de un valor literal (ej: num1 = 3)
                        ensamblador.append(f"    li $t0, {arg1}           # Carga el valor inmediato {arg1} en el registro $t0")
                    else: # Asignación de otra variable (ej: suma = t4)
                        ensamblador.append(f"    lw $t0, {arg1}        # Carga el valor de la variable '{arg1}' en $t0")
                    ensamblador.append(f"    sw $t0, {res}         # Guarda el contenido de $t0 en la variable '{res}'")
                
                elif op in ('+', '-', '*', '/'):
                    ensamblador.append(f"    lw $t0, {arg1}        # Carga '{arg1}' en $t0")
                    ensamblador.append(f"    lw $t1, {arg2}        # Carga '{arg2}' en $t1")
                    if op == '+': ensamblador.append("    add $t2, $t0, $t1   # $t2 = $t0 + $t1")
                    if op == '-': ensamblador.append("    sub $t2, $t0, $t1   # $t2 = $t0 - $t1")
                    if op == '*': ensamblador.append("    mul $t2, $t0, $t1   # $t2 = $t0 * $t1")
                    if op == '/': ensamblador.append("    div $t2, $t0, $t1   # $t2 = $t0 / $t1")
                    ensamblador.append(f"    sw $t2, {res}         # Guarda el resultado en la variable temporal '{res}'")

                elif op == 'IMPDIG':
                    ensamblador.append(f"    lw $a0, {arg1}        # Carga el valor a imprimir en el registro de argumento $a0")
                    ensamblador.append("    li $v0, 1           # Carga el servicio de sistema 1 (print_int)")
                    ensamblador.append("    syscall             # Ejecuta la llamada al sistema")
                    # Imprimir un salto de línea para que se vea mejor
                    ensamblador.append("    li $v0, 4           # Servicio 4 (print_string)")
                    ensamblador.append("    la $a0, newline     # Carga la dirección de 'newline'")
                    ensamblador.append("    syscall")

                # --- Fin del programa ---
                ensamblador.append("\n# Fin del programa")
                ensamblador.append("    li $v0, 10          # Servicio 10 (exit)")
                ensamblador.append("    syscall")
            return "\n".join(ensamblador)


        # --- Optimizaciones ---
        def is_variable(val):
            return isinstance(val, str) and re.match(r'^[a-zA-Z_][a-zA-Z0-9_]*$', val)

        def find_rep(mapping, v):
            # Encuentra la representación final de v en el mapping de copias
            seen = set()
            while v in mapping and mapping[v] != v and mapping[v] not in seen:
                seen.add(v)
                v = mapping[v]
            return mapping.get(v, v)

        def copy_propagation(cuads):
            mapping = {}  # var -> representative (another var or const string)
            new_quads = []
            for op, a1, a2, res in cuads:
                # Reemplaza operandos por sus representantes si existen
                if is_variable(a1):
                    rep = find_rep(mapping, a1)
                    if rep is not None:
                        a1 = rep
                if is_variable(a2):
                    rep = find_rep(mapping, a2)
                    if rep is not None:
                        a2 = rep

                # Si es una copia simple (:=), actualizar mapping
                if op == ':=' and a2 is None and is_variable(res):
                    # Si a1 es variable o constante, mapear res -> a1
                    mapping[res] = a1
                    # No emitir el cuádruplo todavía: la copia puede eliminarse si res no se usa
                    new_quads.append((op, a1, a2, res))
                else:
                    # Esta instrucción escribe en 'res' (si existe): invalidar mapping de res
                    if isinstance(res, str) and res in mapping:
                        # eliminar dependencias que apunten a res
                        keys_to_remove = [k for k, v in mapping.items() if v == res]
                        for k in keys_to_remove:
                            del mapping[k]
                        del mapping[res]
                    new_quads.append((op, a1, a2, res))

            # Segunda pasada: eliminar copias redundantes (res := x donde res no se usa)
            # Para simplicidad dejamos la lista como está; otras pasadas (DCE) limpiarán instrucciones muertas.
            return new_quads

        def constant_folding(cuads):
            # Reemplaza operaciones entre constantes por asignaciones de constantes
            def is_int_literal(x):
                return isinstance(x, str) and re.match(r'^-?\d+$', x)

            new = []
            for op, a1, a2, res in cuads:
                if op in ('+', '-', '*', '/') and is_int_literal(a1) and is_int_literal(a2):
                    try:
                        i1 = int(a1)
                        i2 = int(a2)
                        if op == '+': val = i1 + i2
                        elif op == '-': val = i1 - i2
                        elif op == '*': val = i1 * i2
                        elif op == '/':
                            # División entera consistente con MIPS
                            val = i1 // i2 if i2 != 0 else 0
                        new.append((':=', str(val), None, res))
                        continue
                    except Exception:
                        pass
                new.append((op, a1, a2, res))
            return new

        def dead_code_elimination(cuads):
            # Eliminación de código muerto mediante análisis de vivacidad simple (backward)
            live = set()
            keep = [False] * len(cuads)
            # Instrucciones con efectos secundarios que siempre deben conservarse
            side_effect_ops = {'IMPDIG', 'IMPCAD', 'LEERDIG', 'LEERCAD'}

            for i in range(len(cuads)-1, -1, -1):
                op, a1, a2, res = cuads[i]
                has_side_effect = op in side_effect_ops
                uses = set()
                if isinstance(a1, str) and is_variable(a1): uses.add(a1)
                if isinstance(a2, str) and is_variable(a2): uses.add(a2)

                # Si la instrucción produce un resultado en 'res'
                if isinstance(res, str) and is_variable(res):
                    # Si el resultado es requerido (está vivo) o la instrucción tiene side-effects, la guardamos
                    if res in live or has_side_effect:
                        keep[i] = True
                        # Las variables usadas en esta instrucción se vuelven vivas
                        live.update(uses)
                        # Y el resultado deja de estar vivo porque ahora está definido aquí
                        if res in live:
                            live.discard(res)
                    else:
                        # No usado: instrucción muerta si no tiene efectos secundarios
                        keep[i] = has_side_effect
                        if keep[i]:
                            live.update(uses)
                            if res in live:
                                live.discard(res)
                else:
                    # No produce resultado (p. ej. prints), conservarla
                    keep[i] = True
                    live.update(uses)

            # Reconstruir lista filtrando
            new = [cuads[i] for i in range(len(cuads)) if keep[i]]
            return new

        def optimize_cuadruplos(cuads):
            prev = None
            current = list(cuads)
            # Iterar hasta convergencia o tope de N pasadas
            for _ in range(6):
                after_cf = constant_folding(current)
                after_cp = copy_propagation(after_cf)
                after_dce = dead_code_elimination(after_cp)
                if after_dce == current:
                    current = after_dce
                    break
                current = after_dce
            return current


        # --- Generación de código intermedio (cuadruplos) ---
        cuadruplos = []
        if parse_tree_root:
            # Busca el nodo de sentencias en el árbol
            for hijo in parse_tree_root[1]:
                if isinstance(hijo, tuple) and hijo[0] == 'sentencias':
                    generar_cuadruplos(hijo, cuadruplos=cuadruplos)
                    break
            # print("\n--- Código Intermedio (Cuadruplos) ---")
            # for i, quad in enumerate(cuadruplos, 1):
            #     print(f"{i:02}: {quad}")

            # Guardar los cuadruplos en un archivo .quad
            archivo_salida_quad = archivo_fuente.replace(".txt", ".quad")
            with open(archivo_salida_quad, "w", encoding="utf-8") as archivo_quad:
                archivo_quad.write(f"{'Num':<5} {'Operador':<10} {'Arg1':<15} {'Arg2':<15} {'Resultado':<15}\n")
                archivo_quad.write("-" * 60 + "\n")
                for i, quad in enumerate(cuadruplos, 1):
                    op, arg1, arg2, res = quad
                    archivo_quad.write(f"{i:<5} {op:<10} {str(arg1):<15} {str(arg2):<15} {str(res):<15}\n")

            # Aplicar optimizaciones (copy propagation, constant folding, dead code elimination)
            cuadruplos_opt = optimize_cuadruplos(cuadruplos)

            # Guardar cuadruplos optimizados
            archivo_salida_opt = archivo_fuente.replace(".txt", ".opt.quad")
            with open(archivo_salida_opt, "w", encoding="utf-8") as archivo_opt:
                archivo_opt.write(f"{'Num':<5} {'Operador':<10} {'Arg1':<15} {'Arg2':<15} {'Resultado':<15}\n")
                archivo_opt.write("-" * 60 + "\n")
                for i, quad in enumerate(cuadruplos_opt, 1):
                    op, arg1, arg2, res = quad
                    archivo_opt.write(f"{i:<5} {op:<10} {str(arg1):<15} {str(arg2):<15} {str(res):<15}\n")

            # Generar codigo usando cuadruplos optimizados
            codigo_final = generarCodigoMips(cuadruplos_opt)
            print("\nGeneracion de Codigo")
            print(codigo_final)
            
            nombre_archivo_salida = archivo_fuente.replace(".txt", ".asm")
            with open(nombre_archivo_salida, "w", encoding="utf-8") as f:
                f.write(codigo_final)
            print(f"\nCódigo ensamblador guardado en '{nombre_archivo_salida}'")

        

    except ValueError as e:
        print(f"\nError durante el análisis léxico: {e}")

    



