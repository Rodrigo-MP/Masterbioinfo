# ## Ejercicio 8
# Realizar un programa que pida al usuario el nombre de un fichero, lo abra, cuente
# cuántas palabras de cada tipo existen y lo muestre por pantalla.
#/home/rodrigo/github/Masterbioinfo/Python/cuento.txt
f = open(input('Introduce el nombre de tu fichero: '))
texto = f.read().split()
dictado = {}
for word in texto:
    if word not in dictado.keys():
        dictado[word]=1
    else:
        count = dictado[word]
        dictado[word]=count+1
print(dictado)
#/home/rodrigo/github/Masterbioinfo/Python/cuento.txt
#lo primero tener una lista con todas las palabras. Luego ir recorriendo cada una e ir lanzando la funcion
# de contar
#### Ejercicio 9
# Abrir un fichero de genbank con datos de adn.
f = open('/home/rodrigo/github/Masterbioinfo/Python/ejemploGenbank.gb','r+')
f.readline(81:165)
print(f)
# Contar cuántas veces aparece cada una de las bases en la cadena de DNA.
contadorA = 0
contadorG = 0
contadorT = 0
contadorC = 0
for base in f:
    if base == 'A' or base == 'a':
        contadorA = contadorA + 1
    elif base == 'G' or base == 'g':
        contadorG = contadorG + 1
    elif base == 'T' or base == 't':
        contadorT = contadorT + 1
    elif base == 'C' or base == 'c':
        contadorC = contadorC +1
print('A:', contadorA, 'G:', contadorG, 'T:', contadorT, 'C:', contadorC)
# Usando un diccionario que tiene como clave los codones y como valor el  aminoácido correspondiente, convertir todo el DNA en aminoácidos y guardarlo en un fichero llamado amino.txt.
# El diccionario que tenemos subido los codones están puestos en mayúsculas
# cadena = "atgc"
cadena_adn = "ATTGGCTCG"
for i in range(0, len(cadena_adn), 3):
    codon = cadena_adn[i] + cadena_adn[i+1] + cadena_adn[i+2]
    print(codon)
cadena_mayuscula = cadena.upper()
print(cadena)
print(cadena_mayuscula)
# Contar cuántos aminoácidos de cada tipo hay.
#
# Por pantalla solicitar al usuario un porcentaje y mostrar cuáles son los aminoácidos por encima de ese porcentaje.
