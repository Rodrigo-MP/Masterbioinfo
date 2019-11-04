# %% markdown
# # Máster en Bioinformática Aplicada a Medicina Personalizada y Salud
# # curso 2019-2020
# %% markdown
# # Nombre y apellidos:
# %%
# Pruebas
lista_elementos = [1,2,3,4,5,6]
elemento_a_buscar = 3
for elemento in lista_elementos :
    if elemento == elemento_a_buscar :
       print('El elemento existe en la lista')
# %%
friends = ['Joseph', 'Glenn', 'Sally']
for friend in friends:
print('Happy New Year:',friend)
print('Done!')
# %%
### Cadenas
print('C:\nopath')
print(r'C:\nopath')
print('C:\\nopath\nC:\path')
### find
frase = 'En un lugar de la mancha'
pos = frase.find('lugar')
print(pos)
### split
frase = 'En un lugar de la mancha'n
palabras=frase.split(' ')
print(palabras)
### strip
frase = "       En un lugar"
frase = frase.strip()
print(frase)

#### Programita que compara 2 numeros que les metemos

num1 = int(input('Inserta el primer numero '))
num2 = int(input('Inserta el segundo numero '))
if num1 > num2:
    print(num1,"es mayor que",num2)
elif num2 > num1:
    print(num2,"es mayor que",num1)
else:
    print(num1,"es igual que",num2)


# %%
### Pruebas día 3

# %% markdown
# ## Ejercicio 1
# 1. Crear una lista con el nombre de 6 aminoácidos
# 2. Insertar uno más al final
# 3. Pedir el nombre de uno de ellos al usuario y eliminarlo
# 4. Ordenar la lista alfabéticamente
# 5. Imprimir el número de elementos de la lista
#
# Imprimir la lista después de cada paso
# %%
# Solución
# 1
aminoacidos = ['triptofano','glicina', 'alanina', 'valina', 'lisina']
print(aminoacidos)
# 2
A = ['leucina']
aminoacidos.extend(A)
print(aminoacidos)
# 3
aminoacido = input("Introduce un aminoacido:")
if aminoacido in aminoacidos :
    aminoacidos.remove(aminoacido)
print(aminoacidos)
# 4
aminoacidos.sort()
print(aminoacidos)
# 5
len(aminoacidos)

# %% markdown
# ## Ejercicio 2
# Ordenar una lista manteniendo una copia de la versión original.
# %%
# Solución
lista = [1,7,5,4,78,9]
lista_ordenada = lista.sort()
print(lista_ordenada)
print(lista)
lista = [1,7,5,4,78,9]
lista_ordenada = sorted(lista)
print(lista)
print(lista_ordenada)
lista1 = [1,2,3,4,5]
lista2 = lista1
x = 23
lista2.append(x)
print(lista2)
print(lista1)
# %% markdown
# ## Ejercicio 3
# Escribir un programa que pida al usuario números enteros hasta que introduzca un 0. Al finalizar el programa debe mostrar la cantidad de números introducidos.
# %%
# Solución
n=int(input('Introduce numeros enteros: '))
lista_numeros = []
while n!=0:
    lista_numeros.append(n)
    print(lista_numeros)
    n=int(input('Introduce numeros enteros: '))
print(lista_numeros)
# %% markdown
# ### Ejercicio 4
# Pedir al usuario una cadena de adn y contar cuántas veces aparece cada una de las bases.
# Hacer que esto se repita en un bucle infinito que vaya pidiendo distintas cadenas. El programa finalizará si se teclea la palabra fin.
# %%
# Solución
n = input('Introduce la cadena de ADN: ')
contadorA = 0
contadorG = 0
contadorT = 0
contadorC = 0
for base in n:
    if base == 'A' or base == 'a':
        contadorA = contadorA + 1
    elif base == 'G' or base == 'g':
        contadorG = contadorG + 1
    elif base == 'T' or base == 't':
        contadorT = contadorT + 1
    elif base == 'C' or base == 'c':
        contadorC = contadorC +1
print('A:', contadorA, 'G:', contadorG, 'T:', contadorT, 'C:', contadorC)
# %% markdown
# ## Ejercicio 5
# Mostrar por pantalla las tablas de multiplicar del 1 al 9.
# Deberá aparecer algo como:
#
# 	1*1 es 1
# 	1*2 es 2
# 	1*3 es 3
#           …
# %%
# Solución
# %% markdown
# ## Ejercicio 6
# 1. Crear un diccionario con el nombre de un animal como clave, y su nombre en latín como valor.
# 2. Todos los datos se irán pidiendo al usuario por pantalla.
# 3. Cada vez que se introduzcan los datos de un animal se imprimirán todos los datos del diccionario.
# 4. Si un animal ya se ha introducido se le dirá al usuario.
# %%
# Solución
diccionario_animales = {}
animal_español = input('Introduce el animal en español: ')
animal_inglés = input('Introduce el animal en inglés: ')
animal_inglés = 0
while animal_español != 'fin' :
    animal_español = input('Introduce el animal en español: ')
    animal_inglés = input('Introduce el animal en inglés: ')
    if animal_español in diccionario_animales :
        print('Este animal ya está en el diccionario')
    if animal_español == 'fin' :
        break
    else:
        diccionario_animales[animal_español] = animal_inglés
print(diccionario_animales)
break
# Solución del profesor
# %% markdown
# ## Ejercicio 7
# 1. Crear un diccionario con el nombre de las 4 bases nitrogenadas del adn como clave y como valor  otro diccionario con dos elementos: la primera clave es ‘abreviatura’ y la segunda ‘tipo’ (indicando si es púrica o pirimídica).
# 2. Acceder al diccionario para imprimir las abreviaturas.
# 3. Insertar el uracilo.
# 4. Volver a acceder al diccionario para imprimir las abreviaturas.
# 5. Imprimir los nombres de todos las bases, abreviatura y tipo con algún tipo de formato
# %%
# Solución
dic_bases_nitrogenadas = {
    'adenina':{'abreviatura':'A','tipo':'púrica'},
    'guanina':{'abreviatura':'G', 'tipo':'pirimidínica'},
    'timina':{'abreviatura':'T','tipo':'púrica'},
    'citosina':{'abreviatura':'C','tipo':'pirimidínica'}
}
print(dic_bases_nitrogenadas['adenina']['abreviatura'])
print(dic_bases_nitrogenadas['citosina']['abreviatura'])
print(dic_bases_nitrogenadas['guanina']['abreviatura'])
print(dic_bases_nitrogenadas['timina']['abreviatura'])
### Insertar el uracilo
dic_bases_nitrogenadas['uracilo'] = {'abreviatura':'U','tipo':'púrica'}
print(dic_bases_nitrogenadas['uracilo']['abreviatura'])
### Imprime todo
for key in dic_bases_nitrogenadas:
    print(key,': tiene de abreviatura', dic_bases_nitrogenadas[key]['abreviatura'], ', y es de tipo', dic_bases_nitrogenadas[key]['tipo'])
# %% markdown
# ## Ejercicio 8
# Realizar un programa que pida al usuario el nombre de un fichero, lo abra, cuente cuántas palabras de cada tipo existen y lo muestre por pantalla.
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
# Solución
# %% markdown
# ## Ejercicio 9
# Abrir un fichero de genbank con datos de adn.
#
# Contar cuántas veces aparece cada una de las bases en la cadena de DNA.
#
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
# %%
# Solución
# %% markdown
# ## Ejercicio 10
# Escribir un programa que pida al usuario el número del que se quiere calcular el factorial:
#
#     a) Definir una función que calcule el factorial del número.
#     b) Definir una función que calcule el factorial del número de manera recursiva.
# %%
# Solución a
# %%
# Solución b
# %% markdown
# ## Ejercicio 11
# Definir una función que convierta codones en aminoácidos.
# %%
# Solución
# %% markdown
# ## Ejercicio 12
# Definir una función que calcule la media de una lista de valores numéricos.
# %%
# Solución
# %% markdown
# ## Ejercicio 13
# Definir una función que multiplique dos matrices de 4*4
#
# Redefinir la función para que pueda multiplicar matrices de cualquier tamaño
#
# Hacer un programa que pida al usuario el tamaño de las matrices a multiplicar, recoja por teclado los datos de las dos matrices y las multiplique
#
# Hacer un programa que pida al usuario el nombre de dos ficheros donde se encuentran guardadas sendas matrices, las lea y las multiplique guardando el resultado en un fichero cuyo nombre se ha pedido al usuario
# %%
# Solución
# %% markdown
# ## Ejercicio 14
# Crear una función que reciba una cadena de ADN y la convierta en una de ARN.
# %%
# Solución
# %% markdown
# ## Ejercicio 15
# Para el siguiente código:
#
# 	re.match(‘(.*)(cat)(.*)', ‘the cat in the hat’)
#
# Determinar qué se identifica en cada uno de los grupos.
#
# Hacer lo mismo con:
#
# 	re.match(‘(.*)(at)(.*)', ‘the cat in the hat’)

# %%
# Solución
# %% markdown
# ## Ejercicio 16
# En PERL los nombres de variable se forman igual que en Python pero van precedidos del símbolo $.
#
# Crear un programa que pida al usuario una palabra y diga si es una variable legal de PERL

# %%
# Solución
# %% markdown
# ## Ejercicio 17
# En Python los números en punto flotante pueden aparecer con los siguientes formatos:
#
#     1.23
#     1.
#     3.14e-10
#     4E21
#     4.0e+45
#
# Crear un programa que pida al usuario un número y diga si es un número en punto flotante
# %%
# Solución
# %% markdown
# ## Ejercicio 18
# Crear un programa que abra el fichero GenBank.gb y haga lo siguiente:
# 1. Guarde en un fichero toda la información que no se corresponda con los datos de una secuencia de ADN.
# 2. Guarde en otro fichero los datos de secuencia.
# 3. Analice cada línea de secuencia e indique en qué líneas aparece tca y en qué posición dentro de la línea.

# %%
# Solución
# %% markdown
# ## Ejercicio 19
# Crear un programa pida al usuario el nombre de un directorio, busque en este directorio todos los ficheros de tipo GenBank (extension.gb), los abra y diga de qué organismo contiene datos.
#
# Mejorarlo para que mire también en los subdirectorios del directorio dado.
# %%
# Solución
# %% markdown
# ## Ejercicio 20
# Queremos almacenar los artículos que leemos en una base de datos (BD), pero no nos gusta ninguna de las disponibles, por lo tanto vamos a construirla nosotros mismos.
#
# La BD será una lista de diccionarios. Cada diccionario consta de 5 campos: Título del artículo, autores, revista, fecha y el nombre del fichero donde guardamos un resumen del artículo.
#
# El programa nos permitirá hacer una serie de cosas, elegidas por un menú:
# 	1. Introducir un nuevo elemento, esto implicará salvarlo en un archivo.
# 	2. Listar todos los artículos, especificando los 4 primeros campos.
# 	3. Buscar si existe un artículo dando una palabra clave del título.
# 	4. Buscar si existe un artículo dando el nombre de un autor
# 	5. Listar todos los artículos de una determinada revista.
#
# Cada vez que se arranca el programa deberán recuperarse los datos almacenados previamente.

# %%
# Solución
