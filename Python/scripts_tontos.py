# ## Ejercicio 8
# Realizar un programa que pida al usuario el nombre de un fichero, lo abra, cuente
# cuántas palabras de cada tipo existen y lo muestre por pantalla.
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
