import sqlite3

con = sqlite3.connect("dsbd.db")

uf = input("Digite uma UF: ")

nome_cidade = input("Digite um nome de cidade para buscar: ")

cursor = con.cursor()

cursor.execute("SELECT codigo, nome, uf FROM municipio WHERE UPPER(nome) = UPPER(?) AND UPPER(uf) = UPPER(?)", (nome_cidade,uf))

resultado = cursor.fetchone()

if resultado is not None:
    print(resultado)
else:
    print("Cidade não encontrada.")

con.close()




