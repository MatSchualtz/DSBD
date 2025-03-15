def contagemtexto():
    dicionario = {}
    String = input('Digite o Texto Desejado: ')
    for i in range(0,len(String)):
        contagem = String.upper().count(String.upper()[i])
        dicionario[String.upper()[i]] = contagem
        dicionario_ordenado = {}
        for i in sorted(dicionario):
            dicionario_ordenado[i] = dicionario[i]
    return print(dicionario_ordenado)


contagemtexto()