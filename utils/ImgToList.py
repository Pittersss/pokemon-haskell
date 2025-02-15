import json

linhas = []
while True:
    linha = str(input())
    if linha == "":  # Para quando o usuário insere uma linha vazia
        break
    linhas.append(linha)

print(json.dumps(linhas, ensure_ascii=False))
