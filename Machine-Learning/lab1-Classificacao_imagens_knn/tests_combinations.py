import os
import sys
import subprocess
import pandas as pd
import re
import knn

tamanho_min = int(sys.argv[1])
tamanho_max = int(sys.argv[2])
passo = int(sys.argv[3])

output_file = 'img_extract.txt'

models = {'tamanho': [],
          'acuracia': [],
          'precision': [],
          'recall': [],
          'f1_score': []}

for x in range(1, tamanho_min, passo):
    for y in range(1, tamanho_max, passo):
        print(f"\nTestando imagens com tamanho {x}x{y}...")

        extract_command = f"python digits.py {output_file} {x} {y}"
        subprocess.run(extract_command, shell=True)

        print("Rodando kNN...")
        result = knn.main(output_file)

        models["tamanho"].append(f'{x}x{y}')
        models["acuracia"].append(result['acuracia'])
        models["precision"].append(result['precision'])
        models["recall"].append(result['recall'])
        models["f1_score"].append(result['f1_score'])

df_results = pd.DataFrame(models)
df_results.to_csv("classification_results.csv", index=False)

print("\nTestes concluídos! Resultados salvos em 'classification_results.csv'")
