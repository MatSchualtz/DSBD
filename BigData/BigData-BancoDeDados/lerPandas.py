import pandas as pd
import time


start_time = time.time()
df = pd.read_csv('complaints.csv', sep=',')
#df = pd.read_csv('microdados_ed_basica_2022.csv', sep=';', encoding = 'iso-8859-1')
end_time = time.time()
elapsed_time = end_time - start_time

print("Tempo para ler:", elapsed_time, "segundos")


input("Pressione Enter para continuar...")

start_time = time.time()
filtro = df.loc[df['Product'] == 'PRODUTO DE TESTES']
#filtro = df.loc[df['NO_ENTIDADE'] == 'COLEGIO DE TESTES']
end_time = time.time()
elapsed_time = end_time - start_time
print("Tempo para buscar:", elapsed_time, "segundos")


print(filtro)