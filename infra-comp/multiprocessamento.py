from multiprocessing import Process
soma = 0

def f(name, id):
    print('hello, sou', name, id)
    soma += 1
    
if __name__ == '__main__':
    procs = []
    for i in range(4):
        p = Process(target=f, args=('bob filho', i, soma,))
        procs.append(p)
        
print('hello, sou bob pai')
for i in range(4):
    procs[i].start()
for i in range(4):
    procs[i].join()
    
print('hello, sou bob pai, soma=', soma)