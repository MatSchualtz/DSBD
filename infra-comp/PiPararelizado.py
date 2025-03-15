import time
from multiprocessing import Process, Pipe

PROCS = 6
a,b = Pipe()

def pi(start, end, step, sums):
    print ("Start: ", str(start))
    print ("End: ", str(end))
    sum = 0.0
    for i in range(start, end):
        x = (i+0.5) * step
        sum = sum + 4.0/(1.0+x*x)
    sums.send(sum)

if __name__ == "__main__":
    num_steps = 100_000_000 
    sums = 0.0
    step = 1.0/num_steps
    proc_size = num_steps // PROCS
    
    tic = time.time()
    workers = []
    for i in range(PROCS):
        worker = Process(target=pi, args=(i*proc_size, (i+1)*proc_size - 1, step, a))
        workers.append(worker)
    for worker in workers:
        worker.start()
    for worker in workers:
        worker.join()
    toc = time.time()

    for i in range(PROCS):
        sums += b.recv()
    pi = step * sums
    print ("Valor Pi: %.20f" %pi)
    print ("Tempo Pi: %.8f s" %(toc-tic))