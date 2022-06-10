import matplotlib.pyplot as plt
import numpy as np
import csv

def readfile(path):
    X,Y=[],[]
    with open(path,"r") as f:
        csvFile = csv.reader(f)
        for line in csvFile:
            X.append(int(line[0]))
            Y.append(float(line[1]))
    return np.array(X),np.array(Y)

naif = "naif.csv"
miller = "miller.csv"
size,t1 = readfile(naif)
_,t2 = readfile(miller)

plt.close()

plt.plot(size[:-3],np.log(t1[:-3]/t1[0]),"g+")

#plt.plot(size,t2)

