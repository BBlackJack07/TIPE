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
miller = "miller_alone.csv"
s1,t1 = readfile(naif)
s2,t2 = readfile(miller)

plt.close()

#plt.plot(s1[1:],t1[1:],"g+")

#a,b = np.polyfit(s1[1:],np.log(t1[1:]),1)

#plt.plot(s1,np.exp(a*s1+b))

plt.plot(s2,t2,"r+")
c,d = np.polyfit(s2,t2,1)

plt.plot(s2,c*s2+d)
plt.title("Temps de génération d'un nombre pseudo-premier en fonction de sa taille (en bits)")
plt.xlabel("Nombre de bits")
plt.ylabel("Temps (en secondes)")