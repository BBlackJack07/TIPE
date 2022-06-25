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
    return X,Y

naif = "Test_results/naif.csv"
miller = "Test_results/miller_alone3.csv"
s1,t1 = readfile(naif)
s1,t1=np.array(s1),np.array(t1)
s2,t2 = readfile(miller)


#%% Naif
plt.close()

plt.plot(s1[1:],t1[1:],"g+")
plt.yscale("log")
a,b = np.polyfit(s1[1:],np.log(t1[1:]),1)
l="t = {}*exp({} * nb_bits)".format(np.round(np.exp(b),11),np.round(a,4))

plt.plot(s1,np.exp(a*s1+b),label=l)

plt.title("Temps de génération d'un nombre premier en fonction de sa taille (en bits)")
plt.xlabel("Nombre de bits")
plt.ylabel("Temps (en secondes)")
plt.legend()

#%% Miller
plt.close()

s2 = np.array(s2)
t2 = np.array(t2)


plt.plot(s2,t2,"r+")
c,d,e = np.polyfit(s2,t2,2)

plt.plot(s2,c*s2**2+d*s2+e)
plt.title("Temps de génération d'un nombre pseudo-premier en fonction de sa taille (en bits)")
plt.xlabel("Nombre de bits")
plt.ylabel("Temps (en secondes)")

