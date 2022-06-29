import matplotlib.pyplot as plt
import numpy as np
import csv

def readfile(path):
    X,Y,Z=[],[],[]
    with open(path,"r") as f:
        csvFile = csv.reader(f)
        for line in csvFile:
            X.append(int(line[0]))
            Y.append(float(line[1]))
            Z.append(float(line[2]))
    return X,Y,Z

naif = "Test_results/naif.csv"
miller = "Test_results/miller.csv"
s1,t1,u1 = readfile(naif)
s1,t1,u1=np.array(s1),np.array(t1),np.array(u1)
s2,t2,u2 = readfile(miller)
s2,t2,u2 = np.array(s2),np.array(t2),np.array(u2)


#%% Naif
plt.close()

plt.errorbar(s1,t1,u1)
plt.yscale("log")
a,b = np.polyfit(s1[1:],np.log(t1[1:]),1)
l="t = {}*exp({} * nb_bits)".format(np.round(np.exp(b),11),np.round(a,4))

plt.plot(s1,np.exp(a*s1+b),label=l)

plt.title("Temps de génération d'un nombre premier en fonction de sa taille (en bits)")
plt.xlabel("Nombre de bits")
plt.ylabel("Temps (en secondes)")
plt.legend()
plt.show()

#%% Miller
plt.close()

plt.plot(s2,np.sqrt(t2),"r+")
c,d = np.polyfit(s2,np.sqrt(t2),1)

plt.plot(s2,c*s2+d)
plt.title("Temps de génération d'un nombre pseudo-premier en fonction de sa taille (en bits)")
plt.xlabel("Nombre de bits")
plt.ylabel("Temps (en secondes)")
plt.show()

