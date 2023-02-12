# -*- coding: utf-8 -*-
"""
Created on Wed Feb  8 10:29:57 2023
@author: brunelb
"""

"""
tomate
"""

from math import log

test = 'LOXUMNXAMPIIEZQBTWMWQNCILIUSXXNRUAGXXBOLTWLSBRXTTFMTHMKSMUQKKOZDXWXQALXWTIXYVIXDUEKVXQARGIBZXETVHIQN'
test2 = 'DURANTLEMOYENGEEUROPENENPARTICULIERENTRELEXEETLEXIIIESICLEALORSQUELESYSTMEDEDCLINAISONSDELANCIENFRANAISSEFFONDRELESLANGUESDOLCOMMENCENTSEDIFFUSERHORSDELEURDOMAINEDORIGINEDUFAITDESINVASIONSNORMANDESDESLESBRITANNIQUESDUSUDDELITALIEOUBIENDESCROISADESQUIENTABLISSANTDESTATSLATINSAULEVANTFONTDUFRANAISUNEBASEDELALINGUAFRANCAMDITERRANENNEENPARLORDONNANCEDEVILLERSCOTTERTSLEMOYENFRANAISLANGUEMATERNELLEDESDYNASTIESCAPTIENNESDEVIENTUNELANGUEJURIDIQUEETADMINISTRATIVEENFRANCELAMMEPRIODEILCOMMENCESEDIFFUSERPLUSMASSIVEMENTHORSDEUROPEDABORDENAMRIQUEPUISENAFRIQUEENASIEETENOCANIESOUSLEFFETDELEXPANSIONDESEMPIRESCOLONIAUXFRANAISPUISBELGEPARTIRDUDIXSEPTIMESICLEDANSLESOCANSATLANTIQUEINDIENETPACIFIQUELESDPORTATIONSDEPOPULATIONSPRATIQUESPARLESEMPIRESEUROPENSVERSLEURSCOLONIESAMNENTDANSUNCONTEXTEPRINCIPALEMENTDESCLAVAGELAFORMATIONDENOMBREUXCROLESBASELEXICALEFRANAISEENPARLEDCRETRVOLUTIONNAIREDUTHERMIDORANIIETMALGRLEFAITQUILAITTSOUSLANCIENRGIMELALANGUEDESCOURSROYALESETPRINCIRESEUROPENNESLEFRANAISCLASSIQUELANGUEDESLUMIRESDEVIENTLASEULELANGUEOFFICIELLEDELAPREMIRERPUBLIQUEFRANAISEUNEDESPARTICULARITSDUFRANAISSETROUVEDANSLEFAITQUESONDVELOPPEMENTETSACODIFICATIONONTTENPARTIELUVREDEGROUPESINTELLECTUELSCOMMELAPLIADEOUDINSTITUTIONSCOMMELACADMIEFRANAISELEFRANAISESTAINSISOUVENTCONSIDRCOMMEUNELANGUEACADMIQUEPARTIRDUDIXNEUVIMESICLEETMALGRQUELQUESRFORMESAUCOURSDESSICLESSUIVANTSSONORTHOGRAPHECODIFIECOMMENCESEFIGERELLEESTCONSIDRECOMMETRANSPARENTEDANSLESENSDELALECTUREMAISOPAQUEDANSLESENSDELCRITUREAUCOURSDUXXESICLELEFRANAISDEVIENTUNELANGUEDENVERGUREMONDIALEENMMETEMPSQUILSMANCIPEDELEUROPEPARTIRDECESICLELENOMBREDEFRANCOPHONESVIVANTHORSDEUROPEDPASSELENOMBREDELOCUTEURSSURLECONTINENTDORIGINEDELALANGUE'

FR_FREQ = [0.0897488190944856, 0.0071183273904290734, 0.034052183523901, 0.040065262299180275, 0.14744542834962698, 0.018289233036407417, 0.015381376560834726, 0.007394639544216523, 0.07834107445954658, 0.002131550900646044, 0.00043420481309456454, 0.06365705714398495, 0.028802252601939447, 0.08645938869225404, 0.056407152537466615, 0.03132853515085328, 0.008841988921198406, 0.06834120603676268, 0.08216997144774411, 0.06813068249101986, 0.05267035960053157, 0.009907764371521428, 0.00048683569953026935, 0.004026262812331417, 0.0030262759700530255, 0.0005789397507927527]

def pattern_occ(s,l=3):
   d = {}
   for i in range(len(s)-l+1):
      p = s[i:i+l]
      if p in d:
         d[p].append(i)
      else:
         d[p] = [i]
   return d

def suppr_mult_occ(L):
   M = []
   for i in range(len(L)):
      if not (L[i] in L[i+1:]) :
         M.append(L[i])
   return M

def flatten(L):
   M=[]
   for l in L:
      for x in l:
         M.append(x)
   return M

def pgcd(a,b):
   if b == 0:
      return a
   return pgcd(b, a%b)

def lst_pgcd(L):
   p = 0
   for n in L:
      p = pgcd(n,p)

def primefactors(n):
   D = []
   k=0
   while n % 2 == 0:
      k+=1
      n = n // 2
   if k > 0:
      D.append((2,k))
   d = 3
   while n > 1:
      k = 0
      while n % d == 0:
         k+=1
         n = n // d
      if k > 0:
         D.append((d,k))
      d += 2
   return D

def divisors(n):
   def div(P):
      if len(P) == 0:
         return [1]
      (d,k) = P[0]
      R = P[1:]
      LR = div(R)
      L = []
      n = 1
      for i in range(k+1):
         for p in LR:
            L.append(p * n)
         n *= d
      return L
   return div(primefactors(n))

def potential_key_len(s,pattern_length=3):
   L=[]
   d = pattern_occ(s,pattern_length)
   for k,l in d.items():
      if len(l)>=2:
         for i in range(len(l) - 1):
            L.append(l[i+1] - l[i])
   L = suppr_mult_occ(L)
   M = [divisors(n) for n in L]
   M = suppr_mult_occ(flatten(M))
   M.sort()
   return M

def cipher(c,k):
   ic, ik = ord(c) - ord('A'), ord(k) - ord('A')
   return chr((ic + ik) % 26 + ord('A'))

def decipher(c,k):
   ic, ik = ord(c) - ord('A'), ord(k) - ord('A')
   return chr((ic - ik) % 26 + ord('A'))

def vigcipher(s,key):
   lk,ls = len(key),len(s)
   cs = []
   j=0
   for i in range(ls):
      if s[i].isalpha():
         cs.append(cipher(s[i].upper(), key[j%lk].upper()))
         j+=1
   return ''.join(cs)

def vigdecipher(cs,key):
   lk,ls = len(key),len(cs)
   s = []
   j=0
   for i in range(ls):
      if cs[i].isalpha():
         s.append(decipher(cs[i].upper(), key[j%lk].upper()))
         j+=1
   return ''.join(s)

def get_frequencies(cs,lk):
   CF = [26*[0] for _ in range(lk)]
   l = lk * [0]
   for j in range(lk):
      for i in range(j,len(cs),lk):
         CF[j][ord(cs[i].upper()) - ord('A')] += 1
         l[j] += 1
      CF[j] = [CF[j][i] / l[j] for i in range(26)]
   return CF

def rotate_nth(L,n):
   return L[n:] + L[:n]

def dist(A,B):
   S = 0
   for k in range(len(A)):
      S+=(A[k] - B[k]) ** 2
   return S

def min_dist(C,F):
   m, min_d = -1, float('inf')
   for i in range(26):
      d = dist(rotate_nth(C, i), F)
      if d < min_d:
         min_d = d
         m = i
   return m

def frequency_attack(cs,lk,F):
   CF = get_frequencies(cs, lk)
   key = lk * ['']
   for i in range(lk):
      key[i] = chr(min_dist(CF[i],F) + ord('A'))
   return ''.join(key)

def attack(cs,F=FR_FREQ):
   keys = []
   pattern_length = max(3, int(log(len(cs))))
   pot_keylen = potential_key_len(cs,pattern_length)
   while len(pot_keylen) < 1 and pattern_length >= 3:
      pot_keylen = potential_key_len(cs,pattern_length)
      pattern_length -= 1
   for lk in pot_keylen:
      keys.append(frequency_attack(cs, lk, F))
   mk, m = '', float('inf')
   for key in keys:
      s = vigdecipher(cs, key)
      f = get_frequencies(s, 1)[0]
      d = dist(f,F)
      if d < m:
         m = d
         mk = key
   return mk
