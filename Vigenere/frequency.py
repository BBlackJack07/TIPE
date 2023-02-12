
def getlines(filename):
   with open(filename,'r') as f:
      L = f.readlines()
      return L
   return None

def alpha(s):
   s0 = []
   for c in s:
      if c.isalpha() and c.isascii():
         s0.append(c.upper())
   return s0

def build_matrix(S):
   n = 0
   M,F = [26 * [0] for _ in range(26)], 26 * [0]
   for s in S:
      s0 = alpha(s)
      for i in range(len(s0)-1):
         M[ord(s0[i]) - ord('A')][ord(s0[i+1]) - ord('A')] += 1
         F[ord(s0[i]) - ord('A')] += 1
         if i == len(s0) - 2:
            F[ord(s0[i+1]) - ord('A')] += 1
         n+=1
   for k in range(26):
      F[k] /= n
      for l in range(26):
         M[k][l] /= n
   return M,F
