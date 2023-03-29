D = int(input())
f = [[] for i in range(2)]
r = [[] for i in range(2)]
for i in range(2):
    for k in range(D):
        f[i].append(input())
    for k in range(D):
        r[i].append(input())
 
b = [[0 for j in range(D * D * D)] for i in range(2)]
n = 0
for i in range(2):
    for x in range(D):
        for y in range(D):
            for z in range(D):
                if f[i][z][x] == '1' and r[i][z][y] == '1':
                    n += 1
                    b[i][x * D * D + y * D + z] = n
 
print(n)
print(' '.join(map(str, b[0])))
print(' '.join(map(str, b[1])))
