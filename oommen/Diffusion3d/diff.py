import math
import numpy as np
import csv
def compute(un,sx,sy,sz):
    un2=[]
    for i in range(len(un)):
        r=[]
        for j in range(len(un[i])):
            s=[]
            for k in range(len(un[i][j])):
                if i==0 or i==len(un)-1 or j==0 or j==len(un[i])-1 or k==0 or k==len(un[i][j])-1:
                    e=(1-2*sx-2*sy-2*sz)*un[i][j][k]
                    e = e+sx*(un[i][j][k]) if i==0 else e+sx*(un[i-1][j][k])
                    e = e+sx*(un[i][j][k]) if i==len(un)-1 else e+sx*(un[i+1][j][k])
                    e = e+sy*(un[i][j][k]) if j==0 else e+sy*(un[i][j-1][k])
                    e = e+sy*(un[i][j][k]) if j==len(un[i])-1 else e+sy*(un[i][j+1][k])
                    e = e+sz*(un[i][j][k]) if k==0 else e+sz*(un[i][j][k-1])
                    e = e+sz*(un[i][j][k]) if k==len(un[i][j])-1 else e+sz*(un[i][j][k+1])
                    s.append(e)
                    e=0

                else:
                    s.append((1-2*sx-2*sy-2*sz)*un[i][j][k]+sx*(un[i+1][j][k]+un[i-1][j][k])+sy*(un[i][j+1][k]+un[i][j-1][k])+sz*(un[i][j][k+1]+un[i][j][k-1]))
            r.append(s)
        un2.append(r)
    return(un2)

def f_initial(x,y,z):
    return math.sin(x)+math.sin(y)+math.sin(z)
def diffusion3d(n,m,x0,xa,y0,ya,z0,za,t,k):
    dx=(xa-x0)/n
    dy=(ya-y0)/n
    dz=(za-z0)/n
    dt=t/m
    sx=k*dt/(dx**2)
    sy=k*dt/(dy**2)
    sz=k*dt/(dz**2)
    p=[list(np.arange(x0,xa,dx)),list(np.arange(y0,ya,dy)),list(np.arange(z0,za,dz))]
    ui=[]
    for i in p[0]:
        r=[]
        for j in p[1]:
            s = [f_initial(i,j,k) for k in p[2]]
            r.append(s)
        ui.append(r)
    final = [ui]
    final.extend(compute(final[i],sx,sy,sz) for i in range(m))
    return(final)
final=diffusion3d(10,200,0,2*np.pi,0,2*np.pi,0,2*np.pi,250,0.041)
print(final)