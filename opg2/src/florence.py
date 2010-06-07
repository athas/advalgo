#!/usr/bin/python3.0
#Author: Ronni Elken Lindsgaard
#Email: ronni.lindsgaard@gmail.com
#Version: 
#Description: 
#License: Use and distribute freely, but don't remove author, email etc. (This block of comments)

from math import *
import random
import copy

#remember to choose from nodes not within d
points = [(1.797,0.397),		(1.653,0.776),		(1.430,0.883),		(1.004,0.377),		(1.057,0.577),		(1.377,0.703),		(0.790,1.043),		(0.784,0.763),		(2.530,0.730),		(2.744,0.790),		(2.530,0.397),		(2.824,0.610),		(1.624,1.277),		(1.417,1.390),		(1.184,1.510),		(0.824,1.703),		(2.070,1.083),		(2.004,1.757),		(2.351,1.173),		(2.264,1.373),		(2.378,1.313),		(2.658,1.639),		(2.565,2.476),		(2.589,2.278),		(2.114,2.185),		(2.072,2.330),		(2.225,2.258),		(1.938,1.847),		(1.815,1.811),		(1.654,2.015),		(1.759,1.983),		(1.310,2.011),		(1.326,2.658),		(1.219,2.880),		(1.512,2.813),		(2.320,2.991),		(2.314,3.389),		(2.191,3.339),		(1.797,3.941),		(2.644,4.292),		(3.577,4.320),		(4.177,2.696),		(3.852,2.389),		(4.199,1.702),		(2.807,2.300),		(2.774,2.020),		(2.761,1.900),		(2.277,1.837),		(2.230,1.970),		(2.530,1.930),		(2.670,1.790),		(1.633,1.498),		(1.787,1.331),		(1.827,1.431),		(2.557,0.250),		(2.290,0.050),		(2.237,0.437),		(2.084,0.717),		(2.417,0.917),		(2.217,0.917),		(1.552,1.079),		(1.257,1.217),		(1.404,1.277),		(0.937,1.477),		(0.877,1.230),		(0.650,1.677),		(0.157,1.910),		(0.550,2.037),		(1.024,1.763),		(1.197,1.830),		(1.370,1.683),		(1.524,1.750),		(1.524,1.617),		(1.677,1.663),		(1.890,2.317),		(2.072,2.530),		(2.841,3.329),		(0.510,1.423),		(2.037,0.870),		(1.217,2.205)]

lp = len(points)

edges = []

def distance(((x1,y1),(x2,y2))):
  x = abs(x1-x2)
  y = abs(y1-y2)
  return sqrt(x**2+y**2)


for i in range(1,lp):
  edges += [(points[i],points[(i+1) % lp])]

def walk(edges):
  cost = 0
  for i in edges:
    cost += distance(i)
  return cost

def tweak1(edges):
  while 1:
    r1 = random.randint(0,len(edges)-1)
    r2 = random.randint(0,len(edges)-1)
  
    (u1,v1) = edges[r1]
    (u2,v2) = edges[r2]
    
    if u1 == u2 or u1 == v2:
      continue
    if v1 == u2 or v1 == v2:
      continue
    break

  edges[r1] = (u1,u2)
  edges[r2] = (v1,v2)
 
  d = max(r1,r2) - min(r1,r2)

  if r1 < r2:
    for i in range(r1+1,r2-1):
      (v1,v2) = edges[i]
      edges[i] = (v2,v1)
  else:
    for i in range(1,len(edges)-d-1):
      (v1,v2) = edges[(r1+i) % len(edges)]
      edges[(r1+i) % len(edges)] = (v2,v1)
  return edges

def hillclimb(a,b):
  if walk(a) < walk(b):
    return a
  else:
    return b

sol = edges
i = 0
while 1:
  print "Looping" + str(i)
  if i >= 1000:
    break
  new = tweak1(copy.copy(sol))
  if walk(sol) < walk(new):
    print "Nothing changed"
    i+=1

    continue
  sol = new

#  if compare != sol:
#    print "choose something better"
#    i=0
  #sol = new
  i=0

print sol
print walk(sol)
