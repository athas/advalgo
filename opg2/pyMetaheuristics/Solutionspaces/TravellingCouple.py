from Solutionspace import Solutionspace
from random import *
from math import *
from copy import copy
import os.path

class TravellingCouple(Solutionspace):
    dist = 0.2
    
    def initialize(self, options):
        self.load(options.filename)
        if len(self.solution) == 0:
            self.randomize()
            
        #Figure out which nodes rule out other nodes
        self.groups = []
        for i in range(0,len(self.solution)):
            self.groups.append([self.solution[i]])
            for j in range(i,len(self.solution)):
                if (self.distance(self.solution[i], self.solution[j])) < self.dist:
                    self.groups[i].append(self.solution[j])
#	for i in range(0,len(self.solution)):
#		self.dTweak()        

    def randomize(self):
        print "This should be a random space"
        pass

    def tweak(self):
        #perform a D-tweak in 5% of the cases:
        if randint(0,100) <= 4:
            #Distance-tweaks
            return self.dTweak()
        elif randint(0,100) <= 15:
            #Swap edges, and pairs.
            return self.swapTweak()
        else:
            #Move entire sections around
            return self.popTweak()

    def dTweak(self):
        #Find an element to remove in the current solution
        i = randint(0,len(self.groups)-1)
        while True:
            if len(self.groups[i]) > 1 and self.groups[i][0] in self.solution:
                break;
            else:
               # print 'dTweakLoop'+str(i)
                i = (i + 1) % len(self.groups)
        # i now points to an element that overlaps another, which is in the graph
        elmindex = self.solution.index(self.groups[i][0])
        elm = self.solution.pop(elmindex)
        def f(x): return elm in x
        grp = filter(f, copy(self.groups))
        # grp is now all clusters including x
        # find a list that has no members of sol and inserts
        i = randint(0,len(grp)-1)
        t = i

        manipulated = [elm]
        while 1:
            g = grp[i]
            memb = False
            for m in g:
                memb = memb or m in self.solution
            
            if memb == False: # sublist has no members of solution
                ins = g[randint(0,len(g)-1)]
                self.solution.insert(elmindex, ins) # Insert one at random
                manipulated.insert(0,ins)
                
            i = (i + 1) % len(grp)
            if i == t: break
            
        return manipulated                
            
            
                
    def swapTweak(self):
            # Only allow disjoint edges
            r1 = 0
            r2 = 0
            while abs(r1-r2)<1:
                r1 = randint(1,len(self.solution)-1)
                r2 = randint(1,len(self.solution)-1)
            #number of elements we wanna swap [r1+i] <=> [r2-i]
            r = int(floor((abs(r1 - r2))/2))
            #Swap two nodes
            if r == 0:
                temp = self.solution[r1]
                self.solution[r1] = self.solution[r2]
                self.solution[r2] = temp
            #swap two edges
            else:
                for i in range(0, r):
                    temp = self.solution[(r1+i)%len(self.solution)]
                    self.solution[(r1+i)%len(self.solution)] = self.solution[(r2-i)%len(self.solution)]
                    self.solution[(r2-i)%len(self.solution)] = temp
                    
            return [self.solution[r1], self.solution[r2]]

    def popTweak(self):
        popi = randint(0,len(self.solution)-1)
        count = randint(1,floor(len(self.solution)/2))
        insi = randint(0,(len(self.solution)-1-count)%len(self.solution))
        manipulated = []
        while count > 0:
            pop = self.solution.pop(popi)
            self.solution.insert(insi,pop)
            manipulated.insert(0,pop)
            count-=1
        # Return the manipulated nodes
        return manipulated

    def assess(self):
        sum = 0
        for i in range(0,len(self.solution)):
            sum += self.distance(self.solution[i], self.solution[(i+1) % len(self.solution)])
        return sum
        
    def distance(self, a, b):
        (xa,ya) = a
        (xb,yb) = b
        return sqrt(abs(xa-xb)**2+abs(ya-yb)**2)        
