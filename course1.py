#HAItech,2018.02.14
from useful import *
from random import *
from LoadFile import *
from PrintFile import *

courses=LoadFile("/root/a/useful1.txt")

space_time=SpaceTime()

length = courses.__len__()
# length=3
mincredit=9
bixiucredit=3
sequence = range(1,length)
aimcredit=16.5
weight=1
POP=GeneratePopulation(courses,sequence,length)
lower=aimcredit*weight+mincredit+bixiucredit
record=[[]for score in range(int(aimcredit*(weight+1)-lower)*10)]

Maxscore=[-1]*5000
Maxscore[-1]=0
while Maxscore[-1]!=Maxscore[-4999]:
    POP,record=Fitness(POP,courses,aimcredit,weight,record,lower)

    Maxscore.append(POP.maxscore)
    POP=ReproduceOffspring(POP,sequence,length)
    POP.generation=POP.generation+1

c=PrintFile("/root/a/result.txt", record, courses)
PrintResult(POP,courses)
