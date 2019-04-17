from random import *
class Classes(object ):
    def __init__(self,week,day,time):
        self.week=week
        self.day=day
        self.time=time
    def __str__(self):
        return '(week%d day%d time%d)'%(self.week,self.day,self.time)

class Lectures(object):
    def __init__(self,place,classes):
        self.place=place
        self.classes=classes
    def __str__(self):
        return '(place%d week%d day%d time%d,%d)'%(self.place,self.classes[0].week,self.classes[1].day,self.classes[0].time,self.classes[1].time)

class Courses(object):
    def __init__(self,name,teacher,credit,desire,lectures):
        self.name=name
        self.teacher=teacher
        self.credit=credit
        self.desire=desire
        self.lectures=lectures
    def __str__(self):
        return '%s,%s,%s'%(self.name,self.teacher,self.credit)

class Individual(object):
    def __init__(self, gene=[], score=0,credits=0):
        self.gene=gene
        self.score=score
        self.credits=credits
    def __str__(self):
        return '%s,%f'%(self.gene,self.credits)

class Population(object):
    def __init__(self,population=[],number=0,maxscore=-1,generation=0):
        self.population=population
        self.number=number
        self.maxscore=maxscore
        self.generation=generation
    def __str__(self):
        return '%dth:MAXind[%d]=%f\n %s'%(self.generation,self.number,self.maxscore,self.population[self.number])

def SpaceTime():
    space_time=[[[False for time in range(14)] for day in range(8)] for week in range(19)]
    return space_time

def GeneratePopulation(courses,sequence,length):
    POP=Population()
    i=0
    while i<10:
        genelength=randint(0,length-1)
        individuals =GenerateIndividual(sequence, genelength)
        POP.population.append(individuals)
        i=i+1
    return POP

def GenerateIndividual(sequence, genelength):
    gene = []
    score = 0
    credits=0
    individuals = Individual(gene, score,credits)
    individuals.gene = set(sample(sequence, genelength))
    individual=individuals
    individual.gene.add(0)
    return individual

def GenerateIndividualtest(sequence):
    gene = sequence
    score = 0
    credits=0
    individuals = Individual(gene, score,credits)
    individual=individuals
    return individual


def Fitness(POP,courses,aimcredit,weight,record,lower):
    n=-1

    for individual in POP.population:
        a=set([])
        n = n + 1
        temp = 0
        if POP.number==n and POP.generation!=0:
            continue
        st=SpaceTime()
        individual.score = 0
        m = 1
        for course in individual.gene:
            st=SearchSpacetime(courses[course], st)
            if st==False:
                individual.score=0
                individual.credits=0.1
                '''float(courses[course].credit)+individual.credits'''
                temp=0
                break
            else:
                individual.credits=float(courses[course].credit)+individual.credits
                temp=aimcredit-abs(aimcredit-individual.credits)
                individual.score=individual.score+courses[course].desire*courses[course].credit
                a.add(courses[course].name)
                # print(a)
                if a.__len__()<m:
                    individual.score=0
                    individual.credits = 0.1
                    '''float(courses[course].credit)+individual.credits'''
                    temp = 0
                    break
                m=m+1
        individual.score=(individual.score/(individual.credits*5))*aimcredit*weight
        individual.score=individual.score+temp
        if individual.score>POP.maxscore:
            POP.maxscore=individual.score
            POP.number=n
        rec=individual.score-lower

        if rec>=0:
            # nn=0
            individualtest=Individual(individual.gene,individual.score,individual.credits)
            record[int(rec*10)].append(individualtest)
            
    return POP,record

def SearchSpacetime(course,space_time):
    for lectures in course.lectures:
        for classes in lectures.classes:
            if space_time[classes.week][classes.day][classes.time]:
                return False
            space_time[classes.week][classes.day][classes.time]=True
    return space_time

def ReproduceOffspring(POP,sequence, length):
    n=0
    l=POP.population.__len__()
    while n<l:
        if n==POP.number:
            n = n + 1
            continue
        else:
            genelength = randint(0, length-1)
            POP.population[n] = GenerateIndividual(sequence, genelength)
        n = n + 1
    return POP

def PrintResult(POP,courses):
    print('Number',POP.number,'Generation',POP.generation)
    print('Individual',POP.population[POP.number].gene,'Score',POP.population[POP.number].score,'Credits',POP.population[POP.number].credits)
    for course in POP.population[POP.number].gene:
        print(courses[course])