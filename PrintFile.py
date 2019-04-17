from useful import *
def PrintFile(str, record, courses):
    with open(str, 'a+') as f:
        for score in record[::-1]:
            if score==[]:
                continue
            else:
                for individual in score:
                    string='\nscore:%f  credits:%f'%(individual.score , individual.credits)
                    f.write(string)
                    for course in individual.gene:
                        f.write(courses[course].__str__())
        f.write('\n')
    return 0