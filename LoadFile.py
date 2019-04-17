from useful import *
def LoadFile(str):
    with open(str,'r') as f:
        txt=f.readlines()
        courses=[]
        for line in txt:
            information=line.split(" ")
            name=information[0]
            desire=float(information[1])
            teacher=information[3]
            credit=float(information[2])
            day=int(information[4])
            Lec1=int(information[5])
            Lec2=int(information[6])
            interval=int(information[7])
            Cla1=int(information[8])
            Cla2=int(information[9])
            place=int(information[12])
            classes=[]
            lectures=[]
            while Lec1<=Lec2:
                Cla1 = int(information[8])
                classes = []
                while Cla1<=Cla2:
                    Cla=Classes(Lec1, day, Cla1)
                    classes.append(Cla)
                    Cla1=Cla1+1
                Lec=Lectures(place,classes)
                lectures.append(Lec)
                Lec1=Lec1+interval
            if len(information)>=24:
                day = int(information[14])
                Lec1 = int(information[15])
                Lec2 = int(information[16])
                interval = int(information[17])
                Cla1 = int(information[18])
                Cla2 = int(information[19])
                place = int(information[22])
                classes = []
                while Lec1 <= Lec2:
                    Cla1 = int(information[18])
                    classes = []
                    while Cla1 <= Cla2:
                        Cla = Classes(Lec1, day, Cla1)
                        classes.append(Cla)
                        Cla1 = Cla1 + 1
                    Lec = Lectures(place, classes)
                    lectures.append(Lec)
                    Lec1 = Lec1 + interval
            if len(information) >= 34:
                day = int(information[24])
                Lec1 = int(information[25])
                Lec2 = int(information[26])
                interval = int(information[27])
                Cla1 = int(information[28])
                Cla2 = int(information[29])
                place = int(information[32])
                classes = []
                while Lec1 <= Lec2:
                    Cla1 = int(information[28])
                    classes = []
                    while Cla1 <= Cla2:
                        Cla = Classes(Lec1, day, Cla1)
                        classes.append(Cla)
                        Cla1 = Cla1 + 1
                    Lec = Lectures(place, classes)
                    lectures.append(Lec)
                    Lec1 = Lec1 + interval
            Cou=Courses(name,teacher,credit,desire,lectures)
            #print(lectures[0],lectures[1],lectures[2],lectures[3])
            courses.append(Cou)
    return courses

