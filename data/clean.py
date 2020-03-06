import csv

f = open("atlantic_hurricaneDate.txt","r")
list_of_hurricanes = []
hurricane_data = []

f1 = f.readlines()  
for x in f1:
    line = x.split(",")
    if len(line) <= 4:
        list_of_hurricanes.append(line)
    else:
        hurricane_data.append(line)

start = 0
finish = 0

for i in range(0,len(list_of_hurricanes)):
    finish = start + int(list_of_hurricanes[i][2]) -1 
 
    if start - finish == 0:
        hurricane_data[start].insert(0,list_of_hurricanes[i][0])
        hurricane_data[start].insert(0,list_of_hurricanes[i][1])
        hurricane_data[start].insert(0,list_of_hurricanes[i][2])
    else:
        for j in range(start,finish + 1):
            hurricane_data[j].insert(0,list_of_hurricanes[i][0])
            hurricane_data[j].insert(0,list_of_hurricanes[i][1])
            hurricane_data[j].insert(0,list_of_hurricanes[i][2])

    start = finish + 1

f.close()


with open("atlantic_clean.csv","w") as filehandle:
    writer = csv.writer(filehandle)
    writer.writerow(["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"])
    writer.writerows(hurricane_data)

f = open("northEast_northPacific_hurricaneData.txt","r")

list_of_hurricanes1 = []
hurricane_data1 = []

f1 = f.readlines()  
for x in f1:
    line = x.split(",")
    if len(line) <= 4:
        list_of_hurricanes1.append(line)
    else:
        hurricane_data1.append(line)

start = 0
finish = 0

for i in range(0,len(list_of_hurricanes1)):
    finish = start + int(list_of_hurricanes1[i][2]) -1 
 
    if start - finish == 0:
        hurricane_data1[start].insert(0,list_of_hurricanes1[i][0])
        hurricane_data1[start].insert(0,list_of_hurricanes1[i][1])
        hurricane_data1[start].insert(0,list_of_hurricanes1[i][2])
    else:
        for j in range(start,finish + 1):
            hurricane_data1[j].insert(0,list_of_hurricanes1[i][0])
            hurricane_data1[j].insert(0,list_of_hurricanes1[i][1])
            hurricane_data1[j].insert(0,list_of_hurricanes1[i][2])

    start = finish + 1

f.close()

with open("northPacific_clean.csv","w") as filehandle1:
    writer = csv.writer(filehandle1)
    writer.writerow(["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"])
    writer.writerows(hurricane_data1)



    
