import csv

'''
Reads in the csv data set of supplementary information and 
returns a list of these variables that can then be fed into the linear regression model.
'''

def readinDVar():
	f = open('../../data/Supplementary_Dataset.csv', "r")
	lines = f.read().split("\n") # "\r\n" if needed, skipping header line
	dataS = []

	#print("data set of dependent variables: \t")
	for line in lines:
	    if line != "": # add other needed checks to skip titles
	        cols = line.split(",")
	        dataS.append(cols)
	        #print(cols)
	#print(data)
	#print(len(data))

	return dataS