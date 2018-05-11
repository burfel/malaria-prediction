import csv

'''
Reads in the csv data set of host and pathogen maps and 
returns a list of proportions of host and pathogen proportion.
'''

# computes the proportion of host and pathogen maps 
def compProp(data):
	proportions = [] # [prop_host_maps, prop_pathogen_maps]
	totalMaps = []
	for i in range(len(data)):
		#print(data[i])
		#print(data[i][1])
		totalMaps.append(int(data[i][1]) + int(data[i][2]))
		#print(totalMaps[i])
		proportions.append([int(data[i][1]) / int(totalMaps[i]), int(data[i][2]) / int(totalMaps[i])])
	#print(proportions)
	return proportions

def prop():
	f = open('../../data/hg_pf_readcounts.csv', "r")
	lines = f.read().split("\n")[1:] # "\r\n" if needed, skipping header line
	data = []

	#print("data set of host and pathogen maps: \t")
	for line in lines:
	    if line != "": # add other needed checks to skip titles
	        cols = line.split(",")
	        data.append(cols)
	        #print(cols)
	#print(data)
	#print(len(data))

	prop = compProp(data)
	#print("host and pathogen proportions of the samples = ", prop)

	return prop