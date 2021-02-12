
import os.path as path
import argparse

#ARGUMENTS
#create parser object, declare args with imput file and aminoacid str
parseltongue = argparse.ArgumentParser(description="Filter")
parseltongue.add_argument("-i", "--input",metavar="path/to/file",
  help="File with Markov Sir results from R script",required=True)
args = parseltongue.parse_args() #parse args
#save file path in own variable
FilePath = args.input

#initiate filter variables
header=''
lines=[]
with open(FilePath, 'r') as source:
    for i,line in enumerate(source):
        if (i<2):
            header+=line[4:len(line)-2]
            continue
        else:
            if (line[0]=='['):
                k=0
                for char in line[4:]:
                    if char=='\s':
                        k+=1
                    else:
                        break
                lines.append(line[4+k:])
            else:
                break
    #print header
    line=header.rstrip().split()
    for j,column in enumerate(line):
        if(j<len(line)-1):
            print(column, end=',')
        else:
            print(column)
    #print content
    for line in lines:
        line=line.rstrip().split()
        for j,column in enumerate(line):
            if(j<len(line)-1):
                print(column, end=',')
            else:
                print(column)
