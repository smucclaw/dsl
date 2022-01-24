import spacy_udpipe
import sys
import treefrom
import time
import os
import shutil
from pathlib import Path

filename = sys.argv[-2]
print('load ', filename)
abstractGrammar = sys.argv[-1]
print('load abstract ', abstractGrammar)

# massage the ud_relations to only have the labels
def extractUDLabels(line):
  words = line.partition( ": ")
  label = words[0]
  if label == 'case':
    label = 'case_'
  newLabel = treefrom.replaceColon(label)
  return(newLabel)

# get categories from ud_relations
def getCats():
  udLabels = []
  for line in open(os.path.join(sys.path[0], "ud_relations"), "r"):
    labels = extractUDLabels(line)
    udLabels.append(labels)
  return udLabels

def coerceFunsAbs(cat):
  return [(cat + "_"), ":", "X", "->", cat, ";"]

def coerceFunsConcrete(cat):
  return [(cat + "_"), "x", "= TODO ;"]

def writeLabels():
  with open(abstractGrammar + '.label', 'w+') as labelFile:
    for eachFun in treefrom.uniqueFuns():
      eachLabel = "#fun " + eachFun[0].replace(': root ', 'head')
      labelFile.write(eachLabel + "\n")

# create an abstract GF file with user entered name
def makeAbstractGF(userGrammar):
  abstractGF = open (abstractGrammar + ".gf", "w+")
  abstractGF.truncate(0)
  abstractGF.seek(0)
  abstractGF.write(
            "abstract "
          + abstractGrammar
          + " = {"
          + "\n\n\tflags"
          + "\n\t\tstartcat = UDS ;"
          + "\n\n\tcat"
          )

  for line in getCats():
    abstractGF.write("\n\t\t" + line)
    abstractGF.write(" ;")

  abstractGF.write(
    "\n\n\t -- coercion funs"
    + "\n\n\tfun"
  )

  # get coercedFuns
  for line in getCats():
    abstractGF.write("\n\t\t" + " ".join(coerceFunsAbs(line)))

  abstractGF.write( "\n\n\tfun\n" )
  print('length of unique funs ', len(treefrom.uniqueFuns()))
  for line in treefrom.uniqueFuns():
    abstractGF.write("\t\t" + line[0] + " ;\n")
    abstractGF.write("\t--" + line[1] + " ;\n\n")
  abstractGF.write("}")
  abstractGF.close()



def makeConcreteGF(userGrammar):
  concreteGF = open (abstractGrammar+ "Eng.gf", "w+")
  concreteGF.truncate(0)
  concreteGF.seek(0)
  concreteGF.write(
          "concrete "
        + abstractGrammar
        + "Eng of "
        + abstractGrammar
        + " = {"
        + "\n\n\tlincat"
        + "\n"
        )
  for line in getCats():
    concreteGF.write("\n\t\t"
                    + line
                    + " = X ;"
                    )
  concreteGF.write(
         "\n\n\tlin"
        + "\n\t\t-- the coercion funs"
        )
  for line in getCats():
    concreteGF.write("\n\t\t" + " ".join(coerceFunsConcrete(line)))

  concreteGF.write("\n\n\t\t-- the actual funs")

  for line in treefrom.uniqueFuns():
      function  = line[0].partition( ": ")
      fun = function[2]
      concreteGF.write("\n\t\t-- : " + fun)
      funName = function[0]
      simpleFuns = fun.replace("-> ", "")
      argFuns = simpleFuns.replace("UDS", "")
      concreteGF.write("\n\t\t"
                      + funName
                      + argFuns
                      + "= TODO ;"
                      )
  concreteGF.write("\n}")
  concreteGF.close()

writeLabels()
makeAbstractGF(abstractGrammar)
makeConcreteGF(abstractGrammar)

print("check")
print(getCats())