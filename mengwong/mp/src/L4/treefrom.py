import spacy_udpipe
import sys

spacy_udpipe.download("en")
nlp = spacy_udpipe.load("en")

texts = sys.argv[1:]

def removePunct(ls):
  return [l for l in ls if l[2] != 'punct']

def getTree(text):
  for token in nlp(text):
    trees = []
    Tree = {}
    if token.dep_.lower() == 'root':
      Tree['root'] = [token.text, token.lemma_, token.dep_.lower(), token, text]
      unfiltered = [[child.text, child.lemma_, child.dep_, child, text] for child in token.children]
      Tree['children'] = unfiltered
      Tree['children'] = removePunct(unfiltered)
      trees.append(Tree)
      return trees

# get different elements from token [text, lemma, dep, whole token]
def getElements(trees, el):
  fun_elements = []
  for tree in trees:
    fun_elements.append(tree['root'][el])
    children = tree['children']
    for child in children:
      if isinstance(child[el], int) == False:
        fun_elements.append(replaceColon(child[el]))
  return(fun_elements)

def replaceColon(el):
  if el.find(':') != -1:
    ind = el.index(':')
    cap = el[ind+1].upper()
    newStr = el[:ind] + cap + el[ind + 2:]
    return newStr
  return el

def writeFun(trees):
  fun_elements = getElements(trees, 2)
  fun_name = '_'.join(fun_elements)
  fun_elements = [e.replace('case', 'case_') for e in fun_elements]
  fun = fun_name + " : " + ' -> '.join(fun_elements) + ' -> UDS'
  return [fun, getElements(trees, 4)[0]]

# def writeCat(trees):
def getFuns():
  allFuns = []

  for text in texts:
    text = text.rstrip()

    allTrees = getTree(text)

    allFuns.append(writeFun(allTrees))

  return allFuns

print(getFuns())

def uniqueFuns():
  outfile = []

  fun_dict = {}
  for f in getFuns():
    if f[0] not in fun_dict:
      fun_dict[f[0]] = f[1]
  fun_list = []
  for f,s in fun_dict.items():
    outfile.append([f, s])
  return sorted(outfile)

def writeLabels():
  # with open(abstractGrammar + '.label', 'w+') as labelFile:
  labels = []
  for eachFun in uniqueFuns():
    print(eachFun)
    eachLabel = "#fun " + eachFun[0].replace(': root ', 'head')
    labels.push(eachLabel + "\n")
  return labels