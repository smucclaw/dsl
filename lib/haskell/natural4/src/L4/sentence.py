import spacy
import spacy_udpipe
import sys

#nlp = spacy.load("en_core_web_sm")
nlp = spacy_udpipe.load("en")
from spacy import displacy

from spacy_conll import init_parser

con = init_parser(
     "en", "udpipe", include_headers=True
)

def getConll(x):
  conll = x._.conll_str
  return conll

text = sys.argv[1:]

doc_con = con(text)
conll = getConll(doc_con)

for line in conll.splitlines()[2:]:

  line_list = line.split()
  line_list[7] = line_list[7].lower()
  if (line_list[1] == 'the'):
    line_list[4] = "Quant"
    line_list[5] = "FORM=0"
  if (line_list[3] == 'NOUN'):
    make_fun = "FUN=" + line_list[2] + "_N"
  elif (line_list[3] == 'ADJ'):
    make_fun = "FUN=" + line_list[2] + "_A"
  elif (line_list[3] == 'DET'):
    if (line_list[2] == 'the'):
      make_fun = "FUN=DefArt"
    else:
      make_fun = "FUN=" + line_list[2] + "_Det"
  elif (line_list[3] == 'VERB'):
    make_fun = "FUN=" + line_list[2] + line_list[4]
  elif (line_list[3] == 'PRON'):
    make_fun = "FUN=" + line_list[3] + line_list[4]
  elif (line_list[3] == 'CCONJ'):
    make_fun = "FUN=" + line_list[2] + "_Conj"
  elif (line_list[3] == 'AUX' and line_list[2] == 'be'):
    make_fun = "FUN=UseComp"
  else:
    make_fun = "_"
  line_list[-1] = make_fun
  print(line_list)

#morpho(conll)

# nlp_pipe = spacy_udpipe.load("en")
# with open("spacy_udpipe.txt", "w") as f:
#     for token in nlp_pipe(text):
#         f.writelines([token.text, " ", token.lemma_, " ", token.pos_, " ", token.dep_, " ", token.head.text, "\n"])
