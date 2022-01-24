import pandas as pd
import numpy as np
import re

fields = ['Predicates']

df = pd.read_csv('pdpa_predicates.csv', skipinitialspace=True, usecols=fields)
# See the keys
print(df.keys())
# See content in 'star_name'
# as a series
sentences_series = df.Predicates
print(type(sentences_series))

# as an array
sentences_array = df[["Predicates"]].to_numpy()
print(type(sentences_array))
# print(sentences_array)
# for loop to read array elements which are the lists in the list
print("There are", len(sentences_array), "predicates altogether.")

# remove unwanted characters in senntences
# rgx_list =["[", "]"]

# def clean_text(rgx_list, text):
#     new_text = text
#     for rgx_match in rgx_list:
#         new_text = re.sub(rgx_match, '', new_text)
#     return new_text

# Split the array of sentences into sentences
for i in sentences_array:
    for text in i:
        print (text)
        # print(len(text.split()))

print ("complete")