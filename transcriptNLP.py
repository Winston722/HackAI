from pandas import read_csv
import numpy as np
import matplotlib.pyplot as plt
from sklearn.feature_extraction.text import TfidfVectorizer
import time
import easygui as eg


transcripts = read_csv("transcripts.csv")
# print(transcripts)
vectorizer = TfidfVectorizer(max_df=0.5, min_df=5, stop_words='english')
vectorized_transcripts = vectorizer.fit(transcripts["transcript"])
print(len(vectorized_transcripts.get_feature_names()))
print(vectorized_transcripts.get_feature_names())

# print("Do stuff!")
