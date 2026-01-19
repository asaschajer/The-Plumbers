import json
from collections import Counter

letters = 'QWERTYUIOPASDFGHJKLZXCVBNM'
data_name = []

for i in letters:
    n = i + '_people.json'
    data_name.append(n)

# if you want to work with the json file(s) you should do it inside the following for loop
for n in range(26):
    with open(f"The-Plumbers/data/{data_name[n]}", encoding='utf-8') as file:
        data = file.read()

print(data) #test if works
