import json

letters = 'QWERTYUIOPASDFGHJKLZXCVBNM'
data_name = []
journalists = []

for i in letters:
    n = i + '_people.json'
    data_name.append(n)

# if you want to work with the json file(s) you should do it inside the following for loop
for n in range(26):
    with open(f"data/{data_name[n]}", encoding='utf-8') as file:
        data = json.load(file)
    for d in data:
        occupation = d['ontology/occupation_label']
        if occupation == 'Journalist' or (occupation is list and 'Journalist' in occupation):
            journalists.append(d)
    with open('journalists.json', 'w', encoding='utf-8') as file:
        json.dump(journalists, file, indent=4, ensure_ascii=0)

with open('journalists.json', encoding='utf-8') as file:
    line = json.load(file)


