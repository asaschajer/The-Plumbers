import json, re

with open("data/A_people.json", encoding="utf-8") as f:
    data = json.load(f)

pattern = re.compile(r"\bjournalist\b", re.IGNORECASE)
records = Data if isinstance(data, list) else (data.get("people") or data.get("persons") or list(data.values()))

names = []

for person in records:
    if not isinstance(person, dict):
        continue

    occ = person.get("ontology/occupation_label", "")
    if isinstance(occ, list):
        occ_text = " ".join(map(str, occ))
    else:
        occ_text = str(occ)

    if pattern.search(occ_text):
        name = person.get("name") or person.get("title") or person.get("ontology/name_label") or "UNKNOWN"
        names.append(name)

print("Count:", le(names))
print("First 20:", names[:20])


















