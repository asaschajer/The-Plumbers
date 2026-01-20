import json
import csv

with open("journalists.json", encoding="utf-8") as f:
    data = json.load(f)

# collect all column names from keys into a set
column_names = set()
for item in data:
    column_names.update(item.keys())

column_names = sorted(column_names)

# write csv
with open("journalists.csv", "w", newline="", encoding="utf-8") as f:
    writer = csv.DictWriter(f, fieldnames=column_names)
    writer.writeheader()

    for item in data:
        row = {}
        for key in column_names:
            value = item.get(key, "")

            if type(value) is list:
                value = " | ".join(map(str, value))

            row[key] = value

        writer.writerow(row)
