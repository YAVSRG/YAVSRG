f = open("../Tables/crescent.txt")
table_rows = f.readlines()
f.close()
f = open("output.txt")
output = f.readlines()
f.close()

new_rows = []

for r in table_rows:
    if "# " in r: continue
    if r not in output: new_rows.append(r)
    
f = open("input.txt", "w+")
for r in new_rows:
    f.write(r.strip() + "\n")