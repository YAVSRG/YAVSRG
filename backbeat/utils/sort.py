f = open("input.txt")
new_items = f.readlines()
f.close()
f = open("output.txt")
output = f.readlines()
f.close()

def compare(left, right):
    print(left.strip(), "(<) vs (>)", right.strip())
    return input("Choose the MORE DIFFICULT file: ") not in "<,l"

def insert_sort(items, new_item):
    l = len(items)
    if l == 0:
        return [new_item]
    elif l == 1:
        if compare(items[0], new_item):
            return items + [new_item]
        else:
            return [new_item] + items
    else:
        m = l // 2
        mid = items[m]
        if compare(mid, new_item):
            return items[0:m+1] + insert_sort(items[m+1:], new_item)
        else:
            return insert_sort(items[0:m], new_item) + items[m:]
            
while new_items:
    output = insert_sort(output, new_items.pop(0))
    
f = open("output.txt", "w+")
for o in output:
    f.write(o.strip() + "\n")
f.close()
f = open("input.txt", "w+")
f.write("")
f.close()