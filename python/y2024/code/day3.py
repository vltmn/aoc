from pathlib import Path
import re

DO_STR = 'do()'
DONT_STR = 'don\'t()'
pattern = r'mul\((\d{1,3}),(\d{1,3})\)'
regex = re.compile(pattern)

def _part1(d: str):  
    found = regex.findall(d)
    s = 0
    for p in found:
        s += int(p[0]) * int(p[1])
    print(s)

def _find_all_indices(v: str, sub: str):
    indexes = []
    index = v.find(sub)
    while index != -1:
        indexes.append(index)
        index = v.find(sub, index + 1)
    return indexes

def _part2(d: str):
    matches = regex.finditer(d)
    do_idxs = [0]
    do_idxs.extend(_find_all_indices(d, DO_STR))
    dont_idxs = _find_all_indices(d, DONT_STR)
    s = 0
        
    for match in matches:
        start = match.start()
        # skip matches where start is greater than
        do_idx = [x for x in do_idxs if x < start]
        dont_idx = [x for x in dont_idxs if x < start]
        if len(dont_idx) > 0 and dont_idx[-1] > do_idx[-1]:
            continue
        
        s += int(match.group(1)) * int(match.group(2))
    print(s)
    
input_path = Path(__file__).parent.parent / "input" / "day3.txt"
with open(input_path, "r", encoding="utf-8") as f:
    input = f.read()
    _part1(input)
    _part2(input)
