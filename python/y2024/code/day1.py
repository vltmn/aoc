from pathlib import Path
from collections import Counter

def _part1(left: list[int], right: list[int]):
    dist = 0
    for lval, rval in zip(left, right):
        dist += abs(rval - lval)
    print(dist)

def _part2(left: list[int], right: list[int]):
    # Calculate number of occurrences of each number in left and right
    rcount = Counter(right)
    allkeys = list(set(left))
    similarity_score = 0
    for key in allkeys:
        similarity_score += key * rcount[key]
    print(similarity_score)
    

input_path = Path(__file__).parent.parent / "input" / "day1.txt"
with open(input_path, "r", encoding="utf-8") as f:
    lines = f.readlines()
    left = []
    right = []
    for line in lines:
        splitted = line.split(" ")
        left.append(int(splitted[0].strip()))
        right.append(int(splitted[-1].strip()))

    left.sort()
    right.sort()
    _part1(left, right)
    _part2(left, right)


