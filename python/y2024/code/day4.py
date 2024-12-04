from pathlib import Path

def _find_values(mtrx: list[list[chr]], value):
    xs = []
    for y, row in enumerate(mtrx):
        for x, v in enumerate(row):
            if v == value:
                xs.append((x, y))
    return xs

def _occurs_with_delta(pos: tuple[int, int], matrix: list[list[chr]], chars: str, dy: int, dx: int) -> bool:
    xmax = len(matrix[0]) - 1
    ymax = len(matrix) - 1
    ddx = dx
    ddy = dy
    for c in chars:
        xpos = pos[0] + ddx
        ypos = pos[1] + ddy
        if xpos < 0 or ypos < 0 or xpos > xmax or ypos > ymax:
            return False
        matrix_value = matrix[ypos][xpos]
        if matrix_value != c:
            return False
        ddx += dx
        ddy += dy
    return True
    
def _part1(matrix: list[list[chr]]):
    xs = _find_values(matrix, "X")
    occurences = 0
    chars = "MAS"
    deltas = [(1, 1), (1, -1), (-1, 1), (-1, -1), (1, 0), (-1, 0), (0, -1), (0, 1)]
    for pos in xs:
        for delta in deltas:
            if _occurs_with_delta(pos, matrix, chars, delta[0], delta[1]):
                occurences += 1
    print(occurences)

def _part2(matrix: list[list[chr]]):
    xmax = len(matrix[0]) - 1
    ymax = len(matrix) - 1
    mid_chars  = _find_values(matrix, "A")
    findstr = list('MAS')
    occurences = 0
    for pos in mid_chars:
        xpos = pos[0]
        ypos = pos[1]
        xvalid = 0 < xpos < xmax
        yvalid = 0 < ypos < ymax
        if not yvalid or not xvalid:
            continue
        # all positions are valid, create the strings
        forward = [matrix[ypos + 1][xpos - 1], 'A', matrix[ypos - 1][xpos + 1]]
        backward = [matrix[ypos - 1][xpos - 1], 'A', matrix[ypos + 1][xpos + 1]]
        fwdvalid = findstr == forward or findstr == list(reversed(forward))
        bckvalid = findstr == backward or findstr == list(reversed(backward))
        if fwdvalid and bckvalid:
            occurences += 1
    print(occurences)


input_path = Path(__file__).parent.parent / "input" / "day4.txt"
with open(input_path, "r", encoding="utf-8") as f:
    input = f.read()
    
    matrix: list[list[chr]] = []
    for row in input.splitlines():
        matrix.append(list(row))
    _part1(matrix)
    _part2(matrix)
