from pathlib import Path


UP = (0, -1)
RIGHT = (1, 0)
DOWN = (0, 1)
LEFT = (-1, 0)
delta_orders = [UP, RIGHT, DOWN, LEFT]

type Pos = tuple[int, int]


def next_delta(delta):
    return delta_orders[(delta_orders.index(delta) + 1) % len(delta_orders)]


def parse_delta(c: chr) -> tuple[int, int]:
    if c == "^":
        return UP
    elif c == "v":
        return DOWN
    elif c == "<":
        return LEFT
    else:
        return RIGHT


def parse(input: str):
    map = []
    guard_pos = None
    guard_delta = None
    for y, line in enumerate(input.splitlines()):
        map_row = []
        for x, c in enumerate(line):
            if c == ".":
                map_row.append(None)
            elif c == "#":
                map_row.append("#")
            else:
                map_row.append(None)
                guard_pos = (x, y)
                guard_delta = parse_delta(c)
        map.append(map_row)

    return map, guard_pos, guard_delta


def pos_inside(pos: tuple[int, int], max_x: int, max_y: int):
    return 0 <= pos[0] < max_x and 0 <= pos[1] < max_y


def add_positions(a, b):
    return (a[0] + b[0], a[1] + b[1])


def walk(map, guard_pos, guard_delta, obstruction=None):
    max_x = len(map[0])
    max_y = len(map)
    visited = set()

    while (
        pos_inside(guard_pos, max_x, max_y) and (guard_pos, guard_delta) not in visited
    ):
        visited.add((guard_pos, guard_delta))
        next_pos = add_positions(guard_pos, guard_delta)
        if (
            pos_inside(next_pos, max_x, max_y) and map[next_pos[1]][next_pos[0]] == "#"
        ) or (
            obstruction is not None
            and obstruction[0] == next_pos[0]
            and obstruction[1] == next_pos[1]
        ):
            guard_delta = next_delta(guard_delta)
        else:
            guard_pos = next_pos
    return visited, (guard_pos, guard_delta) in visited


def _part1(map, guard_pos, guard_delta):
    visited = walk(map, guard_pos, guard_delta)[0]
    distinct_poses = set(pos for pos, _ in visited)

    print(len(distinct_poses))

def _part2(map, guard_pos: Pos, guard_delta: Pos):
    visited = walk(map, guard_pos, guard_delta)[0]
    distinct_poses = set(pos for pos, _ in visited)

    found_obstructions = 0
    for potential_obstruction in distinct_poses:
        if walk(map, guard_pos, guard_delta, potential_obstruction)[1]:
            found_obstructions += 1
    print(found_obstructions)


input_path = Path(__file__).parent.parent / "input" / "day6.txt"
with open(input_path, "r", encoding="utf-8") as f:
    input = f.read()
    parsed_map, guard_pos, guard_delta = parse(input)
    _part1(parsed_map, guard_pos, guard_delta)
    # Part 2, find number of positions that cause a cycle
    _part2(parsed_map, guard_pos, guard_delta)
