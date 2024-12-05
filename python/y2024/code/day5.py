from pathlib import Path

def parse_input(input: str):
    parts = input.split("\n\n")
    
    rules = [tuple(map(int, line.strip().split("|"))) for line in parts[0].split("\n")]
    updates = [list(map(int, update.strip().split(","))) for update in parts[1].split("\n")]

    return rules, updates

def make_set_map(rules: list[tuple[int, int]]) -> dict[int, set[int]]:
    rule_map = {}
    for rule in rules:
        if rule[1] not in rule_map:
            rule_map[rule[1]] = set()
        rule_map[rule[1]].add(rule[0])
    return rule_map

def determine_first_failure(rule_map:  dict[int, set[int]], update: list[int]) -> tuple[int, set[int]]|None:
    found = set()
    all_values = set(update)
    invalid_idx = None
    for idx, page in enumerate(update):
        if page in rule_map:
            expected_preceding = rule_map[page].intersection(all_values)
            missing = expected_preceding.difference(found)
            if len(missing) != 0:
                invalid_idx = (idx, missing)
                break
        found.add(page)
    return invalid_idx


def part1(rules: list[tuple[int, int]], updates: list[list[int]]):
    rule_map = make_set_map(rules)
    middle_page_sum = 0
    # get correct updates
    for update in updates:
        if determine_first_failure(rule_map, update) is not None:
            continue
        middle_page_sum += update[len(update) // 2]
    print(middle_page_sum)

def move_element(lst: list[int], old_index, new_index):
    element = lst.pop(old_index)
    lst.insert(new_index, element)
    
def part2(rules: list[tuple[int, int]], updates: list[list[int]]):
    rule_map = make_set_map(rules)
    middle_page_sum = 0
    for update in updates:
        failure = determine_first_failure(rule_map, update)
        if failure is None:
            continue
        while failure is not None:
            # Make corrections
            for failed_element in failure[1]:
                move_element(update, update.index(failed_element), failure[0])
            failure = determine_first_failure(rule_map, update)
        middle_page_sum += update[len(update) // 2]
    print(middle_page_sum)
    

input_path = Path(__file__).parent.parent / "input" / "day5.txt"
with open(input_path, "r", encoding="utf-8") as f:
    input = f.read()
    rules, updates = parse_input(input)
    part1(rules, updates)
    part2(rules, updates)