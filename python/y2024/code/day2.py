from pathlib import Path

def _is_safe_report(report: list[int]) -> bool:
    prev = None
    increasing = None
    for v in report:
        if prev is None:
            prev = v
            continue
        elif increasing is None:
            if prev < v:
                increasing = True
            else:
                increasing = False
        elif increasing and prev > v:
            return False
        elif not increasing and prev < v:
            return False
        diff = abs(prev - v)
        if diff > 3:
            return False
        elif diff < 1:
            return False
        prev = v
    return True

def _is_safe_report2(report: list[int]) -> bool:
    for i in range(len(report)):
        skipped_report = report[:i] + report[i+1:]
        if _is_safe_report(skipped_report):
            return True
    return False

def _part1(reports: list[list[int]]):
    safe_reports = 0
    for report in reports:
        if _is_safe_report(report):
            # print("report {0} is safe".format(report))
            safe_reports +=1
    print(safe_reports)
    
def _part2(reports: list[list[int]]):
    safe_reports = 0
    for report in reports:
        if _is_safe_report2(report):
            # print("report {0} is safe".format(report))
            safe_reports +=1
    print(safe_reports)

input_path = Path(__file__).parent.parent / "input" / "day2.txt"
with open(input_path, "r", encoding="utf-8") as f:
    lines = f.readlines()
    # lines = """7 6 4 2 1
    # 1 2 7 8 9
    # 9 7 6 2 1
    # 1 3 2 4 5
    # 8 6 4 4 1
    # 1 3 6 7 9""".split("\n")
    reports: list[list[int]] = []
    for line in lines:
        reports.append([int(v) for v in line.split(" ") if len(v.strip()) > 0])
    # ex = [int(v) for v in "9 7 6 2 1".split(" ") if len(v.strip()) > 0]
    # print(_is_safe_report(ex))
    # _part1(reports)
    _part2(reports)