import re
import heapq as hq
import operator
from itertools import combinations

with open('input.txt') as f:
    lines = [line.rstrip() for line in f]

def do_op(op, x, y):
    match op:
        case 'add':
            return x + y
        case 'mul':
            return x * y
        case 'div':
            return x // y
        case 'mod':
            return x % y
        case 'eql':
            return 1 if x == y else 0

def evaluate(digits):
    regs = {r: 0 for r in ['w','x','y','z']}
    idx = 0
    int_pattern = re.compile(r'-?\d+')
    for line_num, line in enumerate(lines):
        match line.split():
            case ['inp', dst]:
                regs[dst] = digits[idx]
                idx += 1
            case [op, dst, x] if int_pattern.match(x):
                regs[dst] = do_op(op, regs[dst], int(x))
            case [op, dst, src]:
                regs[dst] = do_op(op, regs[dst], regs[src])
    return regs['z']

def neighbors(ds, found_zero):
    def step(x, n):
        return 1+((x-1+n) % 9)
    for n in [4,3,2,1]:
        if found_zero:
            for i,j in combinations(range(14), 2):
                yield tuple(step(ds[x], n) if x==i or x==j else ds[x] for x in range(14))
                yield tuple(step(ds[x], -n) if x==i or x==j else ds[x] for x in range(14))
        else:
            for i in range(14):
                yield tuple(step(ds[x], n) if x==i else ds[x] for x in range(14))
                yield tuple(step(ds[x], -n) if x==i else ds[x] for x in range(14))

def display(digits):
    return ''.join(str(x) for x in digits)

def find_best(start, best, better, score):
    found_zero = False
    queue = [(evaluate(start), score(start), start)]
    visited = {start}
    while len(visited) < 10**4:
        s, _, ds = hq.heappop(queue)
        if s == 0 and better(ds, best):
            found_zero = True
            best = ds
            visited = {v for v in visited if better(v, best)}
        for n in neighbors(ds, found_zero):
            if better(n, best) and n not in visited:
                visited.add(n)
                hq.heappush(queue, (evaluate(n), score(n), n))
    return best

def invert(ds):
    return tuple(9-x for x in ds)

start = tuple(1 for _ in range(14))
best = tuple(1 for _ in range(14))
print(display(find_best(start, best, operator.gt, invert)))

start = tuple(1 for _ in range(14))
best = tuple(9 for _ in range(14))
print(display(find_best(start, best, operator.lt, invert)))
