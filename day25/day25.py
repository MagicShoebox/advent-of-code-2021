with open('input.txt') as f:
    grid = [list(line.rstrip()) for line in f]

height, width = len(grid), len(grid[0])

def ndenumerate(grid):
    for y in range(height):
        for x in range(width):
            yield y, x, grid[y][x]

def step(grid):
    new_grid = [['.' for _ in range(width)] for _ in range(height)]
    for y, x, c in ndenumerate(grid):
        match c:
            case 'v':
                new_grid[y][x] = 'v'
            case '>':
                if grid[y][(x + 1) % width] == '.':
                    new_grid[y][(x + 1) % width] = '>'
                else:
                    new_grid[y][x] = '>'
    grid = new_grid
    new_grid = [['.' for _ in range(width)] for _ in range(height)]
    for y, x, c in ndenumerate(grid):
        match c:
            case '>':
                new_grid[y][x] = '>'
            case 'v':
                if grid[(y + 1) % height][x] == '.':
                    new_grid[(y + 1) % height][x] = 'v'
                else:
                    new_grid[y][x] = 'v'
    return new_grid

steps = 1
new_grid = step(grid)
while new_grid != grid:
    grid = new_grid
    steps += 1
    new_grid = step(grid)

print(steps)
