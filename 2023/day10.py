s = """
.....
.F-7.
.|.|.
.L-J.
.....
"""

grid = [c for c in s if c != '\n']
grid_width = 5
grid_height = 5
def g(x,y):
    return grid[y*grid_width + x]

visited = set()
def walk_pipe(prev_x, prev_y, x, y):
    if (x,y) in visited:
        return
    print(x, y)
    visited.add((x,y))

    c = g(x,y)
    dx = x - prev_x
    dy = y - prev_y

    if c == '-':
        walk_pipe(x,y, x + dx, y)
    elif c == '|':
        walk_pipe(x,y, x, y + dy)
    elif c == 'L':
        # Case 1: from top, dx=0 dy=1, go to (x+1,y) == (x+dy, y+dx)
        # Case 2: from right, dx=-1, dy=0, go to (x, y-1) = (x+dy, y+dx)
        walk_pipe(x,y, x + dy, y + dx)
    elif c == 'J':
        # Case 1: from top, dx=0, dy=1, go to (x-1, y) == (x-dy, y-dx)
        # Case 2: from left, dx=1, dy=0,  go to (x, y-1) == (x-dy, y-dx)
        walk_pipe(x,y, x - dy, y - dx)
    elif c == '7':
        # Case 1: from left, dx=1, dy=0, go to (x, y+1) == (x+dy, y+dx)
        # Case 2: from bottom, dx=0, dy=-1,  go to (x-1, y) == (x+dy, y+dx)
        walk_pipe(x,y, x + dy, y + dx)
    elif c == 'F':
        walk_pipe(x,y, x - dy, y - dx)

walk_pipe(1,2, 1,1)
print(len(visited))
