from collections import defaultdict
import heapq

class Unit:
    def __init__(self, team, y, x):
        self.team = team
        self.enemy = "G" if team == "E" else "E"
        self.x = x
        self.y = y
        self.health = 200
    
    @property
    def pos(self):
        return (self.y, self.x)

    def attack(self, enemies:["Unit"]):
        target = sorted(enemies, key=lambda e: (e.health, e.pos))[0]
        target.health -= 3


class Board:
    def __init__(self):
        self.walls = set()
        self.units:[Unit] = []
        self.height = 0
        self.width = 0

    def render(self, extra=None):
        for y in range(board.height):
            for x in range(board.width):
                p = "."
                if (y, x) in self.walls:
                    p = "#"
                else:
                    for u in self.units:
                        if u.health > 0 and u.pos == (y, x):
                            p = u.team
                print(p, end="")
            print("")
        print(", ".join("%s(%s)" % (u.team, u.health) for u in self.units if u.health > 0))

    def end_round(self):
        self.units = [u for u in self.units if u.health > 0]
        self.units = sorted(self.units, key=lambda u: u.pos)
        return all(u.team == self.units[0].team for u in self.units)

    def shortest_routes(self, u:Unit, targets):
        visited = self.walls | {x.pos for x in self.units if u != x and x.health > 0}
        routes = []
        queue = [(0, [u.pos])]
        while queue:
            distance, route = heapq.heappop(queue)
            current_cell = route[-1]

            if routes and distance > len(routes[0]):
                break

            if current_cell in targets:
                routes.append(route[1:])
                routes = sorted(routes, key=lambda x: len(x))
                continue
            
            if current_cell in visited:
                continue
            visited.add(current_cell)

            options = self.adjacent(*current_cell)
            options = options.difference(visited)
            for cell in options:
                heapq.heappush(queue, (distance + 1, route + [cell]))

        routes = list(filter(lambda r: len(r) >= len(routes[0]), routes))
        return routes

    def adjacent(self, y, x):
        return {cell for cell in ((y, x-1), (y, x+1), (y-1, x), (y+1, x))}

    def adjacent_enemies(self, unit:Unit):
        enemies = []
        adjacent_cells = board.adjacent(*unit.pos)
        for u in board.units:
            if u.health < 1 or u.team == unit.team:
                continue

            if u.pos in adjacent_cells:
                enemies.append(u)

        return enemies

    def find_in_range(self, u:Unit):
        in_range = set()
        for unit in self.units:
            if unit.health < 1 or unit is u or unit.team == u.team:
                continue
            in_range = in_range.union(self.adjacent(unit.y, unit.x))

        occupied = self.walls | {x.pos for x in self.units if u is not x and x.health > 0}
        in_range = in_range.difference(occupied)
        return in_range

    def choose_move(self, u:Unit):
        in_range = self.find_in_range(unit)
        if not in_range:
            return None

        routes = self.shortest_routes(u, in_range)
        if routes and routes[0]:
            return routes[0][0]


with open("input.txt") as f:
    board = Board()
    for y, line in enumerate(f.readlines()):
        for x, cell in enumerate(line):
            if cell == "#":
                board.walls.add((y, x))
            elif cell in ("G", "E"):
                board.units.append(Unit(cell, y, x))
        board.width = x
    board.height = y + 1

for x in range(0, 300):
    for unit in board.units:
        if unit.health < 1:
            continue

        adj_enemies = board.adjacent_enemies(unit)
        if not adj_enemies:
            move = board.choose_move(unit)
            if not move:
                continue
            unit.y, unit.x = move[0], move[1]
        
        adj_enemies = board.adjacent_enemies(unit)
        if adj_enemies:
            unit.attack(adj_enemies)

    victory = board.end_round()
    if victory:
        print("Victory for the {} after {} rounds. {}hp remaining.".format(board.units[0].team, x, sum(u.health for u in board.units)))
        print(x * sum(u.health for u in board.units))
        break
