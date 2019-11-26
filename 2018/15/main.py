import heapq

class Unit:
    def __init__(self, team, y, x, atk=3):
        self.team = team
        self.enemy = "G" if team == "E" else "E"
        self.y = y
        self.x = x
        self.health = 200
        self.atk = atk
    
    @property
    def pos(self):
        return (self.y, self.x)

    @property
    def alive(self):
        return self.health > 0

    def attack(self, enemies:["Unit"]):
        target = sorted(enemies, key=lambda e: (e.health, e.pos))[0]
        target.health -= self.atk


class Board:
    def __init__(self, lines, elf_atk=3):
        self.round = 0
        self.walls = set()
        self.units:[Unit] = []
        for y, line in enumerate(lines):
            for x, cell in enumerate(line):
                if cell == "#":
                    self.walls.add((y, x))
                elif cell == "G":
                    self.units.append(Unit(cell, y, x))
                elif cell == "E":
                    self.units.append(Unit(cell, y, x, elf_atk))

    def end_round(self):
        self.round += 1
        self.units = [u for u in self.units if u.alive]
        self.units = sorted(self.units, key=lambda u: u.pos)
        return all(u.team == self.units[0].team for u in self.units)

    def shortest_routes(self, unit:Unit, targets, blocked):
        visited = blocked
        routes = []
        queue = [(0, [unit.pos])]
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
        adjacent_cells = self.adjacent(*unit.pos)
        return {
            u for u in self.units
            if u.alive and u.team != unit.team and u.pos in adjacent_cells
        }

    def find_in_range(self, unit:Unit, blocked):
        in_range = set()
        for u in self.units:
            if u is unit or not u.alive or u.team == unit.team:
                continue
            in_range = in_range.union(self.adjacent(u.y, u.x))

        in_range = in_range.difference(blocked)
        return in_range

    def choose_move(self, unit:Unit):
        blocked = self.walls | {x.pos for x in self.units if unit is not x and x.alive}
        in_range = self.find_in_range(unit, blocked)

        if in_range:
            routes = self.shortest_routes(unit, in_range, blocked)
            if routes and routes[0]:
                return routes[0][0]
    
    def play_round(self):
        for unit in self.units:
            if not unit.alive:
                continue

            adj_enemies = self.adjacent_enemies(unit)
            if not adj_enemies:
                move = self.choose_move(unit)
                if not move:
                    continue
                unit.y, unit.x = move[0], move[1]
            
            adj_enemies = self.adjacent_enemies(unit)
            if adj_enemies:
                unit.attack(adj_enemies)

        return self.end_round()

with open("input.txt") as f:
    lines = f.readlines()

atk = 3
while True:
    board = Board(lines, elf_atk=atk)
    starting_elves = len([u for u in board.units if u.team == 'E'])
    while not board.play_round(): 
        pass
    if atk == 3:
        print((board.round - 1) * sum(u.health for u in board.units))
    if starting_elves == len([u for u in board.units if u.team == 'E']):
        print((board.round - 1) * sum(u.health for u in board.units))
        break
    atk += 1
