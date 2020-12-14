def get_adj(x, y, seats, stop):
    adj = []

    mods = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, -1), (-1, 1), (1, 1), (-1, -1)]
    for (mod_y, mod_x) in mods:
        new_y, new_x = y, x
        while True:
            new_y, new_x = new_y + mod_y, new_x + mod_x

            if new_y < 0 or new_x < 0:
                break

            if new_y > len(seats) or new_x > len(seats):
                break

            try:
                if seats[new_y][new_x] != ".":
                    adj.append(seats[new_y][new_x])
                    break
            except IndexError:
                print(new_y, new_x)
                pass

            if stop:
                break

    return [x for x in adj if x == "#"]


def pprint(seats):
    for row in seats:
        print(row)


def round(seats, max_occu, stop):
    new_seats = []
    for y, row in enumerate(seats):
        new_row = ""
        for x, seat in enumerate(row):

            if seat == ".":
                new_row += "."
                continue

            occu = get_adj(x, y, seats, stop)
            new_seat = "" + seat
            if seat == "L" and len(occu) == 0:
                new_seat = "#"
            elif seat == "#" and len(occu) >= max_occu:
                new_seat = "L"

            new_row += new_seat
        new_seats.append(new_row)
    return new_seats


with open("input.txt") as f:
    lines = [x.replace("\n", "") for x in f.readlines()]

seats = lines
while True:
    new_seats = round(seats, 4, True)
    if new_seats == seats:
        print(sum(sum(1 for s in r if s == "#") for r in seats))
        break
    seats = new_seats

seats = lines
while True:
    new_seats = round(seats, 5, False)
    if new_seats == seats:
        print(sum(sum(1 for s in r if s == "#") for r in seats))
        break
    seats = new_seats
