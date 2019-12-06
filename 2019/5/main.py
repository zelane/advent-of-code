with open("input.txt") as f:
    prog = [int(x) for x in f.read().split(",")]

arg_counts = {1: 3, 2: 3, 3: 1, 4: 1, 5: 2, 6: 2, 7: 3, 8: 3, 99: 0}


def vals(args, modes, mem, write=False):
    write_address = [args.pop()] if write else []
    vals = []
    for arg in args:
        mode = int(modes.pop())
        vals.append(arg if mode == 1 else mem[arg])
    return tuple(vals + write_address)


def parse_instruction(ins, mem, i):
    opt = int(ins[-2:])
    arg_count = arg_counts[opt]
    modes = list(ins[:-2].zfill(arg_count))
    args = mem[i:i + arg_count]
    return opt, args, modes, arg_count


def run(mem):
    i = 0
    while True:
        ins = str(mem[i])
        i += 1
        opt, args, modes, arg_count = parse_instruction(ins, mem, i)
        i += arg_count

        if opt == 99:
            break

        elif opt == 1:
            a, b, c = vals(args, modes, mem, True)
            mem[c] = a + b

        elif opt == 2:
            a, b, c = vals(args, modes, mem, True)
            mem[c] = a * b

        elif opt == 3:
            x = input("Input: ")
            mem[args[0]] = int(x)

        elif opt == 4:
            a = vals(args, modes, mem)
            print(a[0])

        elif opt == 5:
            a, b = vals(args, modes, mem)
            i = b if a != 0 else i

        elif opt == 6:
            a, b = vals(args, modes, mem)
            i = b if a == 0 else i

        elif opt == 7:
            a, b, c = vals(args, modes, mem, True)
            mem[c] = 1 if a < b else 0

        elif opt == 8:
            a, b, c = vals(args, modes, mem, True)
            mem[c] = 1 if a == b else 0


run(prog)
