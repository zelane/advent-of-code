import md5

def decifer_password(door_id):
    password = ""
    index = 0
    while len(password) != 8:
        to_hash = "%s%s" % (door_id, index)
        hex = md5.new(to_hash).hexdigest()
        if hex[:5] == "00000":
            password += hex[5]
            print password
        index += 1

    return password

assert decifer_password("abc") == "18f47a30"

print(decifer_password("cxdnnyjw"))