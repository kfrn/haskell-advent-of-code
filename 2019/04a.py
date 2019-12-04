def password_analyser(range_start, range_end):
    potential_passwords = list(range(range_start, range_end))
    possible_passwords = list(filter(is_possible_password, potential_passwords))

    return len(possible_passwords)


def is_possible_password(password):
    stringified_password = list(str(password))

    return has_double(stringified_password) and digits_increase(stringified_password)


def has_double(password):
    # Does the password contain two adjacent digits the same?

    current_value = None

    for letter in password:
        if current_value == letter:
            return True
        else:
            current_value = letter

    return False


def digits_increase(password):
    # Is each successive digit equal to or greater than the one preceding it?

    current_value = 0

    for string_number in password:
        number = int(string_number)

        if number >= current_value:
            current_value = number
        else:
            return False

    return True


count = password_analyser(158126, 624574)
print('Count is:', count)
