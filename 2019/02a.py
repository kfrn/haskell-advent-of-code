def intcode_program():
    input_program = read_in_intcode()
    program = update_initial_program_values(input_program)
    chunked_program = [program[i:i + 4]
                       for i in range(0, len(program), 4)]

    for chunk in chunked_program:
        opcode = chunk[0]

        if opcode == 99:
            return program
        else:
            val_1 = program[chunk[1]]
            val_2 = program[chunk[2]]
            new_value = replacement_value(opcode, val_1, val_2)
            insert_position = chunk[3]

            program[insert_position] = new_value

    return program


def read_in_intcode():
    with open('./02_input.txt') as file:
        string_numbers = file.read().split(',')

        return [int(str) for str in string_numbers]


def update_initial_program_values(program):
    program[1] = 12
    program[2] = 2

    return program


def replacement_value(opcode, val_1, val_2):
    if opcode == 1:
        return val_1 + val_2
    elif opcode == 2:
        return val_1 * val_2
    else:
        raise Exception(
            "Invalid opcode! {0} How did this get called?".format(opcode))


"""Replace at line 3 for testing"""
# input = [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]  # output: [3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]
# input = [2, 4, 4, 5, 99, 0]  # output: [2, 4, 4, 5, 99, 9801]


output = intcode_program()
print('Program output is:', output)  # Position 0 = 3790689
