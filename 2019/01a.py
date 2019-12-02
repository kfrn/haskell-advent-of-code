import math


def total_fuel_required():
    masses = read_in_masses()
    fuel_amounts = [calculate_fuel_required(mass) for mass in masses]

    return sum(fuel_amounts)


def read_in_masses():
    with open('./01_input.txt') as file:
        string_numbers = file.read().splitlines()

        return [int(str) for str in string_numbers]


def calculate_fuel_required(mass):
    return math.floor(mass / 3) - 2


total_fuel = total_fuel_required()
print('Total fuel required:', total_fuel) # 3563458
