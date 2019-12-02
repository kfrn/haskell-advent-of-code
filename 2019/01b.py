import math


def total_fuel_requirements():
    masses = read_in_masses()
    fuel_amounts = [fuel_per_module(mass) for mass in masses]

    return sum(fuel_amounts)


def read_in_masses():
    with open('./01_input.txt') as file:
        string_numbers = file.read().splitlines()

        return [int(str) for str in string_numbers]


def fuel_per_module(mass):
    # This could probably be more elegant ... but it gets the job done
    fuel_count = 0
    fuel = calculate_fuel(mass)

    if fuel <= 0:
        pass
    else:
        fuel_count += fuel + fuel_per_module(fuel)

    return fuel_count


def calculate_fuel(mass):
    return math.floor(mass / 3) - 2


total_fuel = total_fuel_requirements()
print('Total fuel required:', total_fuel)  # 5342292
