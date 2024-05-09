from functools import reduce


def factorial(n):
    return reduce(lambda x, y: x * y, range(1, n + 1), 1)


def run_factorial(number):
    result = factorial(number)
    print(f"The factorial of {number} is {result}")


if __name__ == "__main__":
    run_factorial(0)
    run_factorial(1)
    run_factorial(2)
    run_factorial(3)
    run_factorial(4)
    run_factorial(5)
    run_factorial(6)
    run_factorial(7)
    run_factorial(8)
    run_factorial(9)
    run_factorial(10)
