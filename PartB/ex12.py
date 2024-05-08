from functools import reduce


def rewritten_func(): #cumulative sum of even numbers
    nums = [1, 2, 3, 4, 5, 6]
    print(reduce(lambda a, b: a + b, map(lambda x: x**2, filter(lambda x: x % 2 == 0, [1, 2, 3, 4, 5, 6]))))


if __name__ == "__main__":
    rewritten_func()

# its impossible to print the cumulative sums of squared even numbers after each number with only using "functools",
# so just the last one.
