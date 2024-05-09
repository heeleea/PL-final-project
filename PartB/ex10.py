from functools import reduce


def concatenate(strings):
    return reduce(lambda x, y: x + ' ' + y, strings)


if __name__ == "__main__":
    result = concatenate(['Hello', 'world', 'this', 'is', 'a', 'test'])
    print(result)

