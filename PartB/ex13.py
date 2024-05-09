from functools import reduce


def count_palindromes(list_of_lists):
    return [reduce(lambda count, word: count + (word == word[::-1]), sublist, 0) for sublist in list_of_lists]


if __name__ == "__main__":
    input_list = [["madam", "hello", "cookie"], ["radar", "world", "noon"], ["kayak", "lol", "level"]]
    print(count_palindromes(input_list))
