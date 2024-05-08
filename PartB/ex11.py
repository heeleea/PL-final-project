def cumulative_sum_of_squares_even(data):
    return list(map(
        lambda sublist: sum(
            map(lambda x: (lambda y: y*y)(x),
                filter(lambda x: (lambda y: y % 2 == 0)(x), sublist))
        ),
        data
    ))


if __name__ == "__main__":
    data = [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]]
    result = cumulative_sum_of_squares_even(data)
    print(result)
