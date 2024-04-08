from lexical_analysis import run


while True:
    text = input("> ")
    result, error = run(text, file_name='<stdin>')

    if error:
        print(error.to_string())
    else:
        print(result.value)
