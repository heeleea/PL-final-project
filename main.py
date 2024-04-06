from lexical_analysis import run_interperter


while True:
    text = input("> ")
    result, error = run_interperter(text, file_name='<stdin>')

    if error:
        print(error.to_string())
    else:
        print(result)
