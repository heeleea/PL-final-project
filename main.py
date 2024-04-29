from run import run

while True:
    text = input("> ")
    result, error = run(text, file_name='<stdin>')

    if error:
        print(error.to_string())
    elif result:
        print(result.value)

