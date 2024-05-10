from run import execute, print_result, get_multiline_input

print("Enter your code (press Enter on an empty line to finish):")

while True:
    text = get_multiline_input()
    if text.strip() == "":
        continue

    result, error = execute(text, file_name='<stdin>')
    if error:
        print(error.to_string())
    elif result:
        print_result(result)
