from run import run
from semantical_analysis import List

while True:
    text = input("> ")
    if text.strip() == "":
        continue
    result, error = run(text, file_name='<stdin>')

    if error:
        print(error.to_string())
    elif result:
        if isinstance(result, List):
            if len(result.elements) == 1:
                print(repr(result.elements[0]))
            else:
                print(repr(result.elements))
        else:
            print(result.value)

