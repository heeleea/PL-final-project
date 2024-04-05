from lexical_analysis import LexicalAnalysis

while True:
    text = input("> ")
    result, error = run_lexical_analysis(text)

    if error:
        print(error.to_string())
    else:
        print(result)
