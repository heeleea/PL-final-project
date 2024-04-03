from lexical_analysis import LexicalAnalysis

while True:
    text = input(">")
    stream_of_tokens = LexicalAnalysis(text)
    print(stream_of_tokens.__repr__())

