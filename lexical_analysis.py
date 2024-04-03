from token_streamer import Token, TokenDigit, TokenPunctuations, TokenOperation


class LexicalAnalysis:
    def __init__(self, text):
        self.text = text
        self.char_position = -1
        self.current_char = None
        self.proceed()

    def proceed(self):
        self.char_position += 1
        self.current_char = self.text[self.char_position] if self.char_position < len(self.text) else None

    def create_token_stream(self):
        tokens = []

        while self.current_char is not None:
            if self.current_char.isspace():
                if self.current_char == '/n':
                    # TODO: implement position
                    pass
                self.proceed()

            elif self.current_char.isdigit():
                self.detect_number_type()
                pass

            else:
                token_type = None

                for enum_class in {TokenOperation, TokenPunctuations}:
                    token_type = enum_class.value2member_map.get(self.current_char)
                    if token_type:
                        break

                if token_type:
                    tokens.append(Token(enum_class[token_type.name], self.current_char))
                    self.proceed()

                else:
                    # TODO: raise illegal character exception
                    pass

    def detect_number_type(self):
        pass
