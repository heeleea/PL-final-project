from error import Position, IllegalCharError
from token_utils import Token, TokenDigit, TokenPunctuations, TokenOperation


def run_lexical_analysis(input):
    lexer = LexicalAnalysis(input)
    tokens, error = lexer.create_token_stream()

    return tokens, error


class LexicalAnalysis:
    def __init__(self, text):
        self.text = text
        self.char_position = Position(index=-1, line=0, column=-1)
        self.current_char = None
        self.proceed()

    def proceed(self):
        self.char_position.advance(self.current_char)
        self.current_char = self.text[self.char_position.index] if self.char_position.index < len(self.text) else None

    def create_token_stream(self):
        tokens = []

        while self.current_char is not None:
            if self.current_char.isspace():
                if self.current_char == '/n':
                    self.proceed()

            elif self.current_char.isdigit():
                number = self.detect_number_type()
                tokens.append(number)

            else:
                token_type = None

                for enum_class in {TokenOperation, TokenPunctuations}:

                    try:
                        token_type = enum_class(self.current_char)
                        if token_type:
                            break

                    except ValueError:
                        continue

                if token_type:
                    tokens.append(Token(enum_class[token_type.name]))
                    self.proceed()

                else:
                    start_position = self.char_position.get_position()
                    char = self.current_char
                    self.proceed()

                    return [], IllegalCharError(error_details=f"'{char}'",
                                                start_position=start_position,
                                                end_position=self.char_position)

        return tokens, None

    def detect_number_type(self):
        number_string = ''
        dot_count = 0

        while self.current_char is not None and self.current_char.isdigit() or self.current_char == '.':
            if self.current_char == '.':
                if dot_count == 1:
                    break

                dot_count += 1
                number_string += '.'

            else:
                number_string += self.current_char

            self.proceed()

        if dot_count == 0:
            return Token(TokenDigit.INT.name, int(number_string))

        return Token(TokenDigit.FLOAT.name, float(number_string))
