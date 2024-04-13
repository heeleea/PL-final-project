from error import Position, IllegalCharError
from token_utils import Token, Digit, Punctuation, Operation, Utils, InWords, KEYWORDS


class LexicalAnalysis:
    def __init__(self, text, file_name):
        self.text = text
        self.file_name = file_name
        self.char_position = Position(index=-1, line=0, column=-1, file_text=self.text, file_name=self.file_name)
        self.current_char = None
        self.proceed()

    def proceed(self):
        self.char_position.advance(self.current_char)
        self.current_char = self.text[self.char_position.index] if self.char_position.index < len(self.text) else None

    def create_token_stream(self):
        tokens = []

        while self.current_char is not None:
            if self.current_char.isspace():
                self.proceed()

            elif self.current_char.isdigit():
                number = self.detect_number_type()
                tokens.append(number)

            elif self.current_char.isalnum():
                variable = self.detect_identifier()
                tokens.append(variable)

            else:
                token_type = None

                for enum_class in {Operation, Punctuation}:

                    try:
                        token_type = enum_class(self.current_char)
                        if token_type:
                            break

                    except ValueError:
                        continue

                if token_type:
                    token = Token(token_type=enum_class[token_type.name].name, start_position=self.char_position)
                    tokens.append(token)
                    self.proceed()

                else:
                    start_position = self.char_position.get_position()
                    char = self.current_char
                    self.proceed()

                    return [], IllegalCharError(details=f"'{char}'", start_position=start_position, end_position=self.char_position)

        token = Token(token_type=Utils.END.name, start_position=self.char_position)
        tokens.append(token)
        return tokens, None

    def detect_number_type(self):
        number_string = ''
        dot_count = 0
        start_position = self.char_position.get_position()

        while self.current_char is not None and (self.current_char.isdigit() or self.current_char == '.'):
            if self.current_char == '.':
                if dot_count == 1:
                    break

                dot_count += 1
                number_string += '.'

            else:
                number_string += self.current_char

            self.proceed()

        if dot_count == 0:
            return Token(token_type=Digit.INT.name, value=int(number_string), start_position=start_position, end_position=self.char_position)

        return Token(token_type=Digit.FLOAT.name, value=float(number_string), start_position=start_position, end_position=self.char_position)

    def detect_identifier(self):
        identifier_str = ''
        start_position = self.char_position.get_position()

        while self.current_char is not None and (self.current_char.isalnum() or self.current_char == '_'):
            identifier_str += self.current_char
            self.proceed()

        token_type = InWords.KEYWORD.name if identifier_str in KEYWORDS else InWords.IDENTIFIER.name
        return Token(token_type, identifier_str, start_position, self.char_position)

