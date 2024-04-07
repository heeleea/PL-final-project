from error import Position, IllegalCharError
from parser import Parser
from token_utils import Token, TokenDigit, TokenPunctuation, TokenOperation, TokenUtils


def run_interperter(input, file_name):
    lexer = LexicalAnalysis(input, file_name)
    tokens, error = lexer.create_token_stream()
    if error:
        return None, error

    parse = Parser(tokens)
    ast = parse.create_ats()

    return ast.node, ast.error


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

            else:
                token_type = None

                for enum_class in {TokenOperation, TokenPunctuation}:

                    try:
                        token_type = enum_class(self.current_char)
                        if token_type:
                            break

                    except ValueError:
                        continue

                if token_type:
                    tokens.append(Token(token_type=enum_class[token_type.name].name,
                                        start_position=self.char_position))
                    self.proceed()

                else:
                    start_position = self.char_position.get_position()
                    char = self.current_char
                    self.proceed()

                    return [], IllegalCharError(details=f"'{char}'",
                                                start_position=start_position,
                                                end_position=self.char_position)

        tokens.append(Token(token_type=TokenUtils.END.name,
                            start_position=self.char_position))
        return tokens, None

    def detect_number_type(self):
        number_string = ''
        dot_count = 0
        start_position = self.char_position.get_position()

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
            return Token(token_type=TokenDigit.INT.name,
                         value=int(number_string),
                         start_position=start_position,
                         end_position=self.char_position)

        return Token(token_type=TokenDigit.FLOAT.name,
                     value=float(number_string),
                     start_position=start_position,
                     end_position=self.char_position)
