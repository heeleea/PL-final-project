from entities.token import Token
from entities.position import Position
from error import IllegalCharError, ExpectedCharError
from constans.token_names import Digit, Punctuation, ArithmeticOperator, Utils, InWords, KEYWORDS, ComparisonOperator, OperatorPrefix

SPACING = [Punctuation.TAB.value, Punctuation.BACKSLASHN.value]
OPERATOR_PREFIXES = [operator.value for operator in OperatorPrefix.__members__.values()]


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
                if self.current_char in SPACING:
                    new_line = self.create_end_of_line_token()
                    tokens.append(new_line)

                else:
                    self.proceed()

            elif self.current_char.isdigit():
                number = self.detect_number_type()
                tokens.append(number)

            elif self.current_char.isalnum():
                variable = self.detect_identifier()
                tokens.append(variable)

            elif self.current_char == OperatorPrefix.NOT_EQUALS.value:
                token, error = self.detect_not_equals()
                if error:
                    return [], error

                tokens.append(token)

            elif self.current_char == Punctuation.STRING.value:
                token, error = self.detect_string()
                if error:
                    return [], error

                tokens.append(token)

            elif self.current_char == Punctuation.SEMICOLON.value:
                new_line = self.create_end_of_line_token()
                tokens.append(new_line)

            else:
                function, token_type, enum_class = self.token_handlers_factory()
                if function is not None:
                    if token_type is not None and enum_class is not None:
                        token = function(token_type, enum_class)
                    else:
                        token = function()

                    if token:
                        tokens.append(token)

                else:
                    start_position = self.char_position.get_copy()
                    char = self.current_char
                    self.proceed()

                    return [], IllegalCharError(details=f"'{char}'", start_position=start_position, end_position=self.char_position)

        token = Token(token_type=Utils.EOF.name, start_position=self.char_position)
        tokens.append(token)
        return tokens, None

    def token_handlers_factory(self):
        enum_class, token_type = self.get_current_token_type()

        special_handlers = {
            OperatorPrefix.EQUALS: self.detect_comparison,
            OperatorPrefix.LESS_THAN: self.detect_less_than,
            OperatorPrefix.GREATER_THAN: self.detect_greater_than,
        }

        if enum_class is None and token_type is None:
            return None, None, None

        elif token_type and token_type in special_handlers.keys():
            function = special_handlers.get(token_type)
            return function, None, None

        return self.simple_token_handler, enum_class, token_type

    def simple_token_handler(self, enum_class, token_type):
        start_position = self.char_position.get_copy()
        self.proceed()
        token = Token(token_type=enum_class[token_type.name].name,
                      start_position=start_position,
                      end_position=self.char_position)
        return token

    def get_current_token_type(self):
        for enum_class in [OperatorPrefix, Punctuation, ArithmeticOperator]:
            try:
                token_type = enum_class(self.current_char)
                return enum_class, token_type

            except ValueError:
                continue

        return None, None

    def detect_number_type(self):
        number_string = ''
        dot_count = 0
        start_position = self.char_position.get_copy()

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
            token = Token(token_type=Digit.INT.name,
                          value=int(number_string),
                          start_position=start_position,
                          end_position=self.char_position)
            return token

        token = Token(token_type=Digit.FLOAT.name,
                      value=float(number_string),
                      start_position=start_position,
                      end_position=self.char_position)
        return token

    def detect_identifier(self):
        identifier_str = ''
        start_position = self.char_position.get_copy()

        while self.current_char is not None and (self.current_char.isalnum() or self.current_char == '_'):
            identifier_str += self.current_char
            self.proceed()

        token_type = InWords.KEYWORDS.name if identifier_str in KEYWORDS else InWords.IDENTIFIER.name
        token = Token(token_type=token_type,
                      value=identifier_str,
                      start_position=start_position,
                      end_position=self.char_position)
        return token

    def detect_not_equals(self):
        start_position = self.char_position.get_copy()
        self.proceed()

        if self.current_char == '=':
            self.proceed()
            token = Token(token_type=OperatorPrefix.NOT_EQUALS.name,
                          start_position=start_position,
                          end_position=self.char_position)
            return token, None

        self.proceed()
        return None, ExpectedCharError("'=' (after '!')", start_position, self.char_position)

    def detect_comparison(self):
        token_type = OperatorPrefix.EQUALS
        start_position = self.char_position.get_copy()
        self.proceed()

        if self.current_char == '=':
            self.proceed()
            token_type = ComparisonOperator.COMPARISON

        token = Token(token_type=token_type.name,
                      start_position=start_position,
                      end_position=self.char_position)

        return token

    def detect_string(self):
        string = ''
        token_type = InWords.STRING
        start_position = self.char_position.get_copy()
        escape_character = False
        self.proceed()

        escape_characters = {
            'n': '\n',
            't': '\t'
        }

        while self.current_char is not None:

            if escape_character:
                string += escape_characters.get(self.current_char, self.current_char)
                escape_character = False

            elif self.current_char == '\\':
                escape_character = True

            elif self.current_char == Punctuation.STRING.value:
                self.proceed()
                token = Token(token_type=token_type.name,
                              value=string,
                              start_position=start_position,
                              end_position=self.char_position)
                return token, None

            else:
                string += self.current_char

            self.proceed()

        error = ExpectedCharError("\" is missing at the end of the string", start_position, self.char_position)
        return None, error

    def detect_less_than(self):
        token_type = OperatorPrefix.LESS_THAN
        start_position = self.char_position.get_copy()
        self.proceed()

        if self.current_char == OperatorPrefix.EQUALS.value:
            self.proceed()
            token_type = ComparisonOperator.LESS_THAN_EQUALS

        token = Token(token_type=token_type.name,
                      start_position=start_position,
                      end_position=self.char_position)
        return token

    def detect_greater_than(self):
        token_type = OperatorPrefix.GREATER_THAN
        start_position = self.char_position.get_copy()
        self.proceed()

        if self.current_char == OperatorPrefix.EQUALS.value:
            self.proceed()
            token_type = ComparisonOperator.GREATER_THAN_EQUALS

        token = Token(token_type=token_type.name,
                      start_position=start_position,
                      end_position=self.char_position)
        return token

    def create_end_of_line_token(self):
        start_position = self.char_position.get_copy()

        while self.current_char in SPACING or self.current_char == Punctuation.SEMICOLON.value:
            self.proceed()

        token = Token(token_type=InWords.NEWLINE.name,
                      start_position=start_position,
                      end_position=self.char_position)
        return token


