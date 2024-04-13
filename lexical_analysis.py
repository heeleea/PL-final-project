from error import Position, IllegalCharError, ExpectedCharError
from token_utils import Token, Digit, Punctuation, ArithmeticOperator, Utils, InWords, KEYWORDS, ComparisonOperator, OperatorPrefix


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
                self.proceed()

            elif self.current_char.isdigit():
                number = self.detect_number_type()
                tokens.append(number)

            elif self.current_char.isalnum():
                variable = self.detect_identifier()
                tokens.append(variable)

            elif self.current_char in OPERATOR_PREFIXES:
                operator = OperatorPrefix(self.current_char)
                #self.proceed()
                method = self.operators_handlers_factory(operator)
                token = method()
                tokens.append(token)

            else:
                token_type = None

                for enum_class in {ArithmeticOperator, Punctuation}:

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

    def operators_handlers_factory(self, operator):
        operator_functions = {
            OperatorPrefix.NOT_EQUALS: self.detect_not_equals,
            OperatorPrefix.EQUALS: self.detect_comparison,
            OperatorPrefix.LESS_THAN: self.detect_less_than,
            OperatorPrefix.GREATER_THAN: self.detect_greater_than,
        }

        operator_function = operator_functions.get(operator, self.method_not_found)
        return operator_function

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
        start_position = self.char_position.get_position()

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
        start_position = self.char_position.get_position()
        self.proceed()

        if self.current_char == '=':
            self.proceed()
            token = Token(token_type=OperatorPrefix.NOT_EQUALS.name,
                          start_position=start_position,
                          end_position=self.char_position)
            return token, None

        self.proceed()
        return None, ExpectedCharError(start_position, self.char_position, "'=' (after '!')")

    def detect_comparison(self):
        token_type = OperatorPrefix.EQUALS
        start_position = self.char_position.get_position()
        self.proceed()

        if self.current_char == '=':
            self.proceed()
            token_type = ComparisonOperator.COMPARISON

        token = Token(token_type=token_type.name,
                      start_position=start_position,
                      end_position=self.char_position)

        return token

    def detect_less_than(self):
        token_type = OperatorPrefix.LESS_THAN
        start_position = self.char_position.get_position()
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
        start_position = self.char_position.get_position()
        self.proceed()

        if self.current_char == OperatorPrefix.EQUALS.value:
            self.proceed()
            token_type = ComparisonOperator.GREATER_THAN_EQUALS

        token = Token(token_type=token_type.name,
                      start_position=start_position,
                      end_position=self.char_position)
        return token

    def method_not_found(self):
        pass
