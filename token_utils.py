from enum import Enum


class TokenOperation(Enum):
    PLUS = '+'
    MINUS = '-'
    DIVIDE = '/'
    MULTIPLY = '*'

    def __str__(self):
        return self.name


class TokenPunctuation(Enum):
    RIGHT_PARENTHESIS = '('
    LEFT_PARENTHESIS = ')'

    def __str__(self):
        return self.name


class TokenDigit(Enum):
    FLOAT = 'FLOAT'
    INT = 'INT'


class TokenUtils(Enum):
    END = 'END'


class Token:
    def __init__(self, token_type, value=None, start_position=None, end_position=None):
        self.token_type = token_type
        self.value = value

        if start_position:
            self.start_position = start_position.get_position()
            self.end_position = start_position.get_position()
            self.end_position.advance()

        if end_position:
            self.end_position = end_position

    def __repr__(self):
        if not self.value:
            return f"{self.token_type}"

        return f"{self.token_type}:{self.value}"
