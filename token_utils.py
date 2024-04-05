from enum import Enum


class TokenOperation(Enum):
    PLUS = '+'
    MINUS = '-'
    DIVIDE = '/'
    MULTIPLY = '*'

    def __str__(self):
        return self.name


class TokenPunctuations(Enum):
    RIGHT_PARENTHESIS = '('
    LEFT_PARENTHESIS = ')'

    def __str__(self):
        return self.name


class TokenDigit(Enum):
    FLOAT = 'FLOAT'
    INT = 'INT'


class Token:
    def __init__(self, token_type, value=None):
        self.token_type = token_type
        self.value = value

    def __repr__(self):
        if not self.value:
            return f"{self.token_type}"

        return f"{self.token_type}:{self.value}"
