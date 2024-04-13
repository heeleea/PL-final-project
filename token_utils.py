from enum import Enum

KEYWORDS = ['VAR']


class Operation(Enum):
    PLUS = '+'
    MINUS = '-'
    DIVIDE = '/'
    MULTIPLY = '*'
    EQUALS = '='

    def __str__(self):
        return self.name


class Punctuation(Enum):
    LEFT_PARENTHESIS = '('
    RIGHT_PARENTHESIS = ')'

    def __str__(self):
        return self.name


class Digit(Enum):
    FLOAT = 'FLOAT'
    INT = 'INT'


class InWords(Enum):
    IDENTIFIER = 'IDENTIFIER'
    KEYWORD = 'KEYWORDS'


class Utils(Enum):
    END = 'END'


class Token:
    def __init__(self, token_type, value=None, start_position=None, end_position=None):
        self.type = token_type
        self.value = value

        if start_position:
            self.start_position = start_position.get_position()
            self.end_position = start_position.get_position()
            self.end_position.advance()

        if end_position:
            self.end_position = end_position

    def matches(self, token_type, value):
        return self.type == token_type and self.value == value

    def __repr__(self):
        if self.value or self.value == 0:
            return f'{self.type}:{self.value}'

        return f'{self.type}'


