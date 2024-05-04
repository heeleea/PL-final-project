from enum import Enum

KEYWORDS = ['VAR', 'AND', 'OR', 'NOT', 'IF', 'THEN', 'ELIF', 'ELSE', 'FOR', 'TO', 'STEP', 'WHILE', 'STRING', 'FUNC']


class ArithmeticOperator(Enum):
    PLUS = '+'
    MINUS = '-'
    DIVIDE = '/'
    MULTIPLY = '*'
    EQUALS = '='
    POWER = '^'

    def __str__(self):
        return self.name


class OperatorPrefix(Enum):
    EQUALS = "="
    NOT_EQUALS = "!"
    LESS_THAN = "<"
    GREATER_THAN = ">"


class ComparisonOperator(Enum):
    EQUALS = '='
    COMPARISON = '=='
    NOT_EQUALS = "!="
    LESS_THAN = "<"
    GREATER_THAN = ">"
    LESS_THAN_EQUALS = "<="
    GREATER_THAN_EQUALS = ">="
    NOT = "NOT"
    OR = "OR"
    AND = "AND"


class Conditions(Enum):
    IF = 'IF'
    THEN = 'THEN'
    ELIF = 'ELIF'
    ELSE = 'ELSE'


class Punctuation(Enum):
    LEFT_PARENTHESIS = '('
    RIGHT_PARENTHESIS = ')'
    LEFT_SQUARE = '['
    RIGHT_SQUARE = ']'
    COMMA = ','
    FUNCTION_ASSIGNMENT = '~'
    STRING = '"'


    def __str__(self):
        return self.name


class Digit(Enum):
    FLOAT = 'FLOAT'
    INT = 'INT'


class InWords(Enum):
    IDENTIFIER = 'IDENTIFIER'
    KEYWORDS = 'KEYWORDS'
    VAR = 'VAR'
    AND = 'AND'
    OR = 'OR'
    NOT = 'NOT'
    IF = 'IF'
    THEN = 'THEN'
    ELIF = 'ELIF'
    ELSE = 'ELSE'
    FOR = 'FOR'
    TO = 'TO'
    STEP = 'STEP'
    WHILE = 'WHILE'
    FUNC = 'FUNC'
    STRING = 'STRING'
    LIST = 'LIST'


class Utils(Enum):
    END = 'END'


class Token:
    def __init__(self, token_type, value=None, start_position=None, end_position=None):
        self.type = token_type
        self.value = value

        if start_position:
            self.start_position = start_position.get_copy()
            self.end_position = start_position.get_copy()
            self.end_position.advance()

        if end_position:
            self.end_position = end_position

    def matches(self, token_type, value):
        return self.type == token_type and self.value == value

    def __repr__(self):
        if self.value or self.value == 0:
            return f'{self.type}:{self.value}'

        return f'{self.type}'


