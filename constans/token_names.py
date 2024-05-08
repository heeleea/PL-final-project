from enum import Enum

KEYWORDS = ['VAR', 'AND', 'OR', 'NOT', 'IF', 'THEN', 'ELIF', 'ELSE', 'FOR', 'TO', 'STEP', 'WHILE', 'STRING', 'FUNC', 'BLOCK']


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
    SEMICOLON = ';'
    BACKSLASHN = '\n'

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
    NEWLINE = 'NEWLINE'


class Utils(Enum):
    END = 'END'
