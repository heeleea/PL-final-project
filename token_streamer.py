from enum import Enum, auto


class TokenOperation(Enum):
    PLUS = '+'
    MINUS = '-'
    DIVIDE = '/'
    MULTIPLY = '*'


class TokenPunctuations(Enum):
    RIGHT_PARENTHESIS = '('
    LEFT_PARENTHESIS = ')'


class TokenDigit(Enum):
    FLOAT = auto()
    INT = auto()
    
    
class Token:
    def __init__(self, token_type, value):
        self.token_type = token_type
        self.value = value

    def __repr__(self):
        if not self.value:
            return f"{self.token_type}"

        return f"{self.token_type}:{self.value}"
