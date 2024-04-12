from error import InvalidSyntaxError
from ast_nodes import NumberNode, BinaryOperationNode, UnaryOperationNode
from token_utils import TokenDigit, TokenOperation, TokenPunctuation, TokenUtils


NUMBER_TYPES = {TokenDigit.INT.value, TokenDigit.FLOAT.value}

ADDITIVE_OPERATORS = {TokenOperation.PLUS.value, TokenOperation.MINUS.value}
MULTIPLICATIVE_OPERATORS = {TokenOperation.MULTIPLY.value, TokenOperation.DIVIDE.value}
EXPRESSION_STARTERS = {TokenPunctuation.LEFT_PARENTHESIS.value}
EXPRESSION_CLOSERS = {TokenPunctuation.RIGHT_PARENTHESIS.value}

ADDITIVE_OPERATORS_NAMES = {TokenOperation.PLUS.name, TokenOperation.MINUS.name}
MULTIPLICATIVE_OPERATORS_NAMES = {TokenOperation.MULTIPLY.name, TokenOperation.DIVIDE.name}
EXPRESSION_STARTERS_NAMES = {TokenPunctuation.LEFT_PARENTHESIS.name}
EXPRESSION_CLOSERS_NAMES = {TokenPunctuation.RIGHT_PARENTHESIS.name}


class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.token_index = -1
        self.advance()

    def advance(self):
        self.token_index += 1

        if self.token_index < len(self.tokens):
            self.current_token = self.tokens[self.token_index]

        return self.current_token

    def create_ats(self):
        result = self.expression()

        if not result.error and self.current_token.token_type != TokenUtils.END.name:
            operation = f"{TokenOperation.PLUS.value}, {TokenOperation.MINUS.value}, {TokenOperation.MULTIPLY.value} or {TokenOperation.DIVIDE.value}"
            return result.failure(InvalidSyntaxError(f"Expected {operation}",
                                                     self.current_token.start_position,
                                                     self.current_token.end_position))

        return result

    def factor(self):
        result = ParserValidator()
        token = self.current_token

        if token.token_type in ADDITIVE_OPERATORS_NAMES:
            result.register(self.advance())
            factor = result.register(self.factor())

            if result.error:
                return result

            return result.success(UnaryOperationNode(token, factor))

        elif token.token_type in NUMBER_TYPES:
            result.register(self.advance())
            return result.success(NumberNode(token))

        elif token.token_type in EXPRESSION_STARTERS_NAMES:
            result.register(self.advance())
            expression = result.register(self.expression())

            if result.error:
                return result

            if self.current_token.token_type in EXPRESSION_CLOSERS_NAMES:
                result.register(self.advance())
                return result.success(expression)

            return result.failure(InvalidSyntaxError(f"Expected {TokenPunctuation.RIGHT_PARENTHESIS.value}",
                                                     self.current_token.start_position,
                                                     self.current_token.end_position))

        return result.failure(InvalidSyntaxError(f"Expected {TokenDigit.INT.value}, {TokenDigit.FLOAT.value}, {TokenOperation.PLUS.value}, {TokenOperation.MINUS.value} or {TokenPunctuation.LEFT_PARENTHESIS.value}",
                                                 token.start_position, token.end_position))

    def binary_operation(self, func, operations):
        result = ParserValidator()
        left = result.register(func())

        if result.error:
            return result

        while self.current_token.token_type in operations:
            operation_token = self.current_token
            result.register(self.advance())
            right = result.register(func())

            if result.error:
                return result

            left = BinaryOperationNode(left, operation_token, right)

        return result.success(left)

    def expression(self):
        return self.binary_operation(self.term, ADDITIVE_OPERATORS_NAMES)

    def term(self):
        return self.binary_operation(self.factor, MULTIPLICATIVE_OPERATORS_NAMES)


class ParserValidator:
    def __init__(self):
        self.error = None
        self.node = None

    def register(self, result):
        if isinstance(result, ParserValidator):
            if result.error:
                self.error = result.error

            return result.node

        return result

    def success(self, node):
        self.node = node
        return self

    def failure(self, error):
        self.error = error
        return self
