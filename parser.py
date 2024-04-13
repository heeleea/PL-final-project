from error import InvalidSyntaxError
from ast_nodes import NumberNode, BinaryOperationNode, UnaryOperationNode, VariableAccessNode, VariableAssignNode
from token_utils import Digit, Operation, Punctuation, Utils, InWords


NUMBER_TYPES = {Digit.INT.value, Digit.FLOAT.value}

ADDITIVE_OPERATORS = {Operation.PLUS.value, Operation.MINUS.value}
MULTIPLICATIVE_OPERATORS = {Operation.MULTIPLY.value, Operation.DIVIDE.value}
EXPRESSION_STARTERS = {Punctuation.LEFT_PARENTHESIS.value}
EXPRESSION_CLOSERS = {Punctuation.RIGHT_PARENTHESIS.value}
IDENTIFIERS = {}

ADDITIVE_OPERATORS_NAMES = {Operation.PLUS.name, Operation.MINUS.name}
MULTIPLICATIVE_OPERATORS_NAMES = {Operation.MULTIPLY.name, Operation.DIVIDE.name}
EXPRESSION_STARTERS_NAMES = {Punctuation.LEFT_PARENTHESIS.name}
EXPRESSION_CLOSERS_NAMES = {Punctuation.RIGHT_PARENTHESIS.name}
IDENTIFIERS_NAMES = {InWords.IDENTIFIER.name}


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

        if not result.error and self.current_token.type != Utils.END.name:
            operation = f"{Operation.PLUS.value}, {Operation.MINUS.value}, {Operation.MULTIPLY.value} or {Operation.DIVIDE.value}"
            error = InvalidSyntaxError(f"Expected {operation}", self.current_token.start_position, self.current_token.end_position)
            return result.failure(error)

        return result

    def factor(self):
        result = ParserValidator()
        token = self.current_token

        if token.type in ADDITIVE_OPERATORS_NAMES:
            result.register(self.advance())
            factor = result.register(self.factor())

            if result.error:
                return result

            return result.success(UnaryOperationNode(token, factor))

        elif token.type in NUMBER_TYPES:
            result.register(self.advance())
            return result.success(NumberNode(token))

        elif token.type in IDENTIFIERS_NAMES:
            result.register(self.advance())
            variable_node = VariableAccessNode(token)
            return result.success(variable_node)

        elif token.type in EXPRESSION_STARTERS_NAMES:
            result.register(self.advance())
            expression = result.register(self.expression())

            if result.error:
                return result

            if self.current_token.type in EXPRESSION_CLOSERS_NAMES:
                result.register(self.advance())
                return result.success(expression)

            error = InvalidSyntaxError(f"Expected {Punctuation.RIGHT_PARENTHESIS.value}", self.current_token.start_position, self.current_token.end_position)
            return result.failure(error)

        error = InvalidSyntaxError(f"Expected {Digit.INT.value}, {Digit.FLOAT.value}, {Operation.PLUS.value}, {Operation.MINUS.value} or {Punctuation.LEFT_PARENTHESIS.value}", token.start_position, token.end_position)
        return result.failure(error)

    def binary_operation(self, func, operations):
        result = ParserValidator()
        left = result.register(func())

        if result.error:
            return result

        while self.current_token.type in operations:
            operation_token = self.current_token
            result.register(self.advance())
            right = result.register(func())

            if result.error:
                return result

            left = BinaryOperationNode(left, operation_token, right)

        return result.success(left)

    def expression(self):
        result = ParserValidator()

        if self.current_token.matches(InWords.KEYWORD.name, 'VAR'):
            result.register(self.advance())

            if self.current_token.type != InWords.IDENTIFIER.name:
                error = InvalidSyntaxError('Expected identifier', self.current_token.start_position, self.current_token.end_position)
                return result.failure(error)

            variable_name = self.current_token
            result.register(self.advance())

            if self.current_token.type != Operation.EQUALS.name:
                error = InvalidSyntaxError("Expected '='", self.current_token.start_position, self.current_token.end_position)
                return result.failure(error)

            result.register(self.advance())
            expression = result.register(self.expression())

            if result.error:
                return result

            variable_node = VariableAssignNode(variable_name, expression)
            return result.success(variable_node)

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
