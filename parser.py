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
            error_message = f"Expected {Operation.PLUS.value}, {Operation.MINUS.value}, {Operation.MULTIPLY.value} or {Operation.DIVIDE.value}"
            error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
            return result.failure(error)

        return result

    def factor(self):
        result = ParserValidator()
        token = self.current_token

        if token.type in ADDITIVE_OPERATORS_NAMES:
            result.register_advancement()
            self.advance()
            factor = result.register(self.factor())

            if result.error:
                return result

            return result.success(UnaryOperationNode(token, factor))

        elif token.type in NUMBER_TYPES:
            result.register_advancement()
            self.advance()
            return result.success(NumberNode(token))

        elif token.type in IDENTIFIERS_NAMES:
            result.register_advancement()
            self.advance()
            variable_node = VariableAccessNode(token)
            return result.success(variable_node)

        elif token.type in EXPRESSION_STARTERS_NAMES:
            result.register_advancement()
            self.advance()
            expression = result.register(self.expression())

            if result.error:
                return result

            if self.current_token.type in EXPRESSION_CLOSERS_NAMES:
                result.register_advancement()
                self.advance()
                return result.success(expression)

            error_message = f"Expected {Punctuation.RIGHT_PARENTHESIS.value}"
            error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
            return result.failure(error)

        error_message = f"Expected {Digit.INT.value}, {Digit.FLOAT.value}, {InWords.IDENTIFIER.name}, {Operation.PLUS.value}, {Operation.MINUS.value} or {Punctuation.LEFT_PARENTHESIS.value}"
        error = InvalidSyntaxError(error_message, token.start_position, token.end_position)
        return result.failure(error)

    def binary_operation(self, func, operations):
        result = ParserValidator()
        left = result.register(func())

        if result.error:
            return result

        while self.current_token.type in operations:
            operation_token = self.current_token
            result.register_advancement()
            self.advance()
            right = result.register(func())

            if result.error:
                return result

            left = BinaryOperationNode(left, operation_token, right)

        return result.success(left)

    def expression(self):
        result = ParserValidator()

        if self.current_token.matches(InWords.KEYWORD.name, 'VAR'):
            result.register_advancement()
            self.advance()

            if self.current_token.type != InWords.IDENTIFIER.name:
                error = InvalidSyntaxError('Expected identifier', self.current_token.start_position, self.current_token.end_position)
                return result.failure(error)

            variable_name = self.current_token
            result.register_advancement()
            self.advance()

            if self.current_token.type != Operation.EQUALS.name:
                error = InvalidSyntaxError("Expected '='", self.current_token.start_position, self.current_token.end_position)
                return result.failure(error)

            result.register_advancement()
            self.advance()
            expression = result.register(self.expression())

            if result.error:
                return result

            variable_node = VariableAssignNode(variable_name, expression)
            return result.success(variable_node)

        operation_result = self.binary_operation(self.term, ADDITIVE_OPERATORS_NAMES)
        node = result.register(operation_result)

        if result.error:
            error_message = f"Expected {Digit.INT.value}, {Digit.FLOAT.value}, {InWords.IDENTIFIER.name}, {InWords.VAR.name}, {Operation.PLUS.value}, {Operation.MINUS.value} or {Punctuation.LEFT_PARENTHESIS.value}"
            error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
            return result.failure(error)

        return result.success(node)

    def term(self):
        return self.binary_operation(self.factor, MULTIPLICATIVE_OPERATORS_NAMES)


class ParserValidator:
    def __init__(self):
        self.error = None
        self.node = None
        self.advance_count = 0

    def register(self, result):
        self.advance_count += result.advance_count

        if result.error:
            self.error = result.error

        return result.node

    def register_advancement(self):
        self.advance_count += 1

    def success(self, node):
        self.node = node
        return self

    def failure(self, error):
        if not self.error or self.advance_count == 0:
            self.error = error

        return self
