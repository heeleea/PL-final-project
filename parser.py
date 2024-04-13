from error import InvalidSyntaxError
from ast_nodes import NumberNode, BinaryOperationNode, UnaryOperationNode, VariableAccessNode, VariableAssignNode
from token_utils import Digit, ArithmeticOperator, Punctuation, Utils, InWords, ComparisonOperator, KEYWORDS


NUMBER_TYPES = {Digit.INT.value, Digit.FLOAT.value}

ADDITIVE_OPERATORS = {ArithmeticOperator.PLUS.value, ArithmeticOperator.MINUS.value}
MULTIPLICATIVE_OPERATORS = {ArithmeticOperator.MULTIPLY.value, ArithmeticOperator.DIVIDE.value}
EXPRESSION_STARTERS = {Punctuation.LEFT_PARENTHESIS.value}
EXPRESSION_CLOSERS = {Punctuation.RIGHT_PARENTHESIS.value}
IDENTIFIERS = {}
COMPARISON_EXPRESSION = {ComparisonOperator.AND.value, ComparisonOperator.OR.value}

ADDITIVE_OPERATORS_NAMES = {ArithmeticOperator.PLUS.name, ArithmeticOperator.MINUS.name}
MULTIPLICATIVE_OPERATORS_NAMES = {ArithmeticOperator.MULTIPLY.name, ArithmeticOperator.DIVIDE.name}
EXPRESSION_STARTERS_NAMES = {Punctuation.LEFT_PARENTHESIS.name}
EXPRESSION_CLOSERS_NAMES = {Punctuation.RIGHT_PARENTHESIS.name}
IDENTIFIERS_NAMES = {InWords.IDENTIFIER.name}
EXPRESSION_NAMES = {(InWords.KEYWORDS.name, ComparisonOperator.AND.value), (InWords.KEYWORDS.name, ComparisonOperator.OR.value)}
COMPARISON_EXPRESSION_NAMES = {ComparisonOperator.COMPARISON.name, ComparisonOperator.NOT_EQUALS.name, ComparisonOperator.LESS_THAN.name,
                               ComparisonOperator.GREATER_THAN.name, ComparisonOperator.LESS_THAN_EQUALS.name, ComparisonOperator.GREATER_THAN_EQUALS.name}
ARITHMETIC_NAMES = {ArithmeticOperator.PLUS.name, ArithmeticOperator.MINUS.name}


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
            error_message = f"Expected {ArithmeticOperator.PLUS.value}, {ArithmeticOperator.MINUS.value}, {ArithmeticOperator.MULTIPLY.value} or {ArithmeticOperator.DIVIDE.value}"
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

    def binary_operation(self, func_a, operations, func_b=None):
        if func_b is None:
            func_b = func_a

        result = ParserValidator()
        left = result.register(func_a())

        if result.error:
            return result

        while self.current_token.type in operations or (self.current_token.type, self.current_token.value) in operations:
            operation_token = self.current_token
            result.register_advancement()
            self.advance()
            right = result.register(func_b())

            if result.error:
                return result

            left = BinaryOperationNode(left, operation_token, right)

        return result.success(left)

    def expression(self):
        result = ParserValidator()

        if self.current_token.matches(InWords.KEYWORDS.name, 'VAR'):
            result.register_advancement()
            self.advance()

            if self.current_token.type != InWords.IDENTIFIER.name:
                error = InvalidSyntaxError('Expected identifier', self.current_token.start_position, self.current_token.end_position)
                return result.failure(error)

            variable_name = self.current_token
            result.register_advancement()
            self.advance()

            if self.current_token.type != ComparisonOperator.EQUALS.name:
                error = InvalidSyntaxError("Expected '='", self.current_token.start_position, self.current_token.end_position)
                return result.failure(error)

            result.register_advancement()
            self.advance()
            expression = result.register(self.expression())

            if result.error:
                return result

            variable_node = VariableAssignNode(variable_name, expression)
            return result.success(variable_node)

        # operation_result = self.binary_operation(self.term, ADDITIVE_OPERATORS_NAMES)
        operation_result = self.binary_operation(self.comparison_expression, EXPRESSION_NAMES)
        node = result.register(operation_result)

        if result.error:
            error_message = f"Expected {Digit.INT.value}, {Digit.FLOAT.value}, {InWords.IDENTIFIER.name}, {InWords.VAR.name}, {ArithmeticOperator.PLUS.value}, {ArithmeticOperator.MINUS.value} or {Punctuation.LEFT_PARENTHESIS.value}"
            error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
            return result.failure(error)

        return result.success(node)

    def comparison_expression(self):
        validator = ParserValidator()

        if self.current_token.matches(InWords.KEYWORDS.name, 'NOT'):
            token = self.current_token
            validator.register_advancement()
            self.advance()

            node = validator.register(self.comparison_expression())
            if validator.error:
                return validator

            return validator.success(UnaryOperationNode(token, node))

        node = validator.register(self.binary_operation(self.arithmetic_expression, COMPARISON_EXPRESSION_NAMES))

        if validator.error:
            error_message = f"Expected {Digit.INT.value}, {Digit.FLOAT.value}, {InWords.IDENTIFIER.name}, {ArithmeticOperator.PLUS.value}, {ArithmeticOperator.MINUS.value} or {Punctuation.LEFT_PARENTHESIS.value}"
            error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)  # end = self.current_token.end_position
            return validator.failure(error)

        return validator.success(node)

    def arithmetic_expression(self):
        return self.binary_operation(self.term, ARITHMETIC_NAMES)

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
