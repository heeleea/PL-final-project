from error import InvalidSyntaxError
from token_utils import Digit, ArithmeticOperator, Punctuation, Utils, InWords, ComparisonOperator
from ast_nodes import NumberNode, StringNode, ListNode, BinaryOperationNode, UnaryOperationNode, VariableAccessNode, \
    VariableAssignNode, IfNode, ForNode, WhileNode, FunctionDefinitionNode, CallableNode
from constans.parsing_rules import NUMBER_TYPES, STRING_TYPES, EXPRESSION_STARTERS, EXPRESSION_CLOSERS, LIST_CLOSERS, \
    LIST_STARTERS_NAMES, LIST_CLOSERS_NAMES, EXPRESSION_NAMES, EXPRESSION_STARTERS_NAMES, EXPRESSION_CLOSERS_NAMES, \
    IDENTIFIERS_NAMES, COMPARISON_EXPRESSION_NAMES, ADDITIVE_OPERATORS_NAMES, MULTIPLICATIVE_OPERATORS_NAMES, \
    ARITHMETIC_NAMES, POWER_NAMES, LOOP_NAMES


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

    def atom(self):
        result = ParserValidator()
        token = self.current_token

        if token.type in NUMBER_TYPES:
            result.register_advancement()
            self.advance()
            return result.success(NumberNode(token))

        elif token.type in STRING_TYPES:
            result.register_advancement()
            self.advance()
            return result.success(StringNode(token))

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

            else:
                error_message = f"Expected {Punctuation.RIGHT_PARENTHESIS.value}"
                error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
                return result.failure(error)

        elif token.type in LIST_STARTERS_NAMES:
            list_expression = result.register(self.list_expression())
            if result.error:
                return result
            return result.success(list_expression)

        elif token.matches(InWords.KEYWORDS.name, 'IF'):
            if_expression = result.register(self.if_expression())

            if result.error:
                return result

            return result.success(if_expression)

        elif any(token.matches(InWords.KEYWORDS.name, keyword) for keyword in LOOP_NAMES):
            loop_method = self.loop_handlers_factory(token.value)

            if loop_method:
                loop = result.register(loop_method())

                if result.error:
                    return result

                return result.success(loop)

        elif token.matches(InWords.KEYWORDS.name, 'FUNC'):
            function_definition = result.register(self.function_definition())
            if result.error:
                return function_definition

            return result.success(function_definition)

        #TODO edit the error message including any sign missed, also in expression, and so on
        error_message = f"Expected {Digit.INT.value}, {Digit.FLOAT.value}, {InWords.IDENTIFIER.name}, {ArithmeticOperator.PLUS.value}, {ArithmeticOperator.MINUS.value} or {Punctuation.LEFT_PARENTHESIS.value}," \
                        f"{InWords.IF.name}, {InWords.FOR.name}, {InWords.WHILE.name}, {InWords.FUNC.name}"
        error = InvalidSyntaxError(error_message, token.start_position, token.end_position) 
        return result.failure(error)

    def loop_handlers_factory(self, loop):
        loop_functions = {
            InWords.IF.name: self.if_expression,
            InWords.FOR.name: self.for_expression,
            InWords.WHILE.name: self.while_expression,
            InWords.FUNC.name: self.function_definition,
            InWords.LIST.name: self.list_expression

        }

        return loop_functions.get(loop)

    def for_expression(self):
        step_value = None
        validator = ParserValidator()

        if not self.current_token.matches(InWords.KEYWORDS.name, 'FOR'):
            error_message = f"Expected {InWords.FOR.name}"
            error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
            return validator.failure(error)

        validator.register_advancement()
        self.advance()

        if self.current_token.type != InWords.IDENTIFIER.name:
            error = InvalidSyntaxError('Expected identifier', self.current_token.start_position,
                                       self.current_token.end_position)
            return validator.failure(error)

        variable_name = self.current_token
        validator.register_advancement()
        self.advance()

        if self.current_token.type != ComparisonOperator.EQUALS.name:
            error = InvalidSyntaxError('Expected identifier', self.current_token.start_position, self.current_token.end_position)
            return validator.failure(error)

        validator.register_advancement()
        self.advance()

        start_value = validator.register(self.expression())
        if validator.error:
            return validator

        if not self.current_token.matches(InWords.KEYWORDS.name, 'TO'):
            error_message = f"Expected {InWords.TO.name}"
            error = InvalidSyntaxError(error_message, self.current_token.start_position,
                                       self.current_token.end_position)
            return validator.failure(error)

        validator.register_advancement()
        self.advance()

        end_value = validator.register(self.expression())
        if validator.error:
            return validator

        if self.current_token.matches(InWords.KEYWORDS.name, 'STEP'):
            validator.register_advancement()
            self.advance()

            step_value = validator.register(self.expression()) or None
            if validator.error:
                return validator

        if not self.current_token.matches(InWords.KEYWORDS.name, 'THEN'):
            error_message = f"Expected {InWords.THEN.name}"
            error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
            return validator.failure(error)

        validator.register_advancement()
        self.advance()

        body = validator.register(self.expression())
        if validator.error:
            return validator

        for_node = ForNode(variable_name, start_value, end_value, step_value, body)
        return validator.success(for_node)

    def while_expression(self):
        validator = ParserValidator()

        if not self.current_token.matches(InWords.KEYWORDS.name, 'WHILE'):
            error_message = f"Expected {InWords.WHILE.name}"
            error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
            return validator.failure(error)

        validator.register_advancement()
        self.advance()

        condition = validator.register(self.expression())

        if validator.error:
            return validator

        if not self.current_token.matches(InWords.KEYWORDS.name, 'THEN'):
            error_message = f"Expected {InWords.THEN.name}"
            error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
            return validator.failure(error)

        validator.register_advancement()
        self.advance()

        body = validator.register(self.expression())

        if validator.error:
            return validator

        while_node = WhileNode(condition, body)
        return validator.success(while_node)

    def function_definition(self):
        validator = ParserValidator()

        if not self.current_token.matches(InWords.KEYWORDS.name, 'FUNC'):
            error = InvalidSyntaxError(f"Expected {InWords.FUNC.value}",
                                       self.current_token.start_position,
                                       self.current_token.end_position)
            return validator.failure(error)

        validator.register_advancement()
        self.advance()

        if self.current_token.type == InWords.IDENTIFIER.name:
            function_name = self.current_token
            validator.register_advancement()
            self.advance()

            if self.current_token.type not in EXPRESSION_STARTERS_NAMES:
                error = InvalidSyntaxError(f"Expected {EXPRESSION_STARTERS}",
                                           self.current_token.start_position,
                                           self.current_token.end_position)
                validator.failure(error)

        else:
            function_name = None
            if self.current_token.type not in EXPRESSION_STARTERS_NAMES:
                error = InvalidSyntaxError(f"Expected {EXPRESSION_STARTERS}",
                                           self.current_token.start_position,
                                           self.current_token.end_position)
                validator.failure(error)

        validator.register_advancement()
        self.advance()

        arguments = []

        if self.current_token.type == InWords.IDENTIFIER.name:
            arguments.append(self.current_token)
            validator.register_advancement()
            self.advance()

        while self.current_token.type == Punctuation.COMMA.name:
            validator.register_advancement()
            self.advance()

            if self.current_token.type != InWords.IDENTIFIER.name:
                error = InvalidSyntaxError(f"Expected {InWords.IDENTIFIER.value}",
                                           self.current_token.start_position,
                                           self.current_token.end_position)
                validator.failure(error)

            arguments.append(self.current_token)
            validator.register_advancement()
            self.advance()
            
        if self.current_token.type not in EXPRESSION_CLOSERS_NAMES:
            error = InvalidSyntaxError(f"Expected {Punctuation.COMMA.value} or {EXPRESSION_CLOSERS}",
                                       self.current_token.start_position,
                                       self.current_token.end_position)
            validator.failure(error)

        else:
            if self.current_token.type not in EXPRESSION_CLOSERS_NAMES:
                error = InvalidSyntaxError(f"Expected {Punctuation.COMMA.value} or {EXPRESSION_CLOSERS}",
                                           self.current_token.start_position,
                                           self.current_token.end_position)
                validator.failure(error)

        validator.register_advancement()
        self.advance()

        if self.current_token.type != Punctuation.FUNCTION_ASSIGNMENT.name:
            error = InvalidSyntaxError(f"Expected {Punctuation.FUNCTION_ASSIGNMENT.value}",
                                       self.current_token.start_position,
                                       self.current_token.end_position)
            validator.failure(error)

        validator.register_advancement()
        self.advance()

        body = validator.register(self.expression())
        if validator.error:
            return validator

        function_node = FunctionDefinitionNode(function_name, arguments, body)
        return validator.success(function_node)

    def call(self):
        validator = ParserValidator()
        atom = validator.register(self.atom())

        if validator.error:
            return validator

        if self.current_token.type in EXPRESSION_STARTERS_NAMES:
            validator.register_advancement()
            self.advance()

            arguments = []

            if self.current_token.type in EXPRESSION_CLOSERS_NAMES:
                validator.register_advancement()
                self.advance()

            else:
                argument = validator.register(self.expression())
                arguments.append(argument)

                if validator.error:
                    error_message = f"Expected {Punctuation.RIGHT_PARENTHESIS.value}, {InWords.VAR.value}, {InWords.IF.value}, " \
                                    f"{InWords.FOR.value}, {InWords.WHILE.value}, {InWords.FUNC.value}, {Digit.INT.value}" \
                                    f"{Digit.FLOAT.name}, {InWords.IDENTIFIER.name}"
                    error = InvalidSyntaxError(error_message,
                                               self.current_token.start_position,
                                               self.current_token.end_position)
                    validator.failure(error)

                while self.current_token.type == Punctuation.COMMA.name:
                    validator.register_advancement()
                    self.advance()

                    argument = validator.register(self.expression())
                    arguments.append(argument)

                    if validator.error:
                        return validator

                if self.current_token.type not in EXPRESSION_CLOSERS_NAMES:
                    error = InvalidSyntaxError(f"Expected {Punctuation.COMMA.value} or {EXPRESSION_CLOSERS}",
                                               self.current_token.start_position,
                                               self.current_token.end_position)
                    validator.failure(error)

                validator.register_advancement()
                self.advance()

            return validator.success(CallableNode(atom, arguments))

        return validator.success(atom)

    def power(self):
        return self.binary_operation(self.call, POWER_NAMES, self.factor)

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

        return self.power()

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

        node = result.register(self.binary_operation(self.comparison_expression, EXPRESSION_NAMES))

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

    def list_expression(self):
        validator = ParserValidator()
        elements = []
        #TODO check if its a start position arg or a get position
        start_position = self.current_token.start_position.get_copy()

        if self.current_token.type != Punctuation.LEFT_SQUARE.name:
            error = InvalidSyntaxError(f"Expected {Punctuation.LEFT_SQUARE.value}",
                                       self.current_token.start_position,
                                       self.current_token.end_position)
            return validator.failure(error)

        validator.register_advancement()
        self.advance()

        if self.current_token.type == Punctuation.RIGHT_SQUARE.name:
            validator.register_advancement()
            self.advance()
        else:
            expression = validator.register(self.expression())
            elements.append(expression)

            if validator.error:
                error_message = f"Expected {Punctuation.RIGHT_SQUARE.value}, {InWords.VAR.value}, {InWords.IF.value}, " \
                                f"{InWords.FOR.value}, {InWords.WHILE.value}, {InWords.FUNC.value}, {Digit.INT.value}" \
                                f"{Digit.FLOAT.name}, {InWords.IDENTIFIER.name}"
                error = InvalidSyntaxError(error_message,
                                           self.current_token.start_position,
                                           self.current_token.end_position)
                validator.failure(error)

            while self.current_token.type == Punctuation.COMMA.name:
                validator.register_advancement()
                self.advance()

                element = validator.register(self.expression())
                elements.append(element)

                if validator.error:
                    return validator

            if self.current_token.type not in LIST_CLOSERS_NAMES:
                error = InvalidSyntaxError(f"Expected {Punctuation.COMMA.value} or {LIST_CLOSERS}",
                                           self.current_token.start_position,
                                           self.current_token.end_position)
                validator.failure(error)

            validator.register_advancement()
            self.advance()

        list_node = ListNode(elements)
        list_node.set_position(start_position, self.current_token.end_position)
        return validator.success(list_node)

    def if_expression(self):
        result = ParserValidator()
        cases = []
        else_case = None

        if not self.current_token.matches(InWords.KEYWORDS.name, 'IF'):
            error = InvalidSyntaxError("Expected 'IF'", self.current_token.start_position, self.current_token.end_position)
            return result.failure(error)

        result.register_advancement()
        self.advance()

        condition = result.register(self.expression())

        if result.error:
            return result

        if not self.current_token.matches(InWords.KEYWORDS.name, 'THEN'):
            error = InvalidSyntaxError("Expected 'THEN'", self.current_token.start_position, self.current_token.end_position)
            return result.failure(error)

        result.register_advancement()
        self.advance()

        expression = result.register(self.expression())

        if result.error:
            return result

        cases.append((condition, expression))

        while self.current_token.matches(InWords.KEYWORDS.name, 'ELIF'):
            result.register_advancement()
            self.advance()

            condition = result.register(self.expression())

            if result.error:
                return result

            if not self.current_token.matches(InWords.KEYWORDS.name, 'THEN'):
                error = InvalidSyntaxError("Expected 'THEN'", self.current_token.start_position, self.current_token.end_position)
                return result.failure(error)

            result.register_advancement()
            self.advance()

            expression = result.register(self.expression())

            if result.error:
                return result

            cases.append((condition, expression))

        if self.current_token.matches(InWords.KEYWORDS.name, 'ELSE'):
            result.register_advancement()
            self.advance()

            else_case = result.register(self.expression())

            if result.error:
                return result

        return result.success(IfNode(cases, else_case))

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
