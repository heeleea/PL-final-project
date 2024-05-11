from error import InvalidSyntaxError
from error import error_message_generator
from validators.parser_validator import ParserValidator
from constans.token_names import Punctuation, Utils, InWords, ComparisonOperator

from nodes.if_node import IfNode
from nodes.for_node import ForNode
from nodes.list_node import ListNode
from nodes.while_node import WhileNode
from nodes.number_node import NumberNode
from nodes.string_node import StringNode
from nodes.call_node import CallableNode
from nodes.unary_operation_node import UnaryOperationNode
from nodes.variable_access_node import VariableAccessNode
from nodes.variable_assign_node import VariableAssignNode
from nodes.binary_operation_node import BinaryOperationNode
from nodes.function_definition_node import FunctionDefinitionNode

from constans.error_signs import EXPECTED_IDENTIFIER, CREATE_AST, ATOM_MAIN, EXPECTED_FOR, LIST_MAIN, \
    EXPECTED_EQUALS, EXPECTED_TO, EXPECTED_THEN, EXPECTED_END, EXPECTED_WHILE, EXPECTED_FUNC, LIST_EXPRESSION, \
    EXPECTED_START_PARENTHESIS, EXPECTED_COMMA_AND_END_PARENTHESIS, EXPECTED_CLOSE_PARENTHESIS, \
    FUNC_DEFINITION_NEW_LINE, CALL_MAIN, EXPRESSION_MAIN, COMPARISON_EXPRESSIONS_MAIN, EXPECTED_LIST_CLOSERS, \
    EXPECTED_BLOCK

from constans.parsing_rules import NUMBER_TYPES, STRING_TYPES, LIST_STARTERS_NAMES, LIST_CLOSERS_NAMES, EXPRESSION_NAMES, EXPRESSION_STARTERS_NAMES, EXPRESSION_CLOSERS_NAMES, \
    IDENTIFIERS_NAMES, COMPARISON_EXPRESSION_NAMES, ADDITIVE_OPERATORS_NAMES, MULTIPLICATIVE_OPERATORS_NAMES, \
    ARITHMETIC_NAMES, POWER_NAMES, LOOP_NAMES


class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.token_index = -1
        self.advance()

    def advance(self):
        self.token_index += 1
        self.update_current_token()
        return self.current_token

    def reverse(self, amount=1):
        self.token_index -= amount
        self.update_current_token()
        return self.current_token

    def update_current_token(self):
        if self.token_index < len(self.tokens):
            self.current_token = self.tokens[self.token_index]

    def create_ast(self):
        result = self.statements()

        if not result.error and self.current_token.type != Utils.EOF.name:
            error_message = error_message_generator(CREATE_AST)
            error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
            return result.failure(error)

        return result

    def atom(self):
        validator = ParserValidator()
        token = self.current_token

        if token.type in NUMBER_TYPES:
            validator.register_advancement()
            self.advance()
            return validator.success(NumberNode(token))

        elif token.type in STRING_TYPES:
            validator.register_advancement()
            self.advance()
            return validator.success(StringNode(token))

        elif token.type in IDENTIFIERS_NAMES:
            validator.register_advancement()
            self.advance()
            variable_node = VariableAccessNode(token)
            return validator.success(variable_node)

        elif token.type in EXPRESSION_STARTERS_NAMES:
            validator.register_advancement()
            self.advance()
            expression = validator.register(self.expression())

            if validator.error:
                return validator

            if self.current_token.type in EXPRESSION_CLOSERS_NAMES:
                validator.register_advancement()
                self.advance()
                return validator.success(expression)

            else:
                error_message = error_message_generator(EXPECTED_CLOSE_PARENTHESIS)
                error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
                return validator.failure(error)

        elif token.type in LIST_STARTERS_NAMES:
            list_expression = validator.register(self.list_expression())
            if validator.error:
                return validator
            return validator.success(list_expression)

        elif token.matches(InWords.KEYWORDS.name, InWords.IF.name):
            if_expression = validator.register(self.if_expression())

            if validator.error:
                return validator

            return validator.success(if_expression)

        elif any(token.matches(InWords.KEYWORDS.name, keyword) for keyword in LOOP_NAMES):
            loop_method = self.loop_handlers_factory(token.value)

            if loop_method:
                loop = validator.register(loop_method())

                if validator.error:
                    return validator

                return validator.success(loop)

        elif token.matches(InWords.KEYWORDS.name, InWords.FUNC.name):
            function_definition = validator.register(self.function_definition())
            if validator.error:
                return function_definition

            return validator.success(function_definition)

        error_message = error_message_generator(ATOM_MAIN)
        error = InvalidSyntaxError(error_message, token.start_position, token.end_position)
        return validator.failure(error)

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

        if not self.current_token.matches(InWords.KEYWORDS.name, InWords.FOR.name):
            error_message = error_message_generator(EXPECTED_FOR)
            error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
            return validator.failure(error)

        validator.register_advancement()
        self.advance()

        if self.current_token.type != InWords.IDENTIFIER.name:
            error_message = error_message_generator(EXPECTED_IDENTIFIER)
            error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
            return validator.failure(error)

        variable_name = self.current_token
        validator.register_advancement()
        self.advance()

        if self.current_token.type != ComparisonOperator.EQUALS.name:
            error_message = error_message_generator(EXPECTED_EQUALS)
            error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
            return validator.failure(error)

        validator.register_advancement()
        self.advance()

        start_value = validator.register(self.expression())
        if validator.error:
            return validator

        if not self.current_token.matches(InWords.KEYWORDS.name, InWords.TO.name):
            error_message = error_message_generator(EXPECTED_TO)
            error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
            return validator.failure(error)

        validator.register_advancement()
        self.advance()

        end_value = validator.register(self.expression())
        if validator.error:
            return validator

        if self.current_token.matches(InWords.KEYWORDS.name, InWords.STEP.name):
            validator.register_advancement()
            self.advance()

            step_value = validator.register(self.expression()) or None
            if validator.error:
                return validator

        if not self.current_token.matches(InWords.KEYWORDS.name, InWords.THEN.name):
            error_message = error_message_generator(EXPECTED_THEN)
            error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
            return validator.failure(error)

        validator.register_advancement()
        self.advance()

        if self.current_token.type == InWords.NEWLINE.name:
            validator.register_advancement()
            self.advance()

            body = validator.register(self.expression())
            if validator.error:
                return validator

            if self.current_token.type == InWords.NEWLINE.name:
                validator.register_advancement()
                self.advance()

            if not self.current_token.matches(InWords.KEYWORDS.name, Punctuation.END.name):
                error_message = error_message_generator(EXPECTED_END)
                error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
                return validator.failure(error)

            validator.register_advancement()
            self.advance()

            for_node = ForNode(variable_name, start_value, end_value, step_value, body, True)
            return validator.success(for_node)

        body = validator.register(self.expression())
        if validator.error:
            return validator

        for_node = ForNode(variable_name, start_value, end_value, step_value, body, False)
        return validator.success(for_node)

    def while_expression(self):
        validator = ParserValidator()

        if not self.current_token.matches(InWords.KEYWORDS.name, InWords.WHILE.name):
            error_message = error_message_generator(EXPECTED_WHILE)
            error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
            return validator.failure(error)

        validator.register_advancement()
        self.advance()

        condition = validator.register(self.expression())

        if validator.error:
            return validator

        if not self.current_token.matches(InWords.KEYWORDS.name, InWords.THEN.name):
            error_message = error_message_generator(EXPECTED_THEN)
            error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
            return validator.failure(error)

        validator.register_advancement()
        self.advance()

        if self.current_token.type == InWords.NEWLINE.name:
            validator.register_advancement()
            self.advance()

            body = validator.register(self.statements())
            if validator.error:
                return validator

            if self.current_token.type == InWords.NEWLINE.name:
                validator.register_advancement()
                self.advance()

            if not self.current_token.matches(InWords.KEYWORDS.name, Utils.EOF.name):
                error_message = error_message_generator(EXPECTED_END)
                error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
                return validator.failure(error)

            validator.register_advancement()
            self.advance()

            while_node = WhileNode(condition, body, True)
            return validator.success(while_node)

        body = validator.register(self.expression())

        if validator.error:
            return validator

        while_node = WhileNode(condition, body, False)
        return validator.success(while_node)

    def function_definition(self):
        validator = ParserValidator()

        if not self.current_token.matches(InWords.KEYWORDS.name, InWords.FUNC.name):
            error_message = error_message_generator(EXPECTED_FUNC)
            error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
            return validator.failure(error)

        validator.register_advancement()
        self.advance()

        if self.current_token.type == InWords.IDENTIFIER.name:
            function_name = self.current_token
            validator.register_advancement()
            self.advance()

            if self.current_token.type not in EXPRESSION_STARTERS_NAMES:
                error_message = error_message_generator(EXPECTED_START_PARENTHESIS)
                error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
                validator.failure(error)

        else:
            function_name = None
            if self.current_token.type not in EXPRESSION_STARTERS_NAMES:
                error_message = error_message_generator(EXPECTED_START_PARENTHESIS)
                error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
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
                error_message = error_message_generator(EXPECTED_IDENTIFIER)
                error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
                validator.failure(error)

            arguments.append(self.current_token)
            validator.register_advancement()
            self.advance()
            
        if self.current_token.type not in EXPRESSION_CLOSERS_NAMES:
            error_message = error_message_generator(EXPECTED_COMMA_AND_END_PARENTHESIS)
            error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
            validator.failure(error)

        else:
            if self.current_token.type not in EXPRESSION_CLOSERS_NAMES:
                error_message = error_message_generator(EXPECTED_CLOSE_PARENTHESIS)
                error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
                validator.failure(error)

        validator.register_advancement()
        self.advance()

        if self.current_token.type == Punctuation.FUNCTION_ASSIGNMENT.name:
            validator.register_advancement()
            self.advance()

            body = validator.register(self.expression())
            if validator.error:
                return validator

            function_node = FunctionDefinitionNode(function_name, arguments, body, False)
            return validator.success(function_node)

        if self.current_token != InWords.NEWLINE.name:
            error_message = error_message_generator(FUNC_DEFINITION_NEW_LINE)
            error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
            validator.failure(error)

        validator.register_advancement()
        self.advance()

        body = validator.register(self.statements())
        if validator.error:
            return validator

        if self.current_token.matches(InWords.KEYWORDS.name, Utils.EOF.name):
            error_message = error_message_generator(EXPECTED_END)
            error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
            validator.failure(error)

        validator.register_advancement()
        self.advance()

        function_node = FunctionDefinitionNode(function_name, arguments, body, True)
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
                    error_message = error_message_generator(CALL_MAIN)
                    error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
                    return validator.failure(error)

                while self.current_token.type == Punctuation.COMMA.name:
                    validator.register_advancement()
                    self.advance()

                    argument = validator.register(self.expression())
                    arguments.append(argument)

                    if validator.error:
                        return validator

                if self.current_token.type not in EXPRESSION_CLOSERS_NAMES:
                    error_message = error_message_generator(EXPECTED_COMMA_AND_END_PARENTHESIS)
                    error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
                    return validator.failure(error)

                validator.register_advancement()
                self.advance()

            return validator.success(CallableNode(atom, arguments))
        return validator.success(atom)

    def power(self):
        return self.binary_operation(self.call, POWER_NAMES, self.factor)

    def factor(self):
        validator = ParserValidator()
        token = self.current_token

        if token.type in ADDITIVE_OPERATORS_NAMES:
            validator.register_advancement()
            self.advance()
            factor = validator.register(self.factor())

            if validator.error:
                return validator

            return validator.success(UnaryOperationNode(token, factor))

        return self.power()

    def binary_operation(self, func_a, operations, func_b=None):
        if func_b is None:
            func_b = func_a

        validator = ParserValidator()
        left = validator.register(func_a())

        if validator.error:
            return validator

        while self.current_token.type in operations or (self.current_token.type, self.current_token.value) in operations:
            operation_token = self.current_token
            validator.register_advancement()
            self.advance()
            right = validator.register(func_b())

            if validator.error:
                return validator

            left = BinaryOperationNode(left, operation_token, right)

        return validator.success(left)

    def statements(self):
        validator = ParserValidator()
        statements = []
        start_position = self.current_token.start_position.get_copy()

        while self.current_token.type == InWords.NEWLINE.name:
            validator.register_advancement()
            self.advance()

        statement = validator.register(self.expression())

        if validator.error:
            return validator

        statements.append(statement)

        more_statements = True

        while True:
            newline_count = 0
            while self.current_token.type == InWords.NEWLINE.name:
                validator.register_advancement()
                self.advance()
                newline_count += 1
            if newline_count == 0:
                more_statements = False

            if not more_statements:
                break

            statement = validator.try_register(self.expression())

            if not statement:
                self.reverse(validator.to_reverse_count)
                more_statements = False
                continue

            statements.append(statement)

        list_node = ListNode(statements, start_position, self.current_token.end_position.get_copy())
        return validator.success(list_node)

    def expression(self):
        validator = ParserValidator()

        if self.current_token.matches(InWords.KEYWORDS.name, InWords.VAR.name):
            validator.register_advancement()
            self.advance()

            if self.current_token.type != InWords.IDENTIFIER.name:
                error_message = error_message_generator(EXPECTED_IDENTIFIER)
                error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
                return validator.failure(error)

            variable_name = self.current_token
            validator.register_advancement()
            self.advance()

            if self.current_token.type != ComparisonOperator.EQUALS.name:
                error_message = error_message_generator(EXPECTED_EQUALS)
                error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
                return validator.failure(error)

            validator.register_advancement()
            self.advance()
            expression = validator.register(self.expression())

            if validator.error:
                return validator

            variable_node = VariableAssignNode(variable_name, expression)
            return validator.success(variable_node)

        node = validator.register(self.binary_operation(self.comparison_expression, EXPRESSION_NAMES))

        if validator.error:
            error_message = error_message_generator(EXPRESSION_MAIN)
            error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
            return validator.failure(error)

        return validator.success(node)

    def comparison_expression(self):
        validator = ParserValidator()

        if self.current_token.matches(InWords.KEYWORDS.name, InWords.NOT.name):
            token = self.current_token
            validator.register_advancement()
            self.advance()

            node = validator.register(self.comparison_expression())
            if validator.error:
                return validator

            return validator.success(UnaryOperationNode(token, node))

        node = validator.register(self.binary_operation(self.arithmetic_expression, COMPARISON_EXPRESSION_NAMES))

        if validator.error:
            error_message = error_message_generator(COMPARISON_EXPRESSIONS_MAIN)
            error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
            return validator.failure(error)

        return validator.success(node)

    def arithmetic_expression(self):
        return self.binary_operation(self.term, ARITHMETIC_NAMES)

    def list_expression(self):
        validator = ParserValidator()
        elements = []
        start_position = self.current_token.start_position.get_copy()

        if self.current_token.type != Punctuation.LEFT_SQUARE.name:
            error_message = error_message_generator(EXPECTED_LIST_CLOSERS)
            error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
            return validator.failure(error)

        validator.register_advancement()
        self.advance()

        if self.current_token.type == Punctuation.RIGHT_SQUARE.name:
            validator.register_advancement()
            self.advance()
        else:
            element = validator.register(self.expression())
            elements.append(element)

            if validator.error:
                error_message = error_message_generator(LIST_EXPRESSION)
                error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
                return validator.failure(error)

            while self.current_token.type == Punctuation.COMMA.name:
                validator.register_advancement()
                self.advance()

                element = validator.register(self.expression())
                elements.append(element)

                if validator.error:
                    return validator

            if self.current_token.type not in LIST_CLOSERS_NAMES:
                error_message = error_message_generator(LIST_MAIN)
                error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
                return validator.failure(error)

            validator.register_advancement()
            self.advance()

        list_node = ListNode(elements, start_position, self.current_token.end_position.get_copy())
        return validator.success(list_node)

    def if_expression(self):
        validator = ParserValidator()
        all_cases = validator.register(self.if_expression_cases(InWords.IF.name))
        if validator.error:
            return validator

        cases, else_case = all_cases
        if_node = IfNode(cases, else_case)

        return validator.success(if_node)

    def if_expression_cases(self, case_keyword):
        validator = ParserValidator()
        cases = []
        else_case = None

        if not self.current_token.matches(InWords.KEYWORDS.name, case_keyword):
            error_message = error_message_generator(case_keyword)
            error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
            return validator.failure(error)

        validator.register_advancement()
        self.advance()

        condition = validator.register(self.expression())

        if validator.error:
            return validator

        if not self.current_token.matches(InWords.KEYWORDS.name, InWords.THEN.name):
            error_message = error_message_generator(EXPECTED_THEN)
            error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
            return validator.failure(error)

        validator.register_advancement()
        self.advance()

        if self.current_token.type == InWords.NEWLINE.name:
            validator.register_advancement()
            self.advance()

            statements = validator.register(self.statements())
            if validator.error:
                return validator

            cases.append((condition, statements, True))

            if self.current_token.type == InWords.NEWLINE.name:
                validator.register_advancement()
                self.advance()

            if self.current_token.matches(InWords.KEYWORDS.name, Punctuation.END.name):
                validator.register_advancement()
                self.advance()

            else:
                all_cases = validator.register(self.if_expression_b_or_c())
                if validator.error:
                    return validator

                new_cases, else_case = all_cases
                cases.extend(new_cases)

                if self.current_token.type == InWords.NEWLINE.name:
                    validator.register_advancement()
                    self.advance()

        else:
            expression = validator.register(self.expression())
            if validator.error:
                return validator

            cases.append((condition, expression, False))

            if self.current_token.type == InWords.NEWLINE.name:
                validator.register_advancement()
                self.advance()

            all_cases = validator.register(self.if_expression_b_or_c())
            if validator.error:
                return validator

            new_cases, else_case = all_cases
            cases.extend(new_cases)

            if self.current_token.type == InWords.NEWLINE.name:
                validator.register_advancement()
                self.advance()

        return validator.success((cases, else_case))

    def if_expression_b(self):
        return self.if_expression_cases(InWords.ELIF.name)

    def if_expression_c(self):
        validator = ParserValidator()
        else_case = None

        if self.current_token.matches(InWords.KEYWORDS.name, InWords.ELSE.name):
            validator.register_advancement()
            self.advance()

            if self.current_token == InWords.NEWLINE.name:
                validator.register_advancement()
                self.advance()

                statements = validator.register(self.statements())
                if validator.error:
                    return validator

                else_case = (statements, True)

                if self.current_token.matches(InWords.KEYWORDS.name, Punctuation.END.value):
                    validator.register_advancement()
                    self.advance()

                else:
                    error_message = error_message_generator(EXPECTED_BLOCK)
                    error = InvalidSyntaxError(error_message, self.current_token.start_position, self.current_token.end_position)
                    return validator.failure(error)

            else:
                expression = validator.register(self.expression())
                if validator.error:
                    return validator

                else_case = (expression, False)

                if self.current_token.type == InWords.NEWLINE.name:
                    validator.register_advancement()
                    self.advance()

        return validator.success(else_case)

    def if_expression_b_or_c(self):
        validator = ParserValidator()
        cases, else_case = [], None

        if self.current_token.matches(InWords.KEYWORDS.name, InWords.ELIF.name):
            all_cases = validator.register(self.if_expression_b())
            if validator.error:
                return validator
            cases, else_case = all_cases

            if self.current_token.type == InWords.NEWLINE.name:
                validator.register_advancement()
                self.advance()

        else:
            else_case = validator.register(self.if_expression_c())
            if validator.error:
                return validator

            if self.current_token.type == InWords.NEWLINE.name:
                validator.register_advancement()
                self.advance()

        return validator.success((cases, else_case))

    def term(self):
        return self.binary_operation(self.factor, MULTIPLICATIVE_OPERATORS_NAMES)
