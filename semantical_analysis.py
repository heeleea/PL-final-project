from typing import Union

from context import Context
from symbol_table import SymbolTable
from error import CostumedRunTimeError
from token_utils import ArithmeticOperator, ComparisonOperator, InWords
from ast_nodes import BinaryOperationNode, UnaryOperationNode, NumberNode, StringNode,  BasicPosition, VariableAccessNode, VariableAssignNode, IfNode, ForNode, WhileNode, FunctionDefinitionNode, CallableNode, ListNode


class Value(BasicPosition):
    def __init__(self, ):
        super().__init__()
        self.context = None
        self.set_position()
        self.set_context()

    def set_position(self, start_position=None, end_position=None):
        self.start_position = start_position
        self.end_position = end_position
        return self

    def set_context(self, context=None):
        self.context = context
        return self

    def added_to(self, other):
        return None, self.illegal_operation(other)

    def subbed_by(self, other):
        return None, self.illegal_operation(other)

    def multiplied_by(self, other):
        return None, self.illegal_operation(other)

    def divided_by(self, other):
        return None, self.illegal_operation(other)

    def powered_by(self, other):
        return None, self.illegal_operation(other)

    def equals(self, other):
        return None, self.illegal_operation(other)

    def not_equals(self, other):
        return None, self.illegal_operation(other)

    def not_equals(self, other):
        return None, self.illegal_operation(other)

    def greater_than(self, other):
        return None, self.illegal_operation(other)

    def greater_than(self, other):
        return None, self.illegal_operation(other)

    def greater_than_equals(self, other):
        return None, self.illegal_operation(other)

    def logical_or(self, other):
        return None, self.illegal_operation(other)

    def logical_not(self):
        return None, self.illegal_operation()

    def execute(self, args):
        validator = RuntimeValidator()
        validator.failure(self.illegal_operation())
        return validator

    def get_copy(self):
        raise Exception('No copy method defined')

    def is_true(self):
        return False

    def illegal_operation(self, other=None):
        if not other:
            other = self

        error = CostumedRunTimeError('Illegal operation', self.start_position, other.end_position, self.context)
        return error


class Number(Value):
    def __init__(self, value):
        super().__init__()
        self.value = value

    def added_to(self, new_number):
        if isinstance(new_number, Number):
            number = Number(self.value + new_number.value)
            number.set_context(self.context)
            return number, None

        return None, Value.illegal_operation(self.start_position, new_number.end_position)

    def subbed_by(self, new_number):
        if isinstance(new_number, Number):
            number = Number(self.value - new_number.value)
            number.set_context(self.context)
            return number, None

        return None, Value.illegal_operation(self.start_position, new_number.end_position)

    def multiplied_by(self, new_number):
        if isinstance(new_number, Number):
            number = Number(self.value * new_number.value)
            number.set_context(self.context)
            return number, None

        return None, Value.illegal_operation(self.start_position, new_number.end_position)

    def divided_by(self, new_number):
        if isinstance(new_number, Number):
            if new_number.value == 0:
                return None, CostumedRunTimeError('Division By Zero', new_number.start_position, new_number.end_position, self.context)

            number = Number(self.value / new_number.value)
            number.set_context(self.context)
            return number, None

        return None, Value.illegal_operation(self.start_position, new_number.end_position)

    def powered_by(self, power):
        if isinstance(power, Number):
            number = Number(self.value ** power.value).set_context(self.context)
            return number, None

        return None, Value.illegal_operation(self.start_position, power.end_position)

    def get_comparison(self, new_number, operator):
        operations = {
            '==': lambda x, y: int(x == y),
            '!=': lambda x, y: int(x != y),
            '<': lambda x, y: int(x < y),
            '>': lambda x, y: int(x > y),
            '<=': lambda x, y: int(x <= y),
            '>=': lambda x, y: int(x >= y),
            'AND': lambda x, y: int(x and y),
            'OR': lambda x, y: int(x or y)
        }

        if isinstance(new_number, Number):
            operation = operations.get(operator) or operator
            if operation:
                result = operation(self.value, new_number.value)
                number = Number(result)
                number.set_context(self.context)
                return number, None

        return None, Value.illegal_operation(self.start_position, new_number.end_position)

    def equals(self, new_number):
        return self.get_comparison(new_number, ComparisonOperator.COMPARISON.value)

    def not_equals(self, new_number):
        return self.get_comparison(new_number, ComparisonOperator.NOT_EQUALS.value)

    def less_than(self, new_number):
        return self.get_comparison(new_number, ComparisonOperator.LESS_THAN.value)

    def greater_than(self, new_number):
        return self.get_comparison(new_number, ComparisonOperator.GREATER_THAN.value)

    def less_than_equals(self, new_number):
        return self.get_comparison(new_number, ComparisonOperator.LESS_THAN_EQUALS.value)

    def greater_than_equals(self, new_number):
        return self.get_comparison(new_number, ComparisonOperator.GREATER_THAN_EQUALS.value)

    def logical_and(self, new_number):
        return self.get_comparison(new_number, ComparisonOperator.AND.value)

    def logical_or(self, new_number):
        return self.get_comparison(new_number, ComparisonOperator.OR.value)

    def logical_not(self):
        comparison_result = 1 if self.value == 0 else 0
        number = Number(comparison_result)
        number.set_context(self.context)
        return number, None

    def get_copy(self):
        copy = Number(self.value)
        copy.set_position(self.start_position, self.end_position)
        copy.set_context(self.context)
        return copy

    def is_true(self):
        return self.value != 0

    def __repr__(self):
        return str(self.value)


class List(Value):
    def __init__(self, elements):
        super().__init__()
        self.elements = elements

    def added_to(self, new_element):
        updated_list = self.get_copy()
        updated_list.elements.append(new_element)

        return updated_list, None

    def subbed_by(self, index):
        if isinstance(index, Number):
            updated_list = self.get_copy()
            try:
                updated_list.elements.pop(index.value)
                return updated_list, None
            except:
                error_message = 'Elements at this index could not be removed from the list because index is out of bounds'
                error = RuntimeError(index.start_position,
                                     index.end_position,
                                     error_message,
                                     self.context)
                return None, error

        else:
            return None, Value.illegal_operation(self, index)

    def multiplied_by(self, new_list):
        if isinstance(new_list, List):
            updated_list = self.get_copy()
            updated_list.elements.extend(new_list.elements)
            return updated_list, None

        else:
            return None, Value.illegal_operation(self, new_list)

    def divided_by(self, index):
        if isinstance(index, Number):
            try:
                return self.elements[index.value], None

            except IndexError:
                error_message = 'Elements at this index could not be retrieved from the list because index is out of bounds'
                error = RuntimeError(index.start_position,
                                     index.end_position,
                                     error_message,
                                     self.context)
                return None, error

        else:
            return None, Value.illegal_operation(self, index)

    def get_copy(self):
        copied_list = List(self.elements[:])
        copied_list.set_position((self.start_position, self.end_position))
        copied_list.set_context(self.context)
        return copied_list

    def __repr__(self):
        return f"[{', '.join(map(str, self.elements))}]"


class Function(Value):
    def __init__(self, name, arguments, body):
        super().__init__()
        self.name = name or "<anonymous>"
        self.arguments = arguments
        self.body = body

    def execute(self, args):
        validator = RuntimeValidator()
        semantical_analysis = SemanticalAnalysis()
        new_context = Context(self.name, self.context, self.start_position)
        new_context.symbol_table = SymbolTable(new_context.parent.symbol_table)

        if len(args) > len(self.arguments):
            error = CostumedRunTimeError(f"{len(args) - len(self.arguments)} too many args passed into '{self.name}'",
                                         self.start_position,
                                         self.start_position,
                                         self.context)
            return validator.failure(error)

        if len(args) < len(self.arguments):
            error = CostumedRunTimeError(f"{len(self.arguments) - len(args)} too few args passed into '{self.name}'",
                                         self.start_position,
                                         self.end_position,
                                         self.context)
            return validator.failure(error)

        for i in range(len(args)):
            arg_name = self.arguments[i]
            arg_value = args[i]
            arg_value.set_context(new_context)
            new_context.symbol_table.set(arg_name, arg_value)

        value = validator.register(semantical_analysis.transverse(self.body, new_context))
        if validator.error: return validator
        return validator.success(value)

    def get_copy(self):
        copy = Function(self.name, self.body, self.arguments)
        copy.set_context(self.context)
        copy.set_position(self.start_position, self.end_position)
        return copy

    def __repr__(self):
        return f"<function {self.name}>"


class String(Value):
    def __init__(self, value):
        super().__init__()
        self.value = value

    def added_to(self, additional_string):

        if isinstance(additional_string, String):
            return String(self.value + additional_string.value).set_context(self.context), None

        else:
            return None, Value.illegal_operation(self, additional_string)

    def multiplied_by(self, number):

        if isinstance(number, Number):
            return String(self.value * number.value).set_context(self.context), None

        else:
            return None, Value.illegal_operation(self, number)

    def is_true(self):
        return len(self.value) > 0

    def get_copy(self):
        copy = String(self.value)
        copy.set_context(self.context)
        copy.set_position(self.start_position, self.end_position)
        return copy

    def __repr__(self):
        return f'"{self.value}"'


class RuntimeValidator:
    def __init__(self):
        self.value = None
        self.error = None

    def register(self, result):
        if result.error:
            self.error = result.error

        return result.value

    def success(self, value):
        self.value = value
        return self

    def failure(self, error):
        self.error = error
        return self


class SemanticalAnalysis:

    def transverse(self, node: Union[BinaryOperationNode, NumberNode, UnaryOperationNode], context):
        method = self.node_handler_factory(node)
        return method(node, context)

    @staticmethod
    def transverse_variable_access_node(node: VariableAccessNode, context):
        validator = RuntimeValidator()
        variable_name = node.token.value
        value = context.symbol_table.get(variable_name)

        if not value:
            error = CostumedRunTimeError(f"'{variable_name}' is not defined", node.start_position, node.end_position, context)
            return validator.failure(error)

        value = value.get_copy()
        value.set_position(node.start_position, node.end_position)
        return validator.success(value)

    def transverse_variable_assign_node(self, node, context):
        validator = RuntimeValidator()
        variable_name = node.token.value
        value = validator.register(self.transverse(node.value, context))

        if validator.error:
            return validator

        context.symbol_table.set(variable_name, value)
        return validator.success(value)

    def transverse_binary(self, node: BinaryOperationNode, context):
        validator = RuntimeValidator()
        left_operand = validator.register(self.transverse(node.left_node, context))

        if validator.error:
            return validator

        right_operand = validator.register(self.transverse(node.right_node, context))

        if validator.error:
            return validator

        method_type = node.operation.value or node.operation.type
        operation_method = self.operator_handler_factory(method_type, left_operand)
        result, error = operation_method(right_operand)

        if error:
            return validator.failure(error)

        result = result.set_position(node.start_position, node.end_position)
        return validator.success(result)

    def transverse_unary(self, node: UnaryOperationNode, context):
        result = RuntimeValidator()
        number = result.register(self.transverse(node.node, context))

        if result.error:
            return result

        error = None

        if node.operation.type == ArithmeticOperator.MINUS.name:
            number, error = number.multiplied_by(Number(-1))

        elif node.operation.matches(InWords.KEYWORDS.name, 'NOT'):
            number, error = number.logical_not()

        if error:
            return result.failure(error)

        number.set_position(node.start_position, node.end_position)
        return result.success(number)

    @staticmethod
    def transverse_number(node: NumberNode, context):
        number = Number(node.token.value)
        number.set_context(context)
        number.set_position(node.start_position, node.end_position)
        return RuntimeValidator().success(number)

    @staticmethod
    def transverse_string(node: StringNode, context):
        validator = RuntimeValidator()
        string = String(node.token.value).set_context(context)
        return validator.success(string)

    def transverse_error(self, node, context):
        # TODO: raising an exception
        pass

    def transverse_if_node(self, node: IfNode, context):
        validator = RuntimeValidator()

        for condition, expression in node.cases:
            condition_value = validator.register(self.transverse(condition, context))

            if validator.error:
                return validator

            if condition_value.is_true():
                expression_value = validator.register(self.transverse(expression, context))

                if validator.error:
                    return validator

                return validator.success(expression_value)

        if node.else_case:
            else_value = validator.register(self.transverse(node.else_case, context))

            if validator.error:
                return validator

            return validator.success(else_value)

        return validator.success(None)

    def transverse_for_node(self, node: ForNode, context):
        validator = RuntimeValidator()
        elements = []

        start_value = validator.register(self.transverse(node.start_value, context))
        if validator.error:
            return validator

        end_value = validator.register(self.transverse(node.end_value, context))
        if validator.error:
            return validator

        # TODO: try implementing default step_value of 1
        if node.step:
            step_value = validator.register(self.transverse(node.step, context))
            if validator.error:
                return validator
        else:
            step_value = Number(1)

        iteration = start_value.value

        if step_value.value >= 0:
            condition = lambda: iteration < end_value.value
        else:
            condition = lambda: iteration > end_value.value

        while condition():
            context.symbol_table.set(node.token.value, Number(iteration))
            iteration += step_value.value

            element = validator.register(self.transverse(node.loop_body, context))
            elements.append(element)

            if validator.error:
                return validator

        returned_list = List(elements)
        returned_list.set_context(context)
        returned_list.set_position(node.start_position, node.end_position)
        return validator.success(returned_list)

    def transverse_while_node(self, node: WhileNode, context):
        validator = RuntimeValidator()
        elements = []

        while True:
            condition = validator.register(self.transverse(node.condition, context))
            if validator.error:
                return validator

            if not condition.is_true():
                break

            element = validator.register(self.transverse(node.loop_body, context))
            elements.append(element)

            if validator.error:
                return validator

        returned_list = List(elements)
        returned_list.set_context(context)
        returned_list.set_position(node.start_position, node.end_position)
        return validator.success(returned_list)

    def transverse_list_node(self, node: ListNode, context):
        validator = RuntimeValidator()
        elements = []

        for element_node in node.element_node:
            element = validator.register(self.transverse(element_node, context))
            elements.append(element)
            if validator.error:
                return validator

        returned_list = List(elements)
        returned_list.set_context(context)
        returned_list.set_position(node.start_position, node.end_position)
        return validator.success(returned_list)

    @staticmethod
    def transverse_function_definition_node(node: FunctionDefinitionNode, context):
        validator = RuntimeValidator()

        function_name = node.token.value if node.token else None
        body = node.body
        arguments = [argument_name.value for argument_name in node.arguments]

        function = Function(function_name, body, arguments)
        function.set_context(context)
        function.set_position(node.start_position, node.end_position)

        if node.token:
            context.symbol_table.set(function_name, function)

        return validator.success(function)

    def transverse_callable_node(self, node: CallableNode, context):
        validator = RuntimeValidator()
        arguments = []

        callable_node = self.transverse(node.callable_node, context)
        value = validator.register(callable_node)

        if validator.error:
            return validator

        value = value.get_copy()
        value.set_position(node.start_position, node.end_position)

        for argument in node.arguments:
            current_argument = validator.register(self.transverse(argument, context))
            arguments.append(current_argument)

        result = value.execute(arguments)
        return_value = validator.register(result)
        if validator.error:
            return validator

        return validator.success(return_value)

    @staticmethod
    def operator_handler_factory(token_type, node):
        operators = {
            ArithmeticOperator.PLUS.name: node.added_to,
            ArithmeticOperator.MINUS.name: node.subbed_by,
            ArithmeticOperator.MULTIPLY.name: node.multiplied_by,
            ArithmeticOperator.DIVIDE.name: node.divided_by,
            ArithmeticOperator.POWER.name: node.powered_by,
            ComparisonOperator.COMPARISON.name: node.equals,
            ComparisonOperator.NOT_EQUALS.name: node.not_equals,
            ComparisonOperator.LESS_THAN.name: node.less_than,
            ComparisonOperator.GREATER_THAN.name: node.greater_than,
            ComparisonOperator.LESS_THAN_EQUALS.name: node.less_than_equals,
            ComparisonOperator.GREATER_THAN_EQUALS.name: node.greater_than_equals,
            ComparisonOperator.AND.name: node.logical_and,
            ComparisonOperator.OR.name: node.logical_or
        }

        return operators.get(token_type)

    def node_handler_factory(self, node):
        handlers = {
            'BinaryOperationNode': self.transverse_binary,
            'UnaryOperationNode': self.transverse_unary,
            'NumberNode': self.transverse_number,
            'VariableAccessNode': self.transverse_variable_access_node,
            'VariableAssignNode': self.transverse_variable_assign_node,
            'IfNode': self.transverse_if_node,
            'ForNode': self.transverse_for_node,
            'WhileNode': self.transverse_while_node,
            'StringNode': self.transverse_string,
            'FunctionDefinitionNode': self.transverse_function_definition_node,
            'CallableNode': self.transverse_callable_node,
            'ListNode': self.transverse_list_node
        }

        node_name = type(node).__name__
        return handlers.get(node_name, self.transverse_error)


