from typing import Union
from error import CostumedRunTimeError
from validators.run_time_validator import RuntimeValidator
from constans.token_names import ArithmeticOperator, ComparisonOperator, InWords

from entities.list import List
from entities.value import Value
from entities.number import Number
from entities.string import String

from built_ins.variables import NumberRunner
from entities.base_function import BaseFunction

from nodes.if_node import IfNode
from nodes.for_node import ForNode
from nodes.list_node import ListNode
from nodes.while_node import WhileNode
from nodes.call_node import CallableNode
from nodes.number_node import NumberNode
from nodes.string_node import StringNode
from nodes.unary_operation_node import UnaryOperationNode
from nodes.binary_operation_node import BinaryOperationNode
from nodes.variable_access_node import VariableAccessNode
from nodes.function_definition_node import FunctionDefinitionNode



class Function(BaseFunction):
    def __init__(self, name, arg_names, body, should_return_null):
        super().__init__(name)
        self.arg_names = arg_names
        self.body = body
        self.should_return_null = should_return_null

    def execute(self, args):
        validator = RuntimeValidator()
        semantical_analysis = SemanticalAnalysis()
        execution_context = self.generate_new_context()

        validator.register(self.check_and_populate_args(self.arg_names, args, execution_context))
        if validator.error:
            return validator

        value = validator.register(semantical_analysis.transverse(self.body, execution_context))
        if validator.error:
            return validator

        result = NumberRunner.null if self.should_return_null else value
        return validator.success(result)

    def get_copy(self):
        copy = Function(self.name, self.body, self.arg_names, self.should_return_null)
        copy.set_context(self.context)
        copy.set_position(self.start_position, self.end_position)
        return copy

    def __repr__(self):
        return f"<function {self.name}>"



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
        value.set_context(context)
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

        result.set_position(node.start_position, node.end_position)
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

        for condition, expression, should_return_null in node.cases:
            condition_value = validator.register(self.transverse(condition, context))

            if validator.error:
                return validator

            if condition_value.is_true():
                expression_value = validator.register(self.transverse(expression, context))

                if validator.error:
                    return validator

                result = Number.null if should_return_null else expression_value
                return validator.success(result)

        if node.else_case:
            expression, should_return_null = node.else_case
            else_value = validator.register(self.transverse(expression, context))

            if validator.error:
                return validator

            result = Number.null if should_return_null else else_value
            return validator.success(result)

        return validator.success(Number.null)

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
        result = Number.null if node.should_return_null else returned_list
        return validator.success(result)

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

        function = Function(function_name, body, arguments, node.should_return_null)
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

        return_value = return_value.get_copy()
        return_value.set_position(node.start_position, node.end_position)
        return_value.set_context(context)
        return validator.success(return_value)

    @staticmethod
    def get_operators_by_node(node):
        if isinstance(node, String):
            return {
                ArithmeticOperator.PLUS.name: node.added_to,
                ArithmeticOperator.MULTIPLY.name: node.multiplied_by
            }

        elif isinstance(node, List):
            return {
                ArithmeticOperator.PLUS.name: node.added_to,
                ArithmeticOperator.MINUS.name: node.subbed_by,
                ArithmeticOperator.MULTIPLY.name: node.multiplied_by,
                ArithmeticOperator.DIVIDE.name: node.divided_by
            }

        elif isinstance(node, Value):
            return {
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

    def operator_handler_factory(self, token_type, node):
        operators = self.get_operators_by_node(node)
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
