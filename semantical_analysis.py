from typing import Union
from token_utils import Operation
from ast_nodes import BinaryOperationNode, UnaryOperationNode, NumberNode, BasicPosition, VariableAccessNode, VariableAssignNode
from error import CostumedRunTimeError


class Number(BasicPosition):
    def __init__(self, value):
        super().__init__()
        self.value = value
        self.context = None
        self.set_context()
        # self.set_position()

    def set_position(self, start_position=None, end_position=None):
        self.start_position = start_position
        self.end_position = end_position
        return self

    def set_context(self, context=None):
        self.context = context
        return self

    def added_to(self, new_number):
        if isinstance(new_number, Number):
            number = Number(self.value + new_number.value)
            number.set_context(self.context)
            return number, None

    def subbed_by(self, new_number):
        if isinstance(new_number, Number):
            number = Number(self.value - new_number.value)
            number.set_context(self.context)
            return number, None

    def multiplied_by(self, new_number):
        if isinstance(new_number, Number):
            number = Number(self.value * new_number.value)
            number.set_context(self.context)
            return number, None

    def divided_by(self, new_number):
        if isinstance(new_number, Number):
            if new_number.value == 0:
                return None, CostumedRunTimeError('Division By Zero', new_number.start_position, new_number.end_position, self.context)

        number = Number(self.value / new_number.value)
        number.set_context(self.context)
        return number, None

    def get_copy(self):
        copy = Number(self.value)
        copy.set_position(self.start_position, self.end_position)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        return str(self.value)


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

        operation_method = self.operation_handler_factory(node.operation.type, left_operand)
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

        if node.operation.type == Operation.MINUS.name:
            number = number.multiplied_by(Number(-1))

        if error:
            return result.failure(error)

        return result.success(number)

    @staticmethod
    def transverse_number(node: NumberNode, context):
        number = Number(node.token.value)
        number.set_context(context)
        number.set_position(node.start_position, node.end_position)
        return RuntimeValidator().success(number)

    def transverse_error(self, node, context):
        pass

    @staticmethod
    def operation_handler_factory(token_type, node):
        operations = {
            Operation.PLUS.name: node.added_to,
            Operation.MINUS.name: node.subbed_by,
            Operation.MULTIPLY.name: node.multiplied_by,
            Operation.DIVIDE.name: node.divided_by
        }

        return operations.get(token_type)

    def node_handler_factory(self, node):
        handlers = {
            'BinaryOperationNode': self.transverse_binary,
            'UnaryOperationNode': self.transverse_unary,
            'NumberNode': self.transverse_number,
            'VariableAccessNode': self.transverse_variable_access_node,
            'VariableAssignNode': self.transverse_variable_assign_node
        }

        node_name = type(node).__name__
        return handlers.get(node_name, self.transverse_error)


