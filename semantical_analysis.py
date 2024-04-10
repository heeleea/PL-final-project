from token_utils import TokenOperation
from ast_nodes import BinaryOperationNode, UnaryOperationNode, NumberNode, BasicPosition


class Number(BasicPosition):
    def __init__(self, value):
        super().__init__()
        self.value = value
        # self.set_position()

    def set_position(self, start_position=None, end_position=None):
        self.start_position = start_position
        self.end_position = end_position
        return self

    def added_to(self, new_number):
        if isinstance(new_number, Number):
            return Number(self.value + new_number.value), None

    def subbed_by(self, new_number):
        if isinstance(new_number, Number):
            return Number(self.value - new_number.value), None

    def multiplied_by(self, new_number):
        if isinstance(new_number, Number):
            return Number(self.value * new_number.value), None

    def divided_by(self, new_number):
        if isinstance(new_number, Number):
            if new_number.value == 0:
                return None, RuntimeError(new_number.start_position, new_number.end_position, 'Division By Zero')

        return Number(self.value / new_number.value), None


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

    def transverse(self, node):
        method = self.node_handler_factory(node)
        return method(node)

    def transverse_binary(self, node: BinaryOperationNode):
        result = RuntimeValidator()
        left_operand = result.register(self.transverse(node.left_node))

        if result.error:
            return result

        right_operand = result.register(self.transverse(node.right_node))

        if result.error:
            return result

        operation_method = self.operation_handler_factory(node.operation.token_type, left_operand)
        result, error = operation_method(right_operand)

        if error:
            return result.failure(error)

        return result.set_position(node.start_position, node.end_position)

    def transverse_unary(self, node: UnaryOperationNode):
        result = RuntimeValidator()
        number = result.register(self.transverse(node.node))

        if result.error:
            return result

        error = None

        if node.operation.token_type == TokenOperation.MINUS.name:
            number = number.multiplied_by(Number(-1))

        if error:
            return result.failure(error)

        return result.success(number)

    def transverse_number(self, node: NumberNode):
        number = Number(node.token.value).set_position(node.start_position, node.end_position)
        return RuntimeValidator().success(number)

    def transverse_no_visit(self, node):
        pass

    @staticmethod
    def operation_handler_factory(token_type, node):
        operations = {
            TokenOperation.PLUS.name: node.added_to,
            TokenOperation.MINUS.name: node.subbed_by,
            TokenOperation.MULTIPLY.name: node.multiplied_by,
            TokenOperation.DIVIDE.name: node.divided_by
        }

        return operations.get(token_type)

    def node_handler_factory(self, node):
        handlers = {
            'BinaryOperationNode': self.transverse_binary,
            'UnaryOperationNode': self.transverse_unary,
            'NumberNode': self.transverse_number
        }

        node_name = type(node).__name__
        return handlers.get(node_name, self.transverse_no_visit)


