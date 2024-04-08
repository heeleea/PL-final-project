from ast_nodes import BinaryOperationNode, UnaryOperationNode, NumberNode


class SemanticalAnalysis:
    def transverse(self, node):
        method = self.node_handler_factory(node)
        return method(node)

    def transverse_binary(self, node: BinaryOperationNode) -> Number:
        left_operand = self.transverse(node.left_node)
        right_operand = self.transverse(node.right_node)

        operation_method = self.operation_handler_factory(node.operation.token_type, left_operand)
        result = operation_method(right_operand)
        return result.set_position(node.start_position, node.end_position)

    def transverse_unary(self, node: UnaryOperationNode):
        print("unary node")
        self.transverse_binary(node.node)

    def transverse_number(self, node: NumberNode):
        print("number node")

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

