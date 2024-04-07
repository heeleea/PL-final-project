from ast_nodes import BinaryOperationNode, UnaryOperationNode, NumberNode


class SemanticalAnalysis:
    def transverse(self, node):
        method = self.node_handler_factory(node)
        return method(node)

    def transverse_binary(self, node: BinaryOperationNode):
        print("binary node")
        self.transverse(node.left_node)
        self.transverse(node.right_node)

    def transverse_unary(self, node: UnaryOperationNode):
        print("unary node")
        self.transverse_binary(node.node)

    def transverse_number(self, node: NumberNode):
        print("number node")

    def transverse_no_visit(self, node):
        pass

    def node_handler_factory(self, node):
        handlers = {
            'BinaryOperationNode': self.transverse_binary,
            'UnaryOperationNode': self.transverse_unary,
            'NumberNode': self.transverse_number
        }

        node_name = type(node).__name__
        return handlers.get(node_name, self.transverse_no_visit)
