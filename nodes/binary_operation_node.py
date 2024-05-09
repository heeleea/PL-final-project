from entities.basic_position import BasicPosition


class BinaryOperationNode(BasicPosition):
    def __init__(self, left_node, operation, right_node):
        super().__init__()
        self.left_node = left_node
        self.operation = operation
        self.right_node = right_node

        self.start_position = self.left_node.start_position
        self.end_position = self.right_node.end_position

    def __repr__(self):
        return f"({self.left_node}, {self.operation}, {self.right_node})"
