from entities.basic_position import BasicPosition


class UnaryOperationNode(BasicPosition):
    def __init__(self, operation, node):
        super().__init__()
        self.operation = operation
        self.node = node

        self.start_position = self.operation.start_position
        self.end_position = self.node.end_position

    def __repr__(self):
        return f"({self.operation}, {self.node})"

