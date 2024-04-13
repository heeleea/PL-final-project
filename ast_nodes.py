class BasicPosition:
    def __init__(self):
        self.start_position = None
        self.end_position = None


class NumberNode(BasicPosition):
    def __init__(self, token):
        super().__init__()
        self.token = token

        self.start_position = self.token.start_position
        self.end_position = self.token.end_position

    def __repr__(self):
        return f"{self.token}"


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


class UnaryOperationNode(BasicPosition):
    def __init__(self, operation, node):
        super().__init__()
        self.operation = operation
        self.node = node

        self.start_position = self.operation.start_position
        self.end_position = self.node.end_position

    def __repr__(self):
        return f"({self.operation}, {self.node})"


class VariableAccessNode(BasicPosition):
    def __init__(self, token):
        super().__init__()
        self.token = token

        self.start_position = self.token.start_position
        self.end_position = self.token.end_position


class VariableAssignNode(BasicPosition):
    def __init__(self, token, value):
        super().__init__()
        self.token = token
        self.value = value

        self.start_position = self.token.start_position
        self.end_position = self.value.end_position
