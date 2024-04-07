class NumberNode:
    def __init__(self, token):
        self.token = token

    def __repr__(self):
        return f"{self.token}"


class BinaryOperationNode:
    def __init__(self, left_node, operation, right_node):
        self.left_node = left_node
        self.operation = operation
        self.right_node = right_node

    def __repr__(self):
        return f"({self.left_node}, {self.operation}, {self.right_node})"


class UnaryOperationNode:
    def __init__(self, operation, node):
        self.operation = operation
        self.node = node

    def __repr__(self):
        return f"({self.operation}, {self.node})"
