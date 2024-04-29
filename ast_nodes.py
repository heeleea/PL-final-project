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


class StringNode(BasicPosition):
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


class IfNode(BasicPosition):
    def __init__(self, cases, else_case):
        super().__init__()
        self.cases = cases
        self.else_case = else_case

        self.start_position = self.cases[0][0].start_position
        self.end_position = (self.else_case or self.cases[-1][0]).end_position


class ForNode(BasicPosition):
    def __init__(self, token, start_value, end_value, step, loop_body):
        super().__init__()
        self.token = token
        self.start_value = start_value
        self.end_value = end_value
        self.step = step
        self.loop_body = loop_body

        self.start_position = self.token.start_position
        self.end_position = self.loop_body.end_position


class WhileNode(BasicPosition):
    def __init__(self, condition, loop_body):
        super().__init__()
        self.condition = condition
        self.loop_body = loop_body

        self.start_position = self.condition.start_position
        self.end_position = self.loop_body.end_position


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
