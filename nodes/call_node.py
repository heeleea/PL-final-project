from entities.basic_position import BasicPosition


class CallableNode(BasicPosition):
    def __init__(self, callable_node, arguments):
        super().__init__()
        self.callable_node = callable_node
        self.arguments = arguments

        self.start_position = self.callable_node.start_position

        if len(self.arguments) > 0:
            self.end_position = self.arguments[-1].end_position
        else:
            self.end_position = self.callable_node.end_position
