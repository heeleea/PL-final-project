from entities.basic_position import BasicPosition


class VariableAccessNode(BasicPosition):
    def __init__(self, token):
        super().__init__()
        self.token = token

        self.start_position = self.token.start_position
        self.end_position = self.token.end_position
