from entities.basic_position import BasicPosition


class VariableAssignNode(BasicPosition):
    def __init__(self, token, value):
        super().__init__()
        self.token = token
        self.value = value

        self.start_position = self.token.start_position
        self.end_position = self.value.end_position
