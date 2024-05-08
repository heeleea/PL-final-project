from entities.basic_position import BasicPosition


class ForNode(BasicPosition):
    def __init__(self, token, start_value, end_value, step, loop_body, should_return_null):
        super().__init__()
        self.token = token
        self.start_value = start_value
        self.end_value = end_value
        self.step = step
        self.loop_body = loop_body
        self.should_return_null = should_return_null

        self.start_position = self.token.start_position
        self.end_position = self.loop_body.end_position
