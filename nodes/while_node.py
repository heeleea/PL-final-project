from entities.basic_position import BasicPosition


class WhileNode(BasicPosition):
    def __init__(self, condition, loop_body, should_return_null):
        super().__init__()
        self.condition = condition
        self.loop_body = loop_body
        self.should_return_null = should_return_null

        self.start_position = self.condition.start_position
        self.end_position = self.loop_body.end_position
