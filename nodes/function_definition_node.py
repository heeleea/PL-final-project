from entities.basic_position import BasicPosition


class FunctionDefinitionNode(BasicPosition):
    def __init__(self, token, arguments, body, should_return_null):
        super().__init__()
        self.token = token
        self.arguments = arguments
        self.body = body
        self.should_return_null = should_return_null

        if self.token:
            self.start_position = self.token.start_position

        elif len(self.arguments) > 0:
            self.start_position = self.arguments[0].start_position

        else:
            self.start_position = self.body.start_position

        self.end_position = self.body.end_position
