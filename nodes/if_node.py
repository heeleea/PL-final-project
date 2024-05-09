from entities.basic_position import BasicPosition


class IfNode(BasicPosition):
    def __init__(self, cases, else_case):
        super().__init__()
        self.cases = cases
        self.else_case = else_case

        self.start_position = self.cases[0][0].start_position
        self.end_position = (self.else_case or self.cases[-1])[0].end_position
