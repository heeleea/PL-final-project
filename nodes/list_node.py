from entities.basic_position import BasicPosition


class ListNode(BasicPosition):
    def __init__(self, element_node, start_position, end_position):
        super().__init__()
        self.element_node = element_node

        self.start_position = start_position
        self.end_position = end_position
