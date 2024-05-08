from entities.basic_position import BasicPosition


class ListNode(BasicPosition):
    def __init__(self, element_node):
        super().__init__()
        self.element_node = element_node
