__author__ = "David Rusk <drusk@uvic.ca>"


class StatefulCollection(object):
    """
    An ordered collection of objects which have the notion of one of them
    being the 'current' object.
    """

    def __init__(self, items=None):
        if items is None:
            self.items = []
            self.index = -1
        else:
            self.items = items
            self.index = 0

    def __len__(self):
        return len(self.items)

    def __iter__(self):
        return iter(self.items)

    def __getitem__(self, index):
        return self.items[index]

    def append(self, item):
        """Adds a new item to the end of the collection."""
        if len(self) == 0:
            # Special case, we make this the current item
            self.index = 0

        self.items.append(item)

    def insert(self, index, item):
        """
        Insert item at index location.
        """
        self.items.insert(index, item)

    def get_index(self):
        """Returns the index of the current item."""
        return self.index

    def set_current_item(self, item):
        """Repplace the current item with the provided item"""
        self.items[self.index] = item

    def get_current_item(self):
        if self.items:
            return self.items[self.index]
        else:
            return None

    def __next__(self):
        """
        Make the next item in the collection the current item.  Wraps around
        to the beginning after reaching the end.
        """
        self._move(1)

    def previous(self):
        """
        Make the previous item in the collection the current item.  Wraps
        around to the end after reaching the beginning.
        """
        self._move(-1)

    def is_on_last_item(self):
        """
        Returns True if the current item is the last item in the collection.
        """
        return self.index == len(self) - 1

    def _move(self, delta):
        self.index = (self.index + delta) % len(self)