# -*- coding: utf-8 -*-
"""A no-hardware parallel-port stand-in.

The generated PsychoPy script embeds its own copy of this class so that it is
self-contained. This standalone module is provided for testing and for users
who wish to import a shared mock. It mirrors the small slice of the PsychoPy
``parallel.ParallelPort`` interface that lexsync relies on (``setData``).
"""


class MockPort:
    """Record or print trigger codes instead of writing to hardware."""

    def __init__(self, record=False):
        self.record = record
        self.log = []

    def setData(self, data):
        value = int(data)
        if self.record:
            self.log.append(value)
        else:
            print("[lexsync mock trigger] %d" % value)
