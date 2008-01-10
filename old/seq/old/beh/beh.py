# attempting behavioural abstraction in python

class Signal:
    def __init__(self, env):
        self.env = env
    def __setitem__(self, k, v):
        self.env[k] = v
    def __getitem__(self, k):
        return self.env[k]
    def read(self, n):
        'calculate and return n elements'
        pass

class Sine(Signal):
    def __init__(self):
    def get_block(self):
        return "sine-wave"
