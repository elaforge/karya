import sys
from fltk import *

class Drawing(Fl_Widget):
    def __init__(self, X, Y, W, H, L):
        # always intialize the base class!
        Fl_Widget.__init__(self, X, Y, W, H, L)

    # override method draw
    def draw(self):
        # simply paints the canvas red
        fl_color(FL_RED)
        self.redraw()

    # implement event handler
    def handle(self, event):
        if event == FL_PUSH:
            print "FL_PUSH caught!"
            return 1
        else:
            return 0

def main():
    window = Fl_Window(300,500)
    d = Drawing(10,10,280,280, "test")
    window.end()
    window.show(len(sys.argv),sys.argv)
    Fl.run()

main()
