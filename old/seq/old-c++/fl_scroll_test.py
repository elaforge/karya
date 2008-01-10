import sys, os
import fltk
from fltk import *

import futil, geom

class Track(Fl_Widget):
    bg_c = fl_rgb_color(0xff, 0xff, 0xf0)
    def __init__(self, x, y, w, h, label):
        Fl_Widget.__init__(self, x, y, w, h, label)

class Zoom(Fl_Scroll):
    def __init__(self, x, y, w, h, label):
        Fl_Scroll.__init__(self, x, y, w, h, label)
        self.type(0) # no scrollbars
        self.resizable(self)
        self.zoomx = 1
        self.zoomy = 1
        self.magnification_speed = 50.0
        self.zoom_center = None
        self.shift_down = 0
    def zoom(self, x, y, center):
        self.zoomx = x
        self.zoomy = y
        print 'zoom', (x, str(y)), 'at', center

    # def draw(self):
    #     fl_color(FL_WHITE)
    #     fl_rectf(self.x(), self.y(), self.w(), self.h())

    def handle(self, ev):
        if ev == FL_FOCUS:
            return True
        elif ev == FL_KEYDOWN:
            if Fl.event_key() in (FL_Shift_L, FL_Shift_R):
                self.shift_down += 1
                return True
        elif ev == FL_KEYUP:
            if Fl.event_key() in (FL_Shift_L, FL_Shift_R):
                self.shift_down -= 1
                return True
        elif ev == FL_PUSH:
            if self.shift_down:
                self.zoom_center = futil.Event_pos()
                return True
        elif ev == FL_RELEASE:
            self.zoom_center = None
        elif ev == FL_DRAG:
            if self.shift_down and self.zoom_center is not None:
                dy = self.zoom_center.y - Fl.event_y()
                if dy < 0:
                    z = 1.0 / -dy
                else:
                    z = 1 + dy / self.magnification_speed
                self.zoom(1, z, self.zoom_center)
        elif ev == FL_ENTER:
            return True
        return Fl_Scroll.handle(self, ev)

def main():
    w, h = 200, 200
    sbh = 25
    window = Fl_Window(w, h)
    window.resizable(window)
    z = Zoom(0, 0, w, h, 'track')
    btns = []
    for i in range(10):
        s = 'btn %d' % i
        bw = 75
        btns.append((Fl_Button(i*bw, 0, bw, h-sbh, s), s))
    sb = Fl_Scrollbar(0, h - sbh, w, sbh)
    window.end()
    window.show(sys.argv)
    return Fl.run()

if __name__ == '__main__':
    main()
