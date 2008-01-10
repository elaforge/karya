from __future__ import division
import sys, os
import fltk
from fltk import *

import futil, geom

'''
Track is a zoom
and group that puts in subwidgets and fills in background

'''

def wrect(w):
    return geom.Rect(w.x(), w.y(), w.w(), w.h())

class Zoom(Fl_Scroll):
    def __init__(self, x, y, w, h, label):
        Fl_Scroll.__init__(self, x, y, w, h, label)
        self.type(0) # no scrollbars
        self.resizable(self)
        self.zoomx = 1
        self.zoomy = 1
        self.magnification_speed = 100.0
        self.zoom_center = None
        self.shift_down = None
        self.children = []
        self.zoom_callback = None
    def py_add(self, w, label=''):
        self.children.append((w, label, wrect(w)))
    def zoom(self, zx, zy, center):
        zx = max(.01, zx)
        zy = max(.01, zy)
        zd = zx / self.zoomx
        print 's xpos', self.xposition()
        c = self.xposition() # + center.x # + .5
        nc = c * zd
        print center, zd, c, nc
        for (c, _, r) in self.children:
            x = int(r.x * zx)
            y = int(r.y * zy)
            w = int(r.w * zx)
            h = int(r.h * zy)
            c.resize(x, y, w, h)
            # if wrect(self).colliderect(x, y, w, h):
            #     # print _, r, x, y, w, h
            #     c.redraw()

        # self.position(int(nc - center.x + self.xposition()), 0)
        self.position(int(nc), 0)
        print 'xpos', self.xposition()
        self.damage(FL_DAMAGE_ALL)
        if self.zoom_callback:
            self.zoom_callback()
        self.zoomx = zx
        self.zoomy = zy
    def handle(self, ev):
        if ev == FL_FOCUS:
            return True
        elif ev == FL_KEYDOWN:
            if (Fl.event_key() in (FL_Shift_L, FL_Shift_R)
                    and self.shift_down is None):
                self.shift_down = Fl.event_key()
                fl_cursor(FL_CURSOR_NS)
                return True
        elif ev == FL_KEYUP:
            if (self.shift_down is not None
                    and Fl.event_key() == self.shift_down):
                self.shift_down = None
                fl_cursor(FL_CURSOR_DEFAULT)
                return True
        elif ev == FL_PUSH:
            if self.shift_down:
                self.zoom_center = futil.Event_pos()
                self.last_y = self.zoom_center.y
                return True
        elif ev == FL_RELEASE:
            if self.zoom_center is not None:
                self.zoom_center = None
        elif ev == FL_DRAG:
            if self.zoom_center is not None:
                dy = Fl.event_y() - self.last_y
                self.last_y = Fl.event_y()
                dy /= self.magnification_speed
                zf = self.zoomx + dy
                self.zoom(zf, 1, self.zoom_center)
                return True
        elif ev == FL_ENTER:
            if self.shift_down is not None and not Fl.get_key(self.shift_down):
                self.shift_down = None
                fl_cursor(FL_CURSOR_DEFAULT)
        return Fl_Scroll.handle(self, ev)

class Track(Zoom):
    bg_c = fl_rgb_color(0xff, 0xff, 0xf0)
    def __init__(self, x, y, w, h, label):
        Zoom.__init__(self, x, y, w, h, label)
        self.color(self.bg_c)
    def odraw(self):
        d = self.damage()
        if d & FL_DAMAGE_ALL:
            fl_color(self.bg_c)
            fl_rectf(self.x(), self.y(), self.w(), self.h())
            # self.draw_children()
        elif d & FL_DAMAGE_CHILD:
            self.draw_children()
        elif d & FL_DAMAGE_SCROLL:
            Zoom.draw(self)
        else:
            Zoom.draw(self)

class Tracks:
    def __init__(self, w, h):
        pass

class Window_scroll:
    sbh = 25
    def __init__(self, w, h):
        self.w = w
        self.h = h
        self.g = Fl_Group(0, 0, w, h)

        self.z = Track(0, 0, w, h-self.sbh, 'track1')
        self.z.zoom_callback = self.update_hsb
        self.btns = []
        nbtns = 10
        bw = 75
        self.total_w = bw * nbtns
        for i in range(nbtns):
            s = 'btn %d' % i
            # b = Fl_Button(i*bw, 0, bw // 2, (h-self.sbh) // 2, s)
            b = Fl_Button(i*bw, 0, bw, (h-self.sbh), s)
            self.z.py_add(b, s)
        # print [ x[2] for x in self.z.children]
        self.z.end()

        self.hsb = Fl_Scrollbar(0, h - self.sbh, w, self.sbh)
        self.hsb.type(FL_HORIZONTAL)
        self.hsb.callback(self.hscroll)
        self.update_hsb()
        self.g.end()
    def update_hsb(self):
        # int Fl_Slider::scrollvalue(int p, int W, int t, int l) {
        # //  p = position, first line displayed
        # //  w = window, number of lines displayed
        # //  t = top, number of first line
        # //  l = length, total number of lines
        z = self.z
        self.hsb.value(z.xposition(), z.w(), 0, int(self.total_w * z.zoomx))
    def hscroll(self, sb):
        self.z.position(sb.value(), 0)

def main():
    w, h = 200, 200
    total_w = 750
    sbh = 25
    window = Fl_Window(w, h)
    window.resizable(window)
    ws = Window_scroll(w, h)

    window.end()
    window.show(sys.argv)

    def zoom(z):
        z.zoom(z.zoomx + 1, z.zoomy, z.zoom_center)
    z = ws.z
    def f(zf):
        def f():
            z.zoom(zf, 1, geom.Pt(100, 100))
            print 'zxp', z.xposition()
        return f
    def pos(x):
        def f():
            z.position(x, 0)
            ws.update_hsb()
            print 'xp', z.xposition()
        return f
    Fl.add_timeout(.5, pos(100))
    Fl.add_timeout(1, f(1.5))
    Fl.add_timeout(1.5, f(2))
    # Fl.add_timeout(.5, f(2))
    # Fl.add_timeout(1, f(3))
    #     None)
    return Fl.run()

if __name__ == '__main__':
    main()
