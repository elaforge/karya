import sys
from fltk import *
import fltk
import pygame.rect

import traceback, local.hack


'''
click: place cursor according to snap, edit text if clicked on
drag: select time

'''
# available damage bits: 0x10, 0x20, 0x40

class Pt:
    def __init__(self, x, y):
        self.x = x
        self.y = y
    def __cmp__(self, o):
        if o is None:
            return cmp((self.x, self.y), None)
        else:
            return cmp((self.x, self.y), (o.x, o.y))
    def __repr__(self):
        return 'Pt(%d, %d)' %(self.x, self.y)
def Event_pos():
    return Pt(Fl.event_x(), Fl.event_y())
Rect = pygame.rect.Rect
def Rect_pt(p1, p2):
    x = min(p1.x, p2.x)
    y = min(p1.y, p2.y)
    return Rect(x, y, max(p1.x, p2.x) - x, max(p1.y, p2.y) - y)
def Rect_sel(sel):
    return Rect_pt(sel[1], sel[2])

def fmt_damage(d):
    r = []
    for c in ['FL_DAMAGE_ALL', 'FL_DAMAGE_CHILD', 'FL_DAMAGE_EXPOSE',
            'FL_DAMAGE_OVERLAY', 'FL_DAMAGE_SCROLL']:
        if d & getattr(fltk, c):
            r.append(c)
    for c in (0x10, 0x20, 0x40):
        if d & c:
            r.append(hex(c))
    print ' '.join(r)

def draw_rect(r):
    fl_rectf(r.x, r.y, r.w, r.h)

def delta_rect(s, o):
    if (s.topleft == o.topleft or s.topright == o.topright
            or s.bottomleft == o.bottomleft or s.bottomright == o.bottomright):
        draw = []
        blank = []
        if s.left != o.left:
            r = Rect(min(s.left, o.left), s.top, abs(s.left - o.left), s.h)
            if s.left < o.left:
                draw.append(r)
            else:
                blank.append(r)
        if s.right != o.right:
            r = Rect(min(s.right, o.right), s.top,
                abs(s.right - o.right), s.h)
            if s.right < o.right:
                draw.append(r)
            else:
                blank.append(r)
        if s.top != o.top:
            r = Rect(s.left, min(s.top, o.top), s.w, abs(s.top - o.top))
            if s.top < o.top:
                draw.append(r)
            else:
                blank.append(r)
        if s.bottom != o.bottom:
            r = Rect(s.left, min(s.bottom, o.bottom),
                s.w, abs(s.bottom - o.bottom))
            if s.bottom < o.bottom:
                draw.append(r)
            else:
                blank.append(r)
    elif not (s.contains(o) or o.contains(s)):
        draw = [s]
        blank = [o]
    else:
        print "can't make delta:", s, o
    return draw, blank

def delta_rect(o, s):
    if (s.topleft == o.topleft or s.topright == o.topright
            or s.bottomleft == o.bottomleft or s.bottomright == o.bottomright):
        draw = []
        blank = []
        d = draw.append
        b = blank.append
        if s.left < o.left:
            d((s.left, s.top, o.left - s.left, s.h))
        elif s.left > o.left:
            b((o.left, o.top, s.left - o.left, o.h))
        if s.right > o.right:
            d((o.right, s.top, s.right - o.right, s.h))
        elif s.right < o.right:
            b((s.right, o.top, o.right - s.right, o.h))
        if s.top < o.top:
            d((s.left, s.top, s.w, o.top - s.top))
        elif s.top > o.top:
            b((o.left, o.top, o.w, s.top - o.top))
        if s.bottom > o.bottom:
            d((s.left, o.bottom, s.w, s.bottom - o.bottom))
        elif s.bottom < o.bottom:
            b((o.left, s.bottom, o.w, o.bottom - s.bottom))
    elif not (s.contains(o) or o.contains(s)):
        draw = [s]
        blank = [o]
    else:
        print "can't make delta:", s, o
    return map(Rect, draw), map(Rect, blank)

def test():
    r1 = (0, 0, 1, 1)
    r2 = (0, 0, 2, 1)
    print delta_rect(*map(Rect, (r1, r2)))
    r1 = (0, 0, 2, 2)
    r2 = (0, 0, 3, 1)
    print delta_rect(*map(Rect, (r1, r2)))

class Tcanvas(Fl_Widget):
    bg_c = fl_rgb_color(0xff, 0xff, 0xf0)
    sel_c = fl_rgb_color(0xee, 0xee, 0x9e)
    def __init__(self, x, y, w, h, label):
        Fl_Widget.__init__(self, x, y, w, h, label)
        self.text_blocks = {}
        self.drag_button = None
        self.drag_from = None
        self.dragger = Dragger(1)
        self.sel = self.old_sel = None
    def add_text(self, t, pos):
        id = hash((t, pos))
        self.text_blocks[id] = t, pos
        return id
    def del_text(self, id):
        # XXX undraw block
        del self.text_blocks[id]

    def draw(self):
        try:
            d = self.damage()
            # print fmt_damage(d)
            if d & FL_DAMAGE_ALL:
                fl_color(self.bg_c)
                fl_rectf(self.x(), self.y(), self.w(), self.h())
                self.old_sel = None
            if d & (FL_DAMAGE_ALL | FL_DAMAGE_EXPOSE):
                self.draw_sel()
        except:
            traceback.print_exc()
            raise
    def draw_sel(self):
        if self.sel is None:
            if self.old_sel is None:
                pass
            else:
                self.damage(FL_DAMAGE_EXPOSE, self.old_sel)
        else:
            if self.old_sel is None:
                self.damage(FL_DAMAGE_EXPOSE, sel)
            else:
                self.damage(FL_DAMAGE_EXPOSE, self.old_sel)
        if self.sel is None:
            s = Rect(0, 0, 0, 0)
        else:
            s = Rect_pt(self.sel[1], self.sel[2])
        if self.old_sel is None:
            o = Rect(0, 0, 0, 0)
        else:
            o = Rect_pt(self.old_sel[1], self.old_sel[2])
        draw, blank = delta_rect(o, s)
        fl_color(self.sel_c)
        for r in draw:
            draw_rect(r)
            # self.damage(FL_DAMAGE_EXPOSE, r.x, r.y, r.w, r.h)
        fl_color(self.bg_c)
        for r in blank:
            draw_rect(r)
            # self.damage(FL_DAMAGE_EXPOSE, r.x, r.y, r.w, r.h)
        self.old_sel = self.sel
    def draw_sel2(self):
        if self.sel is None:
            s = Rect(0, 0, 0, 0)
        else:
            s = Rect_pt(self.sel[1], self.sel[2])
        if self.old_sel is None:
            o = Rect(0, 0, 0, 0)
        else:
            o = Rect_pt(self.old_sel[1], self.old_sel[2])
        draw, blank = delta_rect(o, s)
        fl_color(self.sel_c)
        for r in draw:
            draw_rect(r)
            # self.damage(FL_DAMAGE_EXPOSE, r.x, r.y, r.w, r.h)
        fl_color(self.bg_c)
        for r in blank:
            draw_rect(r)
            # self.damage(FL_DAMAGE_EXPOSE, r.x, r.y, r.w, r.h)
        self.old_sel = self.sel

    def handle(self, ev):
        try:
            return self.handle_wrap(ev)
        except:
            traceback.print_exc()
            raise
    def handle_wrap(self, evt):
        r = self.dragger.handle(evt)
        sel = self.dragger.get_sel()
        if sel != self.sel:
            self.damage(FL_DAMAGE_EXPOSE)
            self.sel = sel
        return r

    def resize(self, x, y, w, h):
        Fl_Widget.resize(self, x, y, w, h)

class Dragger:
    def __init__(self, button=1):
        self.button = button
        self.drag_button = None
        self.drag_from = None
        self.drag_to = None
    def get_sel(self):
        if self.drag_from is None:
            return None
        else:
            return self.button, self.drag_from, self.drag_to
    def clear_sel(self):
        self.drag_from = None
    def handle(self, evt):
        btn = Fl.event_button()
        pos = Event_pos()
        if evt == FL_PUSH:
            return self.push(pos, btn)
        elif evt == FL_DRAG:
            return self.drag(pos, btn)
        elif evt == FL_RELEASE:
            return self.release(pos, btn)
        else:
            return
    def push(self, pos, btn):
        if self.drag_button is None:
            if btn == self.button:
                self.drag_button = btn
                self.drag_from = self.drag_to = pos
                return True
            else:
                return False
        else: # it was a chord, not interested in further dragging
            if btn == self.drag_button:
                print 'btn %d pressed twice' % btn
            return False
    def drag(self, pos, btn):
        if btn != self.button:
            return False
        elif self.drag_button is None:
            print 'dragging with no button down'
        elif btn != self.drag_button:
            print 'dragging with %d but started with %d' %(btn,
                self.drag_button)
        else:
            self.drag_to = pos
        return True
    def release(self, pos, btn):
        if btn != self.button:
            return False
        elif self.drag_button is None:
            print 'release %d with no push' % btn
        elif self.drag_from is None:
            # drag cancelled
            self.drag_button = self.drag_to = None
        else:
            # drag completed normally
            self.drag_button = None
        return True

class Text_block(Fl_Widget):
    def __init__(self, s):
        self.s = s
    def __hash__(self):
        return hash(self.s)

def main():
    w, h = 300, 200
    win = Fl_Window(w, h)
    tc = Tcanvas(0, 0, w, h, 'track')
    win.resizable(tc)
    win.end()
    win.show(sys.argv)
    tc.add_text(Text_block('just an example'), (0, 0))
    return Fl.run()


if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] == 't':
        test()
    else:
        sys.exit(main())
