import pygame.rect
import fltk

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

Rect = pygame.rect.Rect
def Rect_pt(p1, p2):
    x = min(p1.x, p2.x)
    y = min(p1.y, p2.y)
    return Rect(x, y, max(p1.x, p2.x) - x, max(p1.y, p2.y) - y)
def Rect_sel(sel):
    return Rect_pt(sel[1], sel[2])

def rect_diff(r1, r2):
    if r1.contains(r2):
        return [r1]
    elif r2.contains(r1):
        return [r2]
    else:
        return rect_sub(r1, r2) + rect_sub(r2, r1)

def rect_sub(r1, r2):
    if not r1.contains(r2):
        return [r1] # shortcut, and also avoids returning r1 in slices
    v = []
    a = v.append
    if r1.y < r2.y:
        a(Rect(r1.left, r1.top, r1.w, min(r1.h, r2.top - r1.top)))
    if r2.bottom < r1.bottom:
        a(Rect(r1.left, max(r1.top, r2.bottom),
            r1.w, min(r1.h, r1.bottom - r2.bottom)))
    if r1.x < r2.x and not (r2.top >= r1.bottom or r2.bottom <= r1.top):
        a(Rect(r1.x, r2.y, min(r1.w, r2.x - r1.x),
            min(r2.h, r1.bottom - r2.top, r2.bottom - r1.top)))
    if r2.right < r1.right and not(r2.top >= r1.bottom or r2.bottom <= r1.top):
        a(Rect(max(r1.left, r2.right), max(r1.top, r2.top),
            min(r1.w, r1.right - r2.right),
            min(r2.h, r1.bottom - r2.top, r2.bottom - r1.top)))
    return v

def test():
    r1 = (2, 2, 3, 3)
    for r2 in ([ (x, y, 1, 1) for y in (0, 1, 5, 6) for x in range(7) ]
            + [ (x, y, 1, 1) for y in (2, 3, 4) for x in (0, 1, 5, 6) ]):
        # no intersection
        t = map(tuple, rect_diff(Rect(*r1), Rect(*r2)))
        assert len(t) == 2, (r1, r2, t)
        assert t[0] == r1 and t[1] == r2, (r1, r2, t)
    t1s = []
    for r2 in [ (x, y, 1, 1) for y in (2, 4) for x in (2, 4) ]:
        t1s.append(map(tuple, rect_sub(Rect(*r1), Rect(*r2))))
        t2 = map(tuple, rect_sub(Rect(*r2), Rect(*r1)))
        assert [r2] == t2, (r2, t2)
    assert t1s == [
        [(2, 3, 3, 2), (3, 2, 2, 1)],
        [(2, 3, 3, 2), (2, 2, 2, 1)],
        [(2, 2, 3, 2), (3, 4, 2, 1)],
        [(2, 2, 3, 2), (2, 4, 2, 1)]]
    return

def test2():
    r1 = Rect(0, 0, 2, 1)
    r2 = Rect(0, 0, 1, 1)
    print rect_sub(r1, r2)
    # r1 = Rect(0, 0, 2, 2)
    # print rect_diff(r1, r1.move(1, 0))

