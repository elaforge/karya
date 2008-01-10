import fltk
import geom

def Event_pos():
    return geom.Pt(fltk.Fl.event_x(), fltk.Fl.event_y())

events = ('FL_PUSH', 'FL_DRAG', 'FL_RELEASE', 'FL_MOVE', 'FL_MOUSEWHEEL',
    'FL_ENTER', 'FL_LEAVE', 'FL_FOCUS', 'FL_UNFOCUS', 'FL_KEYDOWN',
    'FL_KEYUP', 'FL_SHORTCUT', 'FL_DEACTIVATE', 'FL_ACTIVATE',
    'FL_HIDE', 'FL_SHOW', 'FL_PASTE', 'FL_SELECTIONCLEAR',
    'FL_DND_ENTER', 'FL_DND_DRAG', 'FL_DND_LEAVE', 'FL_DND_RELEASE',
    )
eventd = {}
for e in events:
    eventd[getattr(fltk, e)] = e

def fmt_ev(ev):
    return eventd.get(ev, hex(ev))

damage_bits = ('FL_DAMAGE_ALL', 'FL_DAMAGE_CHILD', 'FL_DAMAGE_EXPOSE',
    'FL_DAMAGE_OVERLAY', 'FL_DAMAGE_SCROLL')

def fmt_damage(d):
    r = [ d for d in damage_bits if getattr(fltk, d) & d ]
    r += [ hex(d) for d in (0x10, 0x20, 0x40) if d & c ]
    return ' '.join(r)

