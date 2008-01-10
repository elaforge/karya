'''
interface:
place text objects on canvas
zoom and scroll


text object:
'''
import sys, copy
import fltk
from fltk import *

import local.hack, traceback

class Pos:
    ''' window coordinate '''
    def __init__(self, x, y):
        self.x = x
        self.y = y
    def __repr__(self):
        return 'Pos(%r, %r)' %(self.x, self.y)
class Cpos:
    ''' character position '''
    def __init__(self, char, line):
        self.char = char
        self.line = line
    def __cmp__(self, o):
        return cmp((self.line, self.char), (o.line, o.char))
    def __repr__(self):
        return 'Cpos(%r, %r)' %(self.char, self.line)
def Cpos_at_eol(line):
    return Cpos(0, line+1)
class Crange:
    ''' character range, pair of Cpos '''
    def __init__(self, begin, end):
        if begin > end:
            begin, end = end, begin
        self.begin = begin
        self.end = end
    def contains(self, cpos):
        return self.begin <= cpos < self.end
    def overlaps(self, crange):
        return not (crange.end <= self.begin or crange.begin >= self.end)
    def __cmp__(self, o):
        return cmp((self.begin, self.end), (o.begin, o.end))
    def __repr__(self):
        return 'Crange(%r, %r)' %(self.begin, self.end)

def same_line(cpos1, cpos2):
    return (cpos1.line == cpos2.line
        or (cpos1.line == cpos2.line+1 and cpos1.char == 0)
        or (cpos2.line == cpos1.line+1 and cpos2.char == 0))

class Text_buffer:
    def __init__(self):
        self.s = ('hello', 'there', 'world', 'mo')
    def get_text(self, crange):
        # grossly inefficient
        lines = self.s[crange.begin.line : crange.end.line + 1]
        lines = list(lines)
        lines[0] = lines[0][crange.begin.char:]
        lines[-1] = lines[-1][:crange.end.char]
        s = '\n'.join(lines)
        return s
    def get_line(self, line):
        # return line, without \n
        return self.s[line]
        s = self.get_text(Crange(Cpos(0, line), Cpos(0, line+1)))
        if s[-1] == '\n':
            s = s[:-1]
        return s
    def char(self, cpos):
        return self.s[cpos.line][cpos.char]
    def nlines(self):
        return len(self.s)

delimiters = ('{}', '[]', '()', '<>', '``', "''", '""')
open_delimiters = ''.join([ open for (open, close) in delimiters ])
close_delimiters = ''.join([ close for (open, close) in delimiters ])

def find_opening_delim(buffer, end, delim):
    '''scan backward along 'buffer' looking for opposite of 'delim' and return
        its cpos.  Return 'end' if not found. '''
    for open, close in delimiters:
        if delim == close:
            seek = open
            break
    return end
def find_closing_delim(buffer, start, delim):
    '''scan forward along buffer looking for opposite of 'delim' and return
        its cpos.  Return 'start' if not fonud. '''
    for open, close in delimiters:
        if delim == open:
            seek = close
            break
    return start

class Text(fltk.Fl_Widget):
    select_button, execute_button, search_button = 1, 2, 3
    cut_chord = 1, 2
    paste_chord = 1, 3
    selection_colors = (
        (FL_BLACK, fl_rgb_color(0xee, 0xee, 0x9e)), # dark yellow
        (FL_WHITE, fl_rgb_color(0xff, 0, 0)), # red
        (FL_WHITE, fl_rgb_color(0x88, 0xcc, 0x88)), # medium green
    )
    bg_c = fl_rgb_color(0xff, 0xff, 0xdd)
    def __init__(self, x, y, w, h, label=''):
        fltk.Fl_Widget.__init__(self, x, y, w, h, label)
        self.drag_button = None
        self.drag_from = None
        self.buffer = Text_buffer()
        self.selections = [None, None, None]
        fl_font(FL_HELVETICA, 14)
        self.font_height = 14 # fl_height()
        self.visible_lines = (0, h // self.font_height + 1)
        self.damage_ranges = []
    def win_coords(self, pos):
        return Pos(pos.x + self.x(), pos.y + self.y())
    def pos_to_cpos(self, pos):
        '''window coordinates 'pos' to Cpos in text'''
        pos = self.win_coords(pos)
        # XXX assume unscrolled
        line = pos.y // self.font_height
        first_line, last_line = self.visible_lines
        last_line = min(self.buffer.nlines(), last_line)
        if line < first_line:
            return Cpos(0, 0)
        elif line >= last_line:
            return Cpos(len(self.buffer.get_line(last_line-1)), last_line-1)
        s = self.buffer.get_line(line)
        char = 0
        xpos = 0
        # if fl_width(uchar) is a table lookup, fine, else make this more
        # efficient...
        while char < len(s):
            cwidth = fl_width(s[char])
            xpos += cwidth
            if xpos > pos.x:
                break
            char += 1
        return Cpos(char, line)
    def cpos_to_pos(self, cpos):
        ''' 'cpos' to window coordinates 'pos', aligned to the top of the text
            row '''
        s = self.buffer.get_line(cpos.line)
        y = cpos.line * self.font_height
        x = 0
        i = 0
        while i < cpos.char:
            x += fl_width(s[i])
            i += 1
        # fl_width returns floats, so truncate to pixel resolution here...
        return self.win_coords(Pos(int(x), int(y)))
    def crange_to_rect(self, crange):
        begin = self.cpos_to_pos(crange.begin)
        end = self.cpos_to_pos(crange.end)
        if crange.end.char == 0:
            end.y -= self.font_height
            end.x = self.w()
        else:
            end.y += self.font_height
        return begin, end
    def set_selection(self, crange, mode):
        '''set selection to 'crange'.  'mode' is 'select_button',
            'execute_button', or 'search_button'.  If 'crange' is None,
            remove selection (not valid for 'select_button').'''
        if mode == self.select_button:
            assert crange is not None
        old = self.selections[mode-1]
        new = crange
        if old is None:
            self.damage_ranges.append(new)
        elif old.begin == new.begin and old.end == new.end:
            pass # no changes
        elif new.end <= old.begin or new.begin >= old.end:
            # non-contiguous
            self.damage_ranges.append(old)
            self.damage_ranges.append(new)
        elif new.begin < old.begin:
            self.damage_ranges.append(Crange(new.begin, old.begin))
        elif new.end > old.end:
            self.damage_ranges.append(Crange(new.end, old.end))
        self.damage(FL_DAMAGE_EXPOSE) # XXX not sure what the FL_DAMAGE_* do
        self.selections[mode-1] = crange
    def get_selection(self, mode):
        '''return crange of selection 'mode' '''
        return self.selections[mode-1]
    def get_text(self, crange):
        '''return text in buffer from range 'crange' '''
        return self.buffer.get_text(crange)
    def select_word(self, cpos):
        return Crange(Cpos(0, 0), Cpos(0, 0))
    def select_double_click(self, cpos):
        ''' 'cpos' to Crange (start of word -> end of word)
            select line if before 1st char or after last
            select to matching paren / brace / bracket / etc. if inside
            select word otherwise'''
        line_len = len(self.buffer.get_line(cpos.line))
        if cpos.char == 0 or cpos.char == line_len:
            return Crange(Cpos(0, cpos.line), Cpos(line_len, cpos.line))
        elif (self.buffer.char(Cpos(cpos.char-1, cpos.line))
                in open_delimiters):
            return Crange(cpos, find_closing_delim(self.buffer, cpos,
                self.buffer.char(Cpos(cpos.char-1, cpos.line))))
        elif (self.buffer.char(Cpos(cpos.char+1, cpos.line))
                in close_delimiters):
            return Crange(cpos, find_opening_delim(self.buffer, cpos,
                self.buffer.char(Cpos(cpos.char+1, cpos.line))))
        else:
            return self.select_word(cpos)
    def cut(self, crange):
        ''' cut 'crange' '''
        print 'cut', crange
    def paste(self, crange):
        ''' paste 'crange', return crange of pasted text '''
        print 'paste', crange
        return crange
    def handle_chord(self, cpos, b1, b2):
        if (b1, b2) == self.cut_chord:
            self.cut(Crange(self.drag_from, cpos))
            self.set_selection(Crange(self.drag_from, self.drag_from), b1)
        elif (b1, b2) == self.paste_chord:
            crange = self.paste(Crange(self.drag_from, cpos))
            self.set_selection(crange, b1)
        else: # cancel drag
            self.set_selection(None, self.drag_button)
            self.drag_button = self.drag_from = None
    def handle(self, event):
        try:
            return self.handle_wrap(event)
        except:
            traceback.print_exc()
            sys.exit(1)
    def handle_wrap(self, event):
        if event == FL_PUSH:
            button = Fl.event_button()
            cpos = self.pos_to_cpos(Pos(Fl.event_x(), Fl.event_y()))
            if self.drag_button is None:
                self.drag_button = button
                self.drag_from = cpos
            else:
                self.handle_chord(cpos, self.drag_button, button)
                # after a chord, we're done dragging
                self.drag_button = self.drag_from = None
            if button == self.select_button:
                clicks = Fl.event_clicks()
                if clicks:
                    Fl.event_clicks(0)
                self.handle_select(cpos, clicks)
        elif event == FL_DRAG:
            # it could be None if we canceled a drag
            if self.drag_button is not None:
                cpos = self.pos_to_cpos(Pos(Fl.event_x(), Fl.event_y()))
                self.set_selection(Crange(self.drag_from, cpos),
                    self.drag_button)
        elif event == FL_RELEASE:
            if self.drag_button is None:
                return 1 # must be post-chord, do nothing
            button = Fl.event_button()
            cpos = self.pos_to_cpos(Pos(Fl.event_x(), Fl.event_y()))
            if button == self.execute_button:
                self.handle_execute(cpos)
            elif button == self.search_button:
                self.handle_search(cpos)
            if button != self.select_button:
                # non-select button deselects immediately
                self.set_selection(None, button)
            self.drag_button = self.drag_from = None
        else:
            return 0
        return 1
    def handle_select(self, cpos, double_click):
        ''' Set selection.  This is called by the FL_PUSH event rather than
            FL_RELEASE, since we can select immediately.'''
        if double_click:
            self.set_selection(self.select_double_click(cpos),
                self.select_button)
        else:
            self.set_selection(Crange(cpos, cpos), self.select_button)
    def get_action_text(self, cpos):
        if cpos == self.drag_from:
            if self.get_selection(self.select_button).contains(cpos):
                return self.get_text(self.get_selection(self.select_button))
            else:
                return self.get_text(self.select_word(cpos))
        else:
            return self.get_text(self.get_selection(self.drag_button))
    def handle_execute(self, cpos):
        s = self.get_action_text(cpos)
        print 'execute', `s`
    def handle_search(self, cpos):
        s = self.get_action_text(cpos)
        print 'search', `s`

    def draw(self):
        '''draw text and selections'''
        try:
            if not self.damage_ranges:
                self.redraw_rect(None)
            else:
                for crange in self.damage_ranges:
                    self.redraw_crange(crange)
                self.damage_ranges[:] = []
        except:
            traceback.print_exc()
            sys.exit(1)
    def redraw_crange(self, crange):
        begin, end = crange.begin, crange.end
        if same_line(begin, end):
            self.redraw_rect(crange)
        else:
            self.redraw_rect(Crange(begin, Cpos_at_eol(begin.line)))
            if end.line > begin.line + 1:
                self.redraw_rect(Crange(Cpos(0, begin.line + 1),
                    Cpos_at_eol(end.line - 1)))
            self.redraw_rect(Crange(Cpos(0, end.line),
                Cpos(end.line, end.char)))
    def redraw_rect(self, crange=None):
        if crange is None:
            b = Pos(self.x(), self.y())
            e = Pos(self.w(), self.h())
        else:
            b, e = self.crange_to_rect(crange)
        fl_push_clip(b.x, b.y, e.x, e.y)
        fl_color(self.bg_c)
        fl_rectf(b.x, b.y, e.x, e.y)
        if crange is not None:
            for i, sel in enumerate(self.selections):
                if sel is None or not crange.overlaps(sel):
                    continue
                if sel.begin == sel.end:
                    continue
                sb = self.cpos_to_pos(sel.begin)
                se = self.cpos_to_pos(sel.end)
                fl_color(self.selection_colors[i][1])
                fl_rectf(sb.x, sb.y, se.x-sb.x, se.y-sb.y)
        self.draw_text(crange)
        sel = self.selections[0]
        if sel and sel.begin == sel.end:
            self.draw_cursor(sel.begin)
    def draw_cursor(self, cpos):
        pos = self.cpos_to_pos(cpos)
        fl_line_style(FL_SOLID | FL_JOIN_MITER, 2)
        fl_color(FL_BLUE)
        fl_line(pos.x, pos.y, pos.x, pos.y + self.font_height)
        fl_line_style(0)
    def draw_text(self, crange):
        print 'drawtext', crange
        fl_font(FL_HELVETICA, 14)
        fl_color(FL_BLACK)
        if crange:
            b, e = crange.begin, crange.end
        if crange is None:
            try:
                for line in range(int(self.h() / self.font_height + 1)):
                    self.draw_text_line(line, 0)
            except IndexError:
                pass
        elif same_line(b, e):
            if e.char == 0:
                self.draw_text_line(b.line, b.char)
            else:
                self.draw_text_line(b.line, b.char, e.char)
        else:
            self.draw_text_line(b.line, b.char)
            for line in range(b.line + 1, e.line):
                self.draw_text_line(line, 0)
            self.draw_text_line(e.line, 0, e.char)
    def draw_text_line(self, line, bc, ec=None):
        b = self.cpos_to_pos(Cpos(bc, line))
        s = self.buffer.get_line(line)
        if ec is None:
            s = s[bc:]
        else:
            s = s[bc:ec]
        print 'text', `s`, line, (bc, ec), b
        fl_draw(s, len(s), b.x, b.y - fl_descent() + fl_height())


def main():
    w, h = 300, 180
    window = Fl_Window(w, h)
    txt = Text(0, 0, w, h, 'track')
    window.end()
    window.show(sys.argv)
    return Fl.run()

def test():
    buf = Text_buffer()
    print `buf.get_line(0)`
    print `buf.get_line(1)`
    print `buf.get_text(Crange(Cpos(3, 0), Cpos(4, 1)))`

def test_text():
    txt = Text(0, 0, 300, 180)
    for x, y in ((0, 0), (90, 0), (0, 90)):
        print (x, y), txt.pos_to_cpos(Pos(x, y))

def text():
    w, h = 300, 180
    window = Fl_Window(w, h)
    buf = Fl_Text_Buffer()
    buf.text('hello world\nthis is a buffer\nhi?')
    txt = Fl_Text_Display(0, 0, w, h)
    txt.buffer(buf)
    txt.show()

    window.show(sys.argv)
    return Fl.run()


if __name__ == '__main__':
    main()

    # def draw_selection(self, crange, (text_c, box_c)):
    #     begin, end = crange.begin, crange.end
    #     if begin.line == end.line and begin.char == end.char:
    #         self.draw_cursor(begin)
    #         return
    #     b = self.cpos_to_pos(begin)
    #     e = self.cpos_to_pos(end)
    #     fl_color(box_c)
    #     if begin.line == end.line:
    #         print 'same line', b, e
    #         fl_rectf(b.x, b.y, e.x-b.x, self.font_height)
    #     else:
    #         fl_rectf(b.x, b.y, self.w(), self.font_height)
    #         if end.line > begin.line + 1:
    #             fl_rectf(self.x(), b.y + self.font_height,
    #                 self.w(), e.y-b.y - self.font_height)
    #         fl_rectf(self.x(), e.y, e.x, self.font_height)
    # def draw_text(self, x, y, w, h):
    #     fl_font(FL_HELVETICA, 14)
    #     line_y = 0
    #     # should calculate where we are in the buffer for current scroll
    #     buf_line = 0 
    #     # fl_push_clip(x, y, w, h)
    #     while line_y < y + h:
    #         try:
    #             s = self.buffer.get_line(buf_line)
    #         except IndexError:
    #             break
    #         self.draw_text_line(s, x, line_y, FL_BLACK)
    #         line_y += self.font_height
    #         buf_line += 1
    #     # fl_pop_clip()
    # def draw_text_line(self, s, x, y, color):
    #     fl_color(color)
    #     fl_draw(s, len(s), x, y - fl_descent() + fl_height())

    # def redraw_selection(self, old, new, mode):
    #     # find the difference between old and new
    #     if old is None:
    #         begin, end = new.begin, new.end
    #         self.draw_selection(begin, end, mode, False)
    #     elif new.begin == old.begin and new.end == old.end:
    #         pass
    #     elif new.end <= old.begin or new.begin >= old.end:
    #         # non-contiguous
    #         self.draw_selection(old.begin, old.end, None, True)
    #         self.draw_selection(new.begin, new.end, mode, False)
    #     else:
    #         # contiguous, optimize drawing
    #         if new.begin != old.begin:
    #             if new.begin < old.begin:
    #                 begin, end = new.begin, old.begin
    #                 erase = False
    #             elif new.begin > old.begin:
    #                 begin, end = old.begin, new.begin
    #                 erase = True
    #         elif new.end != old.end:
    #             if new.end > old.end:
    #                 begin, end = old.end, new.end
    #                 erase = False
    #             elif new.end < old.end:
    #                 begin, end = new.end, old.end
    #                 erase = True
    #         self.draw_selection(begin, end, mode, erase)
    # def draw_selection(self, begin, end, mode, erase):
    #     beginp = self.cpos_to_pos(begin)
    #     endp = self.cpos_to_pos(end)
    #     endp.y += self.font_height
    #     if erase:
    #         text_c, box_c = self.bg_c, FL_BLACK
    #     else:
    #         text_c, box_c = self.selection_colors[mode-1]
    #     if begin.line == end.line and begin.char == end.char:
    #         fl_line_style(FL_SOLID | FL_JOIN_MITER, 2)
    #         if erase:
    #             print '++erase cursor', begin, beginp
    #             fl_color(self.bg_c)
    #         else:
    #             print '++draw cursor', begin, beginp
    #             fl_color(FL_BLUE)
    #         fl_line(beginp.x, beginp.y, endp.x, endp.y)
    #         fl_line_style(0)
    #     else:
    #         s = self.get_text(Crange(begin, end))
    #         if erase:
    #             print '++erase sel', begin, end, `s`
    #         else:
    #             print '++draw sel', begin, end, `s`
    #         fl_color(box_c)
    #         fl_rectf(beginp.x, beginp.y,
    #             endp.x-beginp.x, endp.y-beginp.y, box_c)
    #         self.draw_text_line(s, beginp.x, beginp.y, text_c)
