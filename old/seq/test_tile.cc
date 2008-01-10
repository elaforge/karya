#include <FL/Fl.H>
#include <FL/Fl_Double_Window.H>
#include <FL/Fl_Box.H>
#include <FL/Fl_Button.H>

#include "Tile_ext.h"

typedef Tile_ext Tile;

Fl_Double_Window *w;
Tile *v;
Fl_Button *resize;
Fl_Button *zoom;

void testf(void *)
{
}

void resize_cb(Fl_Widget *w, void *)
{
	resize->value(1);
	zoom->value(0);
}

void zoom_cb(Fl_Widget *w, void *)
{
	resize->value(0);
	zoom->value(1);
}

int main(int argc, char ** argv) {
	w = new Fl_Double_Window(500, 560, "Fl_Versatile");
	v = new Tile(10, 10, 480, 480);
	v->box(FL_THIN_DOWN_BOX);
	Fl_Box *ul = new Fl_Box(10, 10, 240, 240, "ul");
	ul->color(121);
	ul->box(FL_THIN_DOWN_BOX);
	Fl_Box *ur = new Fl_Box(250, 10, 240, 240, "ur");
	ur->color(141);
	ur->box(FL_THIN_DOWN_BOX);
	Fl_Box *ll = new Fl_Box(10, 250, 240, 240, "ll");
	ll->color(151);
	ll->box(FL_THIN_DOWN_BOX);
	Fl_Box *lr = new Fl_Box(250, 250, 240, 240, "lr");
	lr->color(171);
	lr->box(FL_THIN_DOWN_BOX);
	// Fl_Box *resizable = new Fl_Box(30,30,440,440);
	v->end();
	// v->resizable(resizable);

	Fl_Group * g = new Fl_Group(10, 500, 480, 30);
	Fl_Box * b = new Fl_Box(250, 500, 230, 30);
	resize = new Fl_Button(10, 500, 60, 30, "resize");
	resize->type(FL_TOGGLE_BUTTON);
	resize->value(1);
	resize->callback(resize_cb);
	zoom = new Fl_Button(70, 500, 60, 30, "zoom");
	zoom->type(FL_TOGGLE_BUTTON);
	zoom->callback(zoom_cb);
	g->end();
	g->resizable(b);

	w->end();
	w->resizable(v);
	w->size_range(220, 160);

	Fl::visual(FL_DOUBLE|FL_INDEX);
	w->show();
	Fl::add_timeout(.75, testf);
	Fl::run();
	return 0;
};

/*
// move the lower-right corner (sort of):
void Fl_Versatile::resize(int X,int Y,int W,int H) {
  // remember how much to move the child widgets:
  int xw = x();
  int yw = y();
  int ww = w();
  int hw = h();
  int dx = X-xw;
  int dy = Y-yw;
  int dw = W-ww;
  int dh = H-hw;
  short* p = sizes();
  // resize this (skip the Fl_Group resize):
  Fl_Widget::resize(X,Y,W,H);
  // find bottom-right of resiable:
  int OR = p[5];
  int NR = X+W-(p[1]-OR);
  int OB = p[7];
  int NB = Y+H-(p[3]-OB);
  int OL = p[4];
  int NL = X + OL - p[0];
  int OT = p[6];
  int NT = Y + OT - p[2];
  // move everything to be on correct side of new resizable:
  Fl_Widget*const* a = array();
  p += 8;
  for (int i=children(); i--;) {
    Fl_Widget* o = *a++;
    int X = o->x()+dx;
    int R = X+o->w();
    // left & right side
    if (drag_horizontal_ == DRAG_RIGHT) {
      if (*p++ > OL) { X += dw; if (X < NL) X = NL; }
      if (*p++ > OL) { R += dw; if (R < NL) R = NL; }
    }
    else if (drag_horizontal_ == DRAG_PROPORTIONAL_TILE) {
      if (*p >= OR) X += dw; 
      else if (*p > OL) {
	int ddw = (int) ( (float) dw * (float) (X - xw) / (float) (ww) + 0.5 );
	X += ddw;
	if (X < NL) X = NL;
	if (X > NR) X = NR;
      }
      else if (X > NR) X = NR;
      p++;
      if (*p >= OR) R += dw; 
      else if (*p > OL) {
	int ddw = (int) ( (float) dw * (float) (R - xw) / (float) (ww) + 0.5 );
	R += ddw;
	if (R < NL) R = NL; 
	if (R > NR) R = NR;
      }
      else if (R > NR) R = NR;
      p++;
    }
    else if (drag_horizontal_ == DRAG_PROPORTIONAL_RESIZABLE) {
      Fl_Widget * r = resizable();
      if (*p >= OR) X += dw; 
      else if (*p > OL) {
	int ddw = (int) ( (float) dw * (float) (X - r->x()) / (float) (r->w()) + 0.5 );
	X += ddw;
	if (X < NL) X = NL;
	if (X > NR) X = NR;
      }
      else if (X > NR) X = NR;
      p++;
      if (*p >= OR) R += dw; 
      else if (*p > OL) {
	int ddw = (int) ( (float) dw * (float) (R - r->x()) / (float) (r->w()) + 0.5 );
	R += ddw;
	if (R < NL) R = NL; 
	if (R > NR) R = NR;
      }
      else if (R > NR) R = NR;
      p++;
    }
    else {
      if (*p++ >= OR) X += dw; else if (X > NR) X = NR;
      if (*p++ >= OR) R += dw; else if (R > NR) R = NR;
    }
    int Y = o->y()+dy;
    int B = Y+o->h();
    // top & bottom
    if (resize_verticalical_ == DRAG_DOWN) {
      if (*p++ > OT) { Y += dh; if (Y < NT) Y = NT; }
      if (*p++ > OT) { B += dh; if (B < NT) B = NT; }
    }
    else if (resize_verticalical_ == DRAG_PROPORTIONAL_TILE) {
      if (*p >= OB) X += dw; 
      else if (*p > OT) {
	int ddh = (int) ( (float) dh * (float) (Y - yw) / (float) (hw) + 0.5 );
	Y += ddh;
	if (Y < NT) Y = NT;
	if (Y > NB) Y = NB;
      }
      else if (Y > NB) Y = NB;
      p++;
      if (*p >= OB) B += dh; 
      else if (*p > OT) {
	int ddh = (int) ( (float) dh * (float) (B - yw) / (float) (hw) + 0.5 );
	B += ddh;
	if (B < NT) B = NT;
	if (B > NB) B = NB; 
      }
      else if (B > NB) B = NB;
      p++;
    }
    else if (resize_verticalical_ == DRAG_PROPORTIONAL_RESIZABLE) {
      Fl_Widget * r = resizable();
      if (*p >= OB) X += dw; 
      else if (*p > OT) {
	int ddh = (int) ( (float) dh * (float) (Y - r->y()) / (float) (r->h()) + 0.5 );
	Y += ddh;
	if (Y < NT) Y = NT;
	if (Y > NB) Y = NB;
      }
      else if (Y > NB) Y = NB;
      p++;
      if (*p >= OB) B += dh; 
      else if (*p > OT) {
	int ddh = (int) ( (float) dh * (float) (B - r->y()) / (float) (r->h()) + 0.5 );
	B += ddh;
	if (B < NT) B = NT;
	if (B > NB) B = NB; 
      }
      else if (B > NB) B = NB;
      p++;
    }
    else {
      if (*p++ >= OB) Y += dh; else if (Y > NB) Y = NB;
      if (*p++ >= OB) B += dh; else if (B > NB) B = NB;
    }
    o->resize(X,Y,R-X,B-Y); o->redraw();
  }
}

*/
/*
// if (original left edge right of original right edge) x += dw
// else clamp x at new right edge
//		widgets right of O move right with dw, otherwise limit pos to screen
// if (original right edge right of OR) right += dw
// else clamp right at new right edge
//		right edge widgets grow with the dw, otherwise limit size to screen

void Scale_tile::resize(int X,int Y,int W,int H) {
	// remember how much to move the child widgets:
	int dx = X-x();
	int dy = Y-y();
	int dw = W-w();
	int dh = H-h();
	short* p = sizes();
	// resize this (skip the Fl_Group resize):
	Fl_Widget::resize(X,Y,W,H);
	// find bottom-right of resiable:
	int OR = p[5]; // resizable r
	int NR = X+W-(p[1]-OR); // R - (orig_r - resize_r) = R -
	int OB = p[7];
	int NB = Y+H-(p[3]-OB);
	// move everything to be on correct side of new resizable:
	Fl_Widget*const* a = array();
	p += 8;
	for (int i=children(); i--;) {
		Fl_Widget* o = *a++;
		int orig_x = *p++,
			orig_r = *p++,
			orig_y = *p++,
			orig_b = *p++;
			
		int xx = o->x() + dx;
		int R = xx + o->w();
		if (orig_x >= OR)
			xx += dw; 
		else if (xx > NR)
			xx = NR;
		
		if (orig_r >= OR)
			R += dw; 
		else if (R > NR)
			R = NR;
		
		int yy = o->y()+dy;
		int B = yy+o->h(); 
		if (orig_y >= OB)
			yy += dh;
		else if (yy > NB)
			yy = NB;
		if (orig_b >= OB)
			B += dh;
		else if (B > NB)
			B = NB;
		o->resize(xx,yy,R-xx,B-yy); o->redraw();
	}
}
*/

