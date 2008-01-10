#include <FL/Fl.H>
#include <FL/Fl_Widget.H>
#include <FL/Fl_Group.H>
#include <iostream>
#include <typeinfo>
#include "util.h"

#include <stdio.h>

void
print_children(Fl_Group *w)
{
	for (int i = 0; i < w->children(); i++) {
		Fl_Widget *c = w->child(i);
		std::cout << "c " << i << rect_from_widget(*c)
			<< typeid(*c).name() << " \"" << c->label() << "\"\n";
	}
}

static void
print_cinfo(Fl_Widget *w)
{
	Rect r = rect_from_widget(*w);
	printf("(%d %d %d %d) %s \"%s\"", r.x, r.y, r.w, r.h,
		typeid(*w).name(), w->label());
}

void
print_children_r(Fl_Group *w, int recurse)
{
	for (int i = 0; i < w->children(); i++) {
		Fl_Widget *c = w->child(i);
		for (int r = recurse; r; r--)
			printf("\t");
		// for some reason c++ style io silently prints nothing
		// std::cout << "c" << i << ' ' << rect_from_widget(*c)
		// 	<< typeid(*c).name() << " \"" << c->label() << "\"\n";
		printf("c%d: ", i);
		print_cinfo(w->child(i));
		printf("\n");
		Fl_Group *g = dynamic_cast<Fl_Group *>(c);
		if (g)
			print_children_r(g, recurse+1);
	}
}
