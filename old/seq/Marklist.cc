#include <algorithm>

#include "util.h"
#include "s_util.h"
#include "Marklist.h"

namespace seq {
namespace Marklist_impl {

Iterator::Iterator(const Marklist *m)
{
	// initial sublist must be ok with empty zoom. 
	// Stack_marklist_view will complain if iterator starts empty
	const Mlist *mlist = m->sublist()(zoom, 0);
	// gcc 3.3 mistakes below for a function declaration
	Stack_elt e(Trackpos(), mlist);
	// Stack_elt e = Stack_elt(Trackpos(), mlist);
	if (!e.at_end()) {
		stack.push_back(e);
		mark = *e.mark;
	}
}

Iterator &
Iterator::operator++()
{
	Assert(!at_end());
	if (!descend()) {
		// pop while there is no next, or if the next is past our parent
		while (!at_end() && (!last()->next() || past_parent()))
			pop_stack();
	}
	if (!at_end()) {
		mark = *last()->mark;
		mark.pos = last()->pos;
		mark.rank = int(stack.size());
	}
	return *this;
}

void // b seq::Marklist_impl::Iterator::find
Iterator::find(Trackpos p, Zoom_t z)
{
	zoom = z;
	if (at_end())
		return; // no marks?  can't find it then!
	while (stack.size() > 1) { // work up the stack
		const Stack_elt *a = last();
		if (a->pos <= p && p < a->pos + a->mark->extent)
			break;
		else
			pop_stack();
	}

	for (;;) { // work down the stack
		 Stack_elt *a = last();
		 
		 if (a->pos + a->mark->extent <= p) { // before
		 	// go to the next, if you can't, pop and try again
		 	while (!at_end() && !last()->next())
		 		pop_stack();
			if (at_end())
				break;
		 } else if (a->pos < p) { // overlap before
			if (!descend())
				break;
			else {
				while (last()->pos > p) // oops, overshot p
					pop_stack();
				break;
			}
		 } else if (a->pos == p) { // yay, exact match
		 	break;
		 } else { // after
		 	// shouldn't descend until we are <= p
			Assert(stack.size() == 1);
		 	if (!a->prev())
				break;
		}
	}
	if (!at_end()) {
		mark = *last()->mark;
		mark.pos = last()->pos;
		mark.rank = int(stack.size()-1);
	}
}
} // namespace Marklist_impl
} // namespace seq
