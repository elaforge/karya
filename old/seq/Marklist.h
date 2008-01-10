#include <vector>
#include <map>
#include <utility> // for pair<>

namespace seq {

struct Mark {
	Mark() : color(), name(0), mkname(0), font_info(0), width(0),
		rank(0), pos(), show_name(false) {}
	Rgba_color color; // alpha is used for mark on tracks
	// static name, possibly drawn on ruler, and used as an identifier
	const char *name;
	// if 'name' is null, try this to generate a name
	const char *(*mkname)(const Mark *mark);
	const Font_info *font_info;
	unsigned char width; // width of mark on screen
	unsigned char rank; // used as an identifier along with name
	Trackpos pos;
	bool show_name; // draw the name on the ruler?
	// bool operator<(const Mark &o) const { return pos < o.pos; }
	bool operator==(const Mark &o) const {
		return color == o.color && name == o.name &&
			width == o.width && rank == o.rank &&
			show_name == o.show_name && pos == o.pos;
	}
};

/* Marklist could be abstract, but then Marklist_views would have to be
dynamically allocated.  Use typedefs below and let it be concrete for now.
class Marklist_view {
public:
	virtual ~Marklist_view() {}
	virtual void find(Trackpos p, Zoom_t z) = 0;
	// return current mark or 0 if we ran out of marks
	virtual const Mark *mark() const = 0;
	virtual void next() = 0;
};

typedef std::vector<Marklist_view *> Marklists;
*/

namespace Marklist_impl {

class Stack_mark;
typedef util::List<Stack_mark> Mlist;

/*
Marklist_f may return a Mlist if it should be drawn at zoom. It should
handle allocation and deallocation of Mlists itself, possibly deleting
dynamically generated ones when zoom shows they should no longer be
displayed. A toplevel Marklist_f should return its Mlist regardless of
the zoom.

This will become a boost::python::object when this is wrapped ... or I
could make Marklist_f a template argument but I don't think I'll need
that flexibility.
*/
typedef const Mlist *Marklist_f(const Zoom_t zoom, int rank);

struct Stack_mark : public Mark {
	Stack_mark() : extent(), sublist(0) {}
	Trackpos extent;
	Marklist_f *sublist;
};

class Iterator;
class Marklist {
public:
	Marklist(const char *name, Marklist_f *sublist) :
		_name(name), _sublist(sublist)
	{}
	typedef Iterator const_iterator;
	const char *name() const { return _name; }
	const Marklist_f *sublist() const { return _sublist; }
private:
	const char *_name;
	Marklist_f *_sublist;
};

class Stack_elt {
	// stack elt keeps track of: current pos (+ starting pos), Mlist
public:
	Stack_elt(Trackpos p, const Mlist *m) :
		pos(p), mlist(m)
	{ next(); }
	bool next() {
		if (!mlist)
			return false;
		pos += mlist->head().extent;
		mlist = mlist->tail();
		if (!mlist)
			return false;
		mark = &mlist->head();
		return true;
	}
	bool prev() {
		if (!mlist->prev() || !mlist->prev()->prev())
			return false;
		mlist = mlist->prev();
		pos -= mlist->head().extent;
		mark = &mlist->head();
		return true;
	}
	bool at_end() const { return !mlist; }
	const Stack_mark *mark;
	Trackpos pos;
private:
	const Mlist *mlist;
};

class Iterator {
public:
	Iterator(const Marklist *m);
	// Iterator(const Iterator *hint, double z);
	Iterator &operator++();
	// this allows the mark out to clients who could be surprised by it
	// changing on every iteration 
	const Stack_mark &operator*() const { assert(!at_end()); return mark; }
	void find(Trackpos p, Zoom_t z); // modify this to point at <= p
	bool at_end() const { return stack.size() == 0; }
private:
	Stack_elt *last() { return &stack.back(); }
	const Stack_elt *last() const { return &stack.back(); }
	void pop_stack() { stack.pop_back(); }
	bool descend() { // descend as far as possible
		Trackpos from = last()->pos;
		bool descended = false;
		const Mlist *m;
		// next rank is stack.size()-1 + 1
		while (m = last()->mark->sublist(zoom, int(stack.size()))) {
			stack.push_back(Stack_elt(from, m));
			descended = true;
		}
		return descended;
	}
	bool past_parent() const {
		if (stack.size() < 2)
			return false; // no parent, so we can't be past it
		const Stack_elt *p = &stack[stack.size()-2];
		return last()->pos >= p->pos + p->mark->extent;
	}
	Zoom_t zoom;
	Stack_mark mark;
	std::vector<Stack_elt> stack;
	const Marklist *marklist;
};

class Stack_marklist_view { //  : public Marklist_view {
public:
	Stack_marklist_view(const Marklist *m) :
		iter(m)
	{
		// XXX later, throw an exception, unless there's a reason that
		// an empty marklist could be something other than a mistake
		if (iter.at_end()) 
			std::cerr << "warning: empty marklist "
				"(toplevel marklist should always return a list)\n";
	}
	void find(Trackpos p, Zoom_t z) {
		iter.find(p, z);
	}
	const Mark *mark() const { return iter.at_end() ? 0 : &*iter; }
	void next() { ++iter; }
private:
	Marklist::const_iterator iter;
	// cache: find() looks for cached array and sets current_cache_val.
	// Subsequent next()s will fetch from the val, extending it if necessary.
	enum { Max_cache_size = 16 };
	struct cache_key {
		Marklist *m;
		Trackpos p;
		Zoom_t zoom;
		bool operator<(const cache_key &o) const {
			if (m < o.m) return true;
			else if (p < o.p) return true;
			else return zoom < o.zoom;
		}
	};
	typedef std::vector<std::pair<Trackpos, Mark*> > cache_val;
	cache_val *current_cache_val;
	static std::map<cache_key, cache_val> cache;
};
} // namespace Marklist_impl

// interface for clients of the implementation
using Marklist_impl::Stack_mark;
using Marklist_impl::Mlist;
using Marklist_impl::Marklist_f;
using Marklist_impl::Marklist;

typedef Marklist_impl::Stack_marklist_view Marklist_view;
typedef std::vector<Marklist_view> Marklists;
} // namespace seq
