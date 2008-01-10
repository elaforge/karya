#include "models.h"

/*

Track_model

event loop	:: Input -> (Event, BlockId)
|
python
|
Block_dispatch :: (Cmd, BlockId)
|			\		\
Block_state	Block	Block	...

type BlockCmd =
	zoom(zrect) | selection(start, extent) | resize(w, h) | 
	configure(colors, zoom_speed, ...) |
	add_track(t) | remove_track(t) |
	add_ruler(r) | remove_ruler(r) |

class Dispatch:
	block_state_cmds = {
		add_track : (
			lambda st, args: st.add_track(args),
			lambda widget: widget.track_update()),
	}
	def block_dispatch(self, block_id, cmd, args):
		modify, update = block_state_cmds.get(cmd)
		block = self.blocks[block_id]
		if modify:
			modify(block.state, args)
			for b in self.blocks.values():
				if b.state is block.state:
					update(block.widget)
		modify, update = block_widget_cmds = .get(cmd)
		if modify:
			modify(block.widget, args)
			update(block.widget)
	
	def track_dispatch(self, track_id, cmd, args):
		...

*/

struct Defaults {
	// the block will default to its window's size, so this is for
	// whoever makes the window
	Point window_size; 

	int title_size, scrollbar_size, ruler_size;
	int track_size;

	double time_zoom_speed, track_zoom_speed;
	
	Block_model::Colors block_colors;
	Track_model::Colors track_colors;
	Event_model::Colors event_colors;
};
