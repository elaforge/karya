import("stdfaust.lib");

declare description "Simple sine wave.";
declare c1 "pitch";
declare c2 "amp";

process(pitch, amp) = amp * os.osc(pitch);
