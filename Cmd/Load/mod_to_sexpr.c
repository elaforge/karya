// Copyright 2020 Evan Laforge
// This program is distributed under the terms of the GNU General Public
// License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

#include <stdio.h>

#include <xmp.h>
#include <effects.h>


/* This is a tiny standalone program to convert mods to a sexpr format.

   Unlike the rest of the source, this is compiled separately by nix.

   Sexpr look like (symbol num|str ...)

    (metadata (var val) ...)
    (instruments (num name) ...)
    ; play stops at "-", and skips past "+", this is an st3 / it convention.
    (order num|"-"|"+" ...)
    (pattern num
        (track rows
            (row nn instrument vol (fx1 fx1param) (fx2 fx2param))
            ...)
        ...)
*/

// This is incomplete, more are in effects.h.
static const char *fx_name(unsigned char fx, unsigned char param)
{
    if (fx == 0 && param == 0)
        return "";

    static char unknown[32];
    switch (fx) {
    case FX_ARPEGGIO:
        return "arpeggio";
    case FX_PORTA_UP:
        return "up";
    case FX_PORTA_DN:
        return "down";
    case FX_TONEPORTA:
        return "porta";
    case FX_VIBRATO:
        return "vibrato";
    case FX_TONE_VSLIDE:
        return "tone_vslide";
    case FX_VIBRA_VSLIDE:
        return "vibra_vslide";
    case FX_TREMOLO:
        return "tremolo";
    case FX_OFFSET:
        return "offset";
    case FX_VOLSLIDE:
        return "volslide";
    case FX_JUMP:
        return "jump";
    case FX_VOLSET:
        return "volset";
    case FX_BREAK:
        return "break";
    case FX_EXTENDED:
        return "extended";
    case FX_SPEED:
        return "speed";

    case FX_TREMOR:
        return "tremor"; // xy ontime x, offtime y

    case FX_S3M_SPEED:
        return "s3m_speed";
    case FX_S3M_BPM:
        return "s3m_bpm";
    case FX_S3M_ARPEGGIO:
        return "s3m_arpeggio";
    case FX_IT_BREAK:
        return "it_break";
    case FX_IT_BPM:
        return "it_bpm";
    default:
        sprintf(unknown, "%d", fx);
        return unknown;
    }
}

static void print_track(const struct xmp_track *t)
{
    printf("    (track %d\n", t->rows);
    for (int row = 0; row < t->rows; row++) {
        struct xmp_event e = t->event[row];
        if (e.note == 0 && e.ins == 0 && e.vol == 0 && e.fxt == 0
                && e.fxp == 0 && e.f2t == 0 && e.f2p == 0) {
            continue;
        }
        printf("      (%d %d %d %d (\"%s\" %d) (\"%s\" %d))\n",
            row, e.note, e.ins, e.vol,
            fx_name(e.fxt, e.fxp), e.fxp, fx_name(e.f2t, e.f2p), e.f2p);
    }
    printf("    )\n");
}

int main(int argc, char **argv)
{
    if (argc != 2) {
        printf("usage: $0 /path/to/mod\n");
        return 1;
    }
    xmp_context ctx = xmp_create_context();
    int r = xmp_load_module(ctx, argv[1]);
    switch (r) {
    case -XMP_ERROR_FORMAT:
        printf("error format\n");
        break;
    case -XMP_ERROR_DEPACK:
        printf("error depack\n");
        break;
    case -XMP_ERROR_LOAD:
        printf("error load\n");
        break;
    case -XMP_ERROR_SYSTEM:
        perror("loading module");
        break;
    case 0:
        break;
    default:
        printf("unknown error %d\n", r);
    }
    if (r != 0)
        return 1;

    struct xmp_module_info infop;
    xmp_get_module_info(ctx, &infop);
    struct xmp_module *mod = infop.mod;
    printf("(\n");
    printf("(metadata\n");
    printf("  (name \"%s\") (type \"%s\")\n", mod->name, mod->type);
    printf("  (pat %d) (chn %d) (ins %d) (smp %d) (len %d)\n",
        mod->pat, mod->chn, mod->ins, mod->smp, mod->len);
    printf("  (spd %d) (bpm %d) (gvl %d)\n",
        mod->spd, mod->bpm, mod->gvl);
    printf(")\n");

    printf("(instruments\n");
    for (int i = 0; i < mod->ins; i++) {
        if (mod->xxi[i].nsm > 0) {
            printf("  (%d \"%32s\")\n", i+1, mod->xxi[i].name);
        }
    }
    printf(")\n");

    printf("(order ");
    for (int i = 0; i < XMP_MAX_MOD_LENGTH; i++) {
        if (mod->xxo[i] == 255)
            printf("\"-\" ");
        else if (mod->xxo[i] == 254)
            printf("\"+\" ");
        else
            printf("%d ", (int) mod->xxo[i]);
    }
    printf(")\n");

    printf("(patterns\n");
    for (int i = 0; i < mod->pat; i++) {
        struct xmp_pattern *pat = mod->xxp[i];
        printf("  ( ; %d\n", i + 1); // block names start at 1
        for (int chan = 0; chan < mod->chn; chan++) {
            print_track(mod->xxt[pat->index[chan]]);
        }
        printf("  )\n");
    }
    printf(")\n");
    printf(")\n");

    xmp_free_context(ctx);
    return 0;
}
