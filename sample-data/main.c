#include <stdio.h>
#include <assert.h>
#include <auich.h>
#include "jhc_rts_header.h"

extern void trueMain(struct auich_softc *);
void dummy_func(void);
void my_lock(struct ac97_codec_if *);

struct ac97_codec_if_vtbl g_ac97_codec_if_vtbl = { my_lock, 7 };
struct ac97_codec_if g_ac97_codec_if = { &g_ac97_codec_if_vtbl };

struct auich_softc g_auich_softc = {
	&g_ac97_codec_if,
	8888,
	0xdeadbeef,
	256
};

void dummy_func(void)
{
	return;
}

void my_lock(struct ac97_codec_if *aif)
{
	printf("Called \"my_lock\" / aif->vtbl->var == %d\n", aif->vtbl->var);
	assert(7 == aif->vtbl->var);
}

int main(int argc, char *argv[])
{
	hs_init(&argc,&argv);
	_amain();
	trueMain(&g_auich_softc);
	hs_exit();

	return 0;
}
