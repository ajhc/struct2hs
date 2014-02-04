#ifndef __AUICH_H__
#define __AUICH_H__

#include <bus.h>
#include <ac97var.h>

struct auich_softc {
	struct ac97_codec_if *codec_if;
	int	sc_modem_offset;
	bus_space_handle_t aud_ioh;
	bus_size_t aud_size;
};

#endif /* __AUICH_H__ */
