#ifndef __AC97_CODEC_IF_H__
#define __AC97_CODEC_IF_H__

struct ac97_codec_if;

struct ac97_codec_if_vtbl {
	void (*lock)(struct ac97_codec_if *);
	int var;
};

struct ac97_codec_if {
	struct ac97_codec_if_vtbl *vtbl;
};

#endif /* __AC97_CODEC_IF_H__ */
