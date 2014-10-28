
#include <stdio.h>
#include <LibAudioStreamMC/LibAudioStreamMC++.h>

void MemoryRender(AudioStream s1, int render_size);

void FileRender(AudioStream s1, int render_size, const char* filename, bool cut = false);

