
#include "LAS-test.h"

#include <iostream>
#include <fstream>
#include <sstream>

#define RENDER_BUFFER 512

using namespace std;

static void PrintBufferFrame(float** buffer, int channels, int render_slice, ofstream* dst, bool last)
{
    for (int frame = 0; frame < render_slice; frame++) {
    
        *dst << "[";
        for (int i = 0; i < channels; i++) {
            *dst << buffer[i][frame];
            if (i < channels - 1) {
                *dst << " ";
            }
        }
        *dst << "]";
        
        if (last) {
            if (frame < render_slice-1) {
                *dst << ",";
            } 
        } else {
            *dst << ",";
        }
    }
}

void FileRender(AudioStream s, int render_size, const char* filename)
{
    AudioStream stream = MakeRendererSound(s);
    long length = GetLengthSound(stream);
    long channels = GetChannelsSound(stream);
    int res;
    
    bool render_ok = true;
    bool render_last_ok = true;
    
    ofstream* dst = new ofstream(filename);
  
    // Allocate buffers
    float** buffer = new float*[channels];
    for (int i = 0; i < channels; i++) {
        buffer[i] = new float[render_size];
        memset(buffer[i], 0, render_size*sizeof(float));
    }
    
    // Render with complete buffers
    
    int buffer_num = length/render_size;
    int last_buffer = length%render_size;
   
    ResetSound(stream);
    
    for (int i = 0; i < buffer_num; i++) {
        res = ReadSoundPos(stream, buffer, render_size, 0);
        if (res != render_size) {
            printf("FileRender res = %ld %d\n", res, i);
            render_ok = false;
        }
        PrintBufferFrame(buffer, channels, render_size, dst, (last_buffer == 0));
    }
    
    res = ReadSoundPos(stream, buffer, last_buffer, 0);
    if (res != last_buffer) {
        printf("FileRender last_buffer res = %ld\n", res);
        render_last_ok = false;
    }
    
    // Last buffer
    PrintBufferFrame(buffer, channels, res, dst, true);
      
    printf("FileRender render_ok = %d render_last_ok = %d\n", render_ok, render_last_ok);
    dst->close();
    delete dst;
}

void MemoryRender(AudioStream s, int render_size)
{
    AudioStream stream = MakeRendererSound(s);
    long length = GetLengthSound(stream);
    long channels = GetChannelsSound(stream);
    int res;
    
    bool render_ok = true;
    bool render_last_ok = true;
    
    printf("MemoryRender length = %ld channels = %ld\n", length, channels);
    
    // Allocate buffers
    float** buffer = new float*[channels];
    for (int i = 0; i < channels; i++) {
        buffer[i] = new float[render_size];
        memset(buffer[i], 0, render_size*sizeof(float));
    }
     
    // Render with complete buffers
    
    int buffer_num = length/render_size;
    int last_buffer = length%render_size;
    
    printf("MemoryRender buffer_num = %ld last_buffer = %ld\n", buffer_num, last_buffer);
   
    ResetSound(stream);
    
    for (int i = 0; i < buffer_num; i++) {
        res = ReadSoundPos(stream, buffer, render_size, 0);
        if (res != render_size) {
            printf("Render1 res = %ld %d\n", res, i);
            render_ok = false;
        }
    }
    
    res = ReadSoundPos(stream, buffer, last_buffer, 0);
    if (res != last_buffer) {
        printf("Render1 last_buffer res = %ld\n", res);
        render_last_ok = false;
    }
    printf("MemoryRender render_ok = %d render_last_ok = %d\n", render_ok, render_last_ok);
    
    // Render with incomplete buffers
    
    ResetSound(stream);
    
    for (int i = 0; i < buffer_num; i++) {
        res = ReadSoundPos(stream, buffer, render_size/2, 0);
        if (res != render_size/2) {
            printf("Render2 res1 = %ld %d\n", res, i);
            render_ok = false;
        }
        res = ReadSoundPos(stream, buffer, render_size/2, render_size/2);
        if (res != render_size/2) {
            printf("Render2 res2 = %ld %d\n", res, i);
            render_ok = false;
        }
    }
    
    res = ReadSoundPos(stream, buffer, last_buffer, 0);
    if (res != last_buffer) {
        printf("Render2 last_buffer  res = %ld\n", res);
        render_last_ok = false;
    }
    
    printf("MemoryRender render_ok = %d render_last_ok = %d\n", render_ok, render_last_ok);
    ResetSound(stream);
    
    // Free buffers
    buffer = new float*[channels];
    for (int i = 0; i < channels; i++) {
        delete[] buffer[i];
    }
    delete[] buffer;
}
