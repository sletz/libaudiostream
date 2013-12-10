
#include <stdio.h>
#include <LibAudioStream/LibAudioStreamMC++.h>

#define LLVM_EFFECT1 "/Documents/faust-sf/examples/freeverb.dsp"
#define LLVM_EFFECT2 "/Documents/faust-sf/examples/zita_rev1.dsp"
#define LLVM_EFFECT3 "/Documents/faust-sf/examples/freeverb4.dsp"

#define FILENAME1 "/Users/letz/Music/Sounds/levot.wav"
#define FILENAME2 "/Users/letz/Music/Sounds/tango.wav"
#define FILENAME3 "/Users/letz/son1.wav"
#define FILENAME4 "/Users/letz/Music/Sounds/levot-mono.aiff"

// Global context
static long gSampleRate = 0;
static long gBufferSize = 0;
static AudioPlayerPtr gAudioPlayer = 0;

static long BS = 512;
static long SR = 44100;

// Lit la date courante du Player en frames
static audio_frames_t GetCurDate()
{
    AudioRendererPtr renderer = GetAudioPlayerRenderer(gAudioPlayer);
    RendererInfo info;
    GetAudioRendererInfo(renderer, &info);
    printf("frame = %lld usec = %lld sec = %f \n", info.fCurFrame, info.fCurUsec, float(info.fCurUsec)/1000000.);
    return info.fCurFrame;
}

void next()
{
    printf("\nTaper 'n' pour passer au test suivant ou 'q' pour quitter\n\n");
    
    char c;
    while ((c = getchar()) && c != 'q') {
        switch (c) {
            case 'n':
                return;
            default:
                break;
        }
    }
}

int main(int argc, char* argv[])
{
    // Alloue un Player avec 4 entrées/sorties et le backend JACK
    gAudioPlayer = OpenAudioPlayer(4, 4, SR, BS, 65536 * 4, SR * 60 * 10, kJackRenderer, 1);

    // Démarre le Player
    StartAudioPlayer(gAudioPlayer);
    AudioStream s1,s2,s3,s4;
    audio_frames_t date;
    
    /*
    next();
    
    // Lit la date courante en frames et usec/sec
    GetCurDate();
    
    next();
    
    // Lit la date courante en frames et usec/sec
    GetCurDate();
    
    next();
    
    // Joue une région de 5 sec d'un fichier (immédiatement)
    s1 = MakeRegionSound(FILENAME1, 5*SR, 10*SR);
    StartSound(gAudioPlayer, s1, GenRealDate(gAudioPlayer, GetCurDate()));
    
    next();
    
    // Joue une région de 5 sec d'un fichier (immédiatement), arrêt au bout de 3 sec
    s1 = MakeRegionSound(FILENAME2, 5*SR, 10*SR);
    date = GetCurDate();
    StartSound(gAudioPlayer, s1, GenRealDate(gAudioPlayer, date));
    StopSound(gAudioPlayer, s1, GenRealDate(gAudioPlayer, date+3*SR));
    
    
    next();
     
    // Joue une région de 5 sec d'un fichier à 3 sec dans le futur, arrêt au bout de 6 sec
    s1 = MakeRegionSound(FILENAME2, 5*SR, 10*SR);
    date = GetCurDate();
    StartSound(gAudioPlayer, s1, GenRealDate(gAudioPlayer, date+3*SR));
    StopSound(gAudioPlayer, s1, GenRealDate(gAudioPlayer, date+6*SR));
  
    next();
    
    // Joue le mixage de 2 régions (immédiatement)
    s1 = MakeRegionSound(FILENAME1, 5*SR, 10*SR);
    s2 = MakeRegionSound(FILENAME2, 5*SR, 10*SR);
    s3 = MakeMixSound(s1, s2);
    StartSound(gAudioPlayer, s3, GenRealDate(gAudioPlayer, GetCurDate()));
    
    next();
    
    // Joue la sequence de 2 régions (immédiatement)
    s1 = MakeRegionSound(FILENAME1, 5*SR, 10*SR);
    s2 = MakeRegionSound(FILENAME2, 5*SR, 10*SR);
    s3 = MakeSeqSound(s1, s2, SR/2);
    StartSound(gAudioPlayer, s3, GenRealDate(gAudioPlayer, GetCurDate()));
   
    
    next();
    
    // Joue l'application d'un effet Faust sur une région (immédiatement)
    s1 = MakeRegionSound(FILENAME1, 5*SR, 10*SR);
    s2 = MakeEffectSound(s1, MakeFaustAudioEffect(LLVM_EFFECT3, "", ""), SR/2, SR/2);
    StartSound(gAudioPlayer, s2, GenRealDate(gAudioPlayer, GetCurDate()));
    */
     
    next();
    
    
    // Application d'un effet Faust sur l'entrée temps-réel, capturée à partir du démarrage du Player...
    
    // Arrète le Player et le relance immédiatement
    StopAudioPlayer(gAudioPlayer);
    GetCurDate();
    GetCurDate();
    StartAudioPlayer(gAudioPlayer);
    GetCurDate();
    
    s1 = MakeSharedInputSound();
    date = GetCurDate();
    StartSound(gAudioPlayer, s1, GenRealDate(gAudioPlayer, date));
    
    next();
    
    
    // Application d'un effet Faust sur l'entrée temps-réel, capteurée à partir du démarrage du Player...
      
    // Arrète le Player
    StopAudioPlayer(gAudioPlayer);
    
    // Désalloue le Player
    CloseAudioPlayer(gAudioPlayer);
}