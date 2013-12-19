
#include <stdio.h>
#include <string>
#include <LibAudioStream/LibAudioStreamMC++.h>
#include <iostream>
#include <fstream>

#include "LAS-test.h"

#define LLVM_EFFECT1 "/Documents/faust-sf/examples/freeverb.dsp"
#define LLVM_EFFECT2 "/Documents/faust-sf/examples/zita_rev1.dsp"
#define LLVM_EFFECT3 "/Documents/faust-sf/examples/freeverb4.dsp"
#define LLVM_EFFECT4 "/Documents/faust-sf/examples/chorus.dsp"

#define FILENAME1 "/Users/letz/Music/Sounds/levot.wav"
#define FILENAME2 "/Users/letz/Music/Sounds/tango.wav"
#define FILENAME3 "/Users/letz/son1.wav"
#define FILENAME4 "/Users/letz/Music/Sounds/levot-mono.aiff"

// Global context
static long gSampleRate = 0;
static long gBufferSize = 0;
static AudioPlayerPtr gAudioPlayer = 0;

//static long BS = 512;
static long BS = 1024;
static long SR = 44100;

static AudioStream s1,s2,s3,s4,s5,s6;
static SymbolicDate symb1,symb2,symb3,symb4,symb5,symb6;
static audio_frames_t date;

/*
// Accès au flux temps-réel
date = GetCurDate();
s1 = MakeInputSound();
StartSound(gAudioPlayer, s1, GenRealDate(gAudioPlayer, date));

next();

// On arrète le flux...
date = GetCurDate();
StopSound(gAudioPlayer, s1, GenRealDate(gAudioPlayer, date));  

// Application d'un effet Faust (chorus) sur l'entrée temps-réel
date = GetCurDate();
s1 = MakeInputSound();
s2 = MakeEffectSound(s1, MakeFaustAudioEffect(LLVM_EFFECT4, "", ""), SR/2, SR/2);
StartSound(gAudioPlayer, s2, GenRealDate(gAudioPlayer, date));
 
next();

// On arrète le flux...
date = GetCurDate();
StopSound(gAudioPlayer, s2, GenRealDate(gAudioPlayer, date)); 
*/

// Lit la date courante du Player en frames
static audio_frames_t GetCurDate()
{
    AudioRendererPtr renderer = GetAudioPlayerRenderer(gAudioPlayer);
    RendererInfo info;
    GetAudioRendererInfo(renderer, &info);
    printf("frame = %lld usec = %lld sec = %f \n", info.fCurFrame, info.fCurUsec, float(info.fCurUsec)/1000000.);
    return info.fCurFrame;
}

// Avance au test suivant
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

// Affecte la date courante à la date symbolique passé en paramètre
void set_symbolic_date(SymbolicDate symb)
{
    printf("\nTaper 'n' pour instancier la date symbolique ou 'q' pour quitter\n\n");
    
    char c;
    while ((c = getchar()) && c != 'q') {
        switch (c) {
            case 'n':
                SetSymbolicDate(gAudioPlayer, symb, GetCurDate());
                return;
            default:
                break;
        }
    }
}

/// Echo d'un flux
static AudioStream MakeEcho(AudioStream stream)
{
    return MakeMixSound(MakeCopySound(stream),
                        MakeMixSound(MakeSeqSound(MakeNullSound(SR/2), MakeCopySound(stream), 100),
                                     MakeSeqSound(MakeNullSound(SR), MakeCopySound(stream), 100)));
}
 
int main(int argc, char* argv[])
{
    // Alloue un Player avec 4 entrées/sorties, une entrée TR d'au plus 10 min, et le backend JACK
    gAudioPlayer = OpenAudioPlayer(4, 4, SR, BS, 65536*4, SR*60*20, kJackRenderer, 1);

    // Démarre le Player
    StartAudioPlayer(gAudioPlayer);
    
    /*
     next();
    
    // Lit la date courante en frames et usec/sec
    GetCurDate();
    
    next();
    
    // Lit la date courante en frames et usec/sec
    GetCurDate();
    
    next();
    
    // Joue une région de 5 sec d'un fichier à la date courante
    s1 = MakeRegionSound(FILENAME1, 5*SR, 10*SR);
    //MemoryRender(s1, 512);
    StartSound(gAudioPlayer, s1, GenRealDate(gAudioPlayer, GetCurDate()));
   
   
    next();
    
    // Joue un fade d'une région de 5 sec d'un fichier à la date courante
    s1 = MakeFadeSound(MakeRegionSound(FILENAME1, 5*SR, 10*SR), SR, SR);
    //MemoryRender(s1, 512);
    StartSound(gAudioPlayer, s1, GenRealDate(gAudioPlayer, GetCurDate()));
    
    next();
    
    // Joue une selection d'une région de 5 sec d'un fichier à la date courante (gauche)
    s1 = MakeRegionSound(FILENAME1, 5*SR, 10*SR);
    std::vector <int> selection1;
    selection1.push_back(0);
    s2 = MakeSelectSound(s1, selection1);
    //MemoryRender(s2, 512);
    StartSound(gAudioPlayer, s2, GenRealDate(gAudioPlayer, GetCurDate()));
     
    next();
    
    // Joue une selection d'une région de 5 sec d'un fichier à la date courante (droite)
    s1 = MakeRegionSound(FILENAME1, 5*SR, 10*SR);
    std::vector <int> selection2;
    selection2.push_back(1);
    s2 = MakeSelectSound(s1, selection2);
    //MemoryRender(s2, 512);
    StartSound(gAudioPlayer, s2, GenRealDate(gAudioPlayer, GetCurDate()));
     
    next();
     
    // Joue une selection d'une région de 5 sec + parallele d'un silence
    s1 = MakeRegionSound(FILENAME1, 5*SR, 10*SR);
    std::vector <int> selection3;
    selection3.push_back(0);
    s2 = MakeSelectSound(s1, selection3);
    s3 = MakeParSound(MakeNullSound(5*SR), s2);
    StartSound(gAudioPlayer, s3, GenRealDate(gAudioPlayer, GetCurDate()));
    
    next();
     
    // Joue une selection d'une région de 5 sec + parallele d'un silence
    s1 = MakeRegionSound(FILENAME2, 5*SR, 10*SR);
    std::vector <int> selection4;
    selection4.push_back(0);
    s2 = MakeSelectSound(s1, selection4);
    s3 = MakeParSound(s2, MakeNullSound(5*SR));
    StartSound(gAudioPlayer, s3, GenRealDate(gAudioPlayer, GetCurDate()));
     
    next();
     
    // Montage en parallele de 2 selections
    s1 = MakeRegionSound(FILENAME1, 5*SR, 10*SR);
    std::vector <int> selection6;
    selection6.push_back(0);
    s2 = MakeSelectSound(s1, selection6);
    
    s3 = MakeRegionSound(FILENAME2, 5*SR, 10*SR);
    std::vector <int> selection7;
    selection7.push_back(0);
    s4 = MakeSelectSound(s3, selection7);
    
    s5 = MakeParSound(s2, s4);
    //MemoryRender(s5, 512);
    StartSound(gAudioPlayer, s5, GenRealDate(gAudioPlayer, GetCurDate()));
  
    next();
    
    // Joue une région de 5 sec d'un fichier à la date courante, arrêt au bout de 3 sec
    date = GetCurDate();
    s1 = MakeRegionSound(FILENAME1, 5*SR, 10*SR);
    StartSound(gAudioPlayer, s1, GenRealDate(gAudioPlayer, date));
    StopSound(gAudioPlayer, s1, GenRealDate(gAudioPlayer, date+3*SR));
    
    next();
     
    // Joue une région de 5 sec d'un fichier à 3 sec dans le futur, arrêt au bout de 6 sec
    date = GetCurDate();
    s1 = MakeRegionSound(FILENAME2, 5*SR, 10*SR);
    StartSound(gAudioPlayer, s1, GenRealDate(gAudioPlayer, date+3*SR));
    StopSound(gAudioPlayer, s1, GenRealDate(gAudioPlayer, date+6*SR));
  
    next();
    
    // Joue le mixage de 2 régions à la date courante
    s1 = MakeRegionSound(FILENAME1, 5*SR, 10*SR);
    s2 = MakeRegionSound(FILENAME2, 5*SR, 10*SR);
    s3 = MakeMixSound(s1, s2);
    //MemoryRender(s3, 512);
    StartSound(gAudioPlayer, s3, GenRealDate(gAudioPlayer, GetCurDate()));
    
    next();
    
    // Joue la sequence de 2 régions à la date courante
    s1 = MakeRegionSound(FILENAME1, 5*SR, 10*SR);
    s2 = MakeRegionSound(FILENAME2, 5*SR, 10*SR);
    s3 = MakeSeqSound(s1, s2, 1024);
    //MemoryRender(s3, 512);
    StartSound(gAudioPlayer, s3, GenRealDate(gAudioPlayer, GetCurDate()));
    */
    
    next();
    
    // Joue l'application d'un effet Faust (compilé dynamiquement) sur une région à la date courante
    s1 = MakeRegionSound(FILENAME1, 5*SR, 10*SR);
    s2 = MakeEffectSound(s1, MakeFaustAudioEffect(LLVM_EFFECT1, "", ""), SR/2, SR/2);
    //MemoryRender(s2, 512);
    StartSound(gAudioPlayer, s2, GenRealDate(gAudioPlayer, GetCurDate()));
   
    
    next();
    
    
    std::string effect = pathToContent(LLVM_EFFECT1);
    //std::cout << effect;
    
    //std::string effect = "process = _,_;";
    
    // Joue l'application d'un effet Faust (compilé dynamiquement) sur une région à la date courante
    s1 = MakeRegionSound(FILENAME1, 5*SR, 10*SR);
    s2 = MakeEffectSound(s1, MakeRemoteFaustAudioEffect(effect.c_str(), "", ""), SR/2, SR/2);
    //MemoryRender(s2, 512);
    StartSound(gAudioPlayer, s2, GenRealDate(gAudioPlayer, GetCurDate()));
    

    next();
    
    // Application d'un effet Faust (chorus) sur l'entrée temps-réel (*capturée à partir de 0*)
    date = GetCurDate();
    s1 = MakeSharedInputSound();
    // On coupe la section entre 0 et la date courante, et on garde 6 sec à partir de la date courante
    s2 = MakeCutSound(s1, date, date+6*SR);  
    s3 = MakeEffectSound(s2, MakeFaustAudioEffect(LLVM_EFFECT4, "", ""), SR/2, SR/2);
    StartSound(gAudioPlayer, s3, GenRealDate(gAudioPlayer, date));
    
    next();
      
    // Echo sur une région d'un fichier
    date = GetCurDate();
    s1 = MakeRegionSound(FILENAME1, 5*SR, 10*SR);
    s2 = MakeEcho(s1);
    StartSound(gAudioPlayer, s2, GenRealDate(gAudioPlayer, date));
    
    next();
    
    // Echo sur une région du flux TR [date, date + 5 sec]
    date = GetCurDate();
    s1 = MakeCutSound(MakeSharedInputSound(), date, date+5*SR);
    s2 = MakeEcho(s1);
    StartSound(gAudioPlayer, s2, GenRealDate(gAudioPlayer, date));
    
    next();
    
    // Application d'un effet Faust (chorus) sur l'entrée temps-réel (*capturée à partir de 0*)
    // et répétition de la même région avec un autre effet (freeverb), silence d'2 sec entre les 2 répétitions
    date = GetCurDate();
    s1 = MakeSharedInputSound();
    // On coupe la section entre 0 et la date courante, et on garde 6 sec à partir de la date courante
    s2 = MakeCutSound(s1, date, date+6*SR);      
    
    s3 = MakeEffectSound(MakeCopySound(s2), MakeFaustAudioEffect(LLVM_EFFECT4, "", ""), SR/2, SR/2);  // <== application d'un 1° effet
    s4 = MakeEffectSound(MakeCopySound(s2), MakeFaustAudioEffect(LLVM_EFFECT3, "", ""), SR/2, SR/2);  // <== application d'un 2° effet
    
    s5 = MakeSeqSound(s3, MakeSeqSound(MakeNullSound(SR*2), s4, SR/2), SR/2);
    StartSound(gAudioPlayer, s5, GenRealDate(gAudioPlayer, date));
    
    next();
    
    // Echo de : application d'un effet Faust (chorus) sur l'entrée temps-réel (*capturée à partir de 0*)
    // et répétition de la même région avec un autre effet (freeverb), silence d'1 sec entre les 2 répétitions
    date = GetCurDate();
    s1 = MakeSharedInputSound();
    // On coupe la section entre 0 et la date courante, et on garde 6 sec à partir de la date courante
    s2 = MakeCutSound(s1, date, date+6*SR);   
    
    s3 = MakeEffectSound(MakeCopySound(s2), MakeFaustAudioEffect(LLVM_EFFECT4, "", ""), SR/2, SR/2);  // <== application d'un 1° effet
    s4 = MakeEffectSound(MakeCopySound(s2), MakeFaustAudioEffect(LLVM_EFFECT3, "", ""), SR/2, SR/2);  // <== application d'un 2° effet
    
    s5 = MakeSeqSound(s3, MakeSeqSound(MakeNullSound(SR), s4, SR/2), SR/2);
    s6 = MakeEcho(s5); // <<== ECHO
    StartSound(gAudioPlayer, s6, GenRealDate(gAudioPlayer, date));
    
    next();
   
    // Séquence avec date symbolique
    date = GetCurDate();
    symb1 = GenSymbolicDate(gAudioPlayer);
    
    s1 = MakeRegionSound(FILENAME1, 5*SR, 100*SR);
    s2 = MakeRegionSound(FILENAME2, 5*SR, 10*SR);
    StartSound(gAudioPlayer, s1, GenRealDate(gAudioPlayer, date));
    StopSound(gAudioPlayer, s1, symb1);
    StartSound(gAudioPlayer, s2, symb1);
 
    // Instancie la date symbolique à la date courante "capturée"
    set_symbolic_date(symb1);
      
    next();
     
    // Contrôle temporel des effets sur une région d'un fichier
    date = GetCurDate();
    s1 = MakeRegionSound(FILENAME1, 5*SR, 15*SR);
    s2 = MakeEffectSound(s1, MakeFaustAudioEffect(LLVM_EFFECT3, "", ""), SR/2, SR/2);  
    
    // Rampe sur un paramètre de l'effet en 2 sec, qui commence 1 sec après la date courante
    for (int i = 0; i < 100; i++) {
        SetTimedControlValueEffect(gAudioPlayer, "freeverb4", "/Freeverb/Wet", 1.-float(i)*0.01f, GenRealDate(gAudioPlayer, SR+date+i*SR/50));
    }
    
    // Rampe sur un paramètre de l'effet en 2 sec, qui commence 5 sec après la date courante
    for (int i = 0; i < 100; i++) {
        SetTimedControlValueEffect(gAudioPlayer, "freeverb4", "/Freeverb/Wet", float(i)*0.01f, GenRealDate(gAudioPlayer, SR*5+date+i*SR/50));
    }
    
    StartSound(gAudioPlayer, s2, GenRealDate(gAudioPlayer, date));
      
    next();
    
    // Contrôle temporel des effets sur entrée TR
    date = GetCurDate();
    s1 = MakeCutSound(MakeSharedInputSound(), date, date+15*SR);
    s2 = MakeEffectSound(s1, MakeFaustAudioEffect(LLVM_EFFECT3, "", ""), SR/2, SR/2);  
    
    // Rampe sur un paramètre de l'effet en 2 sec, qui commence 1 sec après la date courante
    for (int i = 0; i < 100; i++) {
        SetTimedControlValueEffect(gAudioPlayer, "freeverb4", "/Freeverb/Wet", 1.-float(i)*0.01f, GenRealDate(gAudioPlayer, SR+date+i*SR/50));
    }
    
    // Rampe sur un paramètre de l'effet en 2 sec, qui commence 5 sec après la date courante
    for (int i = 0; i < 100; i++) {
        SetTimedControlValueEffect(gAudioPlayer, "freeverb4", "/Freeverb/Wet", float(i)*0.01f, GenRealDate(gAudioPlayer, SR*5+date+i*SR/50));
    }
    
    StartSound(gAudioPlayer, s2, GenRealDate(gAudioPlayer, date));
      
    next();
    
  
    // Arrète le Player
    StopAudioPlayer(gAudioPlayer);
    
    // Désalloue le Player
    CloseAudioPlayer(gAudioPlayer);
}