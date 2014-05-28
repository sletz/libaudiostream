
#include <stdio.h>
#include <string>
#include <LibAudioStreamMC/LibAudioStreamMC++.h>
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
static long BS = 512;
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
/*
static AudioStream MakeEcho(AudioStream stream)
{
    return MakeMixSound(MakeCopySound(stream),
                        MakeMixSound(MakeSeqSound(MakeNullSound(SR), MakeCopySound(stream), 100),
                                     MakeSeqSound(MakeNullSound(SR*2), MakeCopySound(stream), 100)));
}
*/

static AudioStream MakeEcho(AudioStream stream)
{
    return MakeMixSound(stream,
                        MakeMixSound(MakeSeqSound(MakeNullSound(SR), stream, 100),
                                     MakeSeqSound(MakeNullSound(SR*2), stream, 100)));
}

 
int main(int argc, char* argv[])
{
    // Alloue un Player avec 4 entrées/sorties, une entrée TR d'au plus 10 min, et le backend JACK
    gAudioPlayer = OpenAudioPlayer(2, 2, SR, BS, 65536*4, SR*60*20, kJackRenderer, 1);

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
    //s1 = MakeRegionSound(FILENAME1, 5*SR, 10*SR);
    s1 = MakeRegionSound(FILENAME1, 0, 2*SR);
    //MemoryRender(s1, 512);
    StartSound(gAudioPlayer, s1, GenRealDate(gAudioPlayer, GetCurDate()));
   
    next();
    
    // Joue un fade d'une région de 5 sec d'un fichier à la date courante
    s1 = MakeFadeSound(MakeRegionSound(FILENAME1, 5*SR, 10*SR), SR, SR);
    //MemoryRender(s1, 512);
    StartSound(gAudioPlayer, s1, GenRealDate(gAudioPlayer, GetCurDate()));
  
    next();
    
    // Joue une selection d'une région de 5 sec d'un fichier à la date courante (canal gauche)
    s1 = MakeRegionSound(FILENAME1, 5*SR, 10*SR);
    long selection1[1];
    selection1[0] = 0;
    s2 = MakeSelectSound(s1, selection1, 1);
    //MemoryRender(s2, 512);
    StartSound(gAudioPlayer, s2, GenRealDate(gAudioPlayer, GetCurDate()));
     
    next();
    
    // Joue une selection d'une région de 5 sec d'un fichier à la date courante (canal droite)
    s1 = MakeRegionSound(FILENAME1, 5*SR, 10*SR);
    long selection2[1];
    selection2[0] = 0;
    
    s2 = MakeSelectSound(s1, selection2, 1);
    //MemoryRender(s2, 512);
    StartSound(gAudioPlayer, s2, GenRealDate(gAudioPlayer, GetCurDate()));
     
    next();
     
    // Joue une selection d'une région de 5 sec + parallele d'un silence
    s1 = MakeRegionSound(FILENAME1, 5*SR, 10*SR);
    long selection3[1];
    selection3[0] = 0;
    s2 = MakeSelectSound(s1, selection3, 1);
    s3 = MakeParSound(MakeNullSound(5*SR), s2);
    StartSound(gAudioPlayer, s3, GenRealDate(gAudioPlayer, GetCurDate()));
    
    next();
     
    // Joue une selection d'une région de 5 sec + parallele d'un silence
    s1 = MakeRegionSound(FILENAME2, 5*SR, 10*SR);
    long selection4[1];
    selection4[0] = 0;
    s2 = MakeSelectSound(s1, selection4, 1);
    s3 = MakeParSound(s2, MakeNullSound(5*SR));
    StartSound(gAudioPlayer, s3, GenRealDate(gAudioPlayer, GetCurDate()));
     
    next();
     
    // Montage en parallele de 2 selections
    s1 = MakeRegionSound(FILENAME1, 5*SR, 10*SR);
    long selection6[1];
    selection6[0] = 0;
    s2 = MakeSelectSound(s1, selection6, 1);
    
    s3 = MakeRegionSound(FILENAME2, 5*SR, 10*SR);
    long selection7[1];
    selection7[0] = 0;
    s4 = MakeSelectSound(s3, selection7, 1);
    
    s5 = MakeParSound(s2, s4);
    //MemoryRender(s5, 512);
    StartSound(gAudioPlayer, s5, GenRealDate(gAudioPlayer, GetCurDate()));
     */
    
    next();
    
    // Joue une région de 5 sec d'un fichier à la date courante, arrêt au bout de 3 sec
    date = GetCurDate();
    s1 = MakeRegionSound(FILENAME1, 5*SR, 10*SR);
    StartSound(gAudioPlayer, s1, GenRealDate(gAudioPlayer, date));
    
    //StopSound(gAudioPlayer, s1, GenRealDate(gAudioPlayer, date+3*SR));
    
    next();
    
    StopSound(gAudioPlayer, s1, GenRealDate(gAudioPlayer, 0));
    
    
    next();
     
    // Joue une région de 5 sec d'un fichier à 3 sec dans le futur, arrêt au bout de 6 sec
    date = GetCurDate();
    s1 = MakeRegionSound(FILENAME2, 5*SR, 10*SR);
    StartSound(gAudioPlayer, s1, GenRealDate(gAudioPlayer, date+3*SR));
    StopSound(gAudioPlayer, s1, GenRealDate(gAudioPlayer, date+4*SR));
  
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
    
    next();
    
    // Joue l'application d'un effet Faust (compilé dynamiquement) sur une région à la date courante
    s1 = MakeRegionSound(FILENAME1, 5*SR, 20*SR);
    s2 = MakeEffectSound(s1, MakeFaustAudioEffect(LLVM_EFFECT1, "", ""), SR/2, SR/2);
    //MemoryRender(s2, 512);
    StartSound(gAudioPlayer, s2, GenRealDate(gAudioPlayer, GetCurDate()));
   
    next();
       
    //std::string effect = "process = _,_;";
    
    // Joue l'application d'un effet Faust (compilé dynamiquement) sur une région à la date courante
    s1 = MakeRegionSound(FILENAME1, 5*SR, 20*SR);
    s2 = MakeEffectSound(s1, MakeRemoteFaustAudioEffect(LLVM_EFFECT1, "", ""), SR, SR);
    //MemoryRender(s2, 512);
    StartSound(gAudioPlayer, s2, GenRealDate(gAudioPlayer, GetCurDate()));
   
    next();
    
    // Application d'un effet Faust (chorus) sur l'entrée temps-réel (*capturée à partir de 0*)
    date = GetCurDate();
    s1 = MakeSharedInputSound();
    // On coupe la section entre 0 et la date courante, et on garde 6 sec à partir de la date courante
    s2 = MakeCutSound(s1, date, date+6*SR);  
    s3 = MakeEffectSound(s2, MakeFaustAudioEffect(LLVM_EFFECT4, "", ""), SR*2, SR*2);
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
    
    /*
    s3 = MakeEffectSound(s2, MakeFaustAudioEffect(LLVM_EFFECT4, "", ""), SR/2, SR/2);  // <== application d'un 1° effet
    s4 = MakeEffectSound(s2, MakeFaustAudioEffect(LLVM_EFFECT3, "", ""), SR/2, SR/2);  // <== application d'un 2° effet
    */
    
    s5 = MakeSeqSound(s3, MakeSeqSound(MakeNullSound(SR*2), s4, SR/2), SR/2);
    StartSound(gAudioPlayer, s5, GenRealDate(gAudioPlayer, date));
    
    next();
    
    // Application d'un effet Faust (chorus) sur l'entrée temps-réel (*capturée à partir de 0*)
    // et répétition de la même région avec un autre effet (freeverb), silence d'2 sec entre les 2 répétitions
    AudioEffect f1 = MakeRemoteFaustAudioEffect(LLVM_EFFECT4, "", "");
    AudioEffect f2 = MakeRemoteFaustAudioEffect(LLVM_EFFECT3, "", "");
    
    date = GetCurDate();
    s1 = MakeSharedInputSound();
    // On coupe la section entre 0 et la date courante, et on garde 6 sec à partir de la date courante
    s2 = MakeCutSound(s1, date, date+6*SR);      
    
    s3 = MakeEffectSound(MakeCopySound(s2), f1, SR/2, SR/2);  // <== application d'un 1° effet
    s4 = MakeEffectSound(MakeCopySound(s2), f2, SR/2, SR/2);  // <== application d'un 2° effet
    
    s5 = MakeSeqSound(s3, MakeSeqSound(MakeNullSound(SR*2), s4, SR/2), SR/2);
    StartSound(gAudioPlayer, s5, GenRealDate(gAudioPlayer, date));

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
    s1 = MakeRegionSound(FILENAME1, 5*SR, 20*SR);
    AudioEffect effect1 = MakeFaustAudioEffect(LLVM_EFFECT1, "", "");
    const char* name1 = GetNameEffect(effect1);
    printf("name %s\n", name1);
    s2 = MakeEffectSound(s1, effect1, SR/2, SR/2);  
    
    // Rampe sur un paramètre de l'effet en 2 sec, qui commence 1 sec après la date courante
    for (int i = 0; i < 100; i++) {
        SetTimedControlValueEffect(gAudioPlayer, name1, "/Freeverb/Wet", 1.-float(i)*0.01f, GenRealDate(gAudioPlayer, SR+date+i*SR/50));
    }
    
    // Rampe sur un paramètre de l'effet en 2 sec, qui commence 5 sec après la date courante
    for (int i = 0; i < 100; i++) {
        SetTimedControlValueEffect(gAudioPlayer, name1, "/Freeverb/Wet", float(i)*0.01f, GenRealDate(gAudioPlayer, SR*5+date+i*SR/50));
    }
    
    StartSound(gAudioPlayer, s2, GenRealDate(gAudioPlayer, date));
      
    next();
    
    // Contrôle temporel des effets sur une région d'un fichier
    s1 = MakeRegionSound(FILENAME1, 5*SR, 20*SR);
    AudioEffect effect2 = MakeRemoteFaustAudioEffect(LLVM_EFFECT1, "", "");
    const char* name2 = GetNameEffect(effect2);
    printf("name %s\n", name2);
    s2 = MakeEffectSound(s1, effect2, SR/2, SR/2);  
    
    date = GetCurDate();
    printf("date %lld\n", date);
    
    // Rampe sur un paramètre de l'effet en 2 sec, qui commence 1 sec après la date courante
    for (int i = 0; i < 100; i++) {
        SetTimedControlValueEffect(gAudioPlayer, name2, "/Freeverb/Wet", 1.-float(i)*0.01f, GenRealDate(gAudioPlayer, SR+date+i*SR/50));
    }
    
    // Rampe sur un paramètre de l'effet en 2 sec, qui commence 5 sec après la date courante
    for (int i = 0; i < 100; i++) {
        SetTimedControlValueEffect(gAudioPlayer, name2, "/Freeverb/Wet", float(i)*0.01f, GenRealDate(gAudioPlayer, SR*5+date+i*SR/50));
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