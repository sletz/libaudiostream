
(in-package :au)
;;(in-package :las)
(libaudiostream-framework)

(LibVersion)

;;;=======================
;;; SoundFiles
;;;=======================

;;(defparameter soundfile1 "D:\\acl62-trial\\Alla faglar.wav")
;;(defparameter soundfile2 "D:\\acl62-trial\\BjornenSover.wav")

(defparameter soundfile1 "/Users/letz/Music/Sounds/levot.wav")

(defparameter soundfile1 "/Users/letz/Music/Sounds/guit1.wav")


(defparameter soundfile2 "/Users/letz/Music/Sounds/tango.wav")
;;(defparameter effect1 "/Volumes/Document1/Developpement/ProjectsCVS/FaustCVS/FaustCVS/faust/examples/moduledir/freeverb.so")

(defparameter effect1 "/Documents/faust-sf/examples/freeverb.dsp")
(defparameter effect2 "/Documents/faust-sf/examples/zita_rev1.bc")
(defparameter effect3 "process = _,_;")
(defparameter effect4 "process = component(\"effect.lib\").zita_rev1;")

;;;=======================
;;; Multi-channel Player
;;;=======================

;; Input channels
;; Output channels
;; Number of channels
;; Sample rate
;; Buffer size
;; Disk buffer size
;; Disk buffer size for real-time stream
;; Number of additional low priority streams used for disk access or advance stream computation

;;(setq player (OpenAudioPlayer 2 2 32 44100 1024 65536 65536 kPortAudioRenderer 1))

(defparameter samplerate 44100)

;; WARNING !! when using Jack, the Sample rate and Buffer size values much match the values currently used with Jack server
 
(setq player (OpenAudioPlayer 0 2 32 samplerate 512 65536 (* samplerate 60 10) kJackRenderer 1))
(setq player (OpenAudioPlayer 0 2 32 samplerate 512 65536 (* samplerate 60 10) kPortAudioRenderer 1))

(setq player (OpenAudioPlayer 0 2 32 samplerate 512 65536 (* samplerate 60 10) kCoreAudioRenderer 1))
(setq player (OpenAudioPlayer 2 2 32 samplerate 512 65536 (* samplerate 60 10) kCoreAudioRenderer 1))

(setq player (OpenAudioPlayer 0 2 32 96000 512 65536 (* 96000 60 10) kCoreAudioRenderer 1))
(setq player (OpenAudioPlayer 2 0 32 96000 512 65536 (* 96000 60 10) kCoreAudioRenderer 1))
(setq player (OpenAudioPlayer 2 2 32 96000 512 65536 (* 96000 60 10) kCoreAudioRenderer 1))

(setq player (OpenAudioPlayer 0 2 32 48000 512 65536 (* 48000 60 10) kCoreAudioRenderer 1))
(setq player (OpenAudioPlayer 1 2 32 48000 512 65536 (* 48000 60 10) kCoreAudioRenderer 1))

(setq player (OpenAudioPlayer 2 1 32 48000 512 65536 (* 48000 60 10) kCoreAudioRenderer 1))

(setq player (OpenAudioPlayer 2 2 32 48000 512 65536 (* 48000 60 10) kCoreAudioRenderer 1))

(CloseAudioPlayer player)

;; Start Audio engine execution
;;==============================

(StartAudioPlayer player)
(StopAudioPlayer player)


;; Stream creation using files
;;=============================

(setq s1 (MakeReadSound soundfile1))
(setq s1 (MakeStereoSound (MakeReadSound soundfile1)))

(setq s1 (MakeRegionSound soundfile1 400000 450000) )

(GetLengthSound s1)
(GetChannelsSound s1)

(setq s2 (MakeReadSound soundfile2))

(GetLengthSound s2)
(GetChannelsSound s2)

(setq s3 (MakeLoopSound (MakeRegionSound soundfile1 400000 450000) 50))

(setq s4 (MakeMixSound (MakeRegionSound soundfile2 200000 550000) (MakeLoopSound (MakeRegionSound soundfile1 400000 450000) 10)))

(setq s4 (MakeRegionSound soundfile2 200000 550000))
(setq s4 (MakeMixSound (MakeRegionSound soundfile1 100000 450000) (MakeRegionSound soundfile2 200000 550000)))

(GetLengthSound s3)
(GetChannelsSound s3)

(setq s4 (MakeWriteSound "out.aiff" 
                          (MakeLoopSound 
                           (MakeFadeSound
                           (MakeRegionSound soundfile1 400000 450000)
                           200 200)
                           50) 
                          (logior SF_FORMAT_AIFF SF_FORMAT_PCM_16)))

;; Real-time input (audio thru)
;;==============================

(setq s5 (MakeInputSound))


;; Record real-time input
;;========================

(setq s6 (MakeWriteSound "input.aiff"  (MakeInputSound) 
                          (logior SF_FORMAT_AIFF SF_FORMAT_PCM_16)))


;; When played, files based streams are read "asynchronously" (using an aditionnal feeder thread)
;; To access a stream "synchronously", it has to be wrapped by a "MakeRendererSound" construct. 
;;===============================================================================================


(setq s7 (MakeRendererSound (MakeReadSound soundfile1)))
(GetLengthSound s7)
(GetChannelsSound s7)

(defparameter buffer_size 512)
(defvar buffer (ccl::newptrclear (* 4  buffer_size (GetChannelsSound s7))))

(ReadSound s7 buffer buffer_size (GetChannelsSound s7))

(ccl::%get-single-float  buffer 0)
(ccl::%get-single-float  buffer 1)


;; Use of a Faust effect (http://faudiostream.sf.net)
;;====================================================

(defvar freeverb1 (MakeFaustAudioEffect effect1 "" ""))
(defvar freeverb2 (MakeFaustAudioEffect effect2 "" ""))
(defvar freeverb3 (MakeFaustAudioEffect effect3 "" ""))
(defvar freeverb4 (MakeFaustAudioEffect effect4 "" "/var/tmp" ))

(defvar paramEQ1 (MakeFaustAudioEffect effect2))

(MakeFaustAudioEffect "import(\"music.lib\"); process = vgroup(\"echo-simple\", echo1s);" "" "")

;; Print effect parameters

(GetControlParamEffect freeverb1 0)
(GetControlParamEffect freeverb1 1)
(GetControlParamEffect freeverb1 2)

(defun print-params (effect)
  (dotimes (i (GetControlCountEffect effect))
    (multiple-value-bind (name min max init) (GetControlParamEffect effect i)
      (print (list name min max init (GetControlValueEffect effect i))))))

(print-params freeverb1)
(print-params freeverb2)
(print-params freeverb3)
(print-params freeverb4)
(print-params paramEQ1)

(defvar effect_list1 (MakeAudioEffectList))
(setq effect_list1 (AddAudioEffect effect_list1 freeverb1))

(defvar effect_list2 (MakeAudioEffectList))
(setq effect_list2 (AddAudioEffect effect_list2 freeverb2))

(defvar effect_list3 (MakeAudioEffectList))
(setq effect_list3 (AddAudioEffect effect_list3 freeverb3))

(defvar effect_list4 (MakeAudioEffectList))
(setq effect_list4 (AddAudioEffect effect_list4 freeverb4))


(setq s8 (MakeTransformSound (MakeReadSound soundfile1) effect_list1 100 100))

(setq s8 (MakeTransformSound (MakeReadSound soundfile1) (AddAudioEffect  (MakeAudioEffectList) (MakeFaustAudioEffect "/Documents/LibAudioStream-git/lisp/freeverb.dsp")) 100 100))


;; Change effect parameters : parameters are reseted to their default value each time the "transform" stream is reseted, 
;; typically when the stream is re-started: changing parameters value can be done "on the fly" when the stream is playing  

(SetControlValueEffect freeverb1 0 0.9)
(SetControlValueEffect freeverb1 1 0.9)
(SetControlValueEffect freeverb1 2 0.9)

(SetControlValueEffect freeverb2 1 0.1)
(SetControlValueEffect freeverb2 2 0.1)

(SetControlValueEffect freeverb3 1 0.9)
(SetControlValueEffect freeverb3 2 0.6)

(SetControlValueEffect freeverb4 1 0.1)
(SetControlValueEffect freeverb4 2 0.1)


(SetEffectListAudioPlayer player effect_list1 88100 88100)
(SetEffectListAudioPlayer player effect_list2 44100 44100)


(SetEffectListChannel player 1 effect_list3  44100 44100)
(SetEffectListChannel player 1 effect_list4  44100 44100)


;; Load audio player channels
;;============================

(LoadChannel player s1 1 1.0 1.0 0.0)
(LoadChannel player s2 2 1.0 1.0 0.0)

(LoadChannel player s3 3 1.0 1.0 0.0)
(LoadChannel player s4 4 1.0 1.0 0.0)

(LoadChannel player s5 5 1.0 1.0 0.0)
(LoadChannel player s6 6 1.0 1.0 0.0)

(LoadChannel player s7 7 1.0 1.0 0.0)
(LoadChannel player s8 8 1.0 1.0 0.0)


;; Channels can be started/stopped/continued individually 
;;========================================================

;; Start channels
;;================

(StartChannel player 1)
(StartChannel player 2)
(StartChannel player 3)
(StartChannel player 4)
(StartChannel player 5)
(StartChannel player 6)
(StartChannel player 7)
(StartChannel player 8)

;; Stop channels
;;================

(StopChannel player 1)
(StopChannel player 2)
(StopChannel player 3)
(StopChannel player 4)
(StopChannel player 5)
(StopChannel player 6)
(StopChannel player 7)
(StopChannel player 8)


;; Continue channels
;;====================

(ContChannel player 1)
(ContChannel player 2)
(ContChannel player 3)
(ContChannel player 4)
(ContChannel player 5)
(ContChannel player 6)
(ContChannel player 7)
(ContChannel player 8)

;;==================================================================================
;; NOTE : To start all channels simultanously in a perfectly synchronous manner, 
;; the Audio engine has to be stopped (using StopAudioPlayer), then channels set in 
;; play mode (using StartSound) and StartAudioPlayer will start all channels at 
;; the same time.
;; The same thing can be done to "resume" a set of channels: use 
;; StopAudioPlayer/StartAudioPlayer to stop/resume.
;;==================================================================================


;; Set channel volume (0 1)
;;============================

(SetVolChannel player 1 1.0)
(SetVolChannel player 1 0.5)

;; Set channel pan (0 1)
;;============================

(SetPanChannel player 1 1.0 1.0)  // full left
(SetPanChannel player 1 0.0 0.0)  // full right
(SetPanChannel player 1 1.0 0.0)  // center
(SetPanChannel player 8 1.0 0.0)  // center

(SetPanChannel player 8 1.0 1.0) // full left
(SetPanChannel player 8 0.0 0.0  // full left
(SetPanChannel player 8 1.0 1.0)  // center


;; Set audio player volume (0 1)
;;================================

(SetVolAudioPlayer player 0.6)

;; Set audio player pan  (0 1)
;;================================

(SetPanAudioPlayer player 1.0 1.0)

;; Streams not used anymore in channels MUST be deleted (typically before loading a new stream in a channel)
;;==========================================================================================================

(deletesound s1)

;; Stop and close
;;==================

(StopAudioPlayer player)

(CloseAudioPlayer player)


