

;;;=======================
;;; SoundFiles
;;;=======================

(defparameter soundfile1 "D:\\acl62-trial\\Alla faglar.wav")
(defparameter soundfile2 "D:\\acl62-trial\\BjornenSover.wav")

;(defparameter soundfile1 "Users/letz/levot.wav")
;(defparameter soundfile2 "Users/letz/tango.wav")


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

;;(setq player (OpenAudioPlayer 0 2 32 44100 1024 65536 65536 kPortAudioRenderer))
(setq player (OpenAudioPlayer 0 2 32 44100 1024 65536 65536 kJackRenderer))


;; Start Audio engine execution
;;==============================

(StartAudioPlayer player)

;; Stream creation using files
;;=============================

(setq s1 (MakeReadSound soundfile1))

(GetLengthSound s1)
(GetChannelsSound s1)

(setq s2 (MakeReadSound soundfile2))

(GetLengthSound s2)
(GetChannelsSound s2)

(setq s3 (MakeLoopSound (MakeRegionSound soundfile1 400000 450000) 50))

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


(setq s10 (MakeRendererSound (MakeReadSound soundfile1)))
(GetLengthSound s10)
(GetChannelsSound s10)

(defparameter buffer_size 512)
(defvar buffer (ccl::newptrclear (* 4  buffer_size (GetChannelsSound s10))))

(ReadSound s10 buffer buffer_size (GetChannelsSound s10))

(ccl::%get-long  buffer 0)
(ccl::%get-long  buffer 1)


;; Load audio player channels
;;============================

(LoadChannel player s1 1 120 64)
(LoadChannel player s2 2 120 64)

(LoadChannel player s3 3 120 64)
(LoadChannel player s4 4 120 64)

(LoadChannel player s5 5 120 64)
(LoadChannel player s6 6 120 64)


;; Channels can be started/stopped/continued individually 
;;========================================================

;; Start channels
;;================

(StartSound player 1)
(StartSound player 2)
(StartSound player 3)
(StartSound player 4)
(StartSound player 5)
(StartSound player 6)

;; Stop channels
;;================

(StopSound player 1)
(StopSound player 2)
(StopSound player 3)
(StopSound player 4)
(StopSound player 5)
(StopSound player 6)

;; Continue channels
;;====================

(ContSound player 1)
(ContSound player 2)
(ContSound player 3)
(ContSound player 4)
(ContSound player 5)
(ContSound player 6)


;;==================================================================================
;; NOTE : To start all channels simultanously in a perfectly synchronous manner, 
;; the Audio engine has to be stopped (using StopAudioPlayer), then channels set in 
;; play mode (using StartSound) and StartAudioPlayer will start all channels at 
;; the same time.
;; The same thing can be done to "resume" a set of channels: use 
;; StopAudioPlayer/StartAudioPlayer to stop/resume.
;;==================================================================================


;; Set channel volume (0 127)
;;============================

(SetVolSound player 1 80)
(SetVolSound player 1 50)

;; Set channel pan (0 127)
;;============================

(SetPanSound player 1 80)

;; Set audio player volume (0 127)
;;================================

(SetVolAudioPlayer player 80)

;; Set audio player pan  (0 127)
;;================================

(SetPanAudioPlayer player 64)

;; Streams not used anymore in channels MUST be deleted (typically before loading a new stream in a channel)
;;==========================================================================================================

(deletesound s1)

;; Stop and close
;;==================

(StopAudioPlayer player)

(CloseAudioPlayer player)


