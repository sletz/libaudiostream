;; ========================================================================================
;; The AudioEngine Library is Copyright (c) Grame, Computer Music Research Laboratory 03-04
;;
;; Grame : Computer Music Research Laboratory
;; Web : http://www.grame.fr/Research
;; ========================================================================================

;; This file contains definitions for entry points of the AudioEngine library
;; It must be used with the AudioEngine.dll

;; Utilities
;;===========

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "LibAudioStream.dll"))

(use-package 'ff)


;; libsndfile types

(defparameter SF_FORMAT_WAV   #x010000)
(defparameter SF_FORMAT_AIFF	#x020000)
(defparameter SF_FORMAT_AU	#x030000)
(defparameter SF_FORMAT_RAW	#x040000)
(defparameter SF_FORMAT_PAF	#x050000)
(defparameter SF_FORMAT_SVX	#x060000)
(defparameter SF_FORMAT_NIST	#x070000)
(defparameter SF_FORMAT_VOC	#x080000)
(defparameter SF_FORMAT_IRCAM	#x0A0000)
(defparameter SF_FORMAT_W64	#x0B0000)
(defparameter SF_FORMAT_MAT4	#x0C0000)
(defparameter SF_FORMAT_MAT5	#x0D0000)
	
(defparameter SF_FORMAT_PCM_S8   #x0001)
(defparameter SF_FORMAT_PCM_16   #x0002)
(defparameter SF_FORMAT_PCM_24   #x0003)
(defparameter SF_FORMAT_PCM_32   #x0004)

(defparameter SF_FORMAT_PCM_U #x0005)


;;---------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------
;;
;; 				Player Data Structures
;;
;;---------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------

(defparameter kPortAudioRenderer 0)
(defparameter kJackAudioRenderer 1)

(defparameter no_err 0)
(defparameter open_err -1)
(defparameter load_err -2)
(defparameter file_not_found_err -3)
(defparameter state_err -4)

(def-foreign-type SoundName (* :char))

(def-foreign-type TChannelInfo
(:struct
  (fStatus :long)
  (fVol :long)
  (fPan :long)
  (fLeftOut :long)
  (fRightOut :long)))
  
(def-foreign-type TChannelInfoPtr (* TChannelInfo))
 
;;................................................................................: status
(defun status (e)
  (fslot-value-typed 'TChannelInfo :foreign-static-gc e 'fStatus))

;;................................................................................: vol
(defun vol (e)
  (fslot-value-typed 'TChannelInfo :foreign-static-gc e 'fVol))

;;................................................................................: pan
(defun pan (e)
  (fslot-value-typed 'TChannelInfo :foreign-static-gc e 'fPan))

;;................................................................................: left-out
(defun left-out (e)
  (fslot-value-typed 'TChannelInfo :foreign-static-gc e 'fLeftOut))

;;................................................................................: right-out
(defun right-out (e)
  (fslot-value-typed 'TChannelInfo :foreign-static-gc e 'fRightOut))


;; Open/Close

;;; Build sound

;................................................................................: MakeNullSound
(def-foreign-call 
	(MakeNullSound "MakeNullSound")
  	((length :long))
 :returning :long)

;................................................................................: MakeReadSound
(def-foreign-call 
	(MakeReadSound "MakeReadSound")
  	((name SoundName))
 :returning :long)

;................................................................................: MakeRegionSound
(def-foreign-call 
	(MakeRegionSound "MakeRegionSound")
  	((name SoundName)
  	 (begin :long)
  	 (end :long))
 :returning :long)

;................................................................................: MakeFadeSound
(def-foreign-call 
	(MakeFadeSound "MakeFadeSound")
  	((sound :long)
  	 (fadein :long)
  	 (fadeout :long))
  :returning :long)
  	 
;................................................................................: MakeLoopSound
(def-foreign-call 
	(MakeLoopSound "MakeLoopSound")
  	((sound :long)
  	 (len :long))
  :returning :long)

;................................................................................: MakeCutSound
(def-foreign-call 
	(MakeCutSound "MakeCutSound")
  	((sound :long)
  	 (begin :long)
  	 (end :long))
  :returning :long)

;................................................................................: MakeSeqSound
(def-foreign-call 
	(MakeSeqSound "MakeSeqSound")
  	((sound1 :long)
  	 (sound2 :long)
	 (crossfade :long))
  :returning :long)

;................................................................................: MakeMixSound
(def-foreign-call 
	(MakeMixSound "MakeMixSound")
  	((sound1 :long)
  	 (sound2 :long))
  :returning :long)

;................................................................................: MakeTransformSound
(def-foreign-call 
	(MakeTransformSound "MakeTransformSound")
  	((sound :long)
  	 (effect :long)
  	 (fadein :long)
  	 (fadeout :long))
  :returning :long)

;................................................................................: MakeRegionSound
(def-foreign-call 
	(MakeWriteSound "MakeWriteSound")
  	((name SoundName)
  	 (sound :long)
  	 (format :long))
  :returning :long)

;................................................................................: MakeInputSound
(def-foreign-call 
	(MakeInputSound "MakeInputSound")
  	(:void)
  :returning :long)
  
;................................................................................: MakeRendererSound
(def-foreign-call 
	(MakeRendererSound "MakeRendererSound")
  	((sound :long))
  :returning :long)

;................................................................................: GetLengthSound
(def-foreign-call 
	(GetLengthSound "GetLengthSound")
  	((sound :long))
  :returning :long)

;................................................................................: GetChannelsSound
(def-foreign-call 
	(GetChannelsSound "GetChannelsSound")
  	((sound :long))
  :returning :long)

;................................................................................: DeleteSound
(def-foreign-call 
	(DeleteSound "DeleteSound")
  	((sound :long))
  :returning :void)
  
;................................................................................: OpenAudioPlayer
(def-foreign-call 
	(OpenAudioPlayer "OpenAudioPlayer")
  	((inchan :long)
  	(outchan :long)
  	(channels :long)
  	(sr :long)
  	(bs :long)
  	(sbs :long)
  	(rtbs :long)
	(renderer :long))
  :returning :long)

;;................................................................................: CloseAudioPlayer
(def-foreign-call 
	(CloseAudioPlayer "CloseAudioPlayer")
  	((player :long))
  :returning :void)

;; Channels
;................................................................................: LoadChannel
(def-foreign-call 
	(LoadChannel "LoadChannel")
  	((player :long)
  	(sound :long)
  	(channels :long)
  	(vol :long)
  	(pan :long))
  :returning :long)
  
;................................................................................: GetInfoChannel
(def-foreign-call 
	(GetInfoChannel "GetInfoChannel")
  	((player :long)
  	(chan :long)
  	(info TChannelInfoPtr))
  :returning :void)

;; Transport
;................................................................................: StartAudioPlayer
(def-foreign-call 
	(StartAudioPlayer "StartAudioPlayer")
  	((player :long))
  :returning :void)

;................................................................................: StartAudioPlayer
(def-foreign-call 
	(StopAudioPlayer "StopAudioPlayer")
  	((player :long))
  :returning :void)

;................................................................................: StartSound
(def-foreign-call 
	(StartSound "StartSound")
  	((player :long)
  	 (chan :long))
  :returning :void)

;................................................................................: ContSound
(def-foreign-call 
	(ContSound "ContSound")
  	((player :long)
  	 (chan :long))
  :returning :void)

;................................................................................: StopSound
(def-foreign-call 
	(StopSound "StopSound")
  	((player :long)
  	 (chan :long))
  :returning :void)

;; Params

;................................................................................: SetVolSound
(def-foreign-call 
	(SetVolSound "SetVolSound")
  	((player :long)
  	 (chan :long)
  	 (vol :long))
  :returning :void)
 
;................................................................................: SetPanSound
(def-foreign-call 
	(SetPanSound "SetPanSound")
  	((player :long)
  	 (chan :long)
  	 (vol :long))
  :returning :void)

;; Master

;................................................................................: SetVolAudioPlayer
(def-foreign-call 
	(SetVolAudioPlayer "SetVolAudioPlayer")
  	((player :long)
  	 (vol :long))
  :returning :void)

;................................................................................: SetPanAudioPlayer
(def-foreign-call 
	(SetPanAudioPlayer "SetPanAudioPlayer")
  	((player :long)
  	 (pan :long))
  :returning :void)




