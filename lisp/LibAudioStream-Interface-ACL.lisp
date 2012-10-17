;; ============================================================================================
;; The LibAudioStream Library is Copyright (c) Grame, Computer Music Research Laboratory 03-05
;;
;; Grame : Computer Music Research Laboratory
;; Web : http://www.grame.fr/Research
;; ============================================================================================

;; This file contains definitions for entry points of the LibAudioStream library
;; It must be used with the LibAudioStream.dll

;; Utilities
;;===========

(in-package :cl-user)

;(load "LibAudioStream.dll")

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
(defparameter kJackRenderer 1)
(defparameter kCoreAudioRenderer 2)

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


;................................................................................: LibAudioStream
(def-foreign-call 
	(LibAudioStream "Version")
  	()
 :returning :long)


;;; Build sound

(defun delete-sound (stream)
  (print stream)
  (DeleteSound stream))
    
;................................................................................: MakeNullSound
(def-foreign-call 
	(MakeNullSoundPtr "MakeNullSoundPtr")
  	((length :long))
 :returning :long)

(defun MakeNullSound (length)
  (let ((sound (MakeNullSoundPtr length)))
    (excl::schedule-finalization sound 'delete-sound)
    sound))

;................................................................................: MakeReadSound
(def-foreign-call 
	(MakeReadSoundPtr "MakeReadSoundPtr")
  	((name SoundName))
 :returning :long)

(defun MakeReadSound (name)
  (let ((sound (MakeReadSoundPtr name)))
    (excl::schedule-finalization sound 'delete-sound)
    sound))
;................................................................................: MakeRegionSound
(def-foreign-call 
	(MakeRegionSoundPtr "MakeRegionSoundPtr")
  	((name SoundName)
  	 (begin :long)
  	 (end :long))
  :returning :long)

(defun MakeRegionSound (name begin end)
  (let ((sound (MakeRegionSoundPtr name begin end)))
    (excl::schedule-finalization sound 'delete-sound)
    sound))

;................................................................................: MakeFadeSound
(def-foreign-call 
	(MakeFadeSoundPtr "MakeFadeSoundPtr")
  	((sound :long)
  	 (fadein :long)
  	 (fadeout :long))
  :returning :long)

(defun MakeFadeSound (sound fadein fadeout)
  (let ((sound (MakeFadeSoundPtr sound fadein fadeout)))
    (excl::schedule-finalization sound 'delete-sound)
    sound))

;................................................................................: MakeLoopSound
(def-foreign-call 
	(MakeLoopSoundPtr "MakeLoopSoundPtr")
  	((sound :long)
  	 (len :long))
  :returning :long)

(defun MakeLoopSound (sound len)
  (let ((sound (MakeLoopSoundPtr sound len)))
    (excl::schedule-finalization sound 'delete-sound)
    sound))
;................................................................................: MakeCutSound
(def-foreign-call 
	(MakeCutSoundPtr "MakeCutSoundPtr")
  	((sound :long)
  	 (begin :long)
  	 (end :long))
  :returning :long)

(defun MakeCutSound (sound begin end)
  (let ((sound (MakeCutSoundPtr sound begin end)))
    (excl::schedule-finalization sound 'delete-sound)
    sound))

;................................................................................: MakeSeqSound
(def-foreign-call 
	(MakeSeqSoundPtr "MakeSeqSoundPtr")
  	((sound1 :long)
  	 (sound2 :long)
	 (crossfade :long))
  :returning :long)

(defun MakeSeqSound (sound1 sound2 crossfade)
  (let ((sound (MakeSeqSoundPtr sound1 sound2 crossfade)))
    (excl::schedule-finalization sound 'delete-sound)
    sound))
;................................................................................: MakeMixSound
(def-foreign-call 
	(MakeMixSoundPtr "MakeMixSoundPtr")
  	((sound1 :long)
  	 (sound2 :long))
  :returning :long)

(defun MakeMixSound (sound1 sound2)
  (let ((sound (MakeMixSoundPtr sound1 sound2)))
    (excl::schedule-finalization sound 'delete-sound)
    sound))

;................................................................................: MakeTransformSound
(def-foreign-call 
	(MakeTransformSoundPtr "MakeTransformSoundPtr")
  	((sound :long)
  	 (effect :long)
  	 (fadein :long)
  	 (fadeout :long))
  :returning :long)

(defun MakeTransformSound (sound effect fadein fadeout)
  (let ((sound (MakeTransformSoundPtr sound effect fadein fadeout)))
    (excl::schedule-finalization sound 'delete-sound)
    sound))

;................................................................................: MakeWriteSound
(def-foreign-call 
	(MakeWriteSoundPtr "MakeWriteSoundPtr")
  	((name SoundName)
  	 (sound :long)
  	 (format :long))
  :returning :long)

(defun MakeWriteSound (name sound format)
  (let ((sound (MakeWriteSoundPtr name sound format)))
    (excl::schedule-finalization sound 'delete-sound)
    sound))

;................................................................................: MakeInputSound
(def-foreign-call 
	(MakeInputSoundPtr "MakeInputSoundPtr")
  	(:void)
  :returning :long)

(defun MakeInputSound ()
  (let ((sound (MakeInputSoundPtr)))
    (excl::schedule-finalization sound 'delete-sound)
    sound))
  
;................................................................................: MakeRendererSound
(def-foreign-call 
	(MakeRendererSoundPtr "MakeRendererSoundPtr")
  	((sound :long))
  :returning :long)

(defun MakeRendererSound (sound)
  (let ((sound (MakeRendererSoundPtr sound)))
    (excl::schedule-finalization sound 'delete-sound)
    sound))

;................................................................................: GetLengthSound
(def-foreign-call 
	(GetLengthSound "GetLengthSoundPtr")
  	((sound :long))
  :returning :long)

;................................................................................: GetChannelsSound
(def-foreign-call 
	(GetChannelsSound "GetChannelsSoundPtr")
  	((sound :long))
  :returning :long)


;................................................................................: DeleteSound
(def-foreign-call 
	(DeleteSound "DeleteSoundPtr")
  	((sound :long))
  :returning :void)

;................................................................................: ResetSound
(def-foreign-call 
	(ResetSound "ResetSoundPtr")
  	((sound :long))
  :returning :void)

;................................................................................: ReadSound
(def-foreign-call 
    (ReadSound "ReadSoundPtr")
    ((sound :long)
     (buffer :foreign-address)
     (buffer_size :long)
     (channels :long))
  :returning :long)

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
	(renderer :long)
	(thread_num :long))
  :returning :long)

;;................................................................................: CloseAudioPlayer
(def-foreign-call 
	(CloseAudioPlayer "CloseAudioPlayer")
  	((player :long))
  :returning :void)

;; Channels
;................................................................................: LoadChannel
(def-foreign-call 
	(LoadChannel "LoadChannelPtr")
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
	(StartChannel "StartChannel")
  	((player :long)
  	 (chan :long))
  :returning :void)

;................................................................................: ContSound
(def-foreign-call 
	(ContChannel "ContChannel")
  	((player :long)
  	 (chan :long))
  :returning :void)

;................................................................................: StopSound
(def-foreign-call 
	(StopChannel "StopChannel")
  	((player :long)
  	 (chan :long))
  :returning :void)

;; Params

;................................................................................: SetVolSound
(def-foreign-call 
	(SetVolChannel "SetVolChannel")
  	((player :long)
  	 (chan :long)
  	 (vol :long))
  :returning :void)
 
;................................................................................: SetPanSound
(def-foreign-call 
	(SetPanChannel "SetPanChannel")
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


;;;============================ EFFECTS =================================    

;---------------------------------------------------------------------------: MakeAudioEffectList
(def-foreign-call 
    (MakeAudioEffectListPtr "MakeAudioEffectListPtr")
    ()
  :returning :foreign-address)

(defun MakeAudioEffectList ()
  (let ((effectlist (MakeAudioEffectListPtr)))
    (excl::schedule-finalization effectlist #'(lambda (effectlist) (DeleteEffectList effectlist)))
    effectlist))

;---------------------------------------------------------------------------: DeleteEffectList
(def-foreign-call 
    (DeleteEffectList "DeleteEffectListPtr")
    ((effect-list :foreign-address))
  :returning :void)


;---------------------------------------------------------------------------: DeleteEffect
(def-foreign-call 
    (DeleteEffect "DeleteEffectPtr")
    ((effect :foreign-address))
  :returning :void)

;---------------------------------------------------------------------------: MakeVolAudioEffect
(def-foreign-call 
    (MakeVolAudioEffectPtr "MakeVolAudioEffectPtr")
    ((gain :float))
  :returning :foreign-address)

(defun MakeVolAudioEffect (gain)
  (let ((effect (MakeVolAudioEffectPtr gain)))
    (excl::schedule-finalization effect #'(lambda (effect) (DeleteEffect effect)))
    effect))

;---------------------------------------------------------------------------: AddAudioEffect
(def-foreign-call 
    (AddAudioEffect "AddAudioEffectPtr")
    ((effect-list :foreign-address) 
     (effect :foreign-address))
  :returning :foreign-address)

;---------------------------------------------------------------------------: RemoveAudioEffect
(def-foreign-call 
    (RemoveAudioEffect "RemoveAudioEffectPtr")
    ((effect-list :foreign-address) 
     (effect :foreign-address))
  :returning :foreign-address)

;---------------------------------------------------------------------------: MakeFaustAudioEffect
(def-foreign-call 
    (MakeFaustAudioEffectPtr "MakeFaustAudioEffectPtr")
    ((gain (* :char )))
  :returning :foreign-address)

(defun MakeFaustAudioEffect (s)
  (let ((effect (MakeFaustAudioEffectPtr s)))
    (excl::schedule-finalization effect #'(lambda (effect) (DeleteEffect effect)))
    effect))

;---------------------------------------------------------------------------: GetControlCount
(def-foreign-call 
    (GetControlCountEffect "GetControlCountEffectPtr")
    ((effect :foreign-address))
  :returning :long)

;---------------------------------------------------------------------------: GetControlParam
;;; a tester...
(def-foreign-call 
    (GetControlParamEffectPtr "GetControlParamEffectPtr")
    ((effect :foreign-address)
     (control :long)
     (name :foreign-address)
     (min :foreign-address)
     (max :foreign-address)
     (init :foreign-address))
  :returning :void)

(defun GetControlParamEffect (effect control)
  (let ((name (ff::make-foreign-pointer :size 64))
        (min (ff::make-foreign-pointer :size 4))
        (max (ff::make-foreign-pointer :size 4))
        (init (ff::make-foreign-pointer :size 4)))
    (GetControlParamEffectPtr effect control name min max init)
    (values (excl::native-to-string (ff::foreign-pointer-address name))
            (sys::memref-int  (ff::foreign-pointer-address min) 0 0 :single-float)
            (sys::memref-int  (ff::foreign-pointer-address max) 0 0 :single-float)
            (sys::memref-int  (ff::foreign-pointer-address init) 0 0 :single-float))
    ))

;---------------------------------------------------------------------------: SetControlValue
(def-foreign-call 
    (SetControlValueEffect "SetControlValueEffectPtr")
    ((effect :foreign-address)
     (control :long)
     (value :float))
  :returning :void)

;---------------------------------------------------------------------------: GetControlValue
(def-foreign-call 
    (GetControlValueEffect "GetControlValueEffectPtr")
    ((effect :foreign-address)
     (control :long))
  :returning :float)
