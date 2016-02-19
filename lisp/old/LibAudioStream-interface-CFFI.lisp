;; ============================================================================================
;; The LibAudioStream Library is Copyright (c) Grame, Computer Music Research Laboratory 03-05
;;
;; Grame : Computer Music Research Laboratory
;; Web : http://www.grame.fr/Research
;; ============================================================================================

;; This file contains definitions for entry points of the LibAudioStream library
;; It must be used with the LibAudioStream.Framework located in /System/Library/Frameworks

(in-package :cl-user)

(cl:defpackage "Audio"
  (:nicknames "AU")
   (:use common-lisp))

(in-package :au)

(defvar *liblibaudio* "/Library/Frameworks/LibAudioStream.framework/LibAudioStream")

(defvar *libaudiostream* nil)

(defun libaudiostream-framework ()
  (or *libaudiostream*
     (setq *libaudiostream*
           (if (probe-file *liblibaudio*)
               (progn (cffi:load-foreign-library *liblibaudio*)
                 (hcl::add-special-free-action 'audio-cleanup) 
                 t)))))

;;;  (libaudiostream-framework)

;; libsndfile types

(defparameter SF_FORMAT_WAV #x010000)
(defparameter SF_FORMAT_AIFF #x020000)
(defparameter SF_FORMAT_AU #x030000)
(defparameter SF_FORMAT_RAW #x040000)
(defparameter SF_FORMAT_PAF #x050000)
(defparameter SF_FORMAT_SVX #x060000)
(defparameter SF_FORMAT_NIST #x070000)
(defparameter SF_FORMAT_VOC #x080000)
(defparameter SF_FORMAT_IRCAM #x0A0000)
(defparameter SF_FORMAT_W64 #x0B0000)
(defparameter SF_FORMAT_MAT4 #x0C0000)
(defparameter SF_FORMAT_MAT5 #x0D0000)	
(defparameter SF_FORMAT_PCM_S8 #x0001)
(defparameter SF_FORMAT_PCM_16 #x0002)
(defparameter SF_FORMAT_PCM_24 #x0003)
(defparameter SF_FORMAT_PCM_32 #x0004)
(defparameter SF_FORMAT_PCM_U8 #x0005)

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

(cffi:defcstruct TChannelInfo 
  (fStatus :long)
  (fCurFrame :long)
  (fVol :float)
  (fPanLeft :float)
  (fPanRight :float)
  (fLeftOut :long)
  (fRightOut :long))

(cffi:define-foreign-type audio-name () ':pointer)
(cffi:define-foreign-type sound-ptr () ':pointer)
(cffi:define-foreign-type effect-ptr () ':pointer)
(cffi:define-foreign-type effectlist-ptr () ':pointer)

(defvar *audio-rsrc-manager* nil)
(defvar *effect-rsrc-manager* nil)
(defvar *effectlist-rsrc-manager* nil)

(defun register-rsrc (obj type)
  (case type
    (:sound (push (fli::pointer-address obj) *audio-rsrc-manager*))
    (:effect (push (fli::pointer-address obj) *audio-rsrc-manager*))
    (:effectlist (push (fli::pointer-address obj) *audio-rsrc-manager*)))
  (hcl::flag-special-free-action obj))

(defun audio-cleanup (obj)
  ;;; (print (list "audio cleanup..." obj (fli::pointer-p obj)))
  (when (fli::pointer-p obj)
    (let ((address (fli::pointer-address obj)))
      (cond ((member address *audio-rsrc-manager*)
             (delete address *audio-rsrc-manager*)
             (DeleteSound obj))
            ((member address *effect-rsrc-manager*)
             (delete address *effect-rsrc-manager*)
             (DeleteEffect obj))
            ((member address *effectlist-rsrc-manager*)
             (delete address *effectlist-rsrc-manager*)
             (DeleteEffectList obj))
            (t nil)))))

;;................................................................................: status
(defmacro status (e)
 `(cffi:foreign-slot-value ,e 'TChannelInfo 'fStatus))

;;................................................................................: vol
(defmacro vol (e)
 `(cffi:foreign-slot-value ,e 'TChannelInfo 'fVol))

;;................................................................................: pan
(defmacro panLeft (e)
 `(cffi:foreign-slot-value ,e 'TChannelInfo 'fPanLeft))

(defmacro panRight (e)
 `(cffi:foreign-slot-value ,e 'TChannelInfo 'fPanRight))

;;................................................................................: left-out
(defmacro left-out (e)
 `(cffi:foreign-slot-value ,e 'TChannelInfo 'fLeftOut))

;;................................................................................: right-out
(defmacro right-out (e)
 `(cffi:foreign-slot-value ,e 'TChannelInfo 'fRightOut))

;entrypoints
;................................................................................: LibAudioStream
(cffi:defcfun ("LibVersion" lib-version) :short)

(defun LibVersion () 
 (lib-version))

;;; Build sound

;................................................................................: MakeNullSound
(cffi:defcfun ("MakeNullSoundPtr" make-null-sound-ptr) sound-ptr (length :long))

(defun MakeNullSound (length)
 (let ((snd (make-null-sound-ptr length)))
    (register-rsrc snd :audio)
    snd))

;(setf xxx (make-null-sound-ptr 2000))
;(setf sss (MakeNullSound 100))
;(type-of sss)
;(fli::pointer-address sss)
;(fli::pointer-pointer-type sss)
;(typep sss 'fli::pointer)
;(subtypep (type-of sss) 'fli::pointer)

;................................................................................: MakeReadSound
(cffi:defcfun  ("MakeReadSoundPtr" make-read-sound-ptr) sound-ptr (s audio-name))

(defun MakeReadSound (name)
  (cffi:with-foreign-string (s name)
    (let ((snd (make-read-sound-ptr s)))
      (register-rsrc snd :audio)
    snd)))

;................................................................................: MakeRegionSound
(cffi:defcfun  ("MakeRegionSoundPtr" make-region-sound-ptr) sound-ptr (s audio-name) (begin :long) (end :long))

(defun MakeRegionSound (name begin end)
  (cffi:with-foreign-string (s name)
    (let ((snd (make-region-sound-ptr s begin end)))
    (register-rsrc snd :audio)
    snd)))

;................................................................................: MakStereoSound
(cffi:defcfun ( "MakeStereoSoundPtr" make-stereo-sound) sound-ptr (sound sound-ptr))

(defun MakeStereoSound (sound)
   (let ((snd (make-stereo-sound sound)))
     (register-rsrc snd :audio)
    snd))

;................................................................................: MakeFadeSound
(cffi:defcfun ("MakeFadeSoundPtr" make-fade-sound-ptr) sound-ptr (sound sound-ptr) (fadein :long) (fadeout :long))

(defun MakeFadeSound (sound fadein fadeout)
   (let ((snd (make-fade-sound-ptr sound fadein fadeout)))
     (register-rsrc snd :audio)
    snd))

;................................................................................: MakeLoopSound
(cffi:defcfun ("MakeLoopSoundPtr" make-loop-sound-ptr) sound-ptr (sound sound-ptr) (len :long))

(defun MakeLoopSound (sound len)
   (let ((snd (make-loop-sound-ptr sound len)))
     (register-rsrc snd :audio)
    snd))
                   

;................................................................................: MakeCutSound
(cffi:defcfun  ("MakeCutSoundPtr" make-cut-sound-ptr) sound-ptr (sound sound-ptr) (begin :long) (end :long))

(defun MakeCutSound (sound begin end)
   (let ((snd (make-cut-sound-ptr sound begin end)))
     (register-rsrc snd :audio)
    snd))

;................................................................................: MakeSeqSound
(cffi:defcfun  ("MakeSeqSoundPtr" make-seq-sound-ptr) sound-ptr (s1 sound-ptr) (s2 sound-ptr) (crossfade :long))

(defun MakeSeqSound (s1 s2 crossfade)
   (let ((snd (make-seq-sound-ptr s1 s2 crossfade)))
     (register-rsrc snd :audio)
    snd))

;................................................................................: MakeMixSound
(cffi:defcfun  ("MakeMixSoundPtr" make-mix-sound-ptr) sound-ptr (s1 sound-ptr) (s2 sound-ptr))

(defun MakeMixSound (s1 s2 )
   (let ((snd (make-mix-sound-ptr s1 s2 )))
     (register-rsrc snd :audio)
    snd))

;................................................................................: MakeTransformSound
(cffi:defcfun  ( "MakeTransformSoundPtr" make-transform-sound-ptr) sound-ptr (sound sound-ptr) (effect :pointer) (fadein :long) (fadeout :long))

(defun MakeTransformSound (sound effect fadein fadeout )
   (let ((snd (make-transform-sound-ptr sound effect fadein fadeout )))
     (register-rsrc snd :audio)
    snd))

;................................................................................: MakeWriteSound
(cffi:defcfun  ( "MakeWriteSoundPtr" make-write-sound-ptr) sound-ptr (s audio-name) (sound sound-ptr) (format :long))

(defun MakeWriteSound (name sound format)
   (cffi:with-foreign-string (s name)
     (let ((snd (make-write-sound-ptr s sound format )))
       (register-rsrc snd :audio)
       snd)))
 
;................................................................................: MakeInputSound
(cffi:defcfun  ( "MakeInputSoundPtr" make-input-sound-ptr) sound-ptr)

(defun MakeInputSound ()
   (let ((snd (make-input-sound-ptr)))
     (register-rsrc snd :audio)
     snd))

;................................................................................: MakeRendererSound
(cffi:defcfun  ( "MakeRendererSoundPtr" make-renderer-sound-ptr) sound-ptr (sound sound-ptr))

(defun MakeRendererSound (sound)
  (let ((snd (make-renderer-sound-ptr sound)))
    (register-rsrc snd :audio)
    snd))

;................................................................................: GetLengthSound
(cffi:defcfun  ( "GetLengthSoundPtr" get-lenght-sound-ptr) :long (sound sound-ptr))

(defun GetLengthSound (sound)
  (get-lenght-sound-ptr sound))

;................................................................................: GetChannelsSound
(cffi:defcfun  ( "GetChannelsSoundPtr" get-channel-sound-ptr) :long (sound sound-ptr))

(defun GetChannelsSound (sound)
  (get-channel-sound-ptr sound))

;................................................................................: ResetSound
(cffi:defcfun  ( "ResetSoundPtr" reset-sound-ptr) :void (sound sound-ptr))

(defun ResetSound (sound)
  (reset-sound-ptr sound))

;................................................................................: ReadSound
(cffi:defcfun  ( "ReadSoundPtr" read-sound-ptr) :long (sound sound-ptr) (buffer :pointer) (buffer-size :long) (channels :long))

(defun ReadSound (sound buffer buffer_size channels)
  (read-sound-ptr sound buffer buffer_size channels))

;................................................................................: DeleteSound
(cffi:defcfun  ( "DeleteSoundPtr" delete-sound) :void (sound sound-ptr))

(defun DeleteSound (sound)
  (delete-sound sound))

;................................................................................: OpenAudioPlayer
(cffi:defcfun  ("OpenAudioPlayer" open-audio-player) :pointer
                     (inchan :long)
                     (outchan :long)
                     (channels :long)
                     (sr :long)
                     (bs :long)
                     (sbs :long)
                     (rtbs :long)
                     (renderer :long)
                     (thread_num :long))

(defun OpenAudioPlayer (inchan outchan channels sr bs sbs rtbs renderer thread_num)
  (open-audio-player inchan outchan channels sr bs sbs rtbs renderer thread_num))

;;................................................................................: CloseAudioPlayer
(cffi:defcfun  ( "CloseAudioPlayer" close-audio-player) :void (player :pointer))

(defun CloseAudioPlayer (player)
  (close-audio-player player))

;; Channels
;................................................................................: LoadChannel
(cffi:defcfun  ("LoadChannelPtr" load-channel-ptr) :long
   (player :pointer)
   (sound sound-ptr)
   (chan :long)
   (vol :float)
   (panLeft :float)
   (panRight :float))

(defun LoadChannel (player sound chan vol panLeft panRight)
  (load-channel-ptr player sound chan vol panLeft panRight))

;................................................................................: GetInfoChannel
(cffi:defcfun  ("GetInfoChannel" get-info-channel) :void (player :pointer) (chan :long) (info :pointer))

(defun GetInfoChannel (player chan info) 
  (get-info-channel player chan info))

;; Transport
;................................................................................: StartAudioPlayer
(cffi:defcfun  ("StartAudioPlayer" start-audio-player) :void (player :pointer) )

(defun StartAudioPlayer (player)
  (start-audio-player player))

;................................................................................: StartAudioPlayer
(cffi:defcfun  ("StopAudioPlayer" stop-audio-player) :void (player :pointer) )

(defun StopAudioPlayer (player)
  (stop-audio-player player))

;................................................................................: StartSound
(cffi:defcfun  ("StartChannel" start-channel) :void (player :pointer) (chan :long) )

(defun StartChannel (player chan)
  (start-channel player chan))

;................................................................................: ContSound
(cffi:defcfun  ("ContChannel" cont-channel) :void (player :pointer) (chan :long) )

(defun ContChannel (player chan)
  (cont-channel player chan))

;................................................................................: StopSound
(cffi:defcfun  ("StopChannel" stop-channel) :void (player :pointer) (chan :long) )

(defun StopChannel (player chan)
  (stop-channel player chan))

;; Params

;................................................................................: SetVolChannel
(cffi:defcfun  ("SetVolChannel" set-vol-channel) :void (player :pointer) (chan :long) (vol :float) )

(defun SetVolChannel (player chan vol)
  (set-vol-channel player chan vol))

;................................................................................: SetPanChannel
(cffi:defcfun  ("SetPanChannel" set-pan-channel) :void (player :pointer) (chan :long) (panleft :float) (panright :float))

(defun SetPanChannel (player chan panLeft panRight)
  (set-pan-channel player chan panLeft panRight))

;................................................................................: SetEffectListChannel
(cffi:defcfun  ("SetEffectListChannel" set-effect-list-channel) :void (player :pointer) (chan :long) (effect_list :pointer) (fadein :long) (fadeout :long))

(defun SetEffectListChannel (player chan effect_list fadein fadeout)
  (set-effect-list-channel (player chan effect_list fadein fadeout)))

;; Master

;................................................................................: SetVolAudioPlayer
(cffi:defcfun  ("SetVolAudioPlayer" set-vol-audio-player) :void (player :pointer)  (vol :float))

(defun SetVolAudioPlayer (player vol)
 (set-vol-audio-player player vol))
;................................................................................: SetPanSound
(cffi:defcfun  ("SetPanAudioPlayer" set-pan-audio-player) :void (player :pointer)  (vol :float) (panl :float) (panr :float))

(defun SetPanAudioPlayer (player panLeft panRight)
  (set-pan-audio-player player panLeft panRight))

;................................................................................: SetEffectAudioPlayer
(cffi:defcfun  ("SetEffectListAudioPlayer" set-effect-list-audio-player) :void (player :pointer)  (effectlist :pointer) (fadein :long) (fadeout :long))

(defun SetEffectListAudioPlayer (player effect_list fadein fadeout)
  (set-effect-list-audio-player player effect_list fadein fadeout))

;;;========================== EFFECTS 

(cffi:defcfun  ("DeleteEffectListPtr" delete-effect-list-ptr) :void  (effectlist :pointer) )

(defun DeleteEffectList (effect_list)
 (delete-effect-list-ptr effect_list))

(cffi:defcfun  ("DeleteEffectPtr" delete-effect-ptr) :void  (effect :pointer) )

(defun DeleteEffect (effect)
  (delete-effect-ptr effect))

(cffi:defcfun  ("MakeAudioEffectListPtr" make-audio-effect-list) :pointer  )

(defun MakeAudioEffectList ()
  (let ((effect_list (make-audio-effect-list)))
    (register-rsrc effect_list :effectlist)
    effect_list))

(cffi:defcfun  ("AddAudioEffectPtr" add-audio-effect-ptr) :pointer  (effectlist :pointer) (effect :pointer))
     
(defun AddAudioEffect (effect-list effect)
  (add-audio-effect-ptr effect-list effect))

(cffi:defcfun  ("RemoveAudioEffect" remove-audio-effect-ptr) :pointer  (effectlist :pointer) (effect :pointer))

(defun RemoveAudioEffect (effect-list effect)
  (remove-audio-effect-ptr effect-list effect))

(cffi:defcfun  ("MakeVolAudioEffectPtr" make-vol-audio-effect-ptr) :pointer  (gain :float))

(defun MakeVolAudioEffect (gain)
  (let ((effect (make-vol-audio-effect-ptr gain)))
     (register-rsrc effect :effect)
     effect))

(cffi:defcfun  ("MakeMonoPanAudioEffectPtr" make-mono-pan-audio-effect-ptr) :pointer  (pan :float))

(defun MakeMonoPanAudioEffect (pan)
  (let ((effect (make-mono-pan-audio-effect-ptr pan)))
    (register-rsrc effect :effect)
    effect))

(cffi:defcfun  ("MakeStereoPanAudioEffectPtr" make-stereo-pan-audio-effect-ptr) :pointer  (panl :float) (panr :float))

(defun MakeStereoPanAudioEffect (panLeft panRight)
  (let ((effect (make-stereo-pan-audio-effect-ptr panLeft panRight)))
    (register-rsrc effect :effect)
    effect))

(cffi:defcfun  ("MakeDispatchFaustAudioEffectPtr" make-faust-audio-effect-ptr) :pointer (s1 audio-name) (s2 audio-name) (s3 audio-name))

(defun MakeFaustAudioEffect (name library_path draw_path)
  (cffi:with-foreign-string (s1 name) 
     (cffi:with-foreign-string (s2 library_path)
         (cffi:with-foreign-string (s3 draw_path)
            (let ((effect (make-faust-audio-effect-ptr s1 s2 s3)))
                (register-rsrc effect :effect)
                effect)))))

(cffi:defcfun  ("GetControlCountEffectPtr" get-control-count-effect-ptr) :long  (effect :pointer))

(defun GetControlCountEffect (effect)
 (get-control-count-effect-ptr effect))

(cffi:defcfun  ("GetControlParamEffectPtr" get-control-param-effect-ptr) :void  (effect :pointer) (control :long) (name audio-name) (min :pointer) (max :pointer) (init :pointer))

(defun GetControlParamEffect (effect control)
  (let ((name (cffi::%foreign-alloc 64))
        (min (cffi::%foreign-alloc 4))
        (max (cffi::%foreign-alloc 4))
        (init (cffi::%foreign-alloc 4) ) str minrep maxrep initrep)
 (get-control-param-effect-ptr effect control name min max init)
 (setf str (cffi::foreign-string-to-lisp name))
 (setf minrep (cffi::mem-ref min :float))
 (setf maxrep (cffi::mem-ref max :float))
 (setf initrep (cffi::mem-ref init :float))
 (cffi::foreign-free name)
 (cffi::foreign-free min)
 (cffi::foreign-free max)
 (cffi::foreign-free init)
 (values str minrep maxrep initrep)))

(cffi:defcfun  ("SetControlValueEffectPtr" set-control-value-effect-ptr) :void  (effect :pointer) (control :long) (value :float))

(defun SetControlValueEffect (effect control value)
 (set-control-value-effect-ptr effect control value))

(cffi:defcfun  ("GetControlValueEffectPtr" get-control-value-effect-ptr) :float  (effect :pointer) (control :long))
(defun GetControlValueEffect (effect control)
 (get-control-value-effect-ptr effect control))

