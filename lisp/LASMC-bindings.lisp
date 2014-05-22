;;;LAS MC Bindings test


;;Load CFFI if needed
;(load "/Users/bouche/Documents/OpenMusic/OM6/trunk/OPENMUSIC/code/api/externals/FFI/load-cffi.lisp")

(defpackage :las
  (:use :common-lisp :cffi))

(in-package :las)

(fli:register-module "LibAudioStream" 
                     :real-name (namestring "/Users/bouche/Desktop/LAS\ 2/LibAudioStreamMC.framework/LibAudioStreamMC") ;;;Set the Framework PATH
                     :connection-style :immediate)

(defparameter SF_FORMAT_WAV     #x010000)
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
(defparameter SF_FORMAT_PCM_S8	#x0001)
(defparameter SF_FORMAT_PCM_16	#x0002)
(defparameter SF_FORMAT_PCM_24	#x0003)
(defparameter SF_FORMAT_PCM_32	#x0004)
(defparameter SF_FORMAT_PCM_U8	#x0005)


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
(defparameter close_err -2)
(defparameter load_err -3)
(defparameter file_not_found_err -4)
(defparameter effect_not_found_err -5)
(defparameter player_err -6)

;;; CFFI 0.11
(progn
  (cffi:defctype uint64_t :unsigned-long)
  (cffi:defctype audio-name :pointer)
  (cffi:defctype sound-ptr :pointer)
  (cffi:defctype effect-ptr :pointer)
  (cffi:defctype effectlist-ptr :pointer)
  (cffi:defctype stop-callback :pointer)
  (cffi:defctype stop-callback-ptr :pointer)
  (cffi:defctype context-ptr :pointer)
  (cffi:defctype symbolic-date :pointer)
  (cffi:defctype audio-renderer :pointer)
  (cffi:defctype audio_frames_t :unsigned-long))

(cffi:defcstruct TChannelInfo 
  (fStatus :long)
  (fCurFrame :long)
  (fVol :float)
  (fPanLeft :float)
  (fPanRight :float)
  (fLeftOut :long)
  (fRightOut :long))

(cffi:defcstruct DeviceInfo 
  (fName :long)
  (fMaxInputChannels :long)
  (fMaxOutputChannels :float)
  (fDefaultBufferSize :float)
  (fDefaultSampleRate :float))

(cffi:defcstruct TRendererInfo 
  (fInput :long)
  (fOutput :long)
  (fSampleRate :float)
  (fBufferSize :float)
  (fCurFrame uint64_t)
  (fCurUsec uint64_t)
  (fOutputLatencyFrame :long)
  (fOutputLatencyUsec :float)
  (fInputLatencyFrame :long)
  (fInputLatencyUsec :long))

(defstruct las-sound (ptr nil))
(defstruct las-effect (ptr nil))
(defstruct las-effectlist (ptr nil))

(defmethod las-null-ptr-p ((obj las-sound))
  (cffi:null-pointer-p (las-sound-ptr obj)))

(defmethod las-null-ptr-p ((obj las-effect))
  (cffi:null-pointer-p (las-effect-ptr obj)))

(defmethod las-eql-ptr ((obj1 las-effect) (obj2 las-effect))
  (cffi-sys:pointer-eq (las-effect-ptr obj1) (las-effect-ptr obj2)))

(defmethod las-null-ptr-p ((obj las-effectlist))
  (cffi:null-pointer-p (las-effectlist-ptr obj)))

(defparameter *ptr-counter* 0)

(defun register-rsrc (obj)
  ;(print (list "GC-REGISTERING_PTR" obj))
  (incf *ptr-counter*)
  (when (> *ptr-counter* 100) (sys::gc-all))
  (hcl::flag-special-free-action obj))

(defmethod audio-cleanup ((obj las-sound))
  ;(print (list "GC-AUDIO_CLEANUP" obj))
  (decf *ptr-counter*)
  (DeleteSound obj))

(defmethod audio-cleanup ((obj las-effect))
  ;(print (list "GC-AUDIO_CLEANUP" obj))
  (decf *ptr-counter*)
  (DeleteEffect obj))

(defmethod audio-cleanup ((obj las-effectlist))
  ;(print (list "GC-AUDIO_CLEANUP" obj))
  (decf *ptr-counter*)
  (DeleteEffectList obj))

(defmethod audio-cleanup ((obj t)) nil)

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



;;;==============;;;
;;;===LibTools===;;;
;;;==============;;;

;................................................................................: LibVersion
(cffi:defcfun ("LibVersion" lib-version) :long)
(defun LibVersion () 
 (lib-version))

;................................................................................: GetLastLibError
(cffi:defcfun  ("GetLastLibError" get-last-lib-error) :string)
(defun GetLastLibError ()
  (get-last-lib-error))



;;;============;;;
;;;===Sounds===;;;
;;;============;;;

;................................................................................: MakeNullSound
(cffi:defcfun ("MakeNullSound" make-null-sound) sound-ptr (length :long))
(defun MakeNullSound (length)
  (let ((snd (make-las-sound :ptr (make-null-sound length))))
    (register-rsrc snd)
    snd))

;................................................................................: MakeMultiNullSound
(cffi:defcfun ("MakeMultiNullSound" make-multi-null-sound) sound-ptr (length :long) (channels :long))
(defun MakeMultiNullSound (length channels)
  (let ((snd (make-las-sound :ptr (make-multi-null-sound length channels))))
    (register-rsrc snd)
    snd))

;................................................................................: MakeConstantSound
(cffi:defcfun ("MakeConstantSound" make-constant-sound) sound-ptr (channels :long) (length :long) (value :float))
(defun MakeConstantSound (channels length value)
  (let ((snd (make-las-sound :ptr (make-constant-sound channels length value))))
    (register-rsrc snd)
    snd))

;................................................................................: MakeReadSound
#-lispworks
(cffi:defcfun  ("MakeReadSoundPtr" make-read-sound-ptr) sound-ptr (s audio-name))

#-lispworks
(defun MakeReadSound (name)
  (cffi:with-foreign-string (s name)
    (let ((snd (make-las-sound :ptr (make-read-sound-ptr s))))
      (register-rsrc snd)
      snd)))

#+lispworks
(fli:define-foreign-function (make-read-sound-ptr "MakeReadSoundPtr")
    ((str (:reference-pass (:ef-mb-string :external-format #+cocoa :macos-roman #-cocoa :latin-1))))
  :result-type :pointer)

#+lispworks
(defun MakeReadSound (name)
  (let ((snd (make-las-sound :ptr (make-read-sound-ptr name))))
    (register-rsrc snd)
    snd))

;................................................................................: MakeRegionSound
#-lispworks
(cffi:defcfun  ("MakeRegionSoundPtr" make-region-sound-ptr) sound-ptr (s audio-name) (begin :long) (end :long))

#-lispworks
(defun MakeRegionSound (name begin end)
  (cffi:with-foreign-string (s name)
    (let ((snd (make-las-sound :ptr (make-region-sound-ptr s begin end))))
      (register-rsrc snd)
      snd)))

#+lispworks
(fli:define-foreign-function (make-region-sound-ptr "MakeRegionSound")
    ((str (:reference-pass (:ef-mb-string :external-format #+cocoa :macos-roman #-cocoa :latin-1)))
     (begin :long) (end :long))
  :result-type :pointer)

#+lispworks
(defun MakeRegionSound (name begin end)
  (let ((snd (make-las-sound :ptr (make-region-sound-ptr name begin end))))
    (register-rsrc snd)
    snd))

;................................................................................: MakeFadeSound
(cffi:defcfun ("MakeFadeSound" make-fade-sound) sound-ptr (sound sound-ptr) (fadein :long) (fadeout :long))
(defun MakeFadeSound (sound fadein fadeout)
  (let ((snd (make-las-sound :ptr (make-fade-sound (las-sound-ptr sound) fadein fadeout))))
    (register-rsrc snd)
    snd))

;................................................................................: MakeLoopSound
(cffi:defcfun ("MakeLoopSound" make-loop-sound) sound-ptr (sound sound-ptr) (len :long))
(defun MakeLoopSound (sound len)
  (let ((snd (make-las-sound :ptr (make-loop-sound (las-sound-ptr sound) len))))
    (register-rsrc snd)
    snd))

;................................................................................: MakeCutSound
(cffi:defcfun ("MakeCutSound" make-cut-sound) sound-ptr (sound sound-ptr) (begin :long) (end :long))
(defun MakeCutSound (sound begin end)
  (let ((snd (make-las-sound :ptr (make-cut-sound (las-sound-ptr sound) begin end))))
    (register-rsrc snd)
    snd))

;................................................................................: MakeSeqSound
(cffi:defcfun ("MakeSeqSound" make-seq-sound) sound-ptr (s1 sound-ptr) (s2 sound-ptr) (crossfade :long))
(defun MakeSeqSound (s1 s2 crossfade)
  (let ((snd (make-las-sound :ptr (make-seq-sound (las-sound-ptr s1) (las-sound-ptr s2) crossfade))))
    (register-rsrc snd)
    snd))

;................................................................................: MakeMixSound
(cffi:defcfun ("MakeMixSound" make-mix-sound) sound-ptr (s1 sound-ptr) (s2 sound-ptr))
(defun MakeMixSound (s1 s2)
  (let ((snd (make-las-sound :ptr (make-mix-sound (las-sound-ptr s1) (las-sound-ptr s2)))))
    (register-rsrc snd)
    snd))

;................................................................................: MakeParSound
(cffi:defcfun ("MakeParSound" make-par-sound) sound-ptr (s1 sound-ptr) (s2 sound-ptr))
(defun MakeParSound (s1 s2)
  (let ((snd (make-las-sound :ptr (make-par-sound (las-sound-ptr s1) (las-sound-ptr s2)))))
    (register-rsrc snd)
    snd))

;................................................................................: MakeSelectSound
(cffi:defcfun ("MakeSelectSound" make-select-sound) sound-ptr (s sound-ptr) (vect :pointer)) ;;;;;;/!/!/!/ type const std::vector<int>& selection
(defun MakeSelectSound (s1 s2)
  (let ((snd (make-las-sound :ptr (make-select-sound (las-sound-ptr s) vect))))
    (register-rsrc snd)
    snd))

;................................................................................: MakeEffectSound
(cffi:defcfun ("MakeEffectSound" make-effect-sound) sound-ptr (s sound-ptr) (effect effect-ptr) (fadein :long) (fadeout :long))
(defun MakeEffectSound (s effect fadein fadeout)
  (let ((snd (make-las-sound :ptr (make-effect-sound (las-sound-ptr s) (las-effect-ptr effect) fadein fadeout))))
    (register-rsrc snd)
    snd))

;................................................................................: MakePitchSchiftTimeStretchSound
(cffi:defcfun ("MakePitchSchiftTimeStretchSound" make-pitchshift-timestretch-sound) sound-ptr (s sound-ptr) (pitch :double) (stretch :double))
(defun MakePitchSchiftTimeStretchSound (s pitch stretch)
  (let ((snd (make-las-sound :ptr (make-pitchshift-timestretch-sound (las-sound-ptr s) pitch stretch))))
    (register-rsrc snd)
    snd))

;................................................................................: MakeWriteSound
(cffi:defcfun ("MakeWriteSound" make-write-sound) sound-ptr (s audio-name) (sound sound-ptr) (format :long))
(defun MakeWriteSound (name sound format)
  (cffi:with-foreign-string (s name)
    (let ((snd (make-las-sound :ptr (make-write-sound s (las-sound-ptr sound) format))))
      (register-rsrc snd)
      snd)))

;................................................................................: MakeInputSound
(cffi:defcfun ("MakeInputSound" make-input-sound) sound-ptr)
(defun MakeInputSound ()
  (let ((snd (make-las-sound :ptr (make-input-sound))))
    (register-rsrc snd)
    snd))
 
;................................................................................: MakeSharedInputSound
(cffi:defcfun ("MakeSharedInputSound" make-shared-input-sound) sound-ptr)
(defun MakeSharedInputSound ()
  (let ((snd (make-las-sound :ptr (make-shared-input-sound))))
    (register-rsrc snd)
    snd))

;................................................................................: MakeRendererSound
(cffi:defcfun ("MakeRendererSound" make-renderer-sound) sound-ptr (sound sound-ptr))
(defun MakeRendererSound (sound)
  (let ((snd (make-las-sound :ptr (make-renderer-sound (las-sound-ptr sound)))))
    (register-rsrc snd)
    snd))

;................................................................................: GetLengthSound
(cffi:defcfun ("GetLengthSound" get-length-sound) :long (sound sound-ptr))
(defun GetLengthSound (sound)
  (get-length-sound (las-sound-ptr sound)))

;................................................................................: GetChannelsSound
(cffi:defcfun  ( "GetChannelsSound" get-channels-sound) :long (sound sound-ptr))
(defun GetChannelsSound (sound)
  (get-channels-sound (las-sound-ptr sound)))

;................................................................................: ReadSound
(cffi:defcfun  ("ReadSound" read-sound) :long (sound sound-ptr) (buffer :pointer) (buffer-size :long) (channels :long)) ;;;/!/!/!/!/Pas de channels dans le header LASMCC
(defun ReadSound (sound buffer buffer_size channels)
  (read-sound (las-sound-ptr sound) buffer buffer_size channels))

;................................................................................: ReadSoundPos
(cffi:defcfun  ("ReadSoundPos" read-sound-pos) :long (sound sound-ptr) (buffer :pointer) (buffer-size :long) (frames :long) (pos :long) (channels :long)) ;;;/!/!/!/!/Pas de channels dans le header LASMCC
(defun ReadSoundPos (sound buffer buffer_size channels)
  (read-sound-pos (las-sound-ptr sound) buffer buffer_size frames pos channels))

;................................................................................: ResetSound
(cffi:defcfun ("ResetSound" reset-sound) :void (sound sound-ptr))
(defun ResetSound (sound)
  (reset-sound (las-sound-ptr sound)))

;................................................................................: MakeCopySound
(cffi:defcfun ("MakeCopySound" make-copy-sound) :void (sound sound-ptr))
(defun MakeCopySound (sound)
  (make-copy-sound (las-sound-ptr sound)))


;;;=============;;;
;;;===Effects===;;;
;;;=============;;;

;................................................................................: MakeFaustAudioEffect
(cffi:defcfun  ("MakeFaustAudioEffect" make-faust-audio-effect) effect-ptr (s0 audio-name) (s1 :pointer) (s2 :pointer))
(defun MakeFaustAudioEffect (name library_path draw_path)
  (cffi:with-foreign-string (s0 name)
    (cffi:with-foreign-string (s1 library_path)
      (cffi:with-foreign-string (s2 draw_path)
        (let ((effect (make-las-effect :ptr (make-faust-audio-effect s0 s1 s2))))
          (register-rsrc effect)
          effect)))))

;................................................................................: MakeRemoteFaustAudioEffect
(cffi:defcfun  ("MakeRemoteFaustAudioEffect" make-remote-faust-audio-effect) effect-ptr (s0 audio-name) (s1 :pointer) (s2 :pointer))
(defun MakeRemoteFaustAudioEffect (name library_path draw_path)
  (cffi:with-foreign-string (s0 name)
    (cffi:with-foreign-string (s1 library_path)
      (cffi:with-foreign-string (s2 draw_path)
        (let ((effect (make-las-effect :ptr (make-remote-faust-audio-effect s0 s1 s2))))
          (register-rsrc effect)
          effect)))))

;................................................................................: GetControlCountEffect
(cffi:defcfun ("GetControlCountEffect" get-control-count-effect) :long (effect effect-ptr))
(defun GetControlCountEffect (effect)
  (get-control-count-effect-ptr (las-effect-ptr effect)))

;................................................................................: GetControlParamEffect
(cffi:defcfun ("GetControlParamEffect" get-control-param-effect) :void (effect effect-ptr) (control :long) (name audio-name) (min :pointer) (max :pointer) (init :pointer))
(defun GetControlParamEffect (effect control)
  (let ((name (cffi::%foreign-alloc 64))
        (min (cffi::%foreign-alloc 4))
        (max (cffi::%foreign-alloc 4))
        (init (cffi::%foreign-alloc 4) ) str minrep maxrep initrep)
    (get-control-param-effect (las-effect-ptr effect) control name min max init)
    (setf str (cffi::foreign-string-to-lisp name))
    (setf minrep (cffi::mem-ref min :float))
    (setf maxrep (cffi::mem-ref max :float))
    (setf initrep (cffi::mem-ref init :float))
    (cffi::foreign-free name)
    (cffi::foreign-free min)
    (cffi::foreign-free max)
    (cffi::foreign-free init)
    (list str minrep maxrep initrep)))

;................................................................................: SetControlValueEffect
(cffi:defcfun ("SetControlValueEffect" set-control-value-effect) :void (effect effect-ptr) (control :long) (value :float))
(defun SetControlValueEffect (effect control value)
 (set-control-value-effect (las-effect-ptr effect) control value))

;................................................................................: GetControlValueEffect
(cffi:defcfun ("GetControlValueEffect" get-control-value-effect) :float (effect effect-ptr) (control :long))
(defun GetControlValueEffect (effect control)
  (get-control-value-effect (las-effect-ptr effect) control))

;................................................................................: SetStateEffect
(cffi:defcfun ("SetStateEffect" set-state-effect) :void (effect effect-ptr) (state :long))
(defun SetStateEffect (effect state)
  (set-state-effect (las-effect-ptr effect) state))

;................................................................................: GetStateEffect
(cffi:defcfun ("GetStateEffect" get-state-effect) :long (effect effect-ptr))
(defun GetStateEffect (effect)
  (get-state-effect (las-effect-ptr effect)))

;................................................................................: ResetEffect
(cffi:defcfun ("ResetEffect" reset-effect) :void (effect effect-ptr))
(defun ResetEffect (effect)
  (reset-effect (las-effect-ptr effect)))

;................................................................................: ProcessEffect
(cffi:defcfun ("ProcessEffect" process-effect) :void (effect effect-ptr) (input-buffer :pointer) (output-buffer :pointer) (framesnum :long))
(defun ProcessEffect (effect)
  (process-effect (las-effect-ptr effect) input-buffer output-buffer framesnum))

;................................................................................: GetJsonEffect
(cffi:defcfun ("GetJsonEffect" get-json-effect) :string (effect effect-ptr))
(defun GetJsonEffect (effect)
  (get-json-effect (las-effect-ptr effect)))

;................................................................................: GetNameEffect
(cffi:defcfun ("GetNameEffect" get-name-effect) :string (effect effect-ptr))
(defun GetNameEffect (effect)
  (get-name-effect (las-effect-ptr effect)))

;................................................................................: MakeCopyEffect
(cffi:defcfun ("MakeCopyEffect" make-copy-effect) effect-ptr (effect effect-ptr))
(defun MakeCopyEffect (effect)
  (make-copy-effect (las-effect-ptr effect)))


;;;=============;;;
;;;===Players===;;;
;;;=============;;;

;................................................................................: SetTimedControlValueEffect
(cffi:defcfun ("SetTimedControlValueEffect" set-timed-control-value-effect) :long (player :pointer) (effect :pointer) (path :pointer) (value :float) (date symbolic-date))
(defun SetTimedControlValueEffect (player effect path value date)
  (set-timed-control-value-effect player effect path value date))

;................................................................................: SetAudioLatencies
(cffi:defcfun ("SetAudioLatencies" set-audio-latencies) :void (input-latency :long) (output-latency :long))
(defun SetAudioLatencies (input-latency output-latency)
  (set-audio-latencies input-latency output-latency))

;................................................................................: OpenAudioPlayer
(cffi:defcfun ("OpenAudioPlayer" open-audio-player) :pointer
  (inchan :long)
  (outchan :long)
  ;(channels :long)
  (sr :long)
  (bs :long)
  (sbs :long)
  (rtbs :long)
  (renderer :long)
  (thread_num :long))
(defun OpenAudioPlayer (inchan outchan sr bs sbs rtbs renderer thread_num)
  (open-audio-player inchan outchan sr bs sbs rtbs renderer thread_num))

;................................................................................: CloseAudioPlayer
(cffi:defcfun ("CloseAudioPlayer" close-audio-player) :void (player :pointer))
(defun CloseAudioPlayer (player)
  (close-audio-player player))

;................................................................................: OpenAudioClient
(cffi:defcfun ("OpenAudioClient" open-audio-client) :pointer (manager audio-renderer))
(defun OpenAudioClient (manager)
  (open-audio-client manager))

;................................................................................: CloseAudioPlayer
(cffi:defcfun ("CloseAudioClient" close-audio-client) :void (player :pointer))
(defun CloseAudioClient (player)
  (close-audio-client player))

;................................................................................: StartSound
(cffi:defcfun ("StartSound" start-sound) :long (player :pointer) (s sound-ptr) (date symbolic-date))
(defun StartSound (player s date)
  (start-sound player (las-sound-ptr s) date))

;................................................................................: StopSound
(cffi:defcfun ("StopSound" stop-sound) :long (player :pointer) (s sound-ptr) (date symbolic-date))
(defun StopSound (player s date)
  (stop-sound player (las-sound-ptr s) date))

;................................................................................: GenSymbolicDate
(cffi:defcfun ("GenSymbolicDate" gen-symbolic-date) symbolic-date (player :pointer))
(defun GenSymbolicDate (player)
  (gen-symbolic-date player))

;................................................................................: GenRealDate
(cffi:defcfun ("GenRealDate" gen-real-date) symbolic-date (player :pointer) (date audio_frames_t))
(defun GenRealDate (player date)
  (gen-real-date player date))

;................................................................................: SetSymbolicDate
(cffi:defcfun ("SetSymbolicDate" set-symbolic-date) :long (player :pointer) (symb-date symbolic-date) (real-date audio_frames_t))
(defun SetSymbolicDate (player symb-date real-date)
  (set-symbolic-date player symb-date real-date))

;................................................................................: GetSymbolicDate
(cffi:defcfun ("GetSymbolicDate" get-symbolic-date) audio_frames_t (player :pointer) (symb-date symbolic-date))
(defun GetSymbolicDate (player symb-date)
  (get-symbolic-date player symb-date))

;................................................................................: StartAudioPlayer
(cffi:defcfun ("StartAudioPlayer" start-audio-player) :long (player :pointer))
(defun StartAudioPlayer (player)
  (start-audio-player player))

;................................................................................: StopAudioPlayer
(cffi:defcfun ("StopAudioPlayer" stop-audio-player) :long (player :pointer))
(defun StopAudioPlayer (player)
  (stop-audio-player player))

;................................................................................: ClearAudioPlayer
(cffi:defcfun ("ClearAudioPlayer" clear-audio-player) :long (player :pointer))
(defun ClearAudioPlayer (player)
  (clear-audio-player player))

;................................................................................: GetAudioPlayerRenderer
(cffi:defcfun ("GetAudioPlayerRenderer" get-audio-player-renderer) audio-renderer (player :pointer))
(defun GetAudioPlayerRenderer (player)
  (get-audio-player-renderer player))

;................................................................................: GetDeviceCount
(cffi:defcfun ("GetDeviceCount" get-device-count) :long (renderer audio-renderer))
(defun GetDeviceCount (renderer)
  (get-device-count renderer))

;................................................................................: GetDeviceInfo
(cffi:defcfun ("GetDeviceInfo" get-device-info) :void (renderer audio-renderer) (device-num :long) (info :pointer))
(defun GetDeviceInfo (renderer device-num info)
  (get-device-info renderer device-num info))

;................................................................................: GetDefaultInputDevice
(cffi:defcfun ("GetDefaultInputDevice" get-default-input-device) :long (renderer audio-renderer))
(defun GetDefaultInputDevice (renderer)
  (get-default-input-device renderer))

;................................................................................: GetDefaultOutputDevice
(cffi:defcfun ("GetDefaultOutputDevice" get-default-output-device) :long (renderer audio-renderer))
(defun GetDefaultOutputDevice (renderer)
  (get-default-output-device renderer))

;................................................................................: MakeAudioRenderer
(cffi:defcfun ("MakeAudioRenderer" make-audio-renderer) audio-renderer (renderer :long))
(defun MakeAudioRenderer (renderer)
  (make-audio-renderer renderer))

;................................................................................: DeleteAudioRenderer
(cffi:defcfun ("DeleteAudioRenderer" delete-audio-renderer) :void (renderer audio-renderer))
(defun DeleteAudioRenderer (renderer)
  (delete-audio-renderer renderer))

;................................................................................: OpenAudioRenderer
(cffi:defcfun ("OpenAudioRenderer" open-audio-renderer) :long 
  (renderer audio-renderer) 
  (inputdevice :long) 
  (outputdevice :long) 
  (inchan :long) 
  (outchan :long) 
  (buffersize :long) 
  (samplerate :long))
(defun OpenAudioRenderer (renderer inputdevice outputdevice inchan outchan buffersize samplerate)
  (open-audio-renderer renderer inputdevice outputdevice inchan outchan buffersize samplerate))

;................................................................................: CloseAudioRenderer
(cffi:defcfun ("CloseAudioRenderer" close-audio-renderer) :void (renderer audio-renderer))
(defun CloseAudioRenderer (renderer)
  (close-audio-renderer renderer))

;................................................................................: StartAudioRenderer
(cffi:defcfun ("StartAudioRenderer" start-audio-renderer) :void (renderer audio-renderer))
(defun StartAudioRenderer (renderer)
  (start-audio-renderer renderer))

;................................................................................: StopAudioRenderer
(cffi:defcfun ("StopAudioRenderer" stop-audio-renderer) :void (renderer audio-renderer))
(defun StopAudioRenderer (renderer)
  (stop-audio-renderer renderer))

;................................................................................: GetAudioRendererInfo
(cffi:defcfun ("GetAudioRendererInfo" get-audio-renderer-info) :void (renderer audio-renderer) (info :pointer))
(defun GetAudioRendererInfo (renderer info)
  (get-audio-renderer-info renderer info))

;................................................................................: AddAudioClient
(cffi:defcfun ("AddAudioClient" add-audio-client) :void (renderer audio-renderer) (client :pointer))
(defun AddAudioClient (renderer client)
  (add-audio-client renderer client))

;................................................................................: RemoveAudioClient
(cffi:defcfun ("RemoveAudioClient" remove-audio-client) :void (renderer audio-renderer) (client :pointer))
(defun RemoveAudioClient (renderer client)
  (remove-audio-client renderer client))

;................................................................................: AudioGlobalsInit
(cffi:defcfun ("AudioGlobalsInit" audio-globals-init) :void
  (inchan :long) 
  (outchan :long) 
  (samplerate :long) 
  (buffersize :long) 
  (streambuffersize :long) 
  (rtstreamduration :long)
  (threadnum :long))
(defun AudioGlobalsInit (inchan outchan samplerate buffersize streambuffersize rtstreamduration threadnum)
  (audio-globals-init inchan outchan samplerate buffersize streambuffersize rtstreamduration threadnum))

;................................................................................: AudioGlobalsDestroy
(cffi:defcfun ("AudioGlobalsDestroy" audio-globals-destroy) :void)
(defun AudioGlobalsDestroy ()
  (audio-globals-destroy))