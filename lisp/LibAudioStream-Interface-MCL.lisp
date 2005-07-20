;; ============================================================================================
;; The LibAudioStream Library is Copyright (c) Grame, Computer Music Research Laboratory 03-05
;;
;; Grame : Computer Music Research Laboratory
;; Web : http://www.grame.fr/Research
;; ============================================================================================

;; This file contains definitions for entry points of the LibAudioStream library
;; It must be used with the LibAudioStream.Framework located in /System/Library/Frameworks

;; Utilities
;;===========

(defvar *__CFStringMakeConstantString-slep*
  (ccl::get-slep "__CFStringMakeConstantString"))

(defun CFSTR (string)
  (with-cstrs ((cstr string))
    (ccl::ff-call-slep *__CFStringMakeConstantString-slep*
                       :address cstr 
                       :address)))

(defun create-frameworks-url ()
  (rlet ((fsref :fsref))
    (let* ((err (#_FSFindFolder #$kOnAppropriateDisk #$kFrameworksFolderType #$true fsref)))
      (declare (type (signed-byte 16) err))
      (if (eql #$noErr err)
        (let* ((url (#_CFURLCreateFromFSRef (%null-ptr) fsref)))
          (if (%null-ptr-p url)
            (error "Failed to create URL")
            url))
        (error "Couldn't find system Frameworks folder")))))

(ccl::defloadvar *frameworks-url* nil)

(defun frameworks-url ()
  (or *frameworks-url*
      (setq *frameworks-url* (create-frameworks-url))))

(defun load-framework-bundle (framework-name)
  (let* ((bundle-url 
          (#_CFURLCreateCopyAppendingPathComponent
           (%null-ptr)
           (frameworks-url)    ; file:///System/Library/Frameworks/
           (CFSTR framework-name)
           #$false)))
    (if (%null-ptr-p bundle-url)
      (error "Can't create URL for ~s in system frameworks folder" 
             framework-name)
      (let* ((bundle (#_CFBundleCreate (%null-ptr) bundle-url)))
        (if (%null-ptr-p bundle)
          (error "Can't create bundle for ~s" framework-name)
          (if (eql #$false (#_CFBundleLoadExecutable bundle))

            (error "Couldn't load bundle library for ~s" framework-name)
            bundle))))))

(defun lookup-function-in-framework (symbol-name bundle)
  (let* ((addr (#_CFBundleGetFunctionPointerForName bundle (CFSTR symbol-name))))
    (if (%null-ptr-p addr)
      (error "Couldn't resolve address of foreign function ~s" symbol-name)
      ;; This may be a little confusing: MCL uses fixnums (whose low 2 bits are
      ;; zero) to represent function addresses (whose low 2 bits are zero ...)
      ;; Shove the pointer in a buffer; fetch a signed 32-bit integer, shift it
      ;; right 2 bits ... voila.
      (rlet ((buf :long))
        (setf (%get-ptr buf) addr)
        (ash (%get-signed-long buf) -2)))))

(defmacro get-fun-addr (name framework)
  `(lookup-function-in-framework ,name ,framework))

(defvar *libaudiostream* nil)

(defun libaudiostream-framework ()
  (or *libaudiostream*
      (setq *libaudiostream*
            (load-framework-bundle "LibAudioStream.framework"))))

(libaudiostream-framework)

;; libsndfile types

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
;; 				Audio Player Data Structures
;;
;;---------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------

(defparameter kPortAudioRenderer 0)
(defparameter kJackRenderer 1)

(defparameter no_err 0)
(defparameter open_err -1)
(defparameter load_err -2)
(defparameter file_not_found_err -3)
(defparameter state_err -4)

(defrecord TChannelInfo
  (fStatus :longint)
  (fVol :single-float)
  (fPan :single-float)
  (fLeftOut :longint)
  (fRightOut :longint))
 
;;................................................................................: status
(defmacro status (e)
 `(rref ,e :TChannelInfo.fStatus))

;;................................................................................: vol
(defmacro vol (e)
 `(rref ,e :TChannelInfo.fVol))

;;................................................................................: pan
(defmacro pan (e)
 `(rref ,e :TChannelInfo.fPan))

;;................................................................................: left-out
(defmacro left-out (e)
 `(rref ,e :TChannelInfo.fLeftOut))

;;................................................................................: right-out
(defmacro right-out (e)
 `(rref ,e :TChannelInfo.fRightOut))


;................................................................................: LibAudioStream
(defmacro LibAudioStream ()
  `(ccl::ppc-ff-call (get-fun-addr "Version" *libaudiostream*) 
                                  :signed-fullword))
     

;;; Build sound

;................................................................................: MakeNullSound
(defmacro MakeNullSound (length)
  `(let ((sound (ccl::ppc-ff-call (get-fun-addr "MakeNullSoundPtr" *libaudiostream*) 
                                  :signed-fullword, length
                                  :address)))
     (terminate-when-unreachable sound #'(lambda(sound) (print sound) (DeleteSound sound)))
     sound))

;................................................................................: MakeReadSound
(defmacro MakeReadSound (name)
  `(with-cstrs ((s ,name))
     (let ((sound (ccl::ppc-ff-call (get-fun-addr "MakeReadSoundPtr" *libaudiostream*) 
                                    :address s
                                    :address)))
       (terminate-when-unreachable sound #'(lambda(sound) (print sound) (DeleteSound sound)))
       sound)))

;................................................................................: MakeRegionSound
(defmacro MakeRegionSound (name begin end)
  `(with-cstrs ((s ,name))
     (let ((sound (ccl::ppc-ff-call (get-fun-addr "MakeRegionSoundPtr" *libaudiostream*) 
                                    :address s
                                    :signed-fullword, begin
                                    :signed-fullword, end
                                    :address)))
       (terminate-when-unreachable sound #'(lambda(sound) (print sound) (DeleteSound sound)))
       sound)))


;................................................................................: MakStereoSound
(defmacro MakeStereoSound (sound)
  `(let ((sound (ccl::ppc-ff-call (get-fun-addr "MakeStereoSoundPtr" *libaudiostream*) 
                     :address ,sound 
                     :address)))
      (terminate-when-unreachable sound #'(lambda(sound) (print sound) (DeleteSound sound)))
       sound))

;................................................................................: MakeFadeSound
(defmacro MakeFadeSound (sound fadein fadeout)
  `(let ((sound (ccl::ppc-ff-call (get-fun-addr "MakeFadeSoundPtr" *libaudiostream*) 
                     :address ,sound 
                     :signed-fullword, fadein
                     :signed-fullword, fadeout
                     :address)))
      (terminate-when-unreachable sound #'(lambda(sound) (print sound) (DeleteSound sound)))
       sound))

;................................................................................: MakeLoopSound
(defmacro MakeLoopSound (sound len)
  `(let ((sound (ccl::ppc-ff-call (get-fun-addr "MakeLoopSoundPtr" *libaudiostream*) 
                     :address ,sound 
                     :signed-fullword, len
                     :address)))
      (terminate-when-unreachable sound #'(lambda(sound) (print sound) (DeleteSound sound)))
       sound))

;................................................................................: MakeCutSound
(defmacro MakeCutSound (sound begin end)
  `(let ((sound (ccl::ppc-ff-call (get-fun-addr "MakeCutSoundPtr" *libaudiostream*) 
                        :address ,sound
                        :signed-fullword, begin
                        :signed-fullword, end
                        :address)))
      (terminate-when-unreachable sound #'(lambda(sound) (print sound) (DeleteSound sound)))
       sound))

;................................................................................: MakeSeqSound
(defmacro MakeSeqSound (s1 s2 crossfade)
  `(let ((sound (ccl::ppc-ff-call (get-fun-addr "MakeSeqSoundPtr" *libaudiostream*) 
                                  :address ,s1
                                  :address ,s2
                                  :signed-fullword ,crossfade
                                  :address)))
     (terminate-when-unreachable sound #'(lambda(sound) (print sound) (DeleteSound sound)))
     sound))

;................................................................................: MakeMixSound
(defmacro MakeMixSound (s1 s2)
  `(let ((sound (ccl::ppc-ff-call (get-fun-addr "MakeMixSoundPtr" *libaudiostream*) 
                                  :address ,s1
                                  :address ,s2
                                  :address)))
     (terminate-when-unreachable sound #'(lambda(sound) (print sound) (DeleteSound sound)))
     sound))

;................................................................................: MakeTransformSound
(defmacro MakeTransformSound (sound effect fadein fadeout)
  `(let ((sound (ccl::ppc-ff-call (get-fun-addr "MakeTransformSoundPtr" *libaudiostream*) 
                                  :address ,sound
                                  :address ,effect
                                  :signed-fullword, fadein
                                  :signed-fullword, fadeout
                                  :address)))
     (terminate-when-unreachable sound #'(lambda(sound) (print sound) (DeleteSound sound)))
     sound))

;................................................................................: MakeRegionSound
(defmacro MakeWriteSound (name sound format)
  `(with-cstrs ((s ,name))
     (let ((sound (ccl::ppc-ff-call (get-fun-addr "MakeWriteSoundPtr" *libaudiostream*) 
                                    :address s
                                    :address ,sound
                                    :signed-fullword, format
                                    :address)))
       (terminate-when-unreachable sound #'(lambda(sound) (print sound) (DeleteSound sound)))
       sound)))

;................................................................................: MakeInputSound
(defmacro MakeInputSound ()
  `(let ((sound (ccl::ppc-ff-call (get-fun-addr "MakeInputSoundPtr" *libaudiostream*) 
                     :address)))
      (terminate-when-unreachable sound #'(lambda(sound) (print sound) (DeleteSound sound)))
       sound))

;................................................................................: MakeRendererSound
(defmacro MakeRendererSound (sound)
  `(let ((sound (ccl::ppc-ff-call (get-fun-addr "MakeRendererSoundPtr" *libaudiostream*) 
                     :address ,sound
                     :address)))
      (terminate-when-unreachable sound #'(lambda(sound) (print sound) (DeleteSound sound)))
       sound))

;................................................................................: GetLengthSound
(defmacro GetLengthSound (sound)
  `(ccl::ppc-ff-call (get-fun-addr "GetLengthSoundPtr" *libaudiostream*) 
                     :address ,sound
                     :signed-fullword))

;................................................................................: GetChannelsSound
(defmacro GetChannelsSound (sound)
  `(ccl::ppc-ff-call (get-fun-addr "GetChannelsSoundPtr" *libaudiostream*) 
                     :address ,sound
                     :signed-fullword))

;................................................................................: ResetSound
(defmacro ResetSound (sound)
  `(ccl::ppc-ff-call (get-fun-addr "ResetSoundPtr" *libaudiostream*) 
                     :address ,sound
                     :void))

;................................................................................: ReadSound
(defmacro ReadSound (sound buffer buffer_size channels)
  `(ccl::ppc-ff-call (get-fun-addr "ReadSoundPtr" *libaudiostream*) 
                     :address ,sound
                     :address ,buffer
                     :signed-fullword, buffer_size
                     :signed-fullword, channels
                     :signed-fullword))

;................................................................................: DeleteSound
(defmacro DeleteSound (sound)
  `(ccl::ppc-ff-call (get-fun-addr "DeleteSoundPtr" *libaudiostream*) 
                     :address ,sound
                     :void))

;................................................................................: OpenAudioPlayer
(defmacro OpenAudioPlayer (inchan outchan channels sr bs sbs rtbs renderer thread_num)
  "Opens a new AUdioEngine"
  `(ccl::ppc-ff-call (get-fun-addr "OpenAudioPlayer" *libaudiostream*) 
                     :signed-fullword, inchan
                     :signed-fullword, outchan
                     :signed-fullword, channels
                     :signed-fullword, sr
                     :signed-fullword, bs
                     :signed-fullword, sbs
                     :signed-fullword, rtbs
                     :signed-fullword, renderer
                     :signed-fullword, thread_num
                     :address))

;;................................................................................: CloseAudioPlayer
(defmacro CloseAudioPlayer (player)
  "Closes a AudioEngine"
  `(ccl::ppc-ff-call (get-fun-addr "CloseAudioPlayer" *libaudiostream*) 
                     :address, player
                     :void))

;; Channels
;................................................................................: LoadChannel
(defmacro LoadChannel (player sound chan vol pan)
  `(ccl::ppc-ff-call (get-fun-addr "LoadChannelPtr" *libaudiostream*) 
                     :address ,player
                     :address ,sound
                     :signed-fullword, chan
                     :double-float, vol
                     :double-float, pan
                     :signed-fullword))

;................................................................................: GetInfoChannel
(defmacro GetInfoChannel (player chan info)
  `(ccl::ppc-ff-call (get-fun-addr "GetInfoChannel" *libaudiostream*) 
                     :address ,player
                     :signed-fullword, chan
                     :address ,info
                     :void))

;; Transport
;................................................................................: StartAudioPlayer
(defmacro StartAudioPlayer (player)
  `(ccl::ppc-ff-call (get-fun-addr "StartAudioPlayer" *libaudiostream*) 
                     :address ,player
                     :void))

;................................................................................: StartAudioPlayer
(defmacro StopAudioPlayer (player)
  `(ccl::ppc-ff-call (get-fun-addr "StopAudioPlayer" *libaudiostream*) 
                     :address ,player
                     :void))

;................................................................................: StartSound
(defmacro StartChannel (player chan)
  `(ccl::ppc-ff-call (get-fun-addr "StartChannel" *libaudiostream*) 
                     :address ,player
                     :signed-fullword, chan
                     :void))

;................................................................................: ContSound
(defmacro ContChannel (player chan)
  `(ccl::ppc-ff-call (get-fun-addr "ContChannel" *libaudiostream*) 
                     :address ,player
                     :signed-fullword, chan
                     :void))

;................................................................................: StopSound
(defmacro StopChannel (player chan)
  `(ccl::ppc-ff-call (get-fun-addr "StopChannel" *libaudiostream*) 
                     :address ,player
                     :signed-fullword, chan
                     :void))

;; Params

;................................................................................: SetVolChannel
(defmacro SetVolChannel (player chan vol)
  `(ccl::ppc-ff-call (get-fun-addr "SetVolChannel" *libaudiostream*) 
                     :address ,player
                     :signed-fullword, chan
                     :double-float, vol
                     :void))

;................................................................................: SetPanChannel
(defmacro SetPanChannel (player chan pan)
  `(ccl::ppc-ff-call (get-fun-addr "SetPanChannel" *libaudiostream*) 
                     :address ,player
                     :signed-fullword, chan
                     :double-float, pan
                     :void))

;................................................................................: SetEffectListChannel
(defmacro SetEffectListChannel (player chan effect_list fadein fadeout)
  `(ccl::ppc-ff-call (get-fun-addr "SetEffectListChannel" *libaudiostream*) 
                     :address ,player
                     :signed-fullword, chan
                     :address, effect_list
                     :signed-fullword, fadein
                     :signed-fullword, fadeout
                     :void))

;; Master

;................................................................................: SetVolAudioPlayer
(defmacro SetVolAudioPlayer (player vol)
  `(ccl::ppc-ff-call (get-fun-addr "SetVolAudioPlayer" *libaudiostream*) 
                     :address ,player
                     :double-float, vol
                     :void))

;................................................................................: SetPanSound
(defmacro SetPanAudioPlayer (player pan)
  `(ccl::ppc-ff-call (get-fun-addr "SetPanAudioPlayer" *libaudiostream*) 
                     :address ,player
                     :double-float, pan
                     :void))

;................................................................................: SetEffectAudioPlayer
(defmacro SetEffectListAudioPlayer (player effect_list fadein fadeout)
  `(ccl::ppc-ff-call (get-fun-addr "SetEffectListAudioPlayer" *libaudiostream*) 
                     :address ,player
                     :address, effect_list
                     :signed-fullword, fadein
                     :signed-fullword, fadeout
                     :void))


;;;========================== EFFECTS 

(defmacro DeleteEffectList (effect_list)
  `(ccl::ppc-ff-call (get-fun-addr "DeleteEffectListPtr" *libaudiostream*) 
                     :address ,effect_list
                     :void))

(defmacro DeleteEffect (effect)
  `(ccl::ppc-ff-call (get-fun-addr "DeleteEffectPtr" *libaudiostream*) 
                     :address ,effect
                     :void))

(defmacro MakeAudioEffectList ()
  `(let ((effect_list (ccl::ppc-ff-call (get-fun-addr "MakeAudioEffectListPtr" *libaudiostream*) 
                                        :address)))
     (terminate-when-unreachable effect_list #'(lambda(effect_list) (print effect_list) (DeleteEffectList effect_list)))
     effect_list))
     
(defmacro AddAudioEffect (effect-list effect)
  `(ccl::ppc-ff-call (get-fun-addr "AddAudioEffectPtr" *libaudiostream*)
                     :address ,effect-list
                     :address ,effect
                     :address))

(defmacro RemoveAudioEffect (effect-list effect)
  `(ccl::ppc-ff-call (get-fun-addr "RemoveAudioEffectPtr" *libaudiostream*)
                     :address ,effect-list
                     :address ,effect
                     :address))

(defmacro MakeVolAudioEffect (gain)
  `(let ((effect (ccl::ppc-ff-call (get-fun-addr "MakeVolAudioEffectPtr" *libaudiostream*) 
                                   :double-float ,gain
                                   :address)))
     (terminate-when-unreachable effect #'(lambda(effect) (print effect) (DeleteEffect effect)))
     effect))

(defmacro MakePanAudioEffect (gain)
  `(let ((effect (ccl::ppc-ff-call (get-fun-addr "MakePanAudioEffectPtr" *libaudiostream*) 
                                   :double-float ,gain
                                   :address)))
     (terminate-when-unreachable effect #'(lambda(effect) (print effect) (DeleteEffect effect)))
     effect))


(defmacro MakeFaustAudioEffect (name)
  `(with-cstrs ((s ,name))
     (let ((effect (ccl::ppc-ff-call (get-fun-addr "MakeFaustAudioEffectPtr" *libaudiostream*) 
                                     :address s
                                     :address)))
       (terminate-when-unreachable effect #'(lambda(effect) (print effect) (DeleteEffect effect)))
       effect)))

(defmacro GetControlCount (effect)
 `(ccl::ppc-ff-call (get-fun-addr "GetControlCountPtr" *libaudiostream*) 
                        :address ,effect
                        :signed-fullword))

(defmacro GetControlParam (effect control)
  `(%stack-block ((name 64) (min 4) (max 4) (init 4))
     (ccl::ppc-ff-call (get-fun-addr "GetControlParamPtr" *libaudiostream*) 
                        :address ,effect
                        :signed-fullword ,control
                        :address name
                        :address min
                        :address max
                        :address init
                        :void)
     (values (%get-cstring name) (%get-single-float min) (%get-single-float max) (%get-single-float init))))

(defmacro SetControlValue (effect control value)
 `(ccl::ppc-ff-call (get-fun-addr "SetControlValuePtr" *libaudiostream*) 
                        :address ,effect
                        :signed-fullword ,control
                        :double-float ,value
                        :void))

(defmacro GetControlValue (effect control)
 `(ccl::ppc-ff-call (get-fun-addr "GetControlValuePtr" *libaudiostream*) 
                        :address ,effect
                        :signed-fullword ,control
                        ::double-float))
