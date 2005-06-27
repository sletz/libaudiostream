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
;; 				Player Data Structures
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
  (fVol :longint)
  (fPan :longint)
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


;; Open/Close

;;; Build sound

;................................................................................: MakeNullSound
(defmacro MakeNullSound (length)
  `(ccl::ppc-ff-call (get-fun-addr "MakeNullSound" *libaudiostream*) 
                     :signed-fullword, length
                     :address))

;................................................................................: MakeReadSound
(defmacro MakeReadSound (name)
  `(with-cstrs ((s ,name))
      (ccl::ppc-ff-call (get-fun-addr "MakeReadSound" *libaudiostream*) 
                        :address s
                        :address)))

;................................................................................: MakeRegionSound
(defmacro MakeRegionSound (name begin end)
  `(with-cstrs ((s ,name))
      (ccl::ppc-ff-call (get-fun-addr "MakeRegionSound" *libaudiostream*) 
                        :address s
                        :signed-fullword, begin
                        :signed-fullword, end
                        :address)))

;................................................................................: MakeFadeSound
(defmacro MakeFadeSound (sound fadein fadeout)
  `(ccl::ppc-ff-call (get-fun-addr "MakeFadeSound" *libaudiostream*) 
                     :address ,sound 
                     :signed-fullword, fadein
                     :signed-fullword, fadeout
                     :address))

;................................................................................: MakeLoopSound
(defmacro MakeLoopSound (sound len)
  `(ccl::ppc-ff-call (get-fun-addr "MakeLoopSound" *libaudiostream*) 
                     :address ,sound 
                     :signed-fullword, len
                     :address))

;................................................................................: MakeCutSound
(defmacro MakeCutSound (sound begin end)
  `(ccl::ppc-ff-call (get-fun-addr "MakeCutSound" *libaudiostream*) 
                        :address ,sound
                        :signed-fullword, begin
                        :signed-fullword, end
                        :address))

;................................................................................: MakeSeqSound
(defmacro MakeSeqSound (s1 s2 crossfade)
  `(ccl::ppc-ff-call (get-fun-addr "MakeSeqSound" *libaudiostream*) 
                        :address ,s1
                        :address ,s2
                        :signed-fullword ,crossfade
                        :address))

;................................................................................: MakeMixSound
(defmacro MakeMixSound (s1 s2)
  `(ccl::ppc-ff-call (get-fun-addr "MakeMixSound" *libaudiostream*) 
                        :address ,s1
                        :address ,s2
                        :address))

;................................................................................: MakeTransformSound
(defmacro MakeTransformSound (sound effect fadein fadeout)
  `(ccl::ppc-ff-call (get-fun-addr "MakeTransformSound" *libaudiostream*) 
                        :address ,sound
                        :address ,effect
                        :signed-fullword, fadein
                        :signed-fullword, fadeout
                        :address))

;................................................................................: MakeRegionSound
(defmacro MakeWriteSound (name sound format)
  `(with-cstrs ((s ,name))
     (ccl::ppc-ff-call (get-fun-addr "MakeWriteSound" *libaudiostream*) 
                       :address s
                       :address ,sound
                       :signed-fullword, format
                       :address)))

;................................................................................: MakeInputSound
(defmacro MakeInputSound ()
  `(ccl::ppc-ff-call (get-fun-addr "MakeInputSound" *libaudiostream*) 
                     :address))

;................................................................................: MakeRendererSound
(defmacro MakeRendererSound (sound)
  `(ccl::ppc-ff-call (get-fun-addr "MakeRendererSound" *libaudiostream*) 
                     :address ,sound
                     :address))

;................................................................................: GetLengthSound
(defmacro GetLengthSound (sound)
  `(ccl::ppc-ff-call (get-fun-addr "GetLengthSound" *libaudiostream*) 
                     :address ,sound
                     :signed-fullword))

;................................................................................: GetChannelsSound
(defmacro GetChannelsSound (sound)
  `(ccl::ppc-ff-call (get-fun-addr "GetChannelsSound" *libaudiostream*) 
                     :address ,sound
                     :signed-fullword))

;................................................................................: ReadSound
(defmacro ReadSound (sound buffer buffer_size channels)
  `(ccl::ppc-ff-call (get-fun-addr "ReadSound" *libaudiostream*) 
                     :address ,sound
                     :address ,buffer
                     :signed-fullword, buffer_size
                     :signed-fullword, channels
                     :signed-fullword))

;................................................................................: DeleteSound
(defmacro DeleteSound (sound)
  `(ccl::ppc-ff-call (get-fun-addr "DeleteSound" *libaudiostream*) 
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
  `(ccl::ppc-ff-call (get-fun-addr "LoadChannel" *libaudiostream*) 
                     :address ,player
                     :address ,sound
                     :signed-fullword, chan
                     :signed-fullword, vol
                     :signed-fullword, pan
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
(defmacro StartSound (player chan)
  `(ccl::ppc-ff-call (get-fun-addr "StartSound" *libaudiostream*) 
                     :address ,player
                     :signed-fullword, chan
                     :void))

;................................................................................: ContSound
(defmacro ContSound (player chan)
  `(ccl::ppc-ff-call (get-fun-addr "ContSound" *libaudiostream*) 
                     :address ,player
                     :signed-fullword, chan
                     :void))

;................................................................................: StopSound
(defmacro StopSound (player chan)
  `(ccl::ppc-ff-call (get-fun-addr "StopSound" *libaudiostream*) 
                     :address ,player
                     :signed-fullword, chan
                     :void))

;; Params

;................................................................................: SetVolSound
(defmacro SetVolSound (player chan vol)
  `(ccl::ppc-ff-call (get-fun-addr "SetVolSound" *libaudiostream*) 
                     :address ,player
                     :signed-fullword, chan
                     :signed-fullword, vol
                     :void))

;................................................................................: SetPanSound
(defmacro SetPanSound (player chan pan)
  `(ccl::ppc-ff-call (get-fun-addr "SetPanSound" *libaudiostream*) 
                     :address ,player
                     :signed-fullword, chan
                     :signed-fullword, pan
                     :void))

;; Master

;................................................................................: SetVolAudioPlayer
(defmacro SetVolAudioPlayer (player vol)
  `(ccl::ppc-ff-call (get-fun-addr "SetVolAudioPlayer" *libaudiostream*) 
                     :address ,player
                     :signed-fullword, vol
                     :void))

;................................................................................: SetPanSound
(defmacro SetPanAudioPlayer (player pan)
  `(ccl::ppc-ff-call (get-fun-addr "SetPanAudioPlayer" *libaudiostream*) 
                     :address ,player
                     :signed-fullword, pan
                     :void))

;;;========================== EFFECTS 

(defmacro MakeAudioEffectList ()
  `(ccl::ppc-ff-call (get-fun-addr "MakeAudioEffectList" *libaudiostream*) 
                     :address))

(defmacro AddAudioEffect (effect-list effect)
  `(ccl::ppc-ff-call (get-fun-addr "AddAudioEffect" *libaudiostream*)
                     :address ,effect-list
                     :address ,effect
                     :address))

(defmacro RemoveAudioEffect (effect-list effect)
  `(ccl::ppc-ff-call (get-fun-addr "RemoveAudioEffect" *libaudiostream*)
                     :address ,effect-list
                     :address ,effect
                     :address))

(defmacro MakeVolAudioEffect (gain)
  `(ccl::ppc-ff-call (get-fun-addr "MakeVolAudioEffect" *libaudiostream*) 
                     :double-float ,gain
                     :address))

(defmacro MakeFaustAudioEffect (name)
  `(with-cstrs ((s ,name))
      (ccl::ppc-ff-call (get-fun-addr "MakeFaustAudioEffect" *libaudiostream*) 
                        :address s
                        :address)))

(defmacro GetControlCount (effect)
 `(ccl::ppc-ff-call  (get-fun-addr "GetControlCount" *libaudiostream*) 
                        :address ,effect
                        :signed-fullword))

(defmacro GetControlParam (effect control)
  `(%stack-block ((name 64) (min 4) (max 4) (init 4))
     (ccl::ppc-ff-call  (get-fun-addr "GetControlParam" *libaudiostream*) 
                        :address ,effect
                        :signed-fullword ,control
                        :address name
                        :address min
                        :address max
                        :address init
                        :void)
     (values (%get-cstring name) (%get-single-float min) (%get-single-float max) (%get-single-float init))))

(defmacro SetControlValue (effect control value)
 `(ccl::ppc-ff-call  (get-fun-addr "SetControlValue" *libaudiostream*) 
                        :address ,effect
                        :signed-fullword ,control
                        :double-float ,value
                        :void))

(defmacro GetControlValue (effect control)
 `(ccl::ppc-ff-call  (get-fun-addr "GetControlValue" *libaudiostream*) 
                        :address ,effect
                        :signed-fullword ,control
                        ::double-float))
