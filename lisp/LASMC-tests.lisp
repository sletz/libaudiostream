
(in-package :cl-user)


;;;=============================================
;;; LOAD THIS FILE TO GET EVERYTHING READY
;;;=============================================

;;; CHARGER CES 3 FICHIERS
(load (merge-pathnames "FFI/asdf.lisp" *load-pathname*))
(load (merge-pathnames "FFI/load-cffi.lisp" *load-pathname*))
(load (merge-pathnames "LibAudioStream.lisp" *load-pathname*))

;;; METTRE LE BON PATHNAME OU LA LIB AU BON ENDROIT AVANT
;(defparameter *libaudiostream-pathname* 
;  #+win32
;  "/WINDOWS/system32/LibAudioStream.dll"
;  #+(or darwin macos macosx) 
;  "/Library/Frameworks/LibAudioStreamMC.framework/LibAudioStreamMC"
;  #+(or linux (and clisp unix (not macos)))
;  "/usr/lib/libLibAudioStream.so")

(defparameter *libaudiostream-pathname* 
  (probe-file (make-pathname :directory (append (pathname-directory *load-pathname*) '("bin" "LibAudioStreamMC.framework")) 
                             :name "LibAudioStreamMC")))

(defvar *libaudiostream* nil)
(defun load-libaudiostream-framework ()
  (or *libaudiostream*
      (setq *libaudiostream*
            (if (probe-file *libaudiostream-pathname*)
                (progn 
                  (print (concatenate 'string "Loading LibAudioStream library: " (namestring *libaudiostream-pathname*))) 
                  (fli:register-module "LibAudioStream" 
                                       :real-name (namestring *libaudiostream-pathname*)
                                       :connection-style :immediate)
                  t)))))

; (load-libaudiostream-framework)

;;;=============================================
;;; TEST UTILITIES ON TOP OF LAS
;;;=============================================

;; Apply a same volume on all channels
(defun apply-master-volume (sound)
 (let* ((channels (las::GetChannelsSound sound))
        (effect (las::MakeFaustAudioEffect (format nil "process = par(i,~S,_*hslider(\"Volume\",0.5,0,1,0.01));" channels) "" ""))
        (json (las::GetJsonEffect effect))
        (name (las::GetNameEffect effect)))
   (print (list channels name json))
   (las::MakeEffectSound sound effect 100 100)))

;; Apply separated volume on each channel
(defun apply-separated-volume (sound)
 (let* ((channels (las::GetChannelsSound sound))
        (effect (las::MakeFaustAudioEffect (format nil "process = par(i,~S,_*hslider(\"Volume%2i\",0.5,0,1,0.01));" channels) "" ""))
        (json (las::GetJsonEffect effect))
        (name (las::GetNameEffect effect)))
   (print (list channels name json))
   (las::MakeEffectSound sound effect 100 100)))

(defun get-date (player)
  (cffi:with-foreign-object (renderer-info '(:struct las::RendererInfo))
    (las::get-audio-renderer-info (las::GetAudioPlayerRenderer player) renderer-info) 
    (cffi::mem-aref (cffi:foreign-slot-pointer renderer-info '(:struct las::RendererInfo) 'las::fCurFrame) :unsigned-long-long)))

(defun print-date (player)
  (cffi:with-foreign-object (renderer-info '(:struct las::RendererInfo))
    (las::get-audio-renderer-info (las::GetAudioPlayerRenderer player) renderer-info)
    (print (format nil "frame = ~A / usec = ~A / sec = ~A" 
                   (cffi::mem-aref (cffi:foreign-slot-pointer renderer-info '(:struct las::RendererInfo) 'las::fCurFrame) :unsigned-long-long)
                   (cffi::mem-aref (cffi:foreign-slot-pointer renderer-info '(:struct las::RendererInfo) 'las::fCurUsec) :unsigned-long-long)
                   (/ (cffi::mem-aref (cffi:foreign-slot-pointer renderer-info '(:struct las::RendererInfo) 'las::fCurUsec) :unsigned-long-long) 1000000.0)))
   ))

; getaudiorendererinfo should return
; fInput fOutput fSampleRate fBufferSize fCurFrame fCurUsec fOutputLatencyFrame fOutputLatencyUsec fInputLatencyFrame fInputLatencyUsec)
(defun test-time (player)
  (loop for i from 0 to 5 do 
        (print (las::GetAudioRendererInfo (las::GetAudioPlayerRenderer player)))
        (sleep 1)))


;; (GetLastLibError)


;;;=============================================
;;; REAL TESTS
;;;=============================================

#+jb(defparameter filename1 "/Users/bresson/_SHARED-FILES/IN-FILES/SOUNDFILES/Bassclarinet1.aif")
#+jb(defparameter filename2 "/Users/bresson/_SHARED-FILES/IN-FILES/SOUNDFILES/bach-4ch.aiff")
#-jb(defparameter filename1 "/Users/letz/Music/Sounds/levot.wav")
#-jb(defparameter filename2 "/Users/letz/Music/Sounds/tango.wav")
      
(defvar SR 44100)

(defvar *AudioPlayer* nil)

(defun start-sequence ()
  (load-libaudiostream-framework)
  (setf *AudioPlayer* (las::OpenAudioPlayer 2 2 SR 512 (* 65536 4) (* SR 60 20) las::kcoreaudiorenderer 1))
  ;;(setf *AudioPlayer* (las::OpenAudioPlayer 2 2 SR 512 (* 65536 4) (* SR 60 20) las::kjackrenderer 1))
  (las::StartAudioPlayer *AudioPlayer*)
  )


#|
;;; SEQUENCE DE TEST
;; charge les bindings

(start-sequence)

(las::StartAudioPlayer *AudioPlayer*)
(las::StopAudioPlayer *AudioPlayer*)

;(las::StopAudioPlayer *AudioPlayer*)
;(las::CloseAudioPlayer *AudioPlayer*)
;(las::StartAudioPlayer *AudioPlayer*)
;;;kcoreaudiorenderer marche, ou kjackrenderer si serveur Jack démarré


(las::GetAudioRendererInfo (las::GetAudioPlayerRenderer *AudioPlayer*))
(las::GetAudioPlayerDateInUsec *AudioPlayer*)
(las::GetAudioPlayerDateInFrame *AudioPlayer*)

;;; !!!!
(las::GenRealdate *AudioPlayer* 0)

; (get-date *AudioPlayer*)
; (test-time *AudioPlayer*)

(las::GenSymbolicDate *AudioPlayer*)

;(las::GetDeviceCount (las::GetAudioRendererInfo *AudioPlayer*))
;(las::GetDeviceInfo (las::GetAudioRendererInfo *AudioPlayer*) 0)

(probe-file filename1)
(probe-file filename2)

(setq s1 (las::makeregionsound filename1 (* SR 0) (* SR 5)))  
(setq s2 (las::makeregionsound filename2 (* SR 0) (* SR 20)))  

;(las::startsound *AudioPlayer* s1 (las::GenRealDate *AudioPlayer* (las::GetAudioPlayerDateInFrame *AudioPlayer*)))
;(las::startsound *AudioPlayer* s1 (las::GenRealDate *AudioPlayer* 0))


(las::resetsound s1)
(las::resetsound s3)

(las::stopsound *AudioPlayer* s1 (las::genrealdate *AudioPlayer* 0))
(las::stopsound *AudioPlayer* s2 (las::genrealdate *AudioPlayer* 0)) 


(las::GetNameEffect (las::MakeFaustAudioEffect (format nil "process = par(i,~S,_*hslider(\"Volume\",0.5,0,1,0.01));" 1) "" ""))

(setq s3 (apply-master-volume s1))
(setq s4 (apply-separated-volume (las::makeregionsound filename1 (* SR 2) (* SR 20))))


|#

