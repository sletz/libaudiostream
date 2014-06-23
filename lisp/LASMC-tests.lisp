

(load (merge-pathnames "FFI/asdf.lisp" *load-pathname*))
(load (merge-pathnames "FFI/load-cffi.lisp" *load-pathname*))
(load (merge-pathnames "LibAudioStream.lisp" *load-pathname*))


(in-package :las)


;(defparameter filename1 "/Users/bouche/Desktop/LAS\ 2/audio/30662__erh__tk-4-40.wav")
;(defparameter filename1 "/Users/bresson/_SHARED-FILES/IN-FILES/SOUNDFILES/Bassclarinet1.aif")
;(defparameter filename2 "/Users/bresson/_SHARED-FILES/IN-FILES/SOUNDFILES/bach-4ch.aiff")
(defparameter filename1 "/Users/letz/Music/Sounds/levot.wav")
(defparameter filename2 "/Users/letz/Music/Sounds/tango.wav")

(defvar SR 44100)

;; Apply a same volume on all channels
(defun apply-master-volume (sound)
 (let* ((channels (getchannelssound sound))
        (effect (MakeFaustAudioEffect (format nil "process = par(i,~S,_*hslider(\"Volume\",0.5,0,1,0.01));" channels) "" ""))
        (json (GetJsonEffect effect))
        (name (GetNameEffect effect)))
   (print (list channels name json))
   (MakeEffectSound sound effect 100 100)))

;; Apply separated volume on each channel
(defun apply-separated-volume (sound)
 (let* ((channels (getchannelssound sound))
        (effect (MakeFaustAudioEffect (format nil "process = par(i,~S,_*hslider(\"Volume%2i\",0.5,0,1,0.01));" channels) "" ""))
        (json (GetJsonEffect effect))
        (name (GetNameEffect effect)))
   (print (list channels name json))
   (MakeEffectSound sound effect 100 100)))


(defun print-date ()
  (cffi:with-foreign-object (renderer-info '(:struct rendererinfo))
    (get-audio-renderer-info (getaudioplayerrenderer gaudioplayer) renderer-info)
    (print (format nil "frame = ~A / usec = ~A / sec = ~A" 
                   (cffi::mem-aref (cffi:foreign-slot-pointer renderer-info '(:struct rendererinfo) 'fCurFrame) :unsigned-long-long)
                   (cffi::mem-aref (cffi:foreign-slot-pointer renderer-info '(:struct rendererinfo) 'fCurUsec) :unsigned-long-long)
                   (/ (cffi::mem-aref (cffi:foreign-slot-pointer renderer-info '(:struct rendererinfo) 'fCurUsec) :unsigned-long-long) 1000000.0)))
   ))

(defun get-date ()
  (cffi:with-foreign-object (renderer-info '(:struct rendererinfo))
    (get-audio-renderer-info (getaudioplayerrenderer gaudioplayer) renderer-info) 
    (cffi::mem-aref (cffi:foreign-slot-pointer renderer-info '(:struct rendererinfo) 'fCurFrame) :unsigned-long-long)))



(defun get-date-from-info ()
  (nth 4 (getaudiorendererinfo (getaudioplayerrenderer gaudioplayer))))


; fInput fOutput fSampleRate fBufferSize fCurFrame fCurUsec fOutputLatencyFrame fOutputLatencyUsec fInputLatencyFrame fInputLatencyUsec)
(defun test-time ()
  (loop for i from 0 to 5 do 
        (print (getaudiorendererinfo (getaudioplayerrenderer gaudioplayer)))
        (sleep 1)))



;; (GetLastLibError)



#|
;;; SEQUENCE DE TEST
;; charge les bindings
(las::libaudiostream-framework)

(defvar gAudioPlayer (openaudioplayer 2 2 SR 512 (* 65536 4) (* SR 60 20) kcoreaudiorenderer 1))

;(CloseAudioPlayer gAudioPlayer)
;;;kcoreaudiorenderer marche, ou kjackrenderer si serveur Jack démarré

;Démarre le player
(startaudioplayer gaudioplayer)


; (get-date)
;;; (test-time)

;(stopaudioplayer gaudioplayer)

;(getdevicecount (getaudioplayerrenderer gaudioplayer)) ;;;renvoie bien 1
;(GetDeviceInfo (getaudioplayerrenderer gaudioplayer) 0)

(probe-file filename1)
(probe-file filename2)

(setq s1 (makeregionsound filename1 (* SR 0) (* SR 5)))  
(setq s2 (makeregionsound filename2 (* SR 0) (* SR 20)))  


;;; Problème dans la récupération des arguments côté LAS?

(startsound gaudioplayer (makeregionsound filename1 (* SR 2) (* SR 20)) (genrealdate gaudioplayer (get-date)))
(startsound gaudioplayer (makeregionsound filename2 (* SR 2) (* SR 20)) (genrealdate gaudioplayer 0))  

(startsound gaudioplayer s1 (genrealdate gaudioplayer (getdatefrominfo)))  
(startsound gaudioplayer s2 (genrealdate gaudioplayer 0))

(resetsound s1)
(resetsound s2)

(stopsound gaudioplayer s1 (genrealdate gaudioplayer 0)) 
(stopsound gaudioplayer s2 (genrealdate gaudioplayer 0)) 


(setq s3 (apply-master-volume (makeregionsound filename1 (* SR 2) (* SR 20))))
(setq s4 (apply-separated-volume (makeregionsound filename1 (* SR 2) (* SR 20))))


|#

