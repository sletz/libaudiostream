(in-package :las)

;(defparameter filename1 "/Users/bouche/Desktop/LAS\ 2/audio/30662__erh__tk-4-40.wav")
;(defparameter filename1 "/Users/bresson/_SHARED-FILES/IN-FILES/SOUNDFILES/Bassclarinet1.aif")
;(defparameter filename2 "/Users/bresson/_SHARED-FILES/IN-FILES/SOUNDFILES/bach-4ch.aif")
(defparameter filename1 "/Users/letz/Music/Sounds/levot.wav")
(defparameter filename2 "/Users/letz/Music/Sounds/tango.wav")




(defvar SR 44100)

(defun getcurdate ()
  (cffi:with-foreign-object (renderer-info '(:struct rendererinfo))
     renderer-info)
    (print (format nil "frame = ~A / usec = ~A / sec = ~A" 
                   (fli::dereference (cffi:foreign-slot-pointer renderer-info '(:struct trendererinfo) 'fCurFrame) :type :unsigned-long)
                   (fli::dereference (cffi:foreign-slot-pointer renderer-info '(:struct trendererinfo) 'fCurUsec) :type :unsigned-long)
                   (/ (fli::dereference (cffi:foreign-slot-pointer renderer-info '(:struct trendererinfo) 'fCurUsec) :type :unsigned-long) 1000000.0)))
    (fli:dereference (cffi:foreign-slot-pointer renderer-info '(:struct trendererinfo) 'fCurFrame) :type :unsigned-long))

(defun getcurdate ()
  (nth 4 (getaudiorendererinfo (getaudioplayerrenderer gaudioplayer))))

(defun test-time ()
  (loop for i from 0 to 10 do 
        (print (getaudiorendererinfo (getaudioplayerrenderer gaudioplayer)))
        (sleep 1)))

;;; (test-time)
;;; on voit bien fCurFrame mais fCurUsec toujours à 0

;; (GetLastLibError)

#|
;;; SEQUENCE DE TEST
;; charge les bindings
(las::libaudiostream-framework)
(defvar gAudioPlayer (openaudioplayer 2 2 SR 512 (* 65536 4) (* SR 60 20) kjackrenderer 1))
(CloseAudioPlayer gAudioPlayer)
;;;kcoreaudiorenderer marche, ou kjackrenderer si serveur Jack démarré

;Démarre le player
(startaudioplayer gaudioplayer)

;(stopaudioplayer gaudioplayer)

;(getdevicecount (getaudioplayerrenderer gaudioplayer)) ;;;renvoie bien 1
;(GetDeviceInfo (getaudioplayerrenderer gaudioplayer) 0)

(probe-file filename1)

(setq s1 (makeregionsound filename1 500 1105))

(setq s1 (makeregionsound filename1 (* SR 2) (* SR 20)))  
(setq s2 (makeregionsound filename2 (* SR 2) (* SR 20)))  


;;; Problème dans la récupération des arguments côté LAS?

(startsound gaudioplayer (makeregionsound filename1 (* SR 2) (* SR 20)) (genrealdate gaudioplayer 0))  
(startsound gaudioplayer (makeregionsound filename2 (* SR 2) (* SR 20)) (genrealdate gaudioplayer 0))  

(startsound gaudioplayer s1 (genrealdate gaudioplayer 0))  
(startsound gaudioplayer s2 (genrealdate gaudioplayer 0))

(resetsound s1)
(resetsound s2)

(stopsound gaudioplayer s1 (genrealdate gaudioplayer 0)) 
(stopsound gaudioplayer s2 (genrealdate gaudioplayer 0)) 

|#

