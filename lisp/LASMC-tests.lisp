(in-package :las)

;(defvar filename1 "/Users/bouche/Desktop/LAS\ 2/audio/30662__erh__tk-4-40.wav")
;(defvar filename1 "/Users/bresson/_SHARED-FILES/IN-FILES/SOUNDFILES/Bassclarinet1.aif")
;(defvar filename2 "/Users/bresson/_SHARED-FILES/IN-FILES/SOUNDFILES/bach-4ch.aif")

(defvar SR 44100)

(defun getcurdate ()
  (cffi:with-foreign-object (renderer-info '(:struct rendererinfo))
     renderer-info)
    (print (format nil "frame = ~A / usec = ~A / sec = ~A" 
                   (fli::dereference (cffi:foreign-slot-pointer renderer-info '(:struct trendererinfo) 'fCurFrame) :type :unsigned-long)
                   (fli::dereference (cffi:foreign-slot-pointer renderer-info '(:struct trendererinfo) 'fCurUsec) :type :unsigned-long)
                   (/ (fli::dereference (cffi:foreign-slot-pointer renderer-info '(:struct trendererinfo) 'fCurUsec) :type :unsigned-long) 1000000.0)))
    (fli:dereference (cffi:foreign-slot-pointer renderer-info '(:struct trendererinfo) 'fCurFrame) :type :unsigned-long)))

(defun getcurdate ()
  (nth 4 (getaudiorendererinfo (getaudioplayerrenderer gaudioplayer))))

(defun test-time ()
  (loop for i from 0 to 10 do 
        (print (getaudiorendererinfo (getaudioplayerrenderer gaudioplayer)))
        (sleep 1)))

;;; (test-time)
;;; on voit bien fCurFrame mais fCurUsec toujours à 0


#|
;;; SEQUENCE DE TEST
;; charge les bindings
(las::libaudiostream-framework)
(defvar gAudioPlayer (openaudioplayer 2 2 SR 512 (* 65536 4) (* SR 60 20) kcoreaudiorenderer 1))
;;;kcoreaudiorenderer marche, ou kjackrenderer si serveur Jack démarré

;Démarre le player
(startaudioplayer gaudioplayer)

;(stopaudioplayer gaudioplayer)

;(getdevicecount (getaudioplayerrenderer gaudioplayer)) ;;;renvoie bien 1
;(GetDeviceInfo (getaudioplayerrenderer gaudioplayer) 0)

(setq s1 (makeregionsound filename1 500 1105))  
;;; Problème dans la récupération des arguments côté LAS?

(startsound gaudioplayer s1 (genrealdate gaudioplayer 0))  ;;;; bus error

|#

