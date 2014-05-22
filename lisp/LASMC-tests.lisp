(in-package :las)

(defvar filename1 "/Users/bouche/Desktop/LAS\ 2/audio/30662__erh__tk-4-40.wav")

(defvar SR 44100)

(defun getcurdate ()
  (cffi:with-foreign-object (renderer-info '(:struct trendererinfo))
    (getaudiorendererinfo (getaudioplayerrenderer gaudioplayer) renderer-info)
    (print (format nil "frame = ~A / usec = ~A / sec = ~A" 
                   (fli::dereference (cffi:foreign-slot-pointer renderer-info '(:struct trendererinfo) 'fCurFrame) :type :unsigned-long)
                   (fli::dereference (cffi:foreign-slot-pointer renderer-info '(:struct trendererinfo) 'fCurUsec) :type :unsigned-long)
                   (/ (fli::dereference (cffi:foreign-slot-pointer renderer-info '(:struct trendererinfo) 'fCurUsec) :type :unsigned-long) 1000000.0)))
    (fli:dereference (cffi:foreign-slot-pointer renderer-info '(:struct trendererinfo) 'fCurFrame) :type :unsigned-long)))

;Alloue un Player avec 4 entrées/sorties, une entrée TR d'au plus 10 min, et le backend JACK
(defvar gAudioPlayer (openaudioplayer 2 2 SR 512 (* 65536 4) (* SR 60 20) kcoreaudiorenderer 1)) ;;;kcoreaudiorenderer marche, ou kjackrenderer si serveur Jack démarré

;Démarre le player
(startaudioplayer gaudioplayer)

(getcurdate)  ;;; on voit bien fCurFrame mais fCurUsec toujours à 0

;(setq s1 (makeregionsound filename1 0 1000))   ;;;Problème dans la récupération des arguments côté LAS

;(startsound gaudioplayer s1 (genrealdate gaudioplayer (getcurdate)))  ;;;;du coup bus error

;(getdevicecount (getaudioplayerrenderer gaudioplayer)) ;;;renvoie bien 1
