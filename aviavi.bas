SCREEN _NEWIMAGE(320, 240, 32)
i = 1
DO
    i = i + .1
    SHELL _HIDE "ffmpeg -y -i drop.avi -vframes 1 -ss " + STR$(i) + " -an -s 320x240 fondo1.jpg"
    i& = _LOADIMAGE("fondo1.jpg", 32)
    _PUTIMAGE , i&
LOOP UNTIL INP(&H60) = 1

