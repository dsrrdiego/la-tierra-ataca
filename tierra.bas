$Debug
On Error GoTo 10
'$CONSOLE

Randomize Timer
_Title "LA TIERRA ATACA"
Const SND_SYNC = 0 'Windows controlled
Const SND_ASYNC = 1 'user controlled
Const SND_NODEFAULT = 2 'only plays sound file requested
Const SND_LOOP = 8 'loops the sound. Use ASYNC also to stop later
Const SND_NOSTOP = &H10 'does not allow a sound to be stopped
Const SND_NOWAIT = &H2000 'will not play sound if driver is busy
Const SND_PURGE = &H40 'stop any sound playing

'DECLARE DYNAMIC LIBRARY "winmm"
'    FUNCTION PlaySound% ALIAS PlaySoundA (lpszName AS STRING, BYVAL hModule AS INTEGER, BYVAL dwFlags AS INTEGER)
'    FUNCTION sndPlaySound% ALIAS PlaySoundA (lpszName AS STRING, BYVAL hModule AS INTEGER, BYVAL dwFlags AS INTEGER)
'END DECLARE

FileName$ = "ando"
Dim Shared synch

synch = SND_ASYNC

Dim Shared VISIBLEPP


Dim Shared resolucionX, resolucionY As Integer, NrodeSoldados As Integer
Dim Shared SombraCursorFase, SombraCursorFasePolo
Dim Shared pantalla1 As Integer, pantalla2 As Integer, a&, num, Zoom As Integer
Dim Shared scrollX, scrollY, Funcion
Dim Shared Seleccionado As Integer
Dim Shared nnx, nny, nnxCursor, nnyCursor
Dim Shared tiroNro
Dim Shared img&(2, 10, 10, 10) 'grupo,nro,direccion,fase
Dim Shared sticker&(10), mapaImg&
Dim Shared fondo&(10), cosa&(100, 4)
Dim Shared Texto&, indiCador&
Dim Shared cursor&(5)
Dim Shared letra&
Dim Shared TableroAncho(15) As Integer
Dim Shared Dimension, piSo, pisoSuplente As Integer
Dim Shared Bando As Integer
Dim Shared pistol&(10)
Dim Shared BARRA&, menugraf&, cosaGrande&(10), cosaE&(10), Cuadrado&
Dim Shared alturadepiSo
Dim Shared DiferenciaDePiso
Dim Shared bajar
Dim Shared TEXTO$
'TEXTO$ = "VISIBLE"
Dim Shared Mensaje$
Dim Shared zzz&, circulo&
Dim Shared DireccionReg(2, 10, 21)
Dim Shared despegue, xxxz, yyyz, naveX, naveY, Mapavisible, Aterrizaje
Dim Shared BarraMuestraConstruccion, ConstruccionSeleccionada


Dim Shared listoEditor, valor, CosaAOcultar
Dim Shared Nivel(10)
Dim Shared HeliPuertoX(15), HeliPuertoY(15), NaveEnElAire

'DIM SHARED im&(10, 10, 10, 10)
Dim Shared tiro1&
'DIM SHARED parche
Type casill
    Fondo As Integer
    Cosa As Integer
    '    cosaGrande AS INTEGER
    fichaTipo As Integer
    fichaNro As Integer
    ParedInvisible As Integer
    OcultaX As Integer
    OcultaY As Integer
    Visible As Integer
    Suelo1 As Integer
    Suelo2 As Integer
    Suelo3 As Integer
    Construccion As Integer
    CosaEspecial As Integer
    'CosaEspecialLista AS INTEGER
End Type
Dim Shared casilla(50, 3, -100 To 100, -100 To 100) As casill 'dimension, piso,x,y
Dim Shared Transporte(-20 To 55, -20 To 55) As casill


Type sol
    x As Single
    y As Single
    Piso As Integer
    Dimension As Integer

    Direccion As Integer
    Fase As Integer
    rumboX As Integer
    rumboY As Integer
    nombre As String * 3
    pasoX As Integer
    pasoY As Integer
    ARMA As Integer
    arma1 As Integer
    arma2 As Integer
    arma3 As Integer
    armaSelec As Integer
    CosaEspecial1 As Integer
    CosaEspecial2 As Integer
    CosaEspecial3 As Integer

    VIDA As Integer
    Nombrereal As String * 20

End Type
Dim Shared ficha(5, 5) As sol


Type tiros
    x1 As Integer

    x2 As Integer
    y1 As Integer
    y2 As Integer
    INSTANCIA As Integer
    Dif As Integer
    Dif2 As Integer
    Xini As Integer
    Yini As Integer
    Xfin As Integer
    Yfin As Integer
    tipo As Integer
    listo As Integer
    BANDO As Integer
    FICHANRO As Integer
End Type

Dim Shared tiro(50) As tiros

Type ARMA
    BALAS As Integer
    TIPO As Integer
End Type
Dim Shared ARMA(19) As ARMA

'TYPE listaCosaGrand
'    x AS INTEGER
'    y AS INTEGER
'    Cosa AS INTEGER
'    visible AS INTEGER

'END TYPE






1
letra& = _LoadFont("otros\ethnocen.ttf", 15, "monospace")
Dimension = 1
'Aterrizaje = 1

piSo = 1
alturadepiSo = 6
Bando = 0
Seleccionado = 1
SombraCursorFasePolo = 1
'resolucionX = 200
'resolucionY = 400
Zoom = 40
pantalla1 = 0: pantalla2 = 1

Funcion = 1




zzeta&(0) = _NewImage(700, 500, 32)
_ScreenMove _Middle
Screen zzeta&(0)
escena 1
Cargar



'_FULLSCREEN
zzeta&(1) = _NewImage(1024, 709, 32)
piSo = ficha(Bando, Seleccionado).Piso
centrar 1
_DisplayOrder _Hardware , _Software ', _HARDWARE

'retval% = PlaySound(FileName$, 0, synch)

Do
    Do While _MouseInput: Loop
    Swap pantalla1, pantalla2
    zzeta&(pantalla1) = _NewImage(1024, 709, 32)
    Screen zzeta&(pantalla1)
    _FreeImage zzeta&(pantalla2)

    raton
    disparos

    nnx = Int(((_MouseX - scrollX) - _MouseY + scrollY) / (Zoom * 2))
    nny = Int(((_MouseY - scrollY) + _MouseX - scrollX) / (Zoom * 2))
    pisoSuplente = piSo
    If nnx < 1 Or nnx > TableroAncho(Dimension) Or nny < 1 Or nny > TableroAncho(Dimension) Then
        nnx = nnxAnt
        nny = nnyAnt
    End If
    If casilla(Dimension, piSo, nnx, nny).Fondo = 0 Then
        pisoSuplente = piSo - 1
    End If

    pantalla
    '   IA
    mover
    teclado
    BARRAA
    Mensaj


    'para el despegue

    If despegue = 1 Then despegar
    If Aterrizaje = 1 Then aterrizar
    '   pantallaAuxEditor


    _Display

Loop
System


10
'IF _ERRORLINE = 351 OR _ERRORLINE = 355 THEN RESUME NEXT
Screen , , 1, 1

Print "error", Err, _ErrorLine

Sleep

Resume Next

'*********************************************************************************************************









Sub mover
    For z = 1 To 5


        Caminar 0, z
        Caminar 1, z

    Next z
End Sub










Sub pantalla
    _PutImage , a&
    ficha(0, 1).Nombrereal = Str$(nnx) + Str$(nny)
    x = _MouseX
    y = _MouseY

    If x < 10 Then scrollX = scrollX + 10
    If x > 1020 Then scrollX = scrollX - 10
    If y < 10 Then scrollY = scrollY + 10
    If y > 690 Then scrollY = scrollY - 10


    For piZo = 1 To piSo
        difdePiso = piZo - 1
        For A = 1 To TableroAncho(Dimension)
            For b = 1 To A
                XX = TableroAncho(Dimension) - A + b
                yy = b
                xxx = (XX + yy) * Zoom + scrollX
                '  yyy = ((yy - XX)) * Zoom + scrollY - ((piZo * -Zoom * alturadepiSo) + (alturadepiSo * Zoom))
                yyy = ((yy - XX) - difdePiso) * Zoom + scrollY
                If casilla(Dimension, piZo, XX, yy).Visible = 1 Then
                IF casilla(Dimension, piZo, XX, yy).Fondo <> 0 THEN_
                 _PUTIMAGE (xxx, yyy - Zoom)-(xxx + Zoom * 2, yyy + Zoom), fondo&(casilla(Dimension, piZo, XX, yy).Fondo)
                IF casilla(Dimension, piZo, XX, yy).Cosa <> 0 THEN _
                _PUTIMAGE (xxx, yyy - Zoom)-(xxx + Zoom * 2, yyy + Zoom), cosa&(casilla(Dimension, piZo, XX, yy).Cosa, cosafase)
                End If

            Next b
        Next A
        For A = TableroAncho(Dimension) To 1 Step -1
            For b = 1 To A - 1
                yy = TableroAncho(Dimension) + 1 - A + b
                XX = b
                xxx = (XX + yy) * Zoom + scrollX
                'yyy = ((yy - XX)) * Zoom + scrollY - ((piZo * -Zoom * alturadepiSo) + (alturadepiSo * Zoom))
                yyy = ((yy - XX) - difdePiso) * Zoom + scrollY
                If casilla(Dimension, piZo, XX, yy).Visible = 1 Then
                    If casilla(Dimension, piZo, XX, yy).Fondo <> 0 Then _PutImage (xxx, yyy - Zoom)-(xxx + Zoom * 2, yyy + Zoom), fondo&(casilla(Dimension, piZo, XX, yy).Fondo)
                    If casilla(Dimension, piZo, XX, yy).Cosa <> 0 Then _PutImage (xxx, yyy - Zoom)-(xxx + Zoom * 2, yyy + Zoom), cosa&(casilla(Dimension, piZo, XX, yy).Cosa, cosafase)
                    '  _PUTIMAGE (xxx, yyy - Zoom)-(xxx + Zoom * 2, yyy + Zoom), VISIBLEPP 'cosa&(casilla(Dimension, piZo, XX, yy).Cosa, cosafase)


                End If

            Next b
        Next A


        For A = 1 To TableroAncho(Dimension)
            For b = 1 To A


                XX = TableroAncho(Dimension) - A + b
                yy = b
                xxx = (XX + yy) * Zoom + scrollX
                'yyy = (yy - XX) * Zoom + scrollY '- ((piZo * -Zoom * alturadepiSo) + (alturadepiSo * Zoom))
                yyy = ((yy - XX) - difdePiso) * Zoom + scrollY
                FICHATip = casilla(Dimension, piZo, XX, yy).Construccion
                CosaEsp = casilla(Dimension, piZo, XX, yy).CosaEspecial
                If FICHATip <> 0 Then _PutImage (xxx - 8 * Zoom, yyy - 9 * Zoom)-(xxx + 13 * Zoom, yyy + 14 * Zoom), cosaGrande&(casilla(Dimension, piZo, XX, yy).Construccion) '
                If CosaEsp <> 0 Then _PutImage (xxx, yyy - Zoom * 2)-(xxx + Zoom * 2, yyy + 0.5 * Zoom), cosaE&(casilla(Dimension, piZo, XX, yy).CosaEspecial) '


            Next b
        Next A

        For A = TableroAncho(Dimension) To 1 Step -1
            For b = 1 To A - 1

                yy = TableroAncho(Dimension) + 1 - A + b
                XX = b
                xxx = (XX + yy) * Zoom + scrollX
                ' yyy = (yy - XX) * Zoom + scrollY '- ((piZo * -Zoom '* alturadepiSo) + (alturadepiSo * Zoom))
                yyy = ((yy - XX) - difdePiso) * Zoom + scrollY
                FICHATip = casilla(Dimension, piZo, XX, yy).Construccion
                CosaEsp = casilla(Dimension, piZo, XX, yy).CosaEspecial
                If FICHATip <> 0 Then _PutImage (xxx - 8 * Zoom, yyy - 9 * Zoom)-(xxx + 13 * Zoom, yyy + 14 * Zoom), cosaGrande&(casilla(Dimension, piZo, XX, yy).Construccion) '
                If CosaEsp <> 0 Then _PutImage (xxx, yyy - Zoom * 2)-(xxx + Zoom * 2, yyy + 0.5 * Zoom), cosaE&(casilla(Dimension, piZo, XX, yy).CosaEspecial) '

            Next b
        Next A

        '  sombra soldado cursor
        xxx = (nnx + nny) * Zoom + scrollX
        yyy = ((nny - nnx) - (pisoSuplente - 1)) * Zoom + scrollY

        If casilla(Dimension, pisoSuplente, nnx, nny).fichaTipo <> 0 Then
            '  FOR q = 1 TO 50
            SombraCursorFase = SombraCursorFase + SombraCursorFasePolo
            If SombraCursorFase = 50 Then SombraCursorFasePolo = -1
            If SombraCursorFase = 0 Then SombraCursorFasePolo = 1

            q = SombraCursorFase
            _PutImage (xxx - Zoom + q, yyy - Zoom + q)-(xxx + Zoom * 3 - q, yyy + 2 * Zoom - q), circulo&
            ' NEXT q
        End If
        '  xxx = (nnx + nny) * Zoom + scrollX
        '  yyy = ((nny - nnx) - (pisoSuplente - 1)) * Zoom + scrollY




        For A = 1 To TableroAncho(Dimension)
            For b = 1 To A


                XX = TableroAncho(Dimension) - A + b
                yy = b
                xxx = (XX + yy) * Zoom + scrollX
                'yyy = (yy - XX) * Zoom + scrollY '- ((piZo * -Zoom * alturadepiSo) + (alturadepiSo * Zoom))
                yyy = ((yy - XX) - difdePiso) * Zoom + scrollY
                FICHATip = casilla(Dimension, piZo, XX, yy).fichaTipo
                If FICHATip <> 0 Then
                    pasoX = ficha(FICHATip - 1, casilla(Dimension, piZo, XX, yy).fichaNro).pasoX
                    pasoY = ficha(FICHATip - 1, casilla(Dimension, piZo, XX, yy).fichaNro).pasoY


                    _PutImage (xxx + pasoX, yyy + pasoY - Zoom * 2)-(xxx + pasoX + Zoom * 2, yyy + pasoY + 0.5 * Zoom), img&(FICHATip, casilla(Dimension, piZo, XX, yy).fichaNro, ficha(FICHATip - 1, casilla(Dimension, piZo, XX, yy).fichaNro).Direccion, ficha(FICHATip - 1, casilla(Dimension, piZo, XX, yy).fichaNro).Fase)
                End If

            Next b
        Next A

        For A = TableroAncho(Dimension) To 1 Step -1
            For b = 1 To A - 1

                yy = TableroAncho(Dimension) + 1 - A + b
                XX = b
                xxx = (XX + yy) * Zoom + scrollX
                ' yyy = (yy - XX) * Zoom + scrollY '- ((piZo * -Zoom '* alturadepiSo) + (alturadepiSo * Zoom))
                yyy = ((yy - XX) - difdePiso) * Zoom + scrollY
                FICHATip = casilla(Dimension, piZo, XX, yy).fichaTipo
                If FICHATip <> 0 Then
                    pasoX = ficha(FICHATip - 1, casilla(Dimension, piZo, XX, yy).fichaNro).pasoX
                    pasoY = ficha(FICHATip - 1, casilla(Dimension, piZo, XX, yy).fichaNro).pasoY


                    _PutImage (xxx + pasoX, yyy + pasoY - Zoom * 2)-(xxx + pasoX + Zoom * 2, yyy + pasoY + 0.5 * Zoom), img&(FICHATip, casilla(Dimension, piZo, XX, yy).fichaNro, ficha(FICHATip - 1, casilla(Dimension, piZo, XX, yy).fichaNro).Direccion, ficha(FICHATip - 1, casilla(Dimension, piZo, XX, yy).fichaNro).Fase)
                End If

            Next b
        Next A
        25
    Next piZo

    'puntero
    xxx = (nnx + nny) * Zoom + scrollX
    yyy = ((nny - nnx) - (pisoSuplente - 1)) * Zoom + scrollY
    'SWAP cursor&(1), cursor&(5)

    _PutImage (xxx, yyy - Zoom)-(xxx + Zoom * 2, yyy + Zoom), cursor&(Funcion)
    'tiro

    For A = 1 To tiroNro
        Select Case tiro(A).tipo
            Case 1

                _MapTriangle (0, 0)-(100, 100)-(200, 50), tiro1& To(tiro(tiroNro).Xini, tiro(tiroNro).Yini - 10)-(tiro(tiroNro).Xini, tiro(tiroNro).Yini + 10)-(tiro(tiroNro).Xfin, tiro(tiroNro).Yfin), tiro2&

            Case 2
                Line (tiro(tiroNro).Xini, tiro(tiroNro).Yini)-(tiro(tiroNro).Xfin, tiro(tiroNro).Yfin), _RGB(1, 4, 255, 1)
                'LINE (tiro(tiroNro).x1, tiro(tiroNro).y1)-(tiro(tiroNro).Xfin, tiro(tiroNro).Yfin), _RGB(1, 4, 255, 1)

        End Select

    Next A


End Sub











Sub raton


    If _MouseButton(1) = -1 Then
        Do While _MouseInput: Loop
        ' barra
        'pasar soldado
        If _MouseY > 606 And _MouseY < 635 And _MouseX > 908 And _MouseX < 1004 Then
            PasarSoldado
            Exit Sub
        End If

        'menu
        If _MouseY > 642 And _MouseY < 664 And _MouseX > 908 And _MouseX < 1004 Then
            menu
            Do: i = _MouseInput: Loop Until Not _MouseButton(1)

            Exit Sub
        End If

        'mapa
        Mapavisible = 1
        If Mapavisible = 1 Then
            If _MouseY > 670 And _MouseY < 709 Then 'a
                If _MouseX > 980 And _MouseX < 1024 Then
                    Mapa

                End If
            End If
        End If

        'armas
        If _MouseY > 635 And _MouseY < 700 Then
            'primer boton
            If _MouseX > 320 And _MouseX < 400 Then
                If BarraMuestraConstruccion = 1 And ConstruccionSeleccionada = 1 Then

                    casilla(Dimension, piSo, naveX, naveY).Visible = 0

                    despegue = 1

                    Exit Sub
                End If
                If BarraMuestraConstruccion = 1 And ConstruccionSeleccionada = 2 Then
                    Funcion = 3


                    Exit Sub
                End If
                If BarraMuestraConstruccion = 1 And ConstruccionSeleccionada = 3 Then
                    '  INPUT "que queres comprar?", zer$
                    Mensaje$ = "comprar"

                    Exit Sub
                End If


                If ficha(Bando, Seleccionado).arma1 <> 0 Then
                    ficha(Bando, Seleccionado).armaSelec = 1
                    ficha(Bando, Seleccionado).ARMA = ficha(Bando, Seleccionado).arma1
                End If
            End If
            '2do boton
            If _MouseX > 420 And _MouseX < 500 Then
                If ficha(Bando, Seleccionado).arma2 <> 0 Then
                    ficha(Bando, Seleccionado).armaSelec = 2
                    ficha(Bando, Seleccionado).ARMA = ficha(Bando, Seleccionado).arma2
                End If
            End If
            If _MouseX > 520 And _MouseX < 600 Then
                If ficha(Bando, Seleccionado).arma3 <> 0 Then
                    ficha(Bando, Seleccionado).armaSelec = 3
                    ficha(Bando, Seleccionado).ARMA = ficha(Bando, Seleccionado).arma3
                End If
            End If
            'suelo barra
            If _MouseX > 620 And _MouseX < 700 Then


                If casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo1 <> 0 Or casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).CosaEspecial <> 0 Then
                    'EEP
                    AgarrarArmas 1

                End If
            End If
            '3er boton
            If _MouseX > 720 And _MouseX < 800 Then

                If casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo2 <> 0 Then
                    AgarrarArmas 2
                End If
            End If
            If _MouseX > 820 And _MouseX < 900 Then


                If casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo3 <> 0 Then
                    AgarrarArmas 3
                End If
            End If

            Do: i = _MouseInput: Loop Until Not _MouseButton(1)
            Exit Sub

        End If


        '  tablero
        Select Case Funcion

            Case 0
                If casilla(Dimension, pisoSuplente, nnx, nny).fichaTipo = 1 Then
                    Funcion = 1 ' :
                    Seleccionado = casilla(Dimension, pisoSuplente, nnx, nny).fichaNro
                    Bando = casilla(Dimension, pisoSuplente, nnx, nny).fichaTipo - 1
                    piSo = ficha(Bando, Seleccionado).Piso
                End If
                If casilla(Dimension, pisoSuplente, nnx, nny).fichaTipo = 2 Then
                    Funcion = 0 ' :
                    Seleccionado = casilla(Dimension, pisoSuplente, nnx, nny).fichaNro
                    Bando = casilla(Dimension, pisoSuplente, nnx, nny).fichaTipo - 1
                    'piSo = ficha(Bando, Seleccionado).Piso
                End If

                Do: i = _MouseInput: Loop Until Not _MouseButton(1)

            Case 1 'mover

                If casilla(Dimension, pisoSuplente, nnx, nny).fichaTipo = 1 Then
                    Funcion = 1 ' :
                    Seleccionado = casilla(Dimension, pisoSuplente, nnx, nny).fichaNro
                    Bando = casilla(Dimension, pisoSuplente, nnx, nny).fichaTipo - 1
                    piSo = ficha(Bando, Seleccionado).Piso
                    Do: i = _MouseInput: Loop Until Not _MouseButton(1)
                    Exit Sub
                End If
                If casilla(Dimension, pisoSuplente, nnx, nny).fichaTipo = 2 Then
                    Funcion = 0 ' :
                    'cursorNro = 1
                    '                casilla(dimension,piso,nnx, nny).fichaTipo = 0:
                    Seleccionado = casilla(Dimension, pisoSuplente, nnx, nny).fichaNro
                    Bando = casilla(Dimension, pisoSuplente, nnx, nny).fichaTipo - 1
                    'piSo = ficha(Bando, Seleccionado).Piso
                    Do: i = _MouseInput: Loop Until Not _MouseButton(1)
                    Exit Sub
                End If
                'IF ficha(0, Seleccionado).Piso <> piSo THEN EXIT SUB
                ficha(0, Seleccionado).rumboX = nnx
                ficha(0, Seleccionado).rumboY = nny
                ' retval% = PlaySound("snds\ahivoy.wav", 0, synch)
                For zz = 0 To 14
                    DireccionReg(Bando, Seleccionado, zz) = 0
                Next zz

                Do: i = _MouseInput: Loop Until Not _MouseButton(1)


            Case 3
                casilla(Dimension, piSo, nnx, nny).Construccion = 3
                casilla(Dimension, piSo, nnx, nny).Visible = 1
                Funcion = 1

        End Select
    End If

    'dejar cosas
    If _MouseButton(2) = -1 Then
        If _MouseY > 635 And _MouseY < 700 Then 'armas
            If _MouseX > 320 And _MouseX < 400 Then

                If ficha(Bando, Seleccionado).arma1 <> 0 Or ficha(Bando, Seleccionado).CosaEspecial1 <> 0 Then
                    DejarArma 1
                End If
            End If
            If _MouseX > 420 And _MouseX < 500 Then
                If ficha(Bando, Seleccionado).arma2 <> 0 Or ficha(Bando, Seleccionado).CosaEspecial2 <> 0 Then
                    DejarArma 2
                End If
            End If
            If _MouseX > 520 And _MouseX < 600 Then
                If ficha(Bando, Seleccionado).arma3 <> 0 Or ficha(Bando, Seleccionado).CosaEspecial3 <> 0 Then
                    DejarArma 3
                End If
            End If

            Do: i = _MouseInput: Loop Until Not _MouseButton(1)
            Exit Sub

        End If

        'disparar
        If Funcion = 1 Then
            DISPARAR 0, Seleccionado, _MouseX, _MouseY
            Do: i = _MouseInput: Loop Until Not _MouseButton(2)
        End If


    End If


End Sub


























Sub Cargar
    NrodeSoldados = 2

    ficha(0, 1).nombre = "uno"
    ficha(0, 1).Nombrereal = "Santi el prelutunipo"

    ficha(0, 1).x = 10
    ficha(0, 1).y = 10
    ficha(0, 1).rumboX = 10
    ficha(0, 1).rumboY = 11
    ficha(0, 1).Direccion = 0
    ficha(0, 1).ARMA = 2
    'ficha(0, 1).arma1 = 1
    ficha(0, 1).arma2 = 2
    ficha(0, 1).armaSelec = 2
    ficha(0, 1).VIDA = 10
    ficha(0, 1).Piso = 1
    ficha(0, 1).Dimension = 1

    ficha(0, 2).nombre = "dos"
    ficha(0, 2).Nombrereal = "el truquinos quecosati el prelutunipo"
    ficha(0, 2).x = 30
    ficha(0, 2).y = 30
    ficha(0, 2).rumboX = 30
    ficha(0, 2).rumboY = 31
    ficha(0, 2).ARMA = 3
    ficha(0, 2).arma1 = 3
    ficha(0, 2).arma3 = 4
    ficha(0, 2).armaSelec = 1
    ficha(0, 2).VIDA = 2
    ficha(0, 2).Piso = 1
    ficha(0, 2).Dimension = 1




    ficha(1, 1).nombre = "uno2"
    ficha(1, 1).Nombrereal = "enemigo fatsl"
    ficha(1, 1).x = 15
    ficha(1, 1).y = 20
    ficha(1, 1).Direccion = 0
    ficha(1, 1).VIDA = 3
    ficha(1, 1).ARMA = 5
    ficha(1, 1).arma1 = 5
    ficha(1, 1).arma3 = 6

    ficha(1, 1).Piso = 1
    ficha(1, 1).Dimension = 1


    ficha(1, 2).nombre = "uno2"
    ficha(1, 2).Nombrereal = "no sera patanto"
    ficha(1, 2).x = 17
    ficha(1, 2).y = 30
    ficha(1, 2).VIDA = 0
    ficha(1, 2).ARMA = 6
    ficha(1, 2).arma1 = 5
    'ficha(1, 2).arma2 = 6
    ficha(1, 2).arma3 = 4
    ficha(1, 2).Piso = 1
    ficha(1, 2).Dimension = 1


    ARMA(1).TIPO = 1
    ARMA(1).BALAS = 50
    ARMA(2).TIPO = 1
    ARMA(2).BALAS = 20
    ARMA(3).TIPO = 1
    ARMA(3).BALAS = 25
    ARMA(4).TIPO = 2
    ARMA(4).BALAS = 4
    ARMA(5).TIPO = 2
    ARMA(5).BALAS = 5
    ARMA(6).TIPO = 2
    ARMA(6).BALAS = 6

    _Font _LoadFont("otros\ethnocen.ttf", 20, "monospace")
    'COLOR _RGB(255, 0, 0, 0), _RGB(0, 255, 0, 0)


    Print "CARGANDO "

    I = _LoadImage("otros\fondo.jpg", 32)
    a& = _CopyImage(I, 33)
    I = _LoadImage("otros\mapa.jpg", 32)
    _Source I

    _SetAlpha 110, , I

    mapaImg& = _CopyImage(I, 32)
    'GOTO saltear
    For nro = 1 To NrodeSoldados 'nrodesoldados
        For direccion = 0 To 8
            For fase = 0 To 3

                arch$ = "solds\" + ficha(0, nro).nombre$ + "\" + LTrim$(Str$(0)) + LTrim$(Str$(0)) + LTrim$(Str$(nro - 1)) + LTrim$(Str$(direccion)) + LTrim$(Str$(fase)) + ".jpg"

                I = _LoadImage(arch$, 32)

                _Source I
                clr~& = Point(1, 1)

                _ClearColor clr~&, I
                img&(1, nro, direccion, fase) = _CopyImage(I, 33)
                Progreso
            Next fase
        Next direccion
    Next nro
    ''''provisorio enemigos
    For nro = 1 To NrodeSoldados

        For direccion = 0 To 8 'STEP 3
            For fase = 0 To 3

                'arch$ = ficha(0,1).nombre$ + LTRIM$(STR$(0)) + LTRIM$(STR$(0)) + LTRIM$(STR$(nro - 1)) + LTRIM$(STR$(direccion)) + LTRIM$(STR$(fase)) + ".jpg"

                img&(2, nro, direccion, fase) = img&(1, nro, direccion, fase)
                Progreso
            Next fase
        Next direccion
    Next nro
    saltear:

    For n = 1 To 6
        arch$ = "fondo\fondo" + LTrim$(Str$(n)) + ".jpg"

        I = _LoadImage(arch$, 32)
        _Source I
        '    _PUTIMAGE , I ' fondo&(n)
        clr~& = Point(1, 1) 'find background color of image
        'SLEEP 1 'STOP
        _ClearColor clr~&, I 'fondo&(n) '& 'set background color as transparent
        fondo&(n) = _CopyImage(I, 33)
        Progreso

    Next n
    'VISIBLEPP = _COPYIMAGE(I, 32)
    _Source I 'VISIBLEPP
    _SetAlpha 180, , I 'VISIBLEPP 'affects all colors below bright white
    VISIBLEPP = _CopyImage(I, 33)


    I = _LoadImage("otros\tiro1.jpg", 32)
    '_PUTIMAGE (0, 0)-(100, 100), I ' fondo&(n)
    _Source I
    clr~& = Point(1, 1) 'find background color of image
    'SLEEP 1 'STOP
    _ClearColor clr~&, I 'fondo&(n) '& 'set background color as transparent
    tiro1& = _CopyImage(I, 33)
    'SLEEP
    Progreso
    cero = 0
    For n = 1 To 4
        arch$ = "cosa\cosa" + LTrim$(Str$(n)) + LTrim$(Str$(cero)) + ".jpg"
        I = _LoadImage(arch$, 32)
        ' _PUTIMAGE , I 'cosa&(n, 0)
        _Source I
        clr~& = Point(100, 10) 'find background color of image
        'SLEEP 1 'STOP
        _ClearColor clr~&, I 'cosa&(n, 0) '& 'set background color as transparent
        cosa&(n, 0) = _CopyImage(I, 33)
        Progreso
    Next n


    I = _LoadImage("otros\BARRA.jpg", 32)
    '_PUTIMAGE , I 'BARRA&
    _Source I
    clr~& = Point(1, 1) 'find background color of image
    'SLEEP 1 'STOP
    _ClearColor clr~&, I 'BARRA& '& 'set background color as transparent
    parche = _CopyImage(I, 32)
    BARRA& = _CopyImage(I, 33)
    Progreso

    I = _LoadImage("otros\indicador.jpg", 32)
    indiCador& = _CopyImage(I, 33)
    Progreso

    I = _LoadImage("otros\textobarra.jpg", 32)
    '_PUTIMAGE , I 'Texto&
    _Source I
    clr~& = Point(2, 20) 'find background color of image
    'SLEEP 1 'STOP
    _ClearColor clr~&, I 'Texto& '& 'set background color as transparent
    Texto& = _CopyImage(I, 33)
    Progreso

    I = _LoadImage("otros\cuadrado.jpg", 32)
    '_PUTIMAGE , I 'Texto&
    _Source I
    clr~& = Point(100, 100) 'find background color of image
    'SLEEP 1 'STOP
    _ClearColor clr~&, I 'Texto& '& 'set background color as transparent
    Cuadrado& = _CopyImage(I, 33)
    Progreso

    For n = 1 To 2
        arch$ = "otros\pistol" + LTrim$(Str$(n)) + ".jpg"
        I = _LoadImage(arch$, 32)
        '_PUTIMAGE , I 'pistol&(n)
        _Source I
        clr~& = Point(1, 1) 'find background color of image
        'SLEEP 1 'STOP
        _ClearColor clr~&, I 'pistol&(n) '& 'set background color as transparent
        pistol&(n) = _CopyImage(I, 33)
        Progreso
    Next n

    For n = 0 To 1
        arch$ = "otros\cursor" + LTrim$(Str$(n)) + ".jpg"
        I = _LoadImage(arch$, 32)
        _Source I 'cursor&(n)
        clr~& = Point(1, 1) 'find background color of image
        'SLEEP 1 'STOP
        _ClearColor clr~&, I 'cursor&(n) '& 'set background color as transparent
        cursor&(n) = _CopyImage(I, 33)
        Progreso

    Next n

    cursor&(3) = cursor&(0)
    cursor&(2) = cursor&(0)
    I = _LoadImage("otros\circulo.jpg", 32)
    _Source I

    '_CLEARCOLOR clr~&, I
    topclr~& = clr~& - _RGBA(1, 1, 1, 0) 'get topmost color range just below

    _SetAlpha 90, , I 'affects all colors below bright white
    clr~& = Point(10, 10)
    _SetAlpha 0, clr~&, I 'affects all colors below bright white

    '_SETALPHA 90, , I

    circulo& = _CopyImage(I, 33)
    Progreso

    'I = _LOADIMAGE("otros\cursor12.jpg", 32)

    '_PUTIMAGE , I 'cursor&(5)
    'SLEEP

    'clr~& = POINT(1, 1) 'find background color of image
    'SLEEP 1 'STOP
    '_CLEARCOLOR clr~&, I 'cursor&(5) '& 'set background color as transparent
    'SLEEP
    'cursor&(5) = _COPYIMAGE(I, 33)

    I = _LoadImage("otros\menu.jpg", 32)
    '_PUTIMAGE , I 'menugraf&
    _Source I
    clr~& = Point(1, 1) 'find background color of image
    'SLEEP 1 'STOP
    _ClearColor clr~&, I 'menugraf& '& 'set background color as transparent
    menugraf& = _CopyImage(I, 33)
    Progreso


    For n = 1 To 4
        arch$ = "construccion\cosag" + LTrim$(Str$(n)) + ".jpg"
        I = _LoadImage(arch$, 32)
        '_PUTIMAGE , I 'cosaGrande&(n)
        _Source I
        clr~& = Point(1, 1) 'find background color of image
        'SLEEP 1 'STOP
        _ClearColor clr~&, I 'cosaGrande&(n) '& 'set background color as transparent
        cosaGrande&(n) = _CopyImage(I, 33)
        Progreso
    Next n
    For n = 1 To 2
        arch$ = "cosae\cosae" + LTrim$(Str$(n)) + ".jpg"
        I = _LoadImage(arch$, 32)
        '    _PUTIMAGE , I 'cosaE&(n)
        _Source I
        clr~& = Point(1, 1) 'find background color of image
        'SLEEP 1 'STOP
        _ClearColor clr~&, I 'osaE&(n) '& 'set background color as transparent
        cosaE&(n) = _CopyImage(I, 33)
        Progreso
    Next n
    For n = 1 To 3
        arch$ = "stick\stick" + LTrim$(Str$(n)) + ".jpg"
        I = _LoadImage(arch$, 32)
        _Source I
        '   _PUTIMAGE , I
        clr~& = Point(1, 1)

        _ClearColor clr~&, I
        sticker&(n) = _CopyImage(I, 33)
        Progreso

    Next n


    'FOR a = 1 TO TableroAncho(Dimension)
    '    FOR b = 1 TO TableroAncho(Dimension)
    '        IF casilla(Dimension, piSo, a, b).fichaTipo = 0 THEN casilla(Dimension, piSo, a, b).fichaNro = 0
    '
    '        casilla(Dimension, piSo, a, b).fichaTipo = 0
    '    NEXT b
    'NEXT a


    For z = 1 To NrodeSoldados
        '   casilla(dimension,piso,
        casilla(ficha(0, z).Dimension, ficha(0, z).Piso, ficha(0, z).x, ficha(0, z).y).fichaTipo = 1
        casilla(ficha(0, z).Dimension, ficha(0, z).Piso, ficha(0, z).x, ficha(0, z).y).fichaNro = z
        casilla(ficha(1, z).Dimension, ficha(1, z).Piso, ficha(1, z).x, ficha(1, z).y).fichaTipo = 2
        casilla(ficha(1, z).Dimension, ficha(1, z).Piso, ficha(1, z).x, ficha(1, z).y).fichaNro = z

    Next z


    _FreeImage I
End Sub



















Sub escena (n As Integer)
    Nivel(n) = 1
    TableroAncho(n) = 40

    unTablero = TableroAncho(n) * TableroAncho(n)
    arch$ = "escena\escena.00" + LTrim$(Str$(n))
    Open arch$ For Random As #1
    For a = 1 To TableroAncho(n)
        For b = 1 To TableroAncho(n)

            Get #1, (a * TableroAncho(n)) + b, casilla(n, 1, a, b).Fondo
            Get #1, (TableroAncho(n) * TableroAncho(n)) + (a * TableroAncho(n)) + b, casilla(n, 1, a, b).Cosa
            Get #1, unTablero * 2 + (a * TableroAncho(n)) + b, casilla(n, 2, a, b).Fondo
            Get #1, unTablero * 3 + (a * TableroAncho(n)) + b, casilla(n, 2, a, b).Cosa
            Get #1, unTablero * 4 + (a * TableroAncho(n)) + b, casilla(n, 1, a, b).ParedInvisible
            Get #1, unTablero * 5 + (a * TableroAncho(n)) + b, casilla(n, 1, a, b).OcultaX
            Get #1, unTablero * 6 + (a * TableroAncho(n)) + b, casilla(n, 1, a, b).OcultaY

            Get #1, unTablero * 7 + (a * TableroAncho(n)) + b, casilla(n, 1, a, b).Construccion
            If casilla(n, 1, a, b).Construccion = 4 Then HeliPuertoX(n) = a: HeliPuertoY(n) = b

            Get #1, unTablero * 8 + (a * TableroAncho(n)) + b, casilla(n, 1, a, b).CosaEspecial

        Next b
    Next a
    Get #1, unTablero * 9 + TableroAncho(1), X '
    Get #1, unTablero * 9 + TableroAncho(1) + 1, Y '
    If X <> 0 Then naveX = X
    If Y <> 0 Then naveY = Y
    'HeliPuertoX(1) = 10
    'HeliPuertoY(1) = 10




    Close #1


End Sub









Sub teclado
    If TEXTO$ = UCase$("VISIBLE") Then GoTo VER
    Select Case InKey$
        Case Chr$(27)
            'SYSTEM
            Stop

        Case "+"
            Zoom = Zoom + 1
            centrar 1

        Case "-"
            Zoom = Zoom - 1
            centrar 1
        Case "v"
            _Dest _Console
            Print ficha(0, 2).Dimension
            Print ficha(0, 2).x
            Print ficha(0, 2).y
            Print ficha(0, 2).Piso
            For y = -5 To 8
                For x = -5 To 6
                    'LOCATE y + 6, x + 6: PRINT Transporte(x, y).fichaNro
                    If Transporte(x, y).fichaNro <> 0 Then Print "hay"

                Next x
            Next y
            Print Transporte(0, 0).fichaNro
            _Dest 0





        Case Chr$(13)
            _PrintMode _KeepBackground
            _Font letra& '_LOADFONT("ethnocen.ttf", 15, "monospace")
            Color _RGB(255, 0, 0, 0), _RGB(0, 0, 255, 0)
            _PrintString (10, 10), ">>>"
            Do
                x$ = InKey$
                pantalla
                If x$ = Chr$(13) Then Exit Do
                If x$ <> "" Then
                    a = a + 20
                    TEXTO$ = TEXTO$ + x$
                End If
                _PrintString (60 + a, 10), x$
                _Display
            Loop
            Select Case UCase$(TEXTO$)
                Case "VISIBLE"
                    VER:

                    For YYY = 1 To 100
                        For XXX = 1 To 100

                            casilla(Dimension, piSo, XXX, YYY).Visible = 1
                        Next XXX
                    Next YYY
                    'OR a = 1 TO 10
                    '   ListaCosaGrande(a).visible = 1
                    'NEXT a

                Case "NADA"


                    For YYY = 1 To 100
                        For XXX = 1 To 100

                            casilla(Dimension, piSo, XXX, YYY).Fondo = 0

                        Next XXX
                    Next YYY

                Case "EDITOR"
                    editor
            End Select
            TEXTO$ = ""
        Case "X"
            centrar 1
        Case "Q"
            Dimension = Dimension + 1
            originaL = Seleccionado
            If NrodeSoldados = 1 Then Exit Sub
            otravez:
            Seleccionado = Seleccionado + 1
            If Seleccionado = NrodeSoldados + 1 Then Seleccionado = 1

            If ficha(Bando, Seleccionado).Dimension <> Dimension Then If Seleccionado = originaL Then Exit Sub Else GoTo otravez
            Funcion = 1
            Bando = 0
            centrar Seleccionado

        Case "A"

            Dimension = Dimension - 1
            originaL = Seleccionado
            If NrodeSoldados = 1 Then Exit Sub
            otravez2:
            Seleccionado = Seleccionado + 1
            If Seleccionado = NrodeSoldados + 1 Then Seleccionado = 1

            If ficha(Bando, Seleccionado).Dimension <> Dimension Then If Seleccionado = originaL Then Exit Sub Else GoTo otravez2
            Funcion = 1
            Bando = 0
            centrar Seleccionado

        Case "P"
            piSo = piSo + 1

    End Select
    If piSo = 0 Or piSo = 3 Then piSo = 1

End Sub

Sub centrar (nro As Integer)
    'DiferenciaDePiso = ((piSo * -Zoom * alturadepiSo) + (alturadepiSo * Zoom))
    xxx = (ficha(0, nro).x + ficha(0, nro).y) * Zoom '- DiferenciaDePiso
    yyy = (ficha(0, nro).y - ficha(0, nro).x) * Zoom ' - DiferenciaDePiso

    scrollX = -(xxx - 500)
    scrollY = -(yyy - 350)
    piSo = ficha(0, nro).Piso


End Sub









Sub Caminar (bando As Integer, z As Integer)

    paso = 2.5
    rumboy = Int(ficha(bando, z).rumboY)
    rumboX = Int(ficha(bando, z).rumboX)
    SoldPiso = ficha(bando, z).Piso
    SoldDim = ficha(bando, z).Dimension
    Y = Int(ficha(bando, z).y)
    X = Int(ficha(bando, z).x)
    CasillXX = X
    CasillYY = Y
    If rumboX <> 0 Or rumboy <> 0 Then
        'visibilidad
        For YYY = -4 To 4
            For XXX = -4 To 4

                XXXX = Int(ficha(bando, z).x) + XXX
                YYYY = Int(ficha(bando, z).y) + YYY
                If XXXX >= TableroAncho(SoldDim) Or XXXX <= 0 Or YYYY >= TableroAncho(SoldDim) Or YYYY <= 1 Then GoTo prox

                If XXXX <= 0 Or YYYY <= 0 Or XXXX >= 100 Or YYYY >= 100 Then GoTo prox
                casilla(SoldDim, SoldPiso, XXXX, YYYY).Visible = 1
                prox:
            Next XXX
        Next YYY

        'registro de direcciones
        If DireccionReg(bando, z, 1) <> 0 Then
            For a = 0 To 20 ' STEP -1
                DireccionReg(bando, z, a) = DireccionReg(bando, z, a + 1)
            Next a
            GoTo 2
        End If

        ' IF casilla(Dimension, piSo, rumboX, rumboy).Fondo = 0 THEN EXIT SUB
        If rumboy = Y And rumboX = X Then DireccionReg(bando, z, 0) = 0
        If rumboy < Y And rumboX = X Then DireccionReg(bando, z, 0) = 1
        If rumboy < Y And rumboX > X Then DireccionReg(bando, z, 0) = 2
        If rumboy = Y And rumboX > X Then DireccionReg(bando, z, 0) = 3
        If rumboy > Y And rumboX > X Then DireccionReg(bando, z, 0) = 4
        If rumboy > Y And rumboX = X Then DireccionReg(bando, z, 0) = 5
        If rumboy > Y And rumboX < X Then DireccionReg(bando, z, 0) = 6
        If rumboy = Y And rumboX < X Then DireccionReg(bando, z, 0) = 7
        If rumboy < Y And rumboX < X Then DireccionReg(bando, z, 0) = 8

        'previsor
        2
        If DireccionReg(bando, z, 0) > 8 Then DireccionReg(bando, z, 0) = DireccionReg(bando, z, 0) - 8
        Select Case DireccionReg(bando, z, 0)
            Case 0
                GoTo fuera
            Case 1
                CasillXX = ficha(bando, z).x
                CasillYY = ficha(bando, z).y - 1
            Case 2
                CasillXX = ficha(bando, z).x + 1
                CasillYY = ficha(bando, z).y - 1
            Case 3
                CasillXX = ficha(bando, z).x + 1
                CasillYY = ficha(bando, z).y
            Case 4
                CasillXX = ficha(bando, z).x + 1
                CasillYY = ficha(bando, z).y + 1
            Case 5
                CasillXX = ficha(bando, z).x
                CasillYY = ficha(bando, z).y + 1
            Case 6
                CasillXX = ficha(bando, z).x - 1
                CasillYY = ficha(bando, z).y + 1

            Case 7
                CasillXX = ficha(bando, z).x - 1
                CasillYY = ficha(bando, z).y
                If casilla(Dimension, SoldPiso, CasillXX, CasillYY).Cosa = 4 Then GoTo fuera
            Case 8
                CasillXX = ficha(bando, z).x - 1
                CasillYY = ficha(bando, z).y - 1
        End Select
        If casilla(SoldDim, SoldPiso, CasillXX, CasillYY).Fondo = 0 Then Exit Sub

        'estara ocupado el camino
    IF casilla(SoldDim, SoldPiso, CasillXX, CasillYY).fichaTipo <> 0_
     OR casilla(SoldDim, SoldPiso, CasillXX, CasillYY).Cosa <> 0_
      OR casilla(SoldDim, piSo, CasillXX, CasillYY).ParedInvisible = 1 THEN

            For a = 20 To 1 Step -1
                DireccionReg(bando, z, a) = DireccionReg(bando, z, a - 1)
            Next a
            DireccionReg(bando, z, 0) = DireccionReg(bando, z, 0) + 1
            If DireccionReg(bando, z, 14) <> 0 Then
                DireccionReg(bando, z, 14) = 0
                For a = 20 To 1 Step -1
                    DireccionReg(bando, z, a) = DireccionReg(bando, z, a + 1)
                Next a
                ficha(bando, z).Direccion = 0
            End If

            GoTo 2 'vuelve a previsor
        End If

        fuera:
        ficha(bando, z).Direccion = DireccionReg(bando, z, 0)

        Select Case ficha(bando, z).Direccion
            Case 0
                ficha(bando, z).rumboX = 0
                ficha(bando, z).rumboY = 0
                ficha(bando, z).Fase = 0
                Exit Sub

            Case 1 '           ficha(0,z).Direccion = 1
                pasoY = ficha(bando, z).Fase * Zoom / paso * -1
                pasoX = ficha(bando, z).Fase * Zoom / paso * -1

            Case 2 '          ficha(0,z).Direccion = 2
                pasoY = ficha(bando, z).Fase * Zoom / paso * -1

            Case 3
                pasoY = ficha(bando, z).Fase * Zoom / paso * -1
                pasoX = ficha(bando, z).Fase * Zoom / paso
            Case 4 '        ficha(bando,z).Direccion = 4
                pasoX = ficha(bando, z).Fase * Zoom / paso
                If bajar = 1 Then
                    pasoY = 2 * Zoom + Zoom / 2 * ficha(bando, z).Fase

                End If
            Case 5 '       ficha(bando,z).Direccion = 5
                pasoY = ficha(bando, z).Fase * Zoom / paso
                pasoX = ficha(bando, z).Fase * Zoom / paso
            Case 6 '      ficha(bando,z).Direccion = 6
                pasoY = ficha(bando, z).Fase * Zoom / paso
            Case 7 '     ficha(bando,z).Direccion = 7
                pasoY = ficha(bando, z).Fase * Zoom / paso * 1
                pasoX = ficha(bando, z).Fase * Zoom / paso * -1
                If casilla(SoldDim, piSo, CasillXX, CasillYY).Cosa = 4 Then 'escalera
                    pasoY = pasoY - Zoom / 2 * ficha(bando, z).Fase
                    subir = 1
                End If
            Case 8 '    ficha(bando,z).Direccion = 8

                pasoX = ficha(bando, z).Fase * Zoom / paso * -1
        End Select

        ficha(bando, z).pasoX = pasoX

        ficha(bando, z).pasoY = pasoY

        ficha(bando, z).Fase = ficha(bando, z).Fase + 1
        'SLEEP

        If ficha(bando, z).Fase = 4 Then
            bajar = 0
            ficha(bando, z).pasoX = 0
            ficha(bando, z).pasoY = 0
            casilla(SoldDim, SoldPiso, ficha(bando, z).x, ficha(bando, z).y).fichaTipo = 0
            casilla(SoldDim, SoldPiso, ficha(bando, z).x, ficha(bando, z).y).fichaNro = 0


            Select Case ficha(bando, z).Direccion
                Case 1

                    ficha(bando, z).y = ficha(bando, z).y - 1
                Case 2
                    ficha(bando, z).x = ficha(bando, z).x + 1
                    ficha(bando, z).y = ficha(bando, z).y - 1
                Case 3
                    ficha(bando, z).x = ficha(bando, z).x + 1

                Case 4
                    ficha(bando, z).x = ficha(bando, z).x + 1
                    ficha(bando, z).y = ficha(bando, z).y + 1
                Case 5

                    ficha(bando, z).y = ficha(bando, z).y + 1
                Case 6
                    ficha(bando, z).x = ficha(bando, z).x - 1
                    ficha(bando, z).y = ficha(bando, z).y + 1
                Case 7
                    ficha(bando, z).x = ficha(bando, z).x - 1

                Case 8
                    ficha(bando, z).x = ficha(bando, z).x - 1
                    ficha(bando, z).y = ficha(bando, z).y - 1

            End Select

            'portal dimensional
            If casilla(SoldDim, SoldPiso, ficha(bando, z).x, ficha(bando, z).y).Fondo = 3 Then
                ficha(bando, z).Dimension = 2
                casilla(1, SoldPiso, ficha(bando, z).x, ficha(bando, z).y).fichaTipo = 0
                casilla(1, SoldPiso, ficha(bando, z).x, ficha(bando, z).y).fichaNro = 0
                casilla(2, SoldPiso, ficha(bando, z).x, ficha(bando, z).y).fichaTipo = bando
                casilla(2, SoldPiso, ficha(bando, z).x, ficha(bando, z).y).fichaNro = z
                ficha(bando, z).x = 15
                ficha(bando, z).y = 15
                ficha(bando, z).rumboX = 16
                ficha(bando, z).rumboY = 16
                ficha(bando, z).pasoX = 0
                ficha(bando, z).pasoY = 0
                ficha(bando, z).Direccion = 0
                ficha(bando, z).Fase = 0
                originaL = Seleccionado
                If NrodeSoldados = 1 Then Exit Sub
                otravez:
                Seleccionado = Seleccionado + 1
                If Seleccionado = NrodeSoldados + 1 Then Seleccionado = 1
                If ficha(bando, Seleccionado).Dimension <> Dimension Then If Seleccionado = originaL Then Exit Sub Else GoTo otravez
                Funcion = 1
                bando = 0
                centrar Seleccionado
                For a = 0 To 14
                    DireccionReg(bando, z, a) = 0
                Next a
                Exit Sub
            End If
            'escalera
            '       IF casilla(Dimension, SoldPiso, ficha(bando, z).x, ficha(bando, z).y).Fondo = 6 THEN
            If subir = 1 Then
                subir = 0
                casilla(SoldDim, SoldPiso, ficha(bando, z).x, ficha(bando, z).y).fichaTipo = 0
                casilla(SoldDim, SoldPiso, ficha(bando, z).x, ficha(bando, z).y).fichaNro = 0

                ficha(bando, z).Piso = ficha(bando, z).Piso + 1
                ficha(bando, z).x = ficha(bando, z).x '+ alturadepiSo / 2
                ficha(bando, z).y = ficha(bando, z).y '- alturadepiSo / 2 - 1
                ficha(bando, z).rumboX = 0 ' ficha(bando, z).x - 1
                ficha(bando, z).rumboY = 0 ' ficha(bando, z).y + 1
                ficha(bando, z).Fase = 0
                ficha(bando, z).Direccion = 0

                piSo = piSo + 1
                casilla(SoldDim, piSo, ficha(bando, z).x, ficha(bando, z).y).fichaTipo = bando + 1
                casilla(SoldDim, piSo, ficha(bando, z).x, ficha(bando, z).y).fichaNro = z
                Exit Sub
            End If

            If casilla(SoldDim, SoldPiso, ficha(bando, z).x, ficha(bando, z).y).Fondo = 6 Then
                bajar = 1
                casilla(SoldDim, SoldPiso, ficha(bando, z).x, ficha(bando, z).y).fichaTipo = 0
                casilla(SoldDim, SoldPiso, ficha(bando, z).x, ficha(bando, z).y).fichaNro = 0

                ficha(bando, z).Piso = ficha(bando, z).Piso - 1
                ficha(bando, z).x = ficha(bando, z).x '- alturadepiSo / 2
                ficha(bando, z).y = ficha(bando, z).y '+ alturadepiSo / 2
                ficha(bando, z).rumboX = ficha(bando, z).x ' + 1
                ficha(bando, z).rumboY = ficha(bando, z).y
                ficha(bando, z).Fase = 0
                ficha(bando, z).Direccion = 0

                piSo = piSo - 1
                casilla(SoldDim, piSo, ficha(bando, z).x, ficha(bando, z).y).fichaTipo = bando + 1
                casilla(SoldDim, piSo, ficha(bando, z).x, ficha(bando, z).y).fichaNro = z
                Exit Sub

            End If




            casilla(SoldDim, SoldPiso, ficha(bando, z).x, ficha(bando, z).y).fichaTipo = bando + 1
            casilla(SoldDim, SoldPiso, ficha(bando, z).x, ficha(bando, z).y).fichaNro = z


            ficha(bando, z).Fase = 0
            'SLEEP

        End If
        '    centrar z

        'SLEEP



    End If


End Sub






Sub IA
    SUERTE = Int(Rnd * 50)
    If SUERTE = 3 Then

        For A = 1 To 2
            If ficha(1, A).VIDA <= 0 Then GoTo estaMuerto
            If ficha(1, A).rumboX = 0 Then
                X = Int((Rnd * 5))
                Y = Int((Rnd * 5))

                ficha(1, A).rumboX = ficha(1, A).x + X
                ficha(1, A).rumboY = ficha(1, A).y + Y
            End If
            estaMuerto:

        Next A
    End If

End Sub








Sub DISPARAR (BANDO As Integer, SELEC As Integer, destX As Integer, destY As Integer)
    If ARMA(ficha(BANDO, SELEC).ARMA).BALAS = 0 Then
        ' _PRINTSTRING (350, 300), "NO HAY BALAS": EXIT SUB
        Mensaje$ = "NO HAY MAS BALAS": Exit Sub
    End If
    'retval% = PlaySound("tiro", 0, SND_ASYNC)
    tiroNro = tiroNro + 1
    tiro(tiroNro).tipo = ARMA(ficha(BANDO, SELEC).ARMA).TIPO
    'xxx = ((ficha(BANDO, SELEC).x + ficha(BANDO, SELEC).y) * Zoom + scrollX) + Zoom
    'yyy = (((ficha(BANDO, SELEC).y - ficha(BANDO, SELEC).x) * Zoom + scrollY) - Zoom)
    xxx = ((ficha(BANDO, SELEC).x + ficha(BANDO, SELEC).y) * Zoom + scrollX) + Zoom '- DiferenciaDePiso
    'yyy = (((ficha(BANDO, SELEC).y - ficha(BANDO, SELEC).x) * Zoom + scrollY) - Zoom) - DiferenciaDePiso
    yyy = (((ficha(BANDO, SELEC).y - ficha(BANDO, SELEC).x) * Zoom + scrollY) - Zoom) '- DiferenciaDePiso

    tiro(tiroNro).x1 = xxx
    tiro(tiroNro).y1 = yyy
    tiro(tiroNro).x2 = destX '_MOUSEX
    tiro(tiroNro).y2 = destY '_MOUSEY
    tiro(tiroNro).BANDO = BANDO
    tiro(tiroNro).FICHANRO = SELEC
    ARMA(ficha(BANDO, SELEC).ARMA).BALAS = ARMA(ficha(BANDO, SELEC).ARMA).BALAS - 1
End Sub













Sub disparos

    For a = 1 To tiroNro
        Select Case tiro(a).tipo
            Case 1 'a = 1

                tiro(a).Dif = tiro(a).Dif2
                tiro(a).Dif2 = (Abs(tiro(a).INSTANCIA) + Zoom) * (tiro(a).y1 - tiro(a).y2) / Abs(tiro(a).x2 - tiro(a).x1)

                signo = Abs(tiro(a).x2 - tiro(a).x1) / (tiro(a).x2 - tiro(a).x1)
                tiro(a).Xini = tiro(a).x1 + tiro(a).INSTANCIA
                tiro(a).Yini = tiro(a).y1 - tiro(a).Dif
                tiro(a).Xfin = tiro(a).x1 + tiro(a).INSTANCIA + (Zoom * signo)
                tiro(a).Yfin = tiro(a).y1 - tiro(a).Dif2
                tiro(a).INSTANCIA = tiro(a).INSTANCIA + (Zoom * signo)

                nnxx = Int((((tiro(a).Xini - scrollX) - tiro(a).Yini + scrollY) / (Zoom * 2)))
                nnyy = Int((((tiro(a).Yini - scrollY) + tiro(a).Xini - scrollX) / (Zoom * 2)))


                'nnyyConDifDePiso = INT(((((tiro(a).Yini - scrollY) + tiro(a).Xini - scrollX) + DiferenciaDePiso) / (Zoom * 2)))
                'nnxxConDifDePiso = INT(((((tiro(a).Xini - scrollX) - tiro(a).Yini + scrollY) - DiferenciaDePiso) / (Zoom * 2)))
                PisoSustitut = piSo
                If casilla(Dimension, piSo, nnxx, nnyy).Fondo = 0 Then
                    PisoSustitut = piSo - 1
                    DiferenciaDePiso = ((PisoSustitut * -Zoom * alturadepiSo) + (alturadepiSo * Zoom))
                    '   nnyyConDifDePiso = INT(((((tiro(a).Yini - scrollY) + tiro(a).Xini - scrollX) + DiferenciaDePiso) / (Zoom * 2)))
                    '  nnxxConDifDePiso = INT(((((tiro(a).Xini - scrollX) - tiro(a).Yini + scrollY) - DiferenciaDePiso) / (Zoom * 2)))
                End If
                If Abs(tiro(a).INSTANCIA) > Zoom * 2 Then 'CHOQUES
                    If casilla(Dimension, PisoSustitut, nnxx, nnyy).fichaTipo <> 0 Then
                        X1 = tiro(a).x1
                        Y1 = tiro(a).y1
                        tacho a

                        ficha(casilla(Dimension, PisoSustitut, nnxx, nnyy).fichaTipo - 1, casilla(Dimension, PisoSustitut, nnxx, nnyy).fichaNro).VIDA = ficha(casilla(Dimension, PisoSustitut, nnxx, nnyy).fichaTipo - 1, casilla(Dimension, PisoSustitut, nnxx, nnyy).fichaNro).VIDA - 1

                        If ficha(casilla(Dimension, PisoSustitut, nnxx, nnyy).fichaTipo - 1, casilla(Dimension, PisoSustitut, nnxx, nnyy).fichaNro).VIDA < 0 Then
                            MORIR casilla(Dimension, PisoSustitut, nnxx, nnyy).fichaTipo - 1, casilla(Dimension, PisoSustitut, nnxx, nnyy).fichaNro

                            Exit Sub
                        End If
                        HERIR casilla(Dimension, PisoSustitut, nnxx, nnyy).fichaTipo - 1, casilla(Dimension, PisoSustitut, nnxx, nnyy).fichaNro, X1, Y1

                        Exit Sub
                    End If
                    '               casilla(Dimension, PisoSustitut, nnxx, nnyy).Fondo = 0
                    If casilla(Dimension, PisoSustitut, nnxx, nnyy).Cosa <> 0 Then
                        casilla(Dimension, PisoSustitut, nnxx, nnyy).Cosa = 0

                        tacho a
                        GoTo 3
                    End If

                End If

                If Abs(tiro(a).Xini - tiro(a).x1) > Abs(tiro(a).x2 - tiro(a).x1) Then tacho a
                'SLEEP
            Case 2

                signo = Abs(tiro(a).x2 - tiro(a).x1) / (tiro(a).x2 - tiro(a).x1)
                Do
                    tiro(a).Dif = tiro(a).Dif2

                    tiro(a).Dif2 = (Abs(tiro(a).INSTANCIA) + Zoom) * (tiro(a).y1 - tiro(a).y2) / Abs(tiro(a).x2 - tiro(a).x1)
                    '                signo = ABS(tiro(a).x2 - tiro(a).x1) / (tiro(a).x2 - tiro(a).x1)
                    tiro(a).Xini = tiro(a).x1 + tiro(a).INSTANCIA
                    tiro(a).Yini = tiro(a).y1 - tiro(a).Dif
                    tiro(a).Xfin = tiro(a).x1 + tiro(a).INSTANCIA + (Zoom * signo)
                    tiro(a).Yfin = tiro(a).y1 - tiro(a).Dif2
                    tiro(a).INSTANCIA = tiro(a).INSTANCIA + (Zoom * signo)

                    nnxx = Int((((tiro(a).Xini - scrollX) - tiro(a).Yini + scrollY) / (Zoom * 2)))
                    nnyy = Int((((tiro(a).Yini - scrollY) + tiro(a).Xini - scrollX) / (Zoom * 2)))
                    '  casilla(dimension,piso,nnxx, nnyy).visible = 0



                    If Abs(tiro(a).INSTANCIA) > Zoom * 2 Then 'choques
                        If casilla(Dimension, piSo, nnxx, nnyy).fichaTipo <> 0 Then
                            ' ficha(casilla(dimension,piso,nnxx, nnyy).fichaTipo - 1, casilla(dimension,piso,nnxx, nnyy).fichaNro).rumboX = 2
                            tacho a
                            X1 = tiro(a).x1
                            Y1 = tiro(a).y1

                            HERIR casilla(Dimension, piSo, nnxx, nnyy).fichaTipo - 1, casilla(Dimension, piSo, nnxx, nnyy).fichaNro, X1, Y1

                            '                        HERIR casilla(dimension,piso,nnxx, nnyy).fichaTipo - 1, casilla(dimension,piso,nnxx, nnyy).fichaNro
                            tiro(a).Xini = tiro(a).x1
                            tiro(a).Yini = tiro(a).y1
                            'tiro(a).Xfin = tiro(a).x2
                            'tiro(a).Yfin = tiro(a).y2

                            Exit Do

                        End If
                        If casilla(Dimension, piSo, nnxx, nnyy).Fondo <> 0 Then
                            '           casilla(Dimension, piSo, nnxx, nnyy).Fondo = 0
                            tiro(a).Xini = tiro(a).x1
                            tiro(a).Yini = tiro(a).y1
                            'tiro(a).Xfin = tiro(a).x2
                            'tiro(a).Yfin = tiro(a).y2

                            'GOTO TACHO2
                        End If

                    End If

                    If Abs(tiro(a).Xini - tiro(a).x1) > Abs(tiro(a).x2 - tiro(a).x1) Then Exit Do
                Loop
                tiro(a).Xini = tiro(a).x1
                tiro(a).Yini = tiro(a).y1
                tiro(a).Xfin = tiro(a).x2
                tiro(a).Yfin = tiro(a).y2

                'IF ABS(tiro(a).Xini) > ABS(tiro(a).x2) THEN
                If tiro(a).listo = 1 Then
                    'LINE (tiro(tiroNro).x1, tiro(tiroNro).y1)-(tiro(tiroNro).Xfin, tiro(tiroNro).Yfin), _RGB(0, 44, 200, 1)


                    ' SLEEP

                    'TACHO2:
                    tacho a


                    Exit Sub
                End If
                tiro(a).listo = 1
        End Select
        3
    Next a

End Sub

Sub HERIR (TTIPO As Integer, NNRO As Integer, xORIGEN, yORIGEN)
    DISPARAR TTIPO, NNRO, xORIGEN, yORIGEN
    ficha(TTIPO, NNRO).rumboX = ficha(TTIPO, NNRO).x + 1
    ficha(TTIPO, NNRO).rumboY = ficha(TTIPO, NNRO).y





End Sub












Sub tacho (a As Integer)
    For b = a To tiroNro
        tiro(b).x1 = tiro(b + 1).x1
        tiro(b).y1 = tiro(b + 1).y1
        tiro(b).x2 = tiro(b + 1).x2
        tiro(b).y2 = tiro(b + 1).y2
        tiro(b).INSTANCIA = tiro(b + 1).INSTANCIA
        tiro(b).Dif2 = tiro(b + 1).Dif2
        tiro(b).BANDO = tiro(b + 1).BANDO
        tiro(b).FICHANRO = tiro(b + 1).FICHANRO
        tiro(b).tipo = tiro(b + 1).tipo

    Next b
    tiroNro = tiroNro - 1
    tiro(a).listo = 0
End Sub







Sub MORIR (BANDo, aA)
    'retval% = PlaySound("snds\ou.wav", 0, synch)
    casilla(Dimension, piSo, ficha(BANDo, aA).x, ficha(BANDo, aA).y).fichaNro = 0
    casilla(Dimension, piSo, ficha(BANDo, aA).x, ficha(BANDo, aA).y).fichaTipo = 0
    casilla(Dimension, piSo, ficha(BANDo, aA).x, ficha(BANDo, aA).y).Fondo = 2
    '_DEST _CONSOLE
    casilla(Dimension, piSo, ficha(BANDo, aA).x, ficha(BANDo, aA).y).Suelo1 = ficha(BANDo, aA).arma1
    casilla(Dimension, piSo, ficha(BANDo, aA).x, ficha(BANDo, aA).y).Suelo2 = ficha(BANDo, aA).arma2
    casilla(Dimension, piSo, ficha(BANDo, aA).x, ficha(BANDo, aA).y).Suelo3 = ficha(BANDo, aA).arma3


    'FOR Numero = aA TO NrodeSoldados
    '    ficha(BANDo, Numero).x = ficha(BANDo, Numero + 1).x
    '    ficha(BANDo, Numero).y = ficha(BANDo, Numero + 1).y
    '    ficha(BANDo, Numero).Direccion = ficha(BANDo, Numero + 1).Direccion
    '    ficha(BANDo, Numero).Fase = ficha(BANDo, Numero + 1).Fase
    '    ficha(BANDo, Numero).rumboX = ficha(BANDo, Numero + 1).rumboX
    '    ficha(BANDo, Numero).rumboY = ficha(BANDo, Numero + 1).rumboY
    '    ficha(BANDo, Numero).nombre = ficha(BANDo, Numero + 1).nombre
    '    ficha(BANDo, Numero).pasoX = ficha(BANDo, Numero + 1).pasoX
    '    ficha(BANDo, Numero).pasoY = ficha(BANDo, Numero + 1).pasoY
    '    ficha(BANDo, Numero).ARMA = ficha(BANDo, Numero + 1).ARMA
    '    ficha(BANDo, Numero).VIDA = ficha(BANDo, Numero + 1).VIDA

    'NEXT Numero
    'PRINT "suelo1", casilla(Dimension, piSo, ficha(BANDo, aA).x, ficha(BANDo, aA).y).suelo1
    'PRINT "fichar arma1", ficha(BANDo, Numero).arma1
    'PRINT "x", ficha(BANDo, aA).x '= 0 'ficha(BANDo, numero + 1).x
    'PRINT "y", ficha(BANDo, aA).y '= 0 'ficha(BANDo, numero + 1).y
    'PRINT "derecc", ficha(BANDo, aA).Direccion 'ficha(BANDo, numero + 1).Direccion
    'PRINT "fase", ficha(BANDo, aA).Fase '= 0 'ficha(BANDo, numero + 1).Fase
    'PRINT "vida", ficha(BANDo, aA).VIDA '= 0 'ficha(BANDo, numero + 1).VIDA

    'ficha(BANDo, aA).x = 0 'ficha(BANDo, numero + 1).x
    'ficha(BANDo, aA).y = 0 'ficha(BANDo, numero + 1).y
    'ficha(BANDo, aA).Direccion = 0 'ficha(BANDo, numero + 1).Direccion
    'ficha(BANDo, aA).Fase = 0 'ficha(BANDo, numero + 1).Fase
    'ficha(BANDo, aA).rumboX = 0 'ficha(BANDo, numero + 1).rumboX
    'ficha(BANDo, aA).rumboY = 0 'ficha(BANDo, numero + 1).rumboY
    'ficha(BANDo, aA).nombre = "" 'ficha(BANDo, numero + 1).nombre
    'ficha(BANDo, aA).pasoX = 0 'ficha(BANDo, numero + 1).pasoX
    'ficha(BANDo, aA).pasoY = 0 'ficha(BANDo, numero + 1).pasoY
    'ficha(BANDo, aA).ARMA = 0 'ficha(BANDo, numero + 1).ARMA
    'ficha(BANDo, aA).VIDA = 0 'ficha(BANDo, numero + 1).VIDA

    'IF BANDo = 0 THEN NrodeSoldados = NrodeSoldados - 1
    'PRINT "nrodesold", NrodeSoldados
    'PRINT "bando", BANDo
    'PRINT "numero", aA
    'PRINT "seleccionado", Seleccionado
    If NrodeSoldados = 0 Then Beep: System

    If Seleccionado = aA And BANDo = 0 Then

        Seleccionado = Seleccionado - 1
        If Seleccionado = 0 Then Seleccionado = NrodeSoldados
        'Funcion = 0

    End If




    '_DEST 0



End Sub












Sub BARRAA
    _Font _LoadFont("otros\ethnocen.ttf", 10, "monospace")
    _PutImage (1, 600)-(1020, 709), BARRA&
    _PrintMode _KeepBackground
    Color _RGB(166, 0, 0, 255), _RGB(0, 0, 0, 0)
    seleccionadoAnt = Seleccionado
    bandoAnt = Bando
    If casilla(Dimension, pisoSuplente, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Construccion <> 0 Then

        'Funcion = 4
        Selec = casilla(Dimension, pisoSuplente, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Construccion
        Seleccionado = Selec
        BarraMuestraConstruccion = 1
        ConstruccionSeleccionada = Seleccionado



        Bando = 4
        Do: i = _MouseInput: Loop Until Not _MouseButton(1)
        'EXIT SUB
    Else
        BarraMuestraConstruccion = 0
    End If








    If ficha(Bando, Seleccionado).armaSelec <> 0 Then
        _PutImage ((210 + 100 * ficha(Bando, Seleccionado).armaSelec), 625)-((220 + (100 * ficha(Bando, Seleccionado).armaSelec)) + 90, 700), Cuadrado& '_RGB(244, 0, 6, 1), B

    End If


    If Seleccionado <> 0 Then
        If Bando = 4 Then 'construccion
            _PutImage (20, 631)-(85, 695), cosaGrande&(Seleccionado)
            Select Case Seleccionado
                Case 1

                    _PutImage (320, 625)-(400, 700), sticker&(1)
                Case 2
                    _PutImage (320, 625)-(400, 700), sticker&(2)
                Case 3
                    _PutImage (320, 625)-(400, 700), sticker&(3)
            End Select

            GoTo barrafin
        End If

        'datos soldado
        _PrintString (47, 616), ficha(Bando, Seleccionado).Nombrereal ', text&
        _PutImage (20, 631)-(85, 695), img&(Bando + 1, Seleccionado, 0, 0)
        _PutImage (85, 631)-(300, 700), Texto&
        _PutImage (163, 635)-(163 + (25 * ficha(Bando, Seleccionado).VIDA), 643), indiCador&
        If Bando = 0 Then

            'cosaespecial
            If ficha(Bando, Seleccionado).CosaEspecial1 <> 0 Then
                _PutImage (320, 625)-(400, 700), cosaE&(ficha(Bando, Seleccionado).CosaEspecial1)

            End If
            If ficha(Bando, Seleccionado).CosaEspecial2 <> 0 Then
                _PutImage (420, 625)-(500, 700), cosaE&(ficha(Bando, Seleccionado).CosaEspecial2)

            End If
            If ficha(Bando, Seleccionado).CosaEspecial3 <> 0 Then
                _PutImage (520, 625)-(600, 700), cosaE&(ficha(Bando, Seleccionado).CosaEspecial3)

            End If



            If ARMA(ficha(Bando, Seleccionado).arma1).TIPO <> 0 Then
                _PutImage (320, 625)-(400, 700), pistol&(ARMA(ficha(Bando, Seleccionado).arma1).TIPO)

                '   _PRINTSTRING (355, 630), "     " 'STR$(ARMA(ficha(Bando, Seleccionado).arma1).BALAS), text&
                _PrintString (355, 630), Str$(ARMA(ficha(Bando, Seleccionado).arma1).BALAS) ', text&
            End If
            If ARMA(ficha(Bando, Seleccionado).arma2).TIPO <> 0 Then
                _PutImage (420, 630)-(500, 700), pistol&(ARMA(ficha(Bando, Seleccionado).arma2).TIPO)

                _PrintString (455, 630), Str$(ARMA(ficha(Bando, Seleccionado).arma2).BALAS) ', text&
            End If
            If ARMA(ficha(Bando, Seleccionado).arma3).TIPO <> 0 Then
                _PutImage (520, 625)-(600, 700), pistol&(ARMA(ficha(Bando, Seleccionado).arma3).TIPO)

                _PrintString (555, 630), Str$(ARMA(ficha(Bando, Seleccionado).arma3).BALAS)
            End If

        End If



        'suelo
        '    SELECT CASE casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).CosaEspecial
        '       CASE 1
        If casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).CosaEspecial <> 0 Then
            _PutImage (620, 625)-(700, 700), cosaE&(casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).CosaEspecial) 'pistol&(ARMA(casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo1).TIPO)
        End If
        '  END SELECT

        If casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo1 <> 0 Then
            _PutImage (620, 625)-(700, 700), pistol&(ARMA(casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo1).TIPO)
            '_PRINTSTRING (555, 630), STR$(ARMA(ficha(Bando, Seleccionado).arma3).BALAS)
        End If
        If casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo2 <> 0 Then

            _PutImage (720, 625)-(800, 700), pistol&(ARMA(casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo2).TIPO)
            '_PRINTSTRING (555, 630), STR$(ARMA(ficha(Bando, Seleccionado).arma3).BALAS)
        End If
        If casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo3 <> 0 Then

            _PutImage (820, 625)-(900, 700), pistol&(ARMA(casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo3).TIPO)
            '_PRINTSTRING (555, 630), STR$(ARMA(ficha(Bando, Seleccionado).arma3).BALAS)
        End If




    End If
    barrafin:
    If Mapavisible = 1 Then
        For circ = 1 To 15
            Circle (1000, 685), circ
        Next circ
    End If
    Seleccionado = seleccionadoAnt
    Bando = bandoAnt

End Sub





























Sub menu
    'SCREEN , , , pantalla2
    pantalla
    BARRAA

    _PutImage (100, 109)-(500, 500), menugraf&

    _Display

    Do
        Do While _MouseInput: Loop
        If _MouseButton(1) = -1 Then
            If _MouseX > 125 And _MouseY > 416 And _MouseX < 472 And _MouseY < 478 Then System
            If _MouseX > 125 And _MouseY > 380 And _MouseX < 472 And _MouseY < 412 Then Exit Sub

        End If
        ' _DEST _CONSOLE
        ' LOCATE 1, 1
        ' PRINT STR$(_MOUSEX) + STR$(_MOUSEY)
        ' _DEST 0


    Loop


End Sub

Sub tecladoEditor
    Select Case InKey$
        Case "0"
            valor = 0
        Case "1"
            valor = 1
        Case "2"
            valor = 2
        Case "3"
            valor = 3

        Case "4"
            valor = 4
        Case "5"
            valor = 5
        Case "6"
            valor = 6
        Case "7" 'compu
            ' CosaEspecial(NroDeLista2).x = nnx
            ' CosaEspecial(NroDeLista2).y = nny
            ' CosaEspecial(NroDeLista2).Cosa = 1
            ' CosaEspecial(NroDeLista2).visible = 1
            casilla(Dimension, piSo, nnx, nny).CosaEspecial = 1
            casilla(Dimension, piSo, nnx, nny).Visible = 1

            '   casilla(Dimension, piSo, CosaEspecial(a).x, CosaEspecial(a).y).Suelo1 = CosaEspecial(a).Cosa
            '   NroDeLista2 = NroDeLista2 + 1

            'valor = 7
        Case "y", "Y" 'llave
            '        CosaEspecial(NroDeLista2).x = nnx
            '       CosaEspecial(NroDeLista2).y = nny
            '      CosaEspecial(NroDeLista2).Cosa = 2
            '     CosaEspecial(NroDeLista2).visible = 1
            casilla(Dimension, piSo, nnx, nny).CosaEspecial = 2
            casilla(Dimension, piSo, nnx, nny).Visible = 1

            '   casilla(Dimension, piSo, CosaEspecial(a).x, CosaEspecial(a).y).Suelo1 = CosaEspecial(a).Cosa
            '        NroDeLista2 = NroDeLista2 + 1

        Case "n", "N" 'nave
            'ListaCosaGrande(NroDeLista).x = nnx
            'ListaCosaGrande(NroDeLista).y = nny
            'ListaCosaGrande(NroDeLista).Cosa = 1
            'ListaCosaGrande(NroDeLista).visible = 1
            casilla(Dimension, piSo, nnx, nny).Construccion = 1
            casilla(Dimension, piSo, nnx, nny).Visible = 1
            naveX = nnx
            naveY = nny
            CosaAOcultarX = nnx
            CosaAOcultary = nny

        Case "m", "M" 'municipio
            casilla(Dimension, piSo, nnx, nny).Construccion = 2
            casilla(Dimension, piSo, nnx, nny).Visible = 1
            CosaAOcultarX = nnx
            CosaAOcultary = nny

            'osaAOcultar = 2
        Case "b", "B" 'banco
            casilla(Dimension, piSo, nnx, nny).Construccion = 3
            casilla(Dimension, piSo, nnx, nny).Visible = 1
            CosaAOcultarX = nnx
            CosaAOcultary = nny

            '      CosaAOcultar = 3
        Case "i"
            casilla(Dimension, piSo, nnx, nny).ParedInvisible = 1
            casilla(Dimension, piSo, nnx, nny).OcultaX = CosaAOcultarX 'el nro de lista que oculta
            casilla(Dimension, piSo, nnx, nny).OcultaY = CosaAOcultary 'el nro de lista que ocu
        Case "I"
            CosaAOcultarX = nnx
            CosaAOcultary = nny

        Case Chr$(27)
            listoEditor = 1
            '        STOP

        Case "+"
            Zoom = Zoom + 1
            centrar 1

        Case "-"
            Zoom = Zoom - 1
            centrar 1
            'CASE "Q"
            '   valor = 3
            'CASE "W"
            '   valor = 4

        Case "h", "H"
            casilla(Dimension, piSo, nnx, nny).Construccion = 4
            casilla(Dimension, piSo, nnx, nny).Visible = 1
            CosaAOcultarX = nnx
            CosaAOcultary = nny


        Case "X"
            centrar 1

        Case "P", "p"
            piSo = piSo + 1

        Case "g", "G"
            Beep
            '  SCREEN , , 1, 1

            Open "escena\escena.002" For Random As #1
            unTablero = TableroAncho(1) * TableroAncho(1)
            For a = 1 To TableroAncho(1)
                For b = 1 To TableroAncho(1)

                    Put #1, (a * TableroAncho(1)) + b, casilla(1, 1, a, b).Fondo
                    Put #1, unTablero + (a * TableroAncho(1)) + b, casilla(1, 1, a, b).Cosa
                    Put #1, unTablero * 2 + (a * TableroAncho(1)) + b, casilla(1, 2, a, b).Fondo
                    Put #1, unTablero * 3 + (a * TableroAncho(1)) + b, casilla(1, 2, a, b).Cosa
                    Put #1, unTablero * 4 + (a * TableroAncho(1)) + b, casilla(1, 1, a, b).ParedInvisible
                    Put #1, unTablero * 5 + (a * TableroAncho(1)) + b, casilla(1, 1, a, b).OcultaX
                    Put #1, unTablero * 6 + (a * TableroAncho(1)) + b, casilla(1, 1, a, b).OcultaY

                    Put #1, unTablero * 7 + (a * TableroAncho(1)) + b, casilla(1, 1, a, b).Construccion
                    Put #1, unTablero * 8 + (a * TableroAncho(1)) + b, casilla(1, 1, a, b).CosaEspecial


                Next b
            Next a
            Put #1, unTablero * 9 + TableroAncho(1), naveX '
            Put #1, unTablero * 9 + TableroAncho(1) + 1, naveY '

            '  FOR a = 1 TO 10
            '     PUT #1, unTablero * 6 + a, ListaCosaGrande(a).x
            '    PUT #1, unTablero * 6 + a + 10, ListaCosaGrande(a).y
            '   PUT #1, unTablero * 6 + a + 20, ListaCosaGrande(a).Cosa
            '  PUT #1, unTablero * 6 + a + 30, CosaEspecial(a).x
            ' PUT #1, unTablero * 6 + a + 40, CosaEspecial(a).y
            ' PUT #1, unTablero * 6 + a + 50, CosaEspecial(a).Cosa
            '    GET #1, unTablero * 6 + (a * TableroAncho(1)) + b, casilla(1, 1, a, b).construccion
            '   GET #1, unTablero * 7 + (a * TableroAncho(1)) + b, casilla(1, 1, a, b).cosaespecial
            '  GET #1, unTablero * 8 +  TableroAncho(1)) , navex'
            ' GET #1, unTablero * 8 +  TableroAncho(1))+1 , naveY'

            ' NEXT a
            Close #1
        Case "l"
            'BEEP
            unTablero = TableroAncho(1) * TableroAncho(1)
            Open "escena\escena.002" For Random As #1
            ' SCREEN , , 1, 1
            For a = 1 To TableroAncho(1)
                For b = 1 To TableroAncho(1)


                    Get #1, (a * TableroAncho(1)) + b, casilla(1, 1, a, b).Fondo
                    Get #1, (TableroAncho(1) * TableroAncho(1)) + (a * TableroAncho(1)) + b, casilla(1, 1, a, b).Cosa
                    Get #1, unTablero * 2 + (a * TableroAncho(1)) + b, casilla(1, 2, a, b).Fondo
                    Get #1, unTablero * 3 + (a * TableroAncho(1)) + b, casilla(1, 2, a, b).Cosa
                    Get #1, unTablero * 4 + (a * TableroAncho(1)) + b, casilla(1, 1, a, b).ParedInvisible
                    Get #1, unTablero * 5 + (a * TableroAncho(1)) + b, casilla(1, 1, a, b).OcultaX
                    Get #1, unTablero * 6 + (a * TableroAncho(1)) + b, casilla(1, 1, a, b).OcultaY

                    Get #1, unTablero * 7 + (a * TableroAncho(1)) + b, casilla(1, 1, a, b).Construccion
                    Get #1, unTablero * 8 + (a * TableroAncho(1)) + b, casilla(1, 1, a, b).CosaEspecial


                Next b
            Next a
            Get #1, unTablero * 9 + TableroAncho(1), naveX '
            Get #1, unTablero * 9 + TableroAncho(1) + 1, naveY '

            'FOR a = 1 TO 10
            '   GET #1, unTablero * 6 + a, ListaCosaGrande(a).x
            '  GET #1, unTablero * 6 + a + 10, ListaCosaGrande(a).y
            ' GET #1, unTablero * 6 + a + 20, ListaCosaGrande(a).Cosa
            ' GET #1, unTablero * 6 + a + 30, CosaEspecial(a).x
            ' GET #1, unTablero * 6 + a + 40, CosaEspecial(a).y
            ' GET #1, unTablero * 6 + a + 50, CosaEspecial(a).Cosa
            ' casilla(Dimension, piSo, CosaEspecial(a).x, CosaEspecial(a).y).CosaEspecial = CosaEspecial(a).Cosa
            ' casilla(Dimension, piSo, CosaEspecial(a).x, CosaEspecial(a).y).CosaEspecialLista = a
            ' CosaEspecial(a).visible = 1
            '   IF ListaCosaGrande(a).x THEN NroDeLista = NroDeLista + 1
            '  IF CosaEspecial(a).x <> 0 THEN NroDeLista2 = NroDeLista2 + 1

            '  NEXT a

            Close #1



    End Select
    If piSo = 0 Or piSo = 3 Then piSo = 1

End Sub

Sub ratonEditor
    If _MouseButton(1) = -1 Then

        casilla(Dimension, piSo, nnx, nny).Cosa = valor
        If valor = 0 Then
            casilla(Dimension, piSo, nnx, nny).CosaEspecial = 0
            casilla(Dimension, piSo, nnx, nny).fichaTipo = 0
            casilla(Dimension, piSo, nnx, nny).fichaNro = 0 ' AS INTEGER
            casilla(Dimension, piSo, nnx, nny).ParedInvisible = 0 'AS INTEGER
            casilla(Dimension, piSo, nnx, nny).OcultaX = 0 ' AS INTEGER
            casilla(Dimension, piSo, nnx, nny).OcultaY = 0 ' AS INTEGER

            'Visible AS INTEGER
            '  Suelo1 AS INTEGER
            '  Suelo2 AS INTEGER
            '  Suelo3 AS INTEGER
            casilla(Dimension, piSo, nnx, nny).Construccion = 0 'AS INTEGER
            casilla(Dimension, piSo, nnx, nny).CosaEspecial = 0 'AS INTEGER
            ' CosaEspecial(casilla(Dimension, piSo, nnx, nny).CosaEspecialLista).Cosa = 0 ' AS INTEGER

        End If




    End If

    'disparar
    If _MouseButton(2) = -1 Then
        casilla(Dimension, pisoSuplente, nnx, nny).Fondo = valor


    End If


End Sub


Sub editor
    For A = 1 To TableroAncho(Dimension)
        For B = 1 To TableroAncho(Dimension)
            casilla(Dimension, piSo, A, B).Visible = 1
            casilla(Dimension, piSo, A, B).Cosa = 0
            casilla(Dimension, piSo, A, B).Fondo = 1
            casilla(Dimension, piSo, A, B).ParedInvisible = 0
            casilla(Dimension, piSo, A, B).OcultaX = 0
            casilla(Dimension, piSo, A, B).OcultaY = 0
            casilla(Dimension, piSo, A, B).Construccion = 0
            casilla(Dimension, piSo, A, B).CosaEspecial = 0
            casilla(Dimension, piSo, A, B).Suelo1 = 0
            casilla(Dimension, piSo, A, B).Suelo3 = 0
            casilla(Dimension, piSo, A, B).Suelo2 = 0
        Next B
    Next A

    Do While listoEditor = 0
        Do While _MouseInput: Loop
        Swap pantalla1, pantalla2
        ratonEditor
        nnx = Int(((_MouseX - scrollX) - _MouseY + scrollY) / (Zoom * 2))
        nny = Int(((_MouseY - scrollY) + _MouseX - scrollX) / (Zoom * 2))
        pisoSuplente = piSo
        If nnx < 1 Or nnx > TableroAncho(Dimension) Or nny < 1 Or nny > TableroAncho(Dimension) Then
            nnx = nnxAnt
            nny = nnyAnt
        End If
        pantalla
        pantallaAuxEditor
        tecladoEditor
        Screen , , pantalla1, pantalla2
        _Display
    Loop
    listoEditor = 0

End Sub







Sub despegar Static
    xxxz = (naveX + naveY) * Zoom + scrollX
    yyyz = ((naveY - naveX) - difdePiso) * Zoom + scrollY

    a = a + 15
    _PutImage (xxxz - 8 * Zoom, yyyz - 9 * Zoom - a)-(xxxz + 13 * Zoom, yyyz + 14 * Zoom - a), cosaGrande&(1)
    'Mensaje$ = "despegando" + STR$(xxxz) + STR$(yyyz) + STR$(a)
    'istaCosaGrande(1).Cosa = 0
    'istaCosaGrande(1).visible = 0
    casilla(Dimension, piSo, naveX, naveY).Construccion = 0 '4
    HeliPuertoX(Dimension) = naveX
    HeliPuertoY(Dimension) = naveY

    If juntar = 0 Then
        For y = -5 To 8
            For x = -5 To 6
                Transporte(x, y).fichaNro = casilla(Dimension, piSo, naveX + x, naveY + y).fichaNro
                Transporte(x, y).fichaTipo = casilla(Dimension, piSo, naveX + x, naveY + y).fichaTipo
                Transporte(x, y).CosaEspecial = casilla(Dimension, piSo, naveX + x, naveY + y).CosaEspecial
                Transporte(x, y).Suelo1 = casilla(Dimension, piSo, naveX + x, naveY + y).Suelo1
                Transporte(x, y).Suelo2 = casilla(Dimension, piSo, naveX + x, naveY + y).Suelo2
                Transporte(x, y).Suelo3 = casilla(Dimension, piSo, naveX + x, naveY + y).Suelo3

                casilla(Dimension, piSo, naveX + x, naveY + y).ParedInvisible = 0
                '            casilla(Dimension, piSo, naveX + x, naveY + y).Fondo = 2

                If casilla(Dimension, piSo, naveX + x, naveY + y).fichaNro <> 0 Then
                    ' ficha(casilla(Dimension, piSo, naveX + x, naveY + y).fichaTipo - 1, casilla(Dimension, piSo, naveX + x, naveY + y).fichaNro).x = 0
                    ' ficha(casilla(Dimension, piSo, naveX + x, naveY + y).fichaTipo - 1, casilla(Dimension, piSo, naveX + x, naveY + y).fichaNro).y = 0
                    ' ficha(casilla(Dimension, piSo, naveX + x, naveY + y).fichaTipo - 1, casilla(Dimension, piSo, naveX + x, naveY + y).fichaNro).rumboX = 0
                    ' ficha(casilla(Dimension, piSo, naveX + x, naveY + y).fichaTipo - 1, casilla(Dimension, piSo, naveX + x, naveY + y).fichaNro).rumboY = 0
                    ficha(casilla(Dimension, piSo, naveX + x, naveY + y).fichaTipo - 1, casilla(Dimension, piSo, naveX + x, naveY + y).fichaNro).Dimension = 0

                    casilla(Dimension, piSo, naveX + x, naveY + y).fichaTipo = 0
                    casilla(Dimension, piSo, naveX + x, naveY + y).fichaNro = 0
                    ' agregar a la lista de soldados que se lleva
                End If
                '  IF casilla(Dimension, piSo, naveX + x, naveY + y).CosaEspecial <> 0 THEN
                casilla(Dimension, piSo, naveX + x, naveY + y).CosaEspecial = 0

                'agregae

                '   agregar armas del piso
            Next x
        Next y
        juntar = 1
    End If




    If a > 1000 Then
        despegue = 0
        NaveEnElAire = 1
        Mapavisible = 1
        a = 0
        PasarSoldado
        juntar = 0
    End If
    'SLEEP

End Sub













Sub AgarrarArmas (n)

    'SELECT CASE casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).CosaEspecial
    '    CASE 1

    If casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).CosaEspecial <> 0 Then

        If ficha(Bando, Seleccionado).arma1 = 0 And ficha(Bando, Seleccionado).CosaEspecial1 = 0 Then

            ficha(Bando, Seleccionado).CosaEspecial1 = casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).CosaEspecial
            casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).CosaEspecial = 0
            '   CosaEspecial(casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).CosaEspecialLista).Cosa = 0

        Else
            If ficha(Bando, Seleccionado).arma2 = 0 And ficha(Bando, Seleccionado).CosaEspecial2 = 0 Then
                ficha(Bando, Seleccionado).CosaEspecial2 = casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).CosaEspecial
                'CosaEspecial(casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).CosaEspecialLista).Cosa = 0
                casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).CosaEspecial = 0
                ' _PUTIMAGE (620, 625)-(700, 700), fondo&(7)
            Else
                If ficha(Bando, Seleccionado).arma3 = 0 And ficha(Bando, Seleccionado).CosaEspecial3 = 0 Then

                    ficha(Bando, Seleccionado).CosaEspecial3 = casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).CosaEspecial
                    ' _PUTIMAGE (620, 625)-(700, 700), fondo&(7)
                    casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).CosaEspecial = 0
                    '   CosaEspecial(casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).CosaEspecialLista).Cosa = 0
                End If

            End If
        End If

        Exit Sub
        '_PUTIMAGE (620, 625)-(700, 700), fondo&(7) 'pistol&(ARMA(casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo1).TIPO)
    End If
    'END SELECT

    Select Case n
        Case 1
            If ficha(Bando, Seleccionado).arma1 = 0 And ficha(Bando, Seleccionado).CosaEspecial1 = 0 Then
                ficha(Bando, Seleccionado).arma1 = casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo1
                casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo1 = 0
            Else
                If ficha(Bando, Seleccionado).arma2 = 0 And ficha(Bando, Seleccionado).CosaEspecial2 = 0 Then
                    ficha(Bando, Seleccionado).arma2 = casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo1
                    casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo1 = 0
                Else
                    If ficha(Bando, Seleccionado).arma3 = 0 And ficha(Bando, Seleccionado).CosaEspecial3 = 0 Then
                        ficha(Bando, Seleccionado).arma3 = casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo1
                        casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo1 = 0
                    End If

                End If
            End If
        Case 2
            If ficha(Bando, Seleccionado).arma1 = 0 And ficha(Bando, Seleccionado).CosaEspecial1 = 0 Then
                ficha(Bando, Seleccionado).arma1 = casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo2
                casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo2 = 0
            Else
                If ficha(Bando, Seleccionado).arma2 = 0 And ficha(Bando, Seleccionado).CosaEspecial2 = 0 Then
                    ficha(Bando, Seleccionado).arma2 = casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo2
                    casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo2 = 0
                Else
                    If ficha(Bando, Seleccionado).arma3 = 0 And ficha(Bando, Seleccionado).CosaEspecial3 = 0 Then
                        ficha(Bando, Seleccionado).arma3 = casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo2
                        casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo2 = 0
                    End If

                End If
            End If
        Case 3
            If ficha(Bando, Seleccionado).arma1 = 0 Then
                ficha(Bando, Seleccionado).arma1 = casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo3
                casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo3 = 0
            Else
                If ficha(Bando, Seleccionado).arma2 = 0 Then
                    ficha(Bando, Seleccionado).arma2 = casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo3
                    casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo3 = 0
                Else
                    If ficha(Bando, Seleccionado).arma3 = 0 Then
                        ficha(Bando, Seleccionado).arma3 = casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo3
                        casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo3 = 0
                    End If

                End If
            End If
    End Select




End Sub

Sub DejarArma (n)
    'ficha(Bando, Seleccionado).armaSelec = 1
    'ficha(Bando, Seleccionado).ARMA = ficha(Bando, Seleccionado).arma1
    Select Case n
        Case 1
            ' BEEP
            If ficha(Bando, Seleccionado).CosaEspecial1 <> 0 Then
                '    BEEP
                If casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo1 = 0 Then

                    casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).CosaEspecial = ficha(Bando, Seleccionado).CosaEspecial1
                    '                casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).CosaEspecialLista = NroDeLista2
                    '               CosaEspecial(NroDeLista2).x = ficha(Bando, Seleccionado).x
                    '              CosaEspecial(NroDeLista2).y = ficha(Bando, Seleccionado).y
                    '             CosaEspecial(NroDeLista2).visible = 1
                    '            CosaEspecial(NroDeLista2).Cosa = ficha(Bando, Seleccionado).CosaEspecial1
                    '           NroDeLista2 = NroDeLista2 + 1
                    ficha(Bando, Seleccionado).CosaEspecial1 = 0
                    OrdenarArmas
                End If

                Exit Sub
            End If

            If casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo1 = 0 Then
                casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo1 = ficha(Bando, Seleccionado).arma1

            Else
                If casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo2 = 0 Then
                    casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo2 = ficha(Bando, Seleccionado).arma1

                Else
                    If casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo3 = 0 Then

                        casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo3 = ficha(Bando, Seleccionado).arma1

                    End If
                End If
            End If
            ficha(Bando, Seleccionado).arma1 = 0
            OrdenarArmas


        Case 2
            If ficha(Bando, Seleccionado).CosaEspecial2 <> 0 Then
                '    BEEP
                If casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo1 = 0 Then

                    casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).CosaEspecial = ficha(Bando, Seleccionado).CosaEspecial2
                    '    casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).CosaEspecialLista = NroDeLista2
                    '   CosaEspecial(NroDeLista2).x = ficha(Bando, Seleccionado).x
                    '  CosaEspecial(NroDeLista2).y = ficha(Bando, Seleccionado).y
                    '  CosaEspecial(NroDeLista2).visible = 1
                    '  CosaEspecial(NroDeLista2).Cosa = ficha(Bando, Seleccionado).CosaEspecial2
                    '  NroDeLista2 = NroDeLista2 + 1
                    ficha(Bando, Seleccionado).CosaEspecial2 = 0
                    OrdenarArmas
                End If

                Exit Sub
            End If

            If casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo1 = 0 Then
                casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo1 = ficha(Bando, Seleccionado).arma2

            Else
                If casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo2 = 0 Then
                    casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo2 = ficha(Bando, Seleccionado).arma2

                Else
                    If casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo3 = 0 Then

                        casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo3 = ficha(Bando, Seleccionado).arma2

                    End If
                End If
            End If
            ficha(Bando, Seleccionado).arma2 = 0
            OrdenarArmas

        Case 3
            If ficha(Bando, Seleccionado).CosaEspecial3 <> 0 Then
                If casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo1 = 0 Then

                    casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).CosaEspecial = ficha(Bando, Seleccionado).CosaEspecial3
                    ficha(Bando, Seleccionado).CosaEspecial1 = 0
                    OrdenarArmas
                End If

                Exit Sub
            End If

            If casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo1 = 0 Then
                casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo1 = ficha(Bando, Seleccionado).arma3

            Else
                If casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo2 = 0 Then
                    casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo2 = ficha(Bando, Seleccionado).arma3

                Else
                    If casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo3 = 0 Then

                        casilla(Dimension, piSo, ficha(Bando, Seleccionado).x, ficha(Bando, Seleccionado).y).Suelo3 = ficha(Bando, Seleccionado).arma3

                    End If
                End If
            End If
            ficha(Bando, Seleccionado).arma3 = 0
            OrdenarArmas

    End Select
    Exit Sub
    '        ficha(Bando, Seleccionado).armaSelec = 2
    '       ficha(Bando, Seleccionado).ARMA = 2 ' ficha(Bando, Seleccionado).arma1

End Sub

Sub OrdenarArmas
    Exit Sub
    1
    If ficha(Bando, Seleccionado).arma2 = 0 And ficha(Bando, Seleccionado).CosaEspecial2 = 0 Then
        If ficha(Bando, Seleccionado).CosaEspecial2 = 0 Then
            ficha(Bando, Seleccionado).arma2 = ficha(Bando, Seleccionado).arma3
        Else
            ficha(Bando, Seleccionado).CosaEspecial2 = ficha(Bando, Seleccionado).CosaEspecial3
        End If
    End If
    If ficha(Bando, Seleccionado).arma1 = 0 And ficha(Bando, Seleccionado).CosaEspecial1 = 0 Then
        If ficha(Bando, Seleccionado).CosaEspecial1 = 0 Then
            ficha(Bando, Seleccionado).arma1 = ficha(Bando, Seleccionado).arma2
            GoTo 1
        Else
            ficha(Bando, Seleccionado).CosaEspecial1 = ficha(Bando, Seleccionado).CosaEspecial2
            GoTo 1
        End If
    End If


End Sub

Sub Progreso Static
    'SCREEN , , 1, 1

    Locate 14, 20: Print "]"
    If y = 0 Then y = 2

    a = a + 1
    If a = 25 Then
        a = 2
        y = y + 1
    End If

    Locate y, a
    Print "."
    'SCREEN , , 0, 1

End Sub










Sub pantallaAuxEditor
    _PrintMode _KeepBackground
    _Font letra& '_LOADFONT("ethnocen.ttf", 15, "monospace")
    Color _RGB(255, 0, 0, 0), _RGB(0, 0, 255, 0)

    difdePiso = 0
    For A = 1 To TableroAncho(Dimension)
        For b = 1 To A
            XX = TableroAncho(Dimension) - A + b
            yy = b
            x = (XX + yy) * Zoom + scrollX
            '  yyy = ((yy - XX)) * Zoom + scrollY - ((piZo * -Zoom * alturadepiSo) + (alturadepiSo * Zoom))
            y = ((yy - XX) - difdePiso) * Zoom + scrollY
            If casilla(1, 1, XX, yy).ParedInvisible = 1 Then
                _PutImage (x, y - Zoom)-(x + Zoom * 2, y + Zoom), fondo&(2)
                '    _PRINTSTRING (x, y - Zoom), STR$(casilla(1, 1, XX, yy).Oculta)
            End If
        Next b
    Next A




    For A = TableroAncho(Dimension) To 1 Step -1
        For b = 1 To A - 1
            yy = TableroAncho(Dimension) + 1 - A + b
            XX = b
            x = (XX + yy) * Zoom + scrollX
            'yyy = ((yy - XX)) * Zoom + scrollY - ((piZo * -Zoom * alturadepiSo) + (alturadepiSo * Zoom))
            y = ((yy - XX) - difdePiso) * Zoom + scrollY
            If casilla(1, 1, XX, yy).ParedInvisible = 1 Then
                _PutImage (x, y - Zoom)-(x + Zoom * 2, y + Zoom), fondo&(2)
                '            _PRINTSTRING (x, y - Zoom), STR$(casilla(1, 1, XX, yy).Oculta)
            End If


        Next b
    Next A






End Sub

Sub Mensaj Static
    If Mensaje$ = "" Then Exit Sub
    _PrintMode _KeepBackground
    _Font letra& '_LOADFONT("ethnocen.ttf", 15, "monospace")
    Color _RGB(255, 0, 0, 0), _RGB(0, 0, 255, 0)
    _PutImage (50 + b, 300)-(1050 - b, 450), Cuadrado&

    _PrintString (400, 375), Mensaje$
    b = b + 1
    If b = 250 Then
        b = 0
        Mensaje$ = ""
    End If

End Sub





Sub Mapa
    x = Zoom
    centrar 1
    For a = 1 To 40
        Zoom = Zoom - 1
        ' _DELAY 0.02
        pantalla
        _Display
    Next a

    For q = 1 To 240
        _PutImage , a&
        _SetAlpha q, , mapaImg&
        map& = _CopyImage(mapaImg&, 33)
        _PutImage (0, 0)-(1024, 709), map&
        _Display
        '  _DELAY 0.01
    Next q
    _PutImage , a&

    _PutImage (0, 0)-(1024, 709), map&
    If NaveEnElAire = 1 Then _PutImage (-50, 650)-(200, 750), cosaGrande&(1)
    _PutImage (500, 500)-(600, 600), cosaGrande&(1)
    _PutImage (650, 500)-(750, 600), cosaGrande&(1)
    _Display

    Do

        Do While _MouseInput: Loop
        If _MouseButton(1) = -1 Then
            If _MouseX > 500 And _MouseY > 500 And _MouseX < 600 And _MouseY < 600 Then
                Dimension = 1
                If NaveEnElAire = 1 Then Aterrizaje = 1
                Exit Do
            End If
            If _MouseX > 650 And _MouseY > 500 And _MouseX < 750 And _MouseY < 600 Then
                If Nivel(2) = 0 Then escena 2 'casilla(2, 1, HeliPuertoX(2), HeliPuertoY(2)).Construccion = 1
                Dimension = 2
                piSo = 1

                If NaveEnElAire = 1 Then Aterrizaje = 1
                Exit Do
            End If
        End If




    Loop




    Zoom = x
    PasarSoldado
End Sub

Sub PasarSoldado
    If Bando = 4 Then Bando = 0: Seleccionado = 0
    originaL = Seleccionado

    otravez:
    Seleccionado = Seleccionado + 1
    If Seleccionado = NrodeSoldados + 1 Then Seleccionado = 1

    If ficha(Bando, Seleccionado).Dimension <> Dimension Then If Seleccionado = originaL Then naranja = 1 Else GoTo otravez
    If ficha(Bando, Seleccionado).VIDA <= 0 Then If Seleccionado = originaL Then Exit Sub Else GoTo otravez
    Funcion = 1
    Bando = 0
    piSo = ficha(Bando, Seleccionado).Piso

    centrar Seleccionado

    Do: i = _MouseInput: Loop Until Not _MouseButton(1)

End Sub












Sub aterrizar Static

    xxxz = (HeliPuertoX(Dimension) + HeliPuertoY(Dimension)) * Zoom + scrollX
    yyyz = ((HeliPuertoY(Dimension) - HeliPuertoX(Dimension)) - difdePiso) * Zoom + scrollY
    centrar 2 'casilla(Dimension, piSo, HeliPuertoX(Dimension), HeliPuertoY(Dimension)).fichaNro

    If a <= 0 Then a = 500
    a = a - 5
    _Delay 0.05

    '_PUTIMAGE (xxxz - 8 * Zoom, yyyz - 9 * Zoom)-(xxxz + 13 * Zoom, yyyz + 14 * Zoom), cosaGrande&(1)
    _PutImage (xxxz - 8 * Zoom, yyyz - 9 * Zoom - a)-(xxxz + 13 * Zoom, yyyz + 14 * Zoom - a), cosaGrande&(1)
    '_DISPLAY
    'Mensaje$ = STR$(a)

    'Mensaje$ = STR$(HeliPuertoX(Dimension)) + STR$(HeliPuertoY(Dimension))


    If a <= 0 Then
        Aterrizaje = 0
        '   Mapavisible = 1
        NaveEnElAire = 0
        naveX = HeliPuertoX(Dimension)
        naveY = HeliPuertoY(Dimension)

        casilla(Dimension, piSo, HeliPuertoX(Dimension), HeliPuertoY(Dimension)).Construccion = 1

        For y = -5 To 8
            For x = -5 To 6
                casilla(Dimension, piSo, naveX + x, naveY + y).fichaNro = Transporte(x, y).fichaNro
                casilla(Dimension, piSo, naveX + x, naveY + y).fichaTipo = Transporte(x, y).fichaTipo
                casilla(Dimension, piSo, naveX + x, naveY + y).Suelo1 = Transporte(x, y).Suelo1
                casilla(Dimension, piSo, naveX + x, naveY + y).Suelo2 = Transporte(x, y).Suelo2
                casilla(Dimension, piSo, naveX + x, naveY + y).Suelo3 = Transporte(x, y).Suelo3
                casilla(Dimension, piSo, naveX + x, naveY + y).CosaEspecial = Transporte(x, y).CosaEspecial

                '           casilla(Dimension, piSo, naveX + x, naveY + y).ParedInvisible = 0
                If casilla(Dimension, piSo, naveX + x, naveY + y).fichaNro <> 0 Then
                    ficha(casilla(Dimension, piSo, naveX + x, naveY + y).fichaTipo - 1, casilla(Dimension, piSo, naveX + x, naveY + y).fichaNro).x = naveX + x
                    ficha(casilla(Dimension, piSo, naveX + x, naveY + y).fichaTipo - 1, casilla(Dimension, piSo, naveX + x, naveY + y).fichaNro).y = naveY + y
                    ficha(casilla(Dimension, piSo, naveX + x, naveY + y).fichaTipo - 1, casilla(Dimension, piSo, naveX + x, naveY + y).fichaNro).rumboX = naveX + 1 + x
                    ficha(casilla(Dimension, piSo, naveX + x, naveY + y).fichaTipo - 1, casilla(Dimension, piSo, naveX + x, naveY + y).fichaNro).rumboY = naveY + 1 + y
                    ficha(casilla(Dimension, piSo, naveX + x, naveY + y).fichaTipo - 1, casilla(Dimension, piSo, naveX + x, naveY + y).fichaNro).Dimension = Dimension

                    '     casilla(Dimension, piSo, naveX + x, naveY + y).fichaTipo = 0
                    '    casilla(Dimension, piSo, naveX + x, naveY + y).fichaNro = 0
                    ' agregar a la lista de soldados que se lleva
                End If
                '  IF casilla(Dimension, piSo, naveX + x, naveY + y).CosaEspecial <> 0 THEN
                'asilla(Dimension, piSo, naveX + x, naveY + y).CosaEspecial = 0

                'agregae
                '  END IF
                '   agregar armas del piso
            Next x
        Next y








    End If
    'SLEEP

End Sub
