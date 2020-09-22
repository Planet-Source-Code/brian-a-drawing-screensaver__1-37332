VERSION 5.00
Begin VB.Form Form1 
   BackColor       =   &H00000000&
   BorderStyle     =   0  'None
   ClientHeight    =   825
   ClientLeft      =   4830
   ClientTop       =   930
   ClientWidth     =   1785
   ClipControls    =   0   'False
   ControlBox      =   0   'False
   ForeColor       =   &H00808080&
   LinkTopic       =   "Form1"
   ScaleHeight     =   825
   ScaleWidth      =   1785
   ShowInTaskbar   =   0   'False
   Begin VB.Timer Timer1 
      Left            =   0
      Top             =   0
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private Declare Function ShowCursor& Lib "user32" (ByVal bShow As Long)
Private num, num2, num3, ind, xx, yy As Integer
Private color As Long

Private Sub Form_Load()
    If App.PrevInstance = True Then End
    Form1.BackColor = RGB(0, 0, 0)
    Form1.WindowState = 2
    Timer1.Interval = 1
    Form1.BorderStyle = 0
    Form1.Caption = "Screen Saver"
    xx = 4000
    yy = 8000
    ind = 0
    ShowCursor (0)
End Sub

Private Sub Form_Click()
    ShowCursor (1)
    Timer1.Enabled = False
    End
End Sub

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
    ShowCursor (1)
    Timer1.Enabled = False
    End
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If num < 3 Then
        X = 0
        Y = 0
    ElseIf num >= 3 Then
       ShowCursor (1)
        Timer1.Enabled = False
        End
    End If
    num = num + 1
End Sub

Private Sub Timer1_Timer()
    'House
    If ind = 0 Then
        PSet (xx, yy), RGB(200, 200, 200)
        xx = xx + 15
        If xx >= 6000 Then
            ind = 1
        End If
    ElseIf ind = 1 Then
        PSet (xx, yy), RGB(200, 200, 200)
        yy = yy - 15
        If yy <= 6500 Then
            ind = 2
        End If
    ElseIf ind = 2 Then
        PSet (xx, yy), RGB(200, 200, 200)
        xx = xx - 15
        If xx <= 4000 Then
            ind = 3
        End If
    ElseIf ind = 3 Then
        PSet (xx, yy), RGB(200, 200, 200)
        yy = yy + 15
        If yy >= 8000 Then
            xx = 4000
            ind = 4
        End If
    ElseIf ind = 4 Then
        Line (xx, 6500)-(xx, 8000), RGB(150, 150, 150)
        xx = xx + 15
        If xx >= 6000 Then
            Line (4820, 7200)-(5120, 8000), RGB(52, 52, 11), BF
            xx = 4000
            yy = 6500
            ind = 5
        End If
    'Roof
    ElseIf ind = 5 Then
        PSet (xx, yy), RGB(200, 200, 200)
        yy = yy - 15
        xx = xx + 15
        If xx >= 5000 Then
            ind = 6
        End If
    ElseIf ind = 6 Then
        PSet (xx, yy), RGB(200, 200, 200)
        yy = yy + 15
        xx = xx + 15
        If xx >= 6000 Then
            yy = 6485
            xx = 4015
            num2 = 1985
            ind = 7
        End If
    ElseIf ind = 7 Then
        Line (xx, yy)-(xx + num2, yy), RGB(120, 120, 120)
        yy = yy - 15
        xx = xx + 15
        num2 = num2 - 30
        If yy <= 5500 Then
            Line (5300, 7300)-(5600, 7600), RGB(0, 46, 76), BF
            xx = 0
            yy = 8000
            ind = 8
        End If
    'Grass
    ElseIf ind = 8 Then
        Line (xx, yy)-(xx + 120, yy + 3000), RGB(0, 120, 0), BF
        xx = xx + 60
        If xx >= 11900 Then
            xx = 9400
            yy = 7800
            ind = 9
        End If
'** Bulldozer
    ElseIf ind = 9 Then
        FillStyle = vbFSSolid
        FillColor = RGB(120, 120, 0)
        Circle (xx, yy), 230, RGB(120, 120, 0)
        xx = xx + (130 / 2)
        If xx >= 10130 Then
            xx = 9400
            yy = 7450
            ind = 10
        End If
    ElseIf ind = 10 Then
        Line (xx, yy)-(xx, yy + 200), RGB(130, 130, 0)
        xx = xx + 15
        If xx >= 10100 Then
            xx = 9560
            yy = 7225
            ind = 11
        End If
    ElseIf ind = 11 Then
        Line (xx, yy)-(xx, yy + 270), RGB(150, 150, 0)
        xx = xx + 15
        If xx >= 10000 Then
            xx = 9160
            yy = 7345
            ind = 12
        End If
    ElseIf ind = 12 Then
        Line (xx, yy)-(xx, yy + 665), RGB(100, 100, 100)
        xx = xx - 15
        If xx <= 9090 Then
            xx = 8990
            ind = 13
        End If
    'Vvrrooomm!!
    ElseIf ind = 13 Then
        PSet (9000, 6500), RGB(0, 0, 0)
        ForeColor = RGB(200, 12, 21)
        FontSize = 24
        Print "Vvrrooomm!!"
        xx = xx + 1
        If xx >= 9020 Then
            Line (9000, 6500)-(12000, 6990), RGB(0, 0, 0), BF
            Line (5300, 7300)-(5600, 7600), RGB(255, 255, 0), BF
            xx = 5120
            yy = 7200
            ind = 14
        End If
    'Lights on
    ElseIf ind = 14 Then
        Line (xx, yy)-(xx, yy + 800), RGB(255, 255, 0)
        xx = xx - 15
        If xx <= 4820 Then
            ind = 15
        End If
    ElseIf ind = 15 Then
        Line (xx, yy)-(xx, yy + 800), RGB(52, 52, 11)
        xx = xx - 15
        If xx <= 4520 Then
            xx = 4950
            yy = 7450
            ind = 16
        End If
    'Guy
    ElseIf ind = 16 Then
        FillColor = RGB(230, 230, 120)
        Circle (xx, yy), 100, RGB(230, 230, 120)
        xx = 4950
        yy = 7530
        ind = 17
    ElseIf ind = 17 Then
        PSet (xx, yy), RGB(0, 130, 120)
        yy = yy + 15
        If yy >= 7800 Then
            ind = 18
        End If
    ElseIf ind = 18 Then
        PSet (xx, yy), RGB(0, 46, 76)
        yy = yy + 15
        xx = xx + 15
        If yy >= 8000 Then
            yy = 7800
            xx = 4950
            ind = 19
        End If
    ElseIf ind = 19 Then
        PSet (xx, yy), RGB(0, 46, 76)
        yy = yy + 15
        xx = xx - 15
        If yy >= 8000 Then
            xx = 4950
            yy = 7630
            ind = 20
        End If
    ElseIf ind = 20 Then
        PSet (xx, yy), RGB(0, 130, 120)
        yy = yy + 15
        xx = xx - 15
        If yy >= 7800 Then
            xx = 4950
            yy = 7630
            ind = 21
        End If
    ElseIf ind = 21 Then
        PSet (xx, yy), RGB(0, 130, 120)
        yy = yy + 15
        xx = xx + 15
        If yy >= 7800 Then
            xx = 4960
            yy = 7500
            ind = 22
        End If
    ElseIf ind = 22 Then
        FillColor = RGB(0, 0, 0)
        Circle (xx, yy), 20, RGB(0, 0, 0)
        xx = 9400
        yy = 7800
        Line (7000, 5000)-(11000, 8000), RGB(0, 0, 0), BF
        ind = 23
    'Bulldozer on the move!!
    ElseIf ind = 23 Then
        Line (xx + 100, yy - 320)-(xx + 600, yy - 550), RGB(0, 0, 0), BF
        Line (xx - 100, yy - 125)-(xx + 790, yy - 305), RGB(0, 0, 0), BF
        Line (xx - 270, yy - 455)-(xx - 320, yy + 225), RGB(0, 0, 0), BF
        Circle (xx, yy), 230, RGB(0, 0, 0)
        Circle (xx + 130, yy), 230, RGB(0, 0, 0)
        Circle (xx + 260, yy), 230, RGB(0, 0, 0)
        Circle (xx + 390, yy), 230, RGB(0, 0, 0)
        Circle (xx + 420, yy), 230, RGB(0, 0, 0)
        Circle (xx + 550, yy), 230, RGB(0, 0, 0)
        Circle (xx + 680, yy), 230, RGB(0, 0, 0)
        Circle (xx + 20, yy), 230, RGB(0, 0, 0)
        xx = xx - 15
        FillStyle = vbFSSolid
        FillColor = RGB(120, 120, 0)
        Circle (xx, yy), 230, RGB(120, 120, 0)
        Circle (xx + 130, yy), 230, RGB(120, 120, 0)
        Circle (xx + 260, yy), 230, RGB(120, 120, 0)
        Circle (xx + 390, yy), 230, RGB(120, 120, 0)
        Circle (xx + 420, yy), 230, RGB(120, 120, 0)
        Circle (xx + 550, yy), 230, RGB(120, 120, 0)
        Circle (xx + 680, yy), 230, RGB(120, 120, 0)
        Line (xx + 100, yy - 320)-(xx + 600, yy - 550), RGB(150, 150, 0), BF
        Line (xx - 100, yy - 125)-(xx + 790, yy - 305), RGB(130, 130, 0), BF
        Line (xx - 270, yy - 455)-(xx - 320, yy + 225), RGB(100, 100, 100), BF
        If xx <= 6400 Then
            ind = 24
        End If
    ElseIf ind = 24 Then
        Line (4820, 7200)-(5120, 8000), RGB(255, 255, 0), BF
        xx = 4000
        yy = 5400
        ind = 25
    ElseIf ind = 25 Then
        Line (xx, yy)-(xx + 2030, yy), RGB(0, 0, 0)
        yy = yy + 15
        FillStyle = vbFSTransparent
        Circle (4500, 7600), 120, RGB(89, 89, 0)
        Circle (4600, 7700), 120, RGB(89, 89, 0)
        Circle (4300, 7600), 120, RGB(89, 89, 0)
        Circle (5300, 7700), 120, RGB(89, 89, 0)
        Circle (5500, 7900), 120, RGB(89, 89, 0)
        Circle (4100, 7900), 120, RGB(89, 89, 0)
        If yy >= 8000 Then
            FillStyle = vbFSSolid
            FillColor = RGB(0, 0, 0)
            Circle (4500, 7600), 120, RGB(0, 0, 0)
            Circle (4600, 7700), 120, RGB(0, 0, 0)
            Circle (4300, 7600), 120, RGB(0, 0, 0)
            Circle (5300, 7700), 120, RGB(0, 0, 0)
            Circle (5500, 7900), 120, RGB(0, 0, 0)
            Circle (4100, 7900), 120, RGB(0, 0, 0)
            FillColor = RGB(89, 89, 0)
            Circle (4500, 8000), 60, RGB(89, 89, 0)
            Circle (4600, 8000), 60, RGB(89, 89, 0)
            Circle (4300, 8000), 60, RGB(89, 89, 0)
            Circle (5300, 8000), 60, RGB(89, 89, 0)
            Circle (5500, 8000), 60, RGB(89, 89, 0)
            Circle (4100, 8000), 60, RGB(89, 89, 0)
            Circle (4000, 8000), 60, RGB(89, 89, 0)
            Circle (4900, 8000), 60, RGB(89, 89, 0)
            Circle (5900, 8000), 60, RGB(89, 89, 0)
            Circle (5100, 8000), 60, RGB(89, 89, 0)
            Circle (5700, 8000), 60, RGB(89, 89, 0)
            Circle (4200, 8000), 60, RGB(89, 89, 0)
            Line (4000, 7980)-(6000, 8000), RGB(89, 89, 0), BF
            ind = 26
        End If
    ElseIf ind = 26 Then
        FillColor = RGB(89, 89, 0)
        Circle (4500, 8000), 60, RGB(89, 89, 0)
        Circle (4600, 8000), 60, RGB(89, 89, 0)
        Circle (4300, 8000), 60, RGB(89, 89, 0)
        Circle (5300, 8000), 60, RGB(89, 89, 0)
        Circle (5500, 8000), 60, RGB(89, 89, 0)
        Circle (4100, 8000), 60, RGB(89, 89, 0)
        Circle (4000, 8000), 60, RGB(89, 89, 0)
        Circle (4900, 8000), 60, RGB(89, 89, 0)
        Circle (5900, 8000), 60, RGB(89, 89, 0)
        Circle (5100, 8000), 60, RGB(89, 89, 0)
        Circle (5700, 8000), 60, RGB(89, 89, 0)
        Circle (4200, 8000), 60, RGB(89, 89, 0)
        Line (4000, 7980)-(6000, 8000), RGB(89, 89, 0), BF
        ind = 27
        xx = 6400
        yy = 7800
    ElseIf ind = 27 Then
        Line (xx + 100, yy - 320)-(xx + 600, yy - 550), RGB(0, 0, 0), BF
        Line (xx - 100, yy - 125)-(xx + 790, yy - 305), RGB(0, 0, 0), BF
        Line (xx - 270, yy - 455)-(xx - 320, yy + 225), RGB(0, 0, 0), BF
        Circle (xx, yy), 230, RGB(0, 0, 0)
        Circle (xx + 130, yy), 230, RGB(0, 0, 0)
        Circle (xx + 260, yy), 230, RGB(0, 0, 0)
        Circle (xx + 390, yy), 230, RGB(0, 0, 0)
        Circle (xx + 420, yy), 230, RGB(0, 0, 0)
        Circle (xx + 550, yy), 230, RGB(0, 0, 0)
        Circle (xx + 680, yy), 230, RGB(0, 0, 0)
        Circle (xx + 20, yy), 230, RGB(0, 0, 0)
        xx = xx - 15
        FillStyle = vbFSSolid
        FillColor = RGB(120, 120, 0)
        Circle (xx, yy), 230, RGB(120, 120, 0)
        Circle (xx + 130, yy), 230, RGB(120, 120, 0)
        Circle (xx + 260, yy), 230, RGB(120, 120, 0)
        Circle (xx + 390, yy), 230, RGB(120, 120, 0)
        Circle (xx + 420, yy), 230, RGB(120, 120, 0)
        Circle (xx + 550, yy), 230, RGB(120, 120, 0)
        Circle (xx + 680, yy), 230, RGB(120, 120, 0)
        Line (xx + 100, yy - 320)-(xx + 600, yy - 550), RGB(150, 150, 0), BF
        Line (xx - 100, yy - 125)-(xx + 790, yy - 305), RGB(130, 130, 0), BF
        Line (xx - 270, yy - 455)-(xx - 320, yy + 225), RGB(100, 100, 100), BF
        If xx <= -1000 Then
            xx = 0
            yy = 8000
            ind = 28
        End If
    ElseIf ind = 28 Then
        Line (xx, yy)-(xx + 120, yy + 3000), RGB(0, 120, 0), BF
        xx = xx + 60
        If xx >= 11900 Then
            xx = 1200
            yy = 8000
            ind = 29
            color = RGB(0, 0, 0)
            num3 = 10
        End If
    'SUNRISE
    ElseIf ind = 29 Then
        FillColor = color
        Circle (xx, yy), 500, color
        Line (0, 8000)-(12000, 10000), RGB(0, 120, 0), BF
        xx = xx + 15
        yy = yy - 60
        FillColor = RGB(255, 255, 0)
        Circle (xx, yy), 500, RGB(255, 255, 0)
        color = RGB(0, 0, num3)
        num3 = num3 + 1
        Line (0, 0)-(12000, 7999), RGB(0, 0, num3), BF
        FillColor = RGB(255, 255, 0)
        Circle (xx, yy), 500, RGB(255, 255, 0)
        If num3 >= 247 Then
            num3 = 249
        End If
        If yy <= 1200 Then
            xx = 3000
            yy = 7800
            num2 = 1000
            Line (3350, 8000)-(3600, 7800), &H4080&, BF
            ind = 30
        End If
    'Trees
    ElseIf ind = 30 Then
        Line (xx, yy)-(xx + num2, yy), RGB(0, 190, 0)
        yy = yy - 15
        xx = xx + 3.75
        num2 = num2 - 7.5
        If yy <= 5850 Then
            xx = 11000
            yy = 7800
            num2 = 700
            Line (11200, 8000)-(11400, 7800), &H4080&, BF
            ind = 31
        End If
    ElseIf ind = 31 Then
        Line (xx, yy)-(xx + num2, yy), RGB(0, 110, 70)
        yy = yy - 15
        xx = xx + 7.5
        num2 = num2 - 15
        If yy <= 7080 Then
            xx = 100
            yy = 7800
            num2 = 700
            Line (250, 8000)-(450, 7800), &H4080&, BF
            ind = 32
        End If
    ElseIf ind = 32 Then
        Line (xx, yy)-(xx + num2, yy), RGB(40, 180, 10)
        yy = yy - 15
        xx = xx + 7.5
        num2 = num2 - 15
        If yy <= 7080 Then
            xx = 8500
            yy = 7800
            num2 = 1000
            Line (8700, 8000)-(8900, 7800), &H4080&, BF
            ind = 33
        End If
    ElseIf ind = 33 Then
        Line (xx, yy)-(xx + num2, yy), RGB(45, 200, 72)
        yy = yy - 15
        xx = xx + 3.75
        num2 = num2 - 7.5
        If yy <= 5850 Then
            xx = 5000
            yy = 7800
            ind = 34
        End If
    'Here comes the dozer
    ElseIf ind = 34 Then
        FillStyle = vbFSSolid
        FillColor = RGB(120, 120, 0)
        Circle (xx, yy), 230, RGB(120, 120, 0)
        xx = xx + (130 / 2)
        If xx >= 6130 Then
            xx = 5000
            yy = 7450
            ind = 35
        End If
    ElseIf ind = 35 Then
        Line (xx, yy)-(xx, yy + 200), RGB(130, 130, 0)
        xx = xx + 15
        If xx >= 6100 Then
            xx = 5160
            yy = 7225
            ind = 36
        End If
    ElseIf ind = 36 Then
        Line (xx, yy)-(xx, yy + 270), RGB(150, 150, 0)
        xx = xx + 15
        If xx >= 5900 Then
            xx = 4760
            yy = 7345
            ind = 37
        End If
    ElseIf ind = 37 Then
        Line (xx, yy)-(xx, yy + 665), RGB(100, 100, 100)
        xx = xx + 15
        If xx >= 4790 Then
            xx = 5000
            yy = 3000
            ind = 38
        End If
    ElseIf ind = 38 Then
        FillColor = RGB(29, 0, 29)
        Circle (xx, yy), 450, RGB(29, 0, 29)
        xx = xx + 35
        If xx >= 5600 Then
            ind = 39
        End If
    ElseIf ind = 39 Then
        Line (4500, 2480)-(6100, 2400), RGB(120, 120, 120), BF
        xx = 5000
        ind = 40
    ElseIf ind = 40 Then
        Line (xx, 2800)-(xx, 3200), RGB(29, 0, 29)
        xx = xx - 15
        If xx <= 4000 Then
            FillColor = RGB(29, 0, 29)
            Circle (4000, 3000), 200, RGB(29, 0, 29)
            FillColor = RGB(120, 120, 120)
            Circle (3680, 3000), 300, RGB(120, 120, 120)
            xx = 5800
            yy = 3500
            ind = 41
        End If
    ElseIf ind = 41 Then
        FillColor = color
        Circle (xx, yy), 90, color
        yy = yy + 30
        FillColor = RGB(0, 0, 0)
        Circle (xx, yy), 90, RGB(0, 0, 0)
        If yy >= 7600 Then
            ind = 42
            num3 = 0
            num2 = 15
            color = RGB(num3, 0, 0)
        End If
    ElseIf ind = 42 Then
        num3 = num3 + 1
        num2 = num2 + 15
        color = RGB(num3, 0, 0)
        FillStyle = vbFSTransparent
        Circle (xx, yy), num2, color
        If num2 >= 6000 Then
            Line (0, 0)-(12000, 10000), RGB(255, 20, 0), BF
            xx = 4000
            yy = 8000
            Line (0, 0)-(12000, 10000), RGB(0, 0, 0), BF
            ind = 0
        End If
    End If
End Sub
