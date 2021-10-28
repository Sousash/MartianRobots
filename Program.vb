Imports System

Module Program
    Public pStrMovements As String() = {"N", "S", "W", "E"}
    Public pStrInstructions As String() = {"R", "L", "F"}
    Public x As Integer = 0
    Public y As Integer = 0
    Public pLostRobots() As String = {}
    Public pIntNumLostRobots As Integer = 0

    Sub Main()
        Dim strMarte(2) As String
        Dim strResponse(2) As String
        Dim intNumRobot As Integer = 1

        'Program finishes when user enter 'exit'
        'Get upper-right coordinates of the rectangular world
        Do
            Console.WriteLine("Type rows and columns: (type 'exit' to finish the program)")
            strMarte = Console.ReadLine().Trim().Split(" ")
        Loop While Not fncValidateResponse(strMarte) And UCase(strMarte(0)) <> "EXIT"

        If UCase(strMarte(0)) <> "EXIT" Then
            Dim strInstructions As String = ""
            Dim boContinue As Boolean = False

            Console.WriteLine("Deploying robots on marte...")

            Do
                boContinue = True
                Console.WriteLine("Robot(" & intNumRobot & ") - Waiting for instructions... (type 'exit' to finish the program)")

                'Read movement
                strResponse = Console.ReadLine().Trim().Split(" ")

                If UCase(strResponse(0)) <> "EXIT" Then
                    If Not fncValidateMovement(strResponse) Then boContinue = False

                    If boContinue Then
                        'Read instructions
                        strInstructions = Trim(Console.ReadLine())

                        If UCase(strInstructions) <> "EXIT" Then

                            'Validating movement and instructions

                            If Not fncValidateInstructions(strInstructions) And UCase(strInstructions) <> "EXIT" Then boContinue = False

                            If boContinue Then subMoveRobot(strMarte, strResponse, strInstructions)
                            intNumRobot = intNumRobot + 1
                        End If
                    End If
                End If
            Loop While UCase(strResponse(0)) <> "EXIT" And UCase(strInstructions) <> "EXIT"
        End If

        Console.WriteLine("End of program.")

    End Sub

    Sub subMoveRobot(StrMarte() As String, StrResponse() As String, StrInstructions As String)
        x = StrResponse(0)
        y = StrResponse(1)
        Dim boContinue As Boolean = True
        Dim strPosition As String = ""
        'Console.WriteLine("x:" & pStrResponse(0) & ", y:" & pStrResponse(1) & " -> " & pStrMarte(pStrResponse(0), pStrResponse(1)))

        'Go over the instructions
        For Each ch As Char In StrInstructions
            If boContinue Then
                'Verificate that position + direction hasn't been done yet due to a lost robot
                If Not fncValidatePosition(x, y, StrResponse(2), ch) Then
                    Select Case UCase(ch)
                        Case "F"
                            subMoveForward(StrResponse(2))
                        Case "L"
                            StrResponse(2) = fncMoveLeft(StrResponse(2))
                        Case "R"
                            StrResponse(2) = fncMoveRight(StrResponse(2))
                    End Select
                    'Validate if a robot has been lost
                    boContinue = fncValidateLostRobot(StrMarte(0), StrMarte(1), x, y, ch, StrResponse(2))
                End If
            End If
        Next


        'Show where the robot has ended
        strPosition = x & " " & y & " " & StrResponse(2) & IIf(Not boContinue, " LOST", "")
        Console.WriteLine(strPosition)

    End Sub

    Sub subMoveForward(StrDirection As String)
        Select Case UCase(StrDirection)
            Case "N"
                y = y + 1
            Case "E"
                x = x + 1
            Case "S"
                y = y - 1
            Case "W"
                x = x - 1
        End Select
    End Sub

    'Validate functions
    Function fncValidateResponse(StrResponse() As String) As Boolean
        fncValidateResponse = False

        If UCase(StrResponse(0)) <> "EXIT" Then
            If StrResponse.Length = 2 Then
                If IsNumeric(Trim(StrResponse(0) & StrResponse(1))) Then
                    If (StrResponse(0) <= 50 And StrResponse(1) <= 50) And (StrResponse(0) > 0 And StrResponse(1) > 0) Then
                        fncValidateResponse = True
                    Else
                        Console.WriteLine("Arguments must be between 0 and 50.")
                    End If
                Else
                    Console.WriteLine("Unexpected type of arguments. Must be an integer.")
                End If
            Else
                Console.WriteLine("Unexpected number of arguments. Try 2. Ex.'5 3'.")
            End If
        End If
    End Function

    Function fncValidateMovement(StrMovements() As String) As Boolean
        fncValidateMovement = False
        'Validate X and Y
        If StrMovements.Length = 3 Then
            If IsNumeric(Trim(StrMovements(0) & StrMovements(1))) Then
                If Not IsNumeric(StrMovements(2)) Then
                    If pStrMovements.Contains(UCase(StrMovements(2))) Then
                        fncValidateMovement = True
                    Else
                        Console.WriteLine("Third parameter must be one of the following letter 'N', 'S', 'E' or 'W'.")
                    End If
                Else
                    Console.WriteLine("Third parameter must be a letter ('N', 'S', 'E', 'W').")
                End If
            Else
                Console.WriteLine("First and second parameter must be an integer.")
            End If
        Else
            Console.WriteLine("Unexpected number of arguments. Try 3. Ex.'1 1 N'.")
        End If

    End Function

    Function fncValidateInstructions(StrInstructions As String) As Boolean
        fncValidateInstructions = True

        If Len(StrInstructions) > 101 Then
            fncValidateInstructions = False
            Console.WriteLine("Limit of instructions to 100. Type less instructions.")
        End If

        For Each ch As Char In StrInstructions
            If Not pStrInstructions.Contains(UCase(ch)) Then
                fncValidateInstructions = False
                Console.WriteLine("Founded invalid instruction: '" & ch & "'.")
            End If
        Next

    End Function
    Function fncValidateLostRobot(StrInitialX As Integer, StrInitialY As Integer, StrX As Integer, StrY As Integer, StrLastMovement As Char, StrLastDirection As String) As Boolean
        fncValidateLostRobot = True

        If StrInitialX < StrX Or StrInitialY < StrY Then
            fncValidateLostRobot = False

            'Reverse last direction
            Select Case UCase(StrLastDirection)
                Case "N"
                    y = y - 1
                Case "E"
                    x = x - 1
                Case "S"
                    y = y + 1
                Case "W"
                    x = x + 1
            End Select

            'Add position + instruction to lost robot array
            pIntNumLostRobots = pIntNumLostRobots + 1
            ReDim Preserve pLostRobots(pIntNumLostRobots)
            'Add X + Y + Direction + Instruction
            pLostRobots(pIntNumLostRobots) = x & " " & y & " " & StrLastDirection & " " & StrLastMovement

        End If
    End Function

    Function fncValidatePosition(StrX As Integer, StrY As Integer, StrDirection As String, StrMovement As Char) As Boolean
        fncValidatePosition = False
        'Go through the pLostRobots array checking that move hasn't been done
        If pLostRobots.Length > 0 Then
            For i As Integer = 0 To pLostRobots.Length - 1
                If pLostRobots(i) = StrX & " " & StrY & " " & StrDirection & " " & StrMovement Then
                    fncValidatePosition = True
                End If
            Next
        End If
    End Function

    'Movement functions
    Function fncMoveLeft(pStrPosition As String) As String
        Select Case UCase(pStrPosition)
            Case "N"
                fncMoveLeft = "W"
            Case "E"
                fncMoveLeft = "N"
            Case "S"
                fncMoveLeft = "E"
            Case "W"
                fncMoveLeft = "S"
        End Select
    End Function

    Function fncMoveRight(pStrPosition As String) As String
        Select Case UCase(pStrPosition)
            Case "N"
                fncMoveRight = "E"
            Case "E"
                fncMoveRight = "S"
            Case "S"
                fncMoveRight = "W"
            Case "W"
                fncMoveRight = "N"
        End Select
    End Function

End Module

