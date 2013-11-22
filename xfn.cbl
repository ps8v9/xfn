       IDENTIFICATION DIVISION.
       PROGRAM-ID. XFN.
      * Test program for the XF4 and XF5 procedures, which mimic
      * GNUCOBOL's X"F4" and X"F5" library routines. Some tests are run
      * to compare their relative performance.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Working storage for the XF4 and XF5 procedures.
       01  XFN-COMP-BYTE            BINARY-CHAR.
       01  XFN-COMP-FACTOR          BINARY-SHORT.
       01  XFN-COMP-PRODUCT         BINARY-CHAR.
       01  XFN-COMP-DIVIDEND        BINARY-CHAR.
       01  XFN-COMP-ARRAY.
           02  XFN-COMP-ELEMENT     BINARY-CHAR OCCURS 8.
       01  XFN-DISPLAY-BYTE PIC ZZ9.
       01  XFN-DISPLAY-ARRAY.
           02  XFN-DISPLAY-ELEMENT  PIC 9 OCCURS 8.
       01  XFN-I BINARY-LONG.

      * General working storage for the rest of this program.
       01  WS-ROUTINE  PIC X(10).
       01  WS-TIME1    PIC X(8).
       01  WS-TIME2    PIC X(8).
       01  WS-I        BINARY-LONG.
       01  WS-LIMIT    BINARY-LONG.

       PROCEDURE DIVISION.
      *    Assign an arbitary value to the byte.
           MOVE 42 TO XFN-COMP-BYTE
           MOVE XFN-COMP-BYTE TO XFN-DISPLAY-BYTE

      *    Convert the byte to a table of 8 binary digits.
           DISPLAY "Testing the XF5 procedure:"
           PERFORM XF5 THRU XF5-EXIT
           DISPLAY "  byte: " XFN-DISPLAY-BYTE
           DISPLAY "  bits: " XFN-DISPLAY-ARRAY

      *    Reset the input data.
           MOVE 42 TO XFN-COMP-BYTE
           MOVE XFN-COMP-BYTE TO XFN-DISPLAY-BYTE

      *    Do the same conversion using CALL X"F5".
           DISPLAY 'Testing CALL X"F5":'
           CALL X"F5" USING XFN-COMP-BYTE XFN-COMP-ARRAY
           PERFORM VARYING XFN-I FROM 1 BY 1 UNTIL XFN-I > 8
               MOVE XFN-COMP-ELEMENT(XFN-I)
                 TO XFN-DISPLAY-ELEMENT(XFN-I)
           END-PERFORM
           DISPLAY "  byte: " XFN-DISPLAY-BYTE
           DISPLAY "  bits: " XFN-DISPLAY-ARRAY

      *    Convert the table of binary digits back to a byte.
           DISPLAY "Testing the XF5 procedure:"
           MOVE 0 TO XFN-COMP-BYTE
           PERFORM XF4 THRU XF4-EXIT
           MOVE XFN-COMP-BYTE TO XFN-DISPLAY-BYTE
           DISPLAY "  bits: " XFN-DISPLAY-ARRAY
           DISPLAY "  byte: " XFN-DISPLAY-BYTE

      *    Do the same conversion using CALL X"F4".
           DISPLAY 'Testing CALL X"F4":'
           MOVE 0 TO XFN-COMP-BYTE
           CALL X"F4" USING XFN-COMP-BYTE XFN-COMP-ARRAY
           MOVE XFN-COMP-BYTE TO XFN-DISPLAY-BYTE
           DISPLAY "  bits: " XFN-DISPLAY-ARRAY
           DISPLAY "  byte: " XFN-DISPLAY-BYTE

      *    Run some performance tests.

           MOVE "XF5" TO WS-ROUTINE
           PERFORM BIG-LOOP THRU BIG-LOOP-EXIT

           MOVE 'CALL X"F5"' TO WS-ROUTINE
           PERFORM BIG-LOOP THRU BIG-LOOP-EXIT

           MOVE "XF4" TO WS-ROUTINE
           PERFORM BIG-LOOP THRU BIG-LOOP-EXIT

           MOVE 'CALL X"F4"' TO WS-ROUTINE
           PERFORM BIG-LOOP THRU BIG-LOOP-EXIT

           DISPLAY "Press ENTER to exit ..."
           ACCEPT OMITTED
           GOBACK
           .

      ******************************************************************
      * XF4: Merges 8 binary digits into a single byte. *
      ******************************************************************
       XF4.
           MOVE 0 TO XFN-COMP-BYTE
           MOVE 128 TO XFN-COMP-FACTOR

           PERFORM VARYING XFN-I FROM 1 BY 1 UNTIL XFN-I > 8
               MOVE XFN-DISPLAY-ELEMENT(XFN-I)
                 TO XFN-COMP-ELEMENT(XFN-I)
               MULTIPLY XFN-COMP-FACTOR BY XFN-COMP-ELEMENT(XFN-I)
                 GIVING XFN-COMP-PRODUCT
               ADD XFN-COMP-PRODUCT TO XFN-COMP-BYTE
               DIVIDE 2 INTO XFN-COMP-FACTOR
           END-PERFORM
           .
           XF4-EXIT.
           EXIT
           .

      ******************************************************************
      * XF5: Splits a byte's bits into 8 binary digits. *
      ******************************************************************
       XF5.
           MOVE 128 TO XFN-COMP-DIVIDEND

           PERFORM VARYING XFN-I FROM 1 BY 1 UNTIL XFN-I > 8
               DIVIDE XFN-COMP-DIVIDEND INTO XFN-COMP-BYTE
               GIVING XFN-COMP-ELEMENT(XFN-I) REMAINDER XFN-COMP-BYTE
               DIVIDE 2 INTO XFN-COMP-DIVIDEND
               MOVE XFN-COMP-ELEMENT(XFN-I)
                 TO XFN-DISPLAY-ELEMENT(XFN-I)
           END-PERFORM
           .
           XF5-EXIT.
           EXIT
           .

      ******************************************************************
      * BIG-LOOP: Execute the specified routine WS-LIMIT times. *
      ******************************************************************
       BIG-LOOP.
           MOVE 100000 TO WS-LIMIT
           ACCEPT WS-TIME1 FROM TIME

           EVALUATE WS-ROUTINE
               WHEN = "XF5"
                   PERFORM VARYING WS-I FROM 1 BY 1
                     UNTIL WS-I > WS-LIMIT
                       MOVE 42 TO XFN-COMP-BYTE
                       PERFORM XF5 THRU XF5-EXIT
                   END-PERFORM
               WHEN = 'CALL X"F5"'
                   PERFORM VARYING WS-I FROM 1 BY 1
                     UNTIL WS-I > WS-LIMIT
                       MOVE 42 TO XFN-COMP-BYTE
                       CALL X"F5" USING XFN-COMP-BYTE, XFN-COMP-ARRAY
                   END-PERFORM
               WHEN = "XF4"
                   PERFORM VARYING WS-I FROM 1 BY 1
                     UNTIL WS-I > WS-LIMIT
                       MOVE "0010101010" TO XFN-DISPLAY-ARRAY
                       PERFORM XF4 THRU XF4-EXIT
                   END-PERFORM
               WHEN = 'CALL X"F4"'
                   PERFORM VARYING WS-I FROM 1 BY 1
                     UNTIL WS-I > WS-LIMIT
                       MOVE 42 TO XFN-COMP-BYTE
                       CALL X"F5" USING XFN-COMP-BYTE, XFN-COMP-ARRAY
                   END-PERFORM
               WHEN OTHER
                   DISPLAY "Invalid routine name: " WS-ROUTINE
                   GO TO BIG-LOOP-EXIT
           END-EVALUATE

           ACCEPT WS-TIME2 FROM TIME

           DISPLAY "Start/end times for " WS-LIMIT
                   " iterations of "      WS-ROUTINE ":"
           DISPLAY " " WS-TIME1(1:2) ":" WS-TIME1(3:2) ":"
                       WS-TIME1(5:2) "." WS-TIME1(7:2)
           DISPLAY " " WS-TIME2(1:2) ":" WS-TIME2(3:2) ":"
                       WS-TIME2(5:2) "." WS-TIME2(7:2)
                  .
       BIG-LOOP-EXIT.
           EXIT
           .
