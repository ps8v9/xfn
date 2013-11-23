       IDENTIFICATION DIVISION.
       PROGRAM-ID. XFN.
      * Test program for the XF4 and XF5 procedures, which mimic GNU
      * COBOL's X"F4" and X"F5" library routines. Some tests are run to
      * compare their relative performance.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Lookup table for bit patterns.
       01  BIT-TABLE.
           02 BIT-REC  OCCURS 256 TIMES
                       ASCENDING KEY IS BIT-KEY
                       INDEXED BY BIT-IDX.
              03  BIT-KEY  BINARY-SHORT.
              03  BIT-VAL  PIC X(8).

      * Lookup table for byte values.
       01  BYTE-TABLE.
           02 BYTE-REC  OCCURS 256 TIMES
                        ASCENDING KEY IS BYTE-KEY
                        INDEXED BY BYTE-IDX.
              03  BYTE-KEY  PIC X(8).
              03  BYTE-VAL  BINARY-SHORT.

      * Some fields for lookups and lookup results.
       01  WRK-BIT-VAL  PIC X(8).
       01  WRK-BYTE-VAL BINARY-SHORT.

      * Loop indexes and counters.
       01  I        BINARY-LONG.
       01  J           BINARY-LONG.
       01  LOOKUPS  BINARY-LONG VALUE 0.
       01  MATCHES  BINARY-LONG VALUE 0.

      **** OLD WORKING STORAGE BELOW *******************
      * Working storage for the XF4 and XF5 procedures.
       01  XFN-COMP-BYTE            BINARY-CHAR.
       01  XFN-COMP-FACTOR          BINARY-SHORT.
       01  XFN-COMP-PRODUCT         BINARY-CHAR.
       01  XFN-COMP-DIVIDEND        BINARY-CHAR.
       01  XFN-COMP-ARRAY.
           02  XFN-COMP-ELEMENT     BINARY-CHAR OCCURS 8.
       01  XFN-DISPLAY-BYTE         PIC ZZ9.
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
           PERFORM FILL-BIT-TABLE THRU FILL-BIT-TABLE-EXIT
           PERFORM FILL-BYTE-TABLE THRU FILL-BYTE-TABLE-EXIT

      *    Convert a byte to a table of 8 binary digits using XF5.
           MOVE 42 TO XFN-COMP-BYTE
           MOVE XFN-COMP-BYTE TO XFN-DISPLAY-BYTE
           DISPLAY "Testing the XF5 procedure:"
           PERFORM XF5 THRU XF5-EXIT
           DISPLAY "  byte: " XFN-DISPLAY-BYTE
           DISPLAY "  bits: " XFN-DISPLAY-ARRAY

      *    Do the same conversion using FAST-XF5.
           MOVE 42 TO WRK-BYTE-VAL
           MOVE WRK-BYTE-VAL TO XFN-DISPLAY-BYTE
           DISPLAY "Testing the FAST-XF5 procedure:"
           PERFORM FAST-XF5 THRU FAST-XF5-EXIT
           DISPLAY "  byte: " XFN-DISPLAY-BYTE
           DISPLAY "  bits: " WRK-BIT-VAL

      *    Do the same conversion using CALL X"F5".
           MOVE 42 TO XFN-COMP-BYTE
           DISPLAY 'Testing CALL X"F5":'
           CALL X"F5" USING XFN-COMP-BYTE XFN-COMP-ARRAY
           PERFORM VARYING XFN-I FROM 1 BY 1 UNTIL XFN-I > 8
               MOVE XFN-COMP-ELEMENT(XFN-I)
                 TO XFN-DISPLAY-ELEMENT(XFN-I)
           END-PERFORM
           DISPLAY "  byte: " XFN-DISPLAY-BYTE
           DISPLAY "  bits: " XFN-DISPLAY-ARRAY

      *    Convert the table of binary digits back to a byte using XF4.
           DISPLAY "Testing the XF4 procedure:"
           MOVE 0 TO XFN-COMP-BYTE
           PERFORM XF4 THRU XF4-EXIT
           MOVE XFN-COMP-BYTE TO XFN-DISPLAY-BYTE
           DISPLAY "  bits: " XFN-DISPLAY-ARRAY
           DISPLAY "  byte: " XFN-DISPLAY-BYTE

      *    Do the same using XF4.
           DISPLAY "Testing the FAST-XF4 procedure:"
           MOVE 0 TO WRK-BYTE-VAL
           PERFORM FAST-XF4 THRU FAST-XF4-EXIT
           MOVE WRK-BYTE-VAL TO XFN-DISPLAY-BYTE
           DISPLAY "  bits: " WRK-BIT-VAL
           DISPLAY "  byte: " XFN-DISPLAY-BYTE

      *    Do the same using CALL X"F4".
           DISPLAY 'Testing CALL X"F4":'
           MOVE 0 TO XFN-COMP-BYTE
           CALL X"F4" USING XFN-COMP-BYTE XFN-COMP-ARRAY
           MOVE XFN-COMP-BYTE TO XFN-DISPLAY-BYTE
           DISPLAY "  bits: " XFN-DISPLAY-ARRAY
           DISPLAY "  byte: " XFN-DISPLAY-BYTE

      *    Run some performance tests.

           MOVE "XF5" TO WS-ROUTINE
           PERFORM BIG-LOOP THRU BIG-LOOP-EXIT

           MOVE "FAST-XF5" TO WS-ROUTINE
           PERFORM BIG-LOOP THRU BIG-LOOP-EXIT

           MOVE 'CALL X"F5"' TO WS-ROUTINE
           PERFORM BIG-LOOP THRU BIG-LOOP-EXIT

           MOVE "XF4" TO WS-ROUTINE
           PERFORM BIG-LOOP THRU BIG-LOOP-EXIT

           MOVE "FAST-XF4" TO WS-ROUTINE
           PERFORM BIG-LOOP THRU BIG-LOOP-EXIT

           MOVE 'CALL X"F4"' TO WS-ROUTINE
           PERFORM BIG-LOOP THRU BIG-LOOP-EXIT

           DISPLAY "Press ENTER to exit ..."
           ACCEPT OMITTED
           GOBACK
           .

      ******************************************************************
      * XF4: Merges 8 binary digits into a single byte.                *
      ******************************************************************
       XF4.
           PERFORM VARYING XFN-I FROM 1 BY 1 UNTIL XFN-I > 8
               MOVE XFN-DISPLAY-ELEMENT(XFN-I)
                 TO XFN-COMP-ELEMENT(XFN-I)
           END-PERFORM

           MOVE XFN-COMP-ELEMENT(8) TO XFN-COMP-BYTE
           IF XFN-COMP-ELEMENT(7) = 1  ADD   2 TO XFN-COMP-BYTE  END-IF
           IF XFN-COMP-ELEMENT(6) = 1  ADD   4 TO XFN-COMP-BYTE  END-IF
           IF XFN-COMP-ELEMENT(5) = 1  ADD   8 TO XFN-COMP-BYTE  END-IF
           IF XFN-COMP-ELEMENT(4) = 1  ADD  16 TO XFN-COMP-BYTE  END-IF
           IF XFN-COMP-ELEMENT(3) = 1  ADD  32 TO XFN-COMP-BYTE  END-IF
           IF XFN-COMP-ELEMENT(2) = 1  ADD  64 TO XFN-COMP-BYTE  END-IF
           IF XFN-COMP-ELEMENT(1) = 1  ADD 128 TO XFN-COMP-BYTE  END-IF
           .
       XF4-EXIT.
           EXIT
           .

      ******************************************************************
      * FAST-XF4: Looks up the byte value for a PIC X(8) bit pattern.  *
      *           Uses WRK-BIT-VAL for the lookup. Sets WRK-BYTE-VAL.  *
      ******************************************************************
       FAST-XF4.
           SEARCH ALL BYTE-REC
               WHEN BYTE-KEY(BYTE-IDX) = WRK-BIT-VAL
                   MOVE BYTE-VAL(BIT-IDX) TO WRK-BYTE-VAL
           END-SEARCH
           .
       FAST-XF4-EXIT.
           EXIT
           .

      ******************************************************************
      * XF5: Splits a byte's bits into 8 binary digits.                *
      ******************************************************************
       XF5.
           PERFORM VARYING XFN-I FROM 1 BY 1 UNTIL XFN-I > 8
               MOVE 0 TO XFN-COMP-ELEMENT(XFN-I)
           END-PERFORM

           IF XFN-COMP-BYTE > 127
               MOVE 1 TO XFN-COMP-ELEMENT(1)
               SUBTRACT 128 FROM XFN-COMP-BYTE
           END-IF
           IF XFN-COMP-BYTE > 63
               MOVE 1 TO XFN-COMP-ELEMENT(2)
               SUBTRACT 64 FROM XFN-COMP-BYTE
           END-IF
           IF XFN-COMP-BYTE > 31
               MOVE 1 TO XFN-COMP-ELEMENT(3)
               SUBTRACT 32 FROM XFN-COMP-BYTE
           END-IF
           IF XFN-COMP-BYTE > 15
               MOVE 1 TO XFN-COMP-ELEMENT(4)
               SUBTRACT 16 FROM XFN-COMP-BYTE
           END-IF
           IF XFN-COMP-BYTE > 7
               MOVE 1 TO XFN-COMP-ELEMENT(5)
               SUBTRACT 8 FROM XFN-COMP-BYTE
           END-IF
           IF XFN-COMP-BYTE > 3
               MOVE 1 TO XFN-COMP-ELEMENT(6)
               SUBTRACT 4 FROM XFN-COMP-BYTE
           END-IF
           IF XFN-COMP-BYTE > 1
               MOVE 1 TO XFN-COMP-ELEMENT(7)
               SUBTRACT 2 FROM XFN-COMP-BYTE
           END-IF
           IF XFN-COMP-BYTE > 0
               MOVE 1 TO XFN-COMP-ELEMENT(8)
           END-IF

           PERFORM VARYING XFN-I FROM 1 BY 1 UNTIL XFN-I > 8
               MOVE XFN-COMP-ELEMENT(XFN-I)
                 TO XFN-DISPLAY-ELEMENT(XFN-I)
           END-PERFORM
           .
       XF5-EXIT.
           EXIT
           .

      ******************************************************************
      * FAST-XF5: Looks up the PIC X(8) bit pattern for a byte value.  *
      *           Uses WRK-BYTE-VAL for the lookup. Sets WRK-BIT-VAL.  *
      ******************************************************************
       FAST-XF5.
           SEARCH ALL BIT-REC
               WHEN BIT-KEY(BIT-IDX) = WRK-BYTE-VAL
                   MOVE BIT-VAL(BIT-IDX) TO WRK-BIT-VAL
           END-SEARCH
           .
       FAST-XF5-EXIT.
           EXIT
           .

      ******************************************************************
      * BIG-LOOP: Execute the specified routine WS-LIMIT times. *
      ******************************************************************
       BIG-LOOP.
           MOVE 1000000 TO WS-LIMIT
           ACCEPT WS-TIME1 FROM TIME

           EVALUATE WS-ROUTINE
               WHEN = "XF5"
                   PERFORM VARYING WS-I FROM 1 BY 1
                     UNTIL WS-I > WS-LIMIT
                       MOVE 42 TO XFN-COMP-BYTE
                       PERFORM XF5 THRU XF5-EXIT
                   END-PERFORM
               WHEN = "FAST-XF5"
                   PERFORM VARYING WS-I FROM 1 BY 1
                     UNTIL WS-I > WS-LIMIT
                       MOVE 42 TO WRK-BYTE-VAL
                       PERFORM FAST-XF5 THRU FAST-XF5-EXIT
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
               WHEN = "FAST-XF4"
                   PERFORM VARYING WS-I FROM 1 BY 1
                     UNTIL WS-I > WS-LIMIT
                       MOVE "0010101010" TO WRK-BIT-VAL
                       PERFORM FAST-XF4 THRU FAST-XF4-EXIT
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

       FILL-BIT-TABLE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 256
               COMPUTE BIT-KEY(I) = I - 1
               EVALUATE BIT-KEY(I)
                   WHEN H"00" MOVE "00000000" TO BIT-VAL(I)
                   WHEN H"01" MOVE "00000001" TO BIT-VAL(I)
                   WHEN H"02" MOVE "00000010" TO BIT-VAL(I)
                   WHEN H"03" MOVE "00000011" TO BIT-VAL(I)
                   WHEN H"04" MOVE "00000100" TO BIT-VAL(I)
                   WHEN H"05" MOVE "00000101" TO BIT-VAL(I)
                   WHEN H"06" MOVE "00000110" TO BIT-VAL(I)
                   WHEN H"07" MOVE "00000111" TO BIT-VAL(I)
                   WHEN H"08" MOVE "00001000" TO BIT-VAL(I)
                   WHEN H"09" MOVE "00001001" TO BIT-VAL(I)
                   WHEN H"0a" MOVE "00001010" TO BIT-VAL(I)
                   WHEN H"0b" MOVE "00001011" TO BIT-VAL(I)
                   WHEN H"0c" MOVE "00001100" TO BIT-VAL(I)
                   WHEN H"0d" MOVE "00001101" TO BIT-VAL(I)
                   WHEN H"0e" MOVE "00001110" TO BIT-VAL(I)
                   WHEN H"0f" MOVE "00001111" TO BIT-VAL(I)

                   WHEN H"10" MOVE "00010000" TO BIT-VAL(I)
                   WHEN H"11" MOVE "00010001" TO BIT-VAL(I)
                   WHEN H"12" MOVE "00010010" TO BIT-VAL(I)
                   WHEN H"13" MOVE "00010011" TO BIT-VAL(I)
                   WHEN H"14" MOVE "00010100" TO BIT-VAL(I)
                   WHEN H"15" MOVE "00010101" TO BIT-VAL(I)
                   WHEN H"16" MOVE "00010110" TO BIT-VAL(I)
                   WHEN H"17" MOVE "00010111" TO BIT-VAL(I)
                   WHEN H"18" MOVE "00011000" TO BIT-VAL(I)
                   WHEN H"19" MOVE "00011001" TO BIT-VAL(I)
                   WHEN H"1a" MOVE "00011010" TO BIT-VAL(I)
                   WHEN H"1b" MOVE "00011011" TO BIT-VAL(I)
                   WHEN H"1c" MOVE "00011100" TO BIT-VAL(I)
                   WHEN H"1d" MOVE "00011101" TO BIT-VAL(I)
                   WHEN H"1e" MOVE "00011110" TO BIT-VAL(I)
                   WHEN H"1f" MOVE "00011111" TO BIT-VAL(I)

                   WHEN H"20" MOVE "00100000" TO BIT-VAL(I)
                   WHEN H"21" MOVE "00100001" TO BIT-VAL(I)
                   WHEN H"22" MOVE "00100010" TO BIT-VAL(I)
                   WHEN H"23" MOVE "00100011" TO BIT-VAL(I)
                   WHEN H"24" MOVE "00100100" TO BIT-VAL(I)
                   WHEN H"25" MOVE "00100101" TO BIT-VAL(I)
                   WHEN H"26" MOVE "00100110" TO BIT-VAL(I)
                   WHEN H"27" MOVE "00100111" TO BIT-VAL(I)
                   WHEN H"28" MOVE "00101000" TO BIT-VAL(I)
                   WHEN H"29" MOVE "00101001" TO BIT-VAL(I)
                   WHEN H"2a" MOVE "00101010" TO BIT-VAL(I)
                   WHEN H"2b" MOVE "00101011" TO BIT-VAL(I)
                   WHEN H"2c" MOVE "00101100" TO BIT-VAL(I)
                   WHEN H"2d" MOVE "00101101" TO BIT-VAL(I)
                   WHEN H"2e" MOVE "00101110" TO BIT-VAL(I)
                   WHEN H"2f" MOVE "00101111" TO BIT-VAL(I)

                   WHEN H"30" MOVE "00110000" TO BIT-VAL(I)
                   WHEN H"31" MOVE "00110001" TO BIT-VAL(I)
                   WHEN H"32" MOVE "00110010" TO BIT-VAL(I)
                   WHEN H"33" MOVE "00110011" TO BIT-VAL(I)
                   WHEN H"34" MOVE "00110100" TO BIT-VAL(I)
                   WHEN H"35" MOVE "00110101" TO BIT-VAL(I)
                   WHEN H"36" MOVE "00110110" TO BIT-VAL(I)
                   WHEN H"37" MOVE "00110111" TO BIT-VAL(I)
                   WHEN H"38" MOVE "00111000" TO BIT-VAL(I)
                   WHEN H"39" MOVE "00111001" TO BIT-VAL(I)
                   WHEN H"3a" MOVE "00111010" TO BIT-VAL(I)
                   WHEN H"3b" MOVE "00111011" TO BIT-VAL(I)
                   WHEN H"3c" MOVE "00111100" TO BIT-VAL(I)
                   WHEN H"3d" MOVE "00111101" TO BIT-VAL(I)
                   WHEN H"3e" MOVE "00111110" TO BIT-VAL(I)
                   WHEN H"3f" MOVE "00111111" TO BIT-VAL(I)

                   WHEN H"40" MOVE "01000000" TO BIT-VAL(I)
                   WHEN H"41" MOVE "01000001" TO BIT-VAL(I)
                   WHEN H"42" MOVE "01000010" TO BIT-VAL(I)
                   WHEN H"43" MOVE "01000011" TO BIT-VAL(I)
                   WHEN H"44" MOVE "01000100" TO BIT-VAL(I)
                   WHEN H"45" MOVE "01000101" TO BIT-VAL(I)
                   WHEN H"46" MOVE "01000110" TO BIT-VAL(I)
                   WHEN H"47" MOVE "01000111" TO BIT-VAL(I)
                   WHEN H"48" MOVE "01001000" TO BIT-VAL(I)
                   WHEN H"49" MOVE "01001001" TO BIT-VAL(I)
                   WHEN H"4a" MOVE "01001010" TO BIT-VAL(I)
                   WHEN H"4b" MOVE "01001011" TO BIT-VAL(I)
                   WHEN H"4c" MOVE "01001100" TO BIT-VAL(I)
                   WHEN H"4d" MOVE "01001101" TO BIT-VAL(I)
                   WHEN H"4e" MOVE "01001110" TO BIT-VAL(I)
                   WHEN H"4f" MOVE "01001111" TO BIT-VAL(I)

                   WHEN H"50" MOVE "01010000" TO BIT-VAL(I)
                   WHEN H"51" MOVE "01010001" TO BIT-VAL(I)
                   WHEN H"52" MOVE "01010010" TO BIT-VAL(I)
                   WHEN H"53" MOVE "01010011" TO BIT-VAL(I)
                   WHEN H"54" MOVE "01010100" TO BIT-VAL(I)
                   WHEN H"55" MOVE "01010101" TO BIT-VAL(I)
                   WHEN H"56" MOVE "01010110" TO BIT-VAL(I)
                   WHEN H"57" MOVE "01010111" TO BIT-VAL(I)
                   WHEN H"58" MOVE "01011000" TO BIT-VAL(I)
                   WHEN H"59" MOVE "01011001" TO BIT-VAL(I)
                   WHEN H"5a" MOVE "01011010" TO BIT-VAL(I)
                   WHEN H"5b" MOVE "01011011" TO BIT-VAL(I)
                   WHEN H"5c" MOVE "01011100" TO BIT-VAL(I)
                   WHEN H"5d" MOVE "01011101" TO BIT-VAL(I)
                   WHEN H"5e" MOVE "01011110" TO BIT-VAL(I)
                   WHEN H"5f" MOVE "01011111" TO BIT-VAL(I)

                   WHEN H"60" MOVE "01100000" TO BIT-VAL(I)
                   WHEN H"61" MOVE "01100001" TO BIT-VAL(I)
                   WHEN H"62" MOVE "01100010" TO BIT-VAL(I)
                   WHEN H"63" MOVE "01100011" TO BIT-VAL(I)
                   WHEN H"64" MOVE "01100100" TO BIT-VAL(I)
                   WHEN H"65" MOVE "01100101" TO BIT-VAL(I)
                   WHEN H"66" MOVE "01100110" TO BIT-VAL(I)
                   WHEN H"67" MOVE "01100111" TO BIT-VAL(I)
                   WHEN H"68" MOVE "01101000" TO BIT-VAL(I)
                   WHEN H"69" MOVE "01101001" TO BIT-VAL(I)
                   WHEN H"6a" MOVE "01101010" TO BIT-VAL(I)
                   WHEN H"6b" MOVE "01101011" TO BIT-VAL(I)
                   WHEN H"6c" MOVE "01101100" TO BIT-VAL(I)
                   WHEN H"6d" MOVE "01101101" TO BIT-VAL(I)
                   WHEN H"6e" MOVE "01101110" TO BIT-VAL(I)
                   WHEN H"6f" MOVE "01101111" TO BIT-VAL(I)

                   WHEN H"70" MOVE "01110000" TO BIT-VAL(I)
                   WHEN H"71" MOVE "01110001" TO BIT-VAL(I)
                   WHEN H"72" MOVE "01110010" TO BIT-VAL(I)
                   WHEN H"73" MOVE "01110011" TO BIT-VAL(I)
                   WHEN H"74" MOVE "01110100" TO BIT-VAL(I)
                   WHEN H"75" MOVE "01110101" TO BIT-VAL(I)
                   WHEN H"76" MOVE "01110110" TO BIT-VAL(I)
                   WHEN H"77" MOVE "01110111" TO BIT-VAL(I)
                   WHEN H"78" MOVE "01111000" TO BIT-VAL(I)
                   WHEN H"79" MOVE "01111001" TO BIT-VAL(I)
                   WHEN H"7a" MOVE "01111010" TO BIT-VAL(I)
                   WHEN H"7b" MOVE "01111011" TO BIT-VAL(I)
                   WHEN H"7c" MOVE "01111100" TO BIT-VAL(I)
                   WHEN H"7d" MOVE "01111101" TO BIT-VAL(I)
                   WHEN H"7e" MOVE "01111110" TO BIT-VAL(I)
                   WHEN H"7f" MOVE "01111111" TO BIT-VAL(I)

                   WHEN H"80" MOVE "10000000" TO BIT-VAL(I)
                   WHEN H"81" MOVE "10000001" TO BIT-VAL(I)
                   WHEN H"82" MOVE "10000010" TO BIT-VAL(I)
                   WHEN H"83" MOVE "10000011" TO BIT-VAL(I)
                   WHEN H"84" MOVE "10000100" TO BIT-VAL(I)
                   WHEN H"85" MOVE "10000101" TO BIT-VAL(I)
                   WHEN H"86" MOVE "10000110" TO BIT-VAL(I)
                   WHEN H"87" MOVE "10000111" TO BIT-VAL(I)
                   WHEN H"88" MOVE "10001000" TO BIT-VAL(I)
                   WHEN H"89" MOVE "10001001" TO BIT-VAL(I)
                   WHEN H"8a" MOVE "10001010" TO BIT-VAL(I)
                   WHEN H"8b" MOVE "10001011" TO BIT-VAL(I)
                   WHEN H"8c" MOVE "10001100" TO BIT-VAL(I)
                   WHEN H"8d" MOVE "10001101" TO BIT-VAL(I)
                   WHEN H"8e" MOVE "10001110" TO BIT-VAL(I)
                   WHEN H"8f" MOVE "10001111" TO BIT-VAL(I)

                   WHEN H"90" MOVE "10010000" TO BIT-VAL(I)
                   WHEN H"91" MOVE "10010001" TO BIT-VAL(I)
                   WHEN H"92" MOVE "10010010" TO BIT-VAL(I)
                   WHEN H"93" MOVE "10010011" TO BIT-VAL(I)
                   WHEN H"94" MOVE "10010100" TO BIT-VAL(I)
                   WHEN H"95" MOVE "10010101" TO BIT-VAL(I)
                   WHEN H"96" MOVE "10010110" TO BIT-VAL(I)
                   WHEN H"97" MOVE "10010111" TO BIT-VAL(I)
                   WHEN H"98" MOVE "10011000" TO BIT-VAL(I)
                   WHEN H"99" MOVE "10011001" TO BIT-VAL(I)
                   WHEN H"9a" MOVE "10011010" TO BIT-VAL(I)
                   WHEN H"9b" MOVE "10011011" TO BIT-VAL(I)
                   WHEN H"9c" MOVE "10011100" TO BIT-VAL(I)
                   WHEN H"9d" MOVE "10011101" TO BIT-VAL(I)
                   WHEN H"9e" MOVE "10011110" TO BIT-VAL(I)
                   WHEN H"9f" MOVE "10011111" TO BIT-VAL(I)

                   WHEN H"a0" MOVE "10100000" TO BIT-VAL(I)
                   WHEN H"a1" MOVE "10100001" TO BIT-VAL(I)
                   WHEN H"a2" MOVE "10100010" TO BIT-VAL(I)
                   WHEN H"a3" MOVE "10100011" TO BIT-VAL(I)
                   WHEN H"a4" MOVE "10100100" TO BIT-VAL(I)
                   WHEN H"a5" MOVE "10100101" TO BIT-VAL(I)
                   WHEN H"a6" MOVE "10100110" TO BIT-VAL(I)
                   WHEN H"a7" MOVE "10100111" TO BIT-VAL(I)
                   WHEN H"a8" MOVE "10101000" TO BIT-VAL(I)
                   WHEN H"a9" MOVE "10101001" TO BIT-VAL(I)
                   WHEN H"aa" MOVE "10101010" TO BIT-VAL(I)
                   WHEN H"ab" MOVE "10101011" TO BIT-VAL(I)
                   WHEN H"ac" MOVE "10101100" TO BIT-VAL(I)
                   WHEN H"ad" MOVE "10101101" TO BIT-VAL(I)
                   WHEN H"ae" MOVE "10101110" TO BIT-VAL(I)
                   WHEN H"af" MOVE "10101111" TO BIT-VAL(I)

                   WHEN H"b0" MOVE "10110000" TO BIT-VAL(I)
                   WHEN H"b1" MOVE "10110001" TO BIT-VAL(I)
                   WHEN H"b2" MOVE "10110010" TO BIT-VAL(I)
                   WHEN H"b3" MOVE "10110011" TO BIT-VAL(I)
                   WHEN H"b4" MOVE "10110100" TO BIT-VAL(I)
                   WHEN H"b5" MOVE "10110101" TO BIT-VAL(I)
                   WHEN H"b6" MOVE "10110110" TO BIT-VAL(I)
                   WHEN H"b7" MOVE "10110111" TO BIT-VAL(I)
                   WHEN H"b8" MOVE "10111000" TO BIT-VAL(I)
                   WHEN H"b9" MOVE "10111001" TO BIT-VAL(I)
                   WHEN H"ba" MOVE "10111010" TO BIT-VAL(I)
                   WHEN H"bb" MOVE "10111011" TO BIT-VAL(I)
                   WHEN H"bc" MOVE "10111100" TO BIT-VAL(I)
                   WHEN H"bd" MOVE "10111101" TO BIT-VAL(I)
                   WHEN H"be" MOVE "10111110" TO BIT-VAL(I)
                   WHEN H"bf" MOVE "10111111" TO BIT-VAL(I)

                   WHEN H"c0" MOVE "11000000" TO BIT-VAL(I)
                   WHEN H"c1" MOVE "11000001" TO BIT-VAL(I)
                   WHEN H"c2" MOVE "11000010" TO BIT-VAL(I)
                   WHEN H"c3" MOVE "11000011" TO BIT-VAL(I)
                   WHEN H"c4" MOVE "11000100" TO BIT-VAL(I)
                   WHEN H"c5" MOVE "11000101" TO BIT-VAL(I)
                   WHEN H"c6" MOVE "11000110" TO BIT-VAL(I)
                   WHEN H"c7" MOVE "11000111" TO BIT-VAL(I)
                   WHEN H"c8" MOVE "11001000" TO BIT-VAL(I)
                   WHEN H"c9" MOVE "11001001" TO BIT-VAL(I)
                   WHEN H"ca" MOVE "11001010" TO BIT-VAL(I)
                   WHEN H"cb" MOVE "11001011" TO BIT-VAL(I)
                   WHEN H"cc" MOVE "11001100" TO BIT-VAL(I)
                   WHEN H"cd" MOVE "11001101" TO BIT-VAL(I)
                   WHEN H"ce" MOVE "11001110" TO BIT-VAL(I)
                   WHEN H"cf" MOVE "11001111" TO BIT-VAL(I)

                   WHEN H"d0" MOVE "11010000" TO BIT-VAL(I)
                   WHEN H"d1" MOVE "11010001" TO BIT-VAL(I)
                   WHEN H"d2" MOVE "11010010" TO BIT-VAL(I)
                   WHEN H"d3" MOVE "11010011" TO BIT-VAL(I)
                   WHEN H"d4" MOVE "11010100" TO BIT-VAL(I)
                   WHEN H"d5" MOVE "11010101" TO BIT-VAL(I)
                   WHEN H"d6" MOVE "11010110" TO BIT-VAL(I)
                   WHEN H"d7" MOVE "11010111" TO BIT-VAL(I)
                   WHEN H"d8" MOVE "11011000" TO BIT-VAL(I)
                   WHEN H"d9" MOVE "11011001" TO BIT-VAL(I)
                   WHEN H"da" MOVE "11011010" TO BIT-VAL(I)
                   WHEN H"db" MOVE "11011011" TO BIT-VAL(I)
                   WHEN H"dc" MOVE "11011100" TO BIT-VAL(I)
                   WHEN H"dd" MOVE "11011101" TO BIT-VAL(I)
                   WHEN H"de" MOVE "11011110" TO BIT-VAL(I)
                   WHEN H"df" MOVE "11011111" TO BIT-VAL(I)

                   WHEN H"e0" MOVE "11100000" TO BIT-VAL(I)
                   WHEN H"e1" MOVE "11100001" TO BIT-VAL(I)
                   WHEN H"e2" MOVE "11100010" TO BIT-VAL(I)
                   WHEN H"e3" MOVE "11100011" TO BIT-VAL(I)
                   WHEN H"e4" MOVE "11100100" TO BIT-VAL(I)
                   WHEN H"e5" MOVE "11100101" TO BIT-VAL(I)
                   WHEN H"e6" MOVE "11100110" TO BIT-VAL(I)
                   WHEN H"e7" MOVE "11100111" TO BIT-VAL(I)
                   WHEN H"e8" MOVE "11101000" TO BIT-VAL(I)
                   WHEN H"e9" MOVE "11101001" TO BIT-VAL(I)
                   WHEN H"ea" MOVE "11101010" TO BIT-VAL(I)
                   WHEN H"eb" MOVE "11101011" TO BIT-VAL(I)
                   WHEN H"ec" MOVE "11101100" TO BIT-VAL(I)
                   WHEN H"ed" MOVE "11101101" TO BIT-VAL(I)
                   WHEN H"ee" MOVE "11101110" TO BIT-VAL(I)
                   WHEN H"ef" MOVE "11101111" TO BIT-VAL(I)

                   WHEN H"f0" MOVE "11110000" TO BIT-VAL(I)
                   WHEN H"f1" MOVE "11110001" TO BIT-VAL(I)
                   WHEN H"f2" MOVE "11110010" TO BIT-VAL(I)
                   WHEN H"f3" MOVE "11110011" TO BIT-VAL(I)
                   WHEN H"f4" MOVE "11110100" TO BIT-VAL(I)
                   WHEN H"f5" MOVE "11110101" TO BIT-VAL(I)
                   WHEN H"f6" MOVE "11110110" TO BIT-VAL(I)
                   WHEN H"f7" MOVE "11110111" TO BIT-VAL(I)
                   WHEN H"f8" MOVE "11111000" TO BIT-VAL(I)
                   WHEN H"f9" MOVE "11111001" TO BIT-VAL(I)
                   WHEN H"fa" MOVE "11111010" TO BIT-VAL(I)
                   WHEN H"fb" MOVE "11111011" TO BIT-VAL(I)
                   WHEN H"fc" MOVE "11111100" TO BIT-VAL(I)
                   WHEN H"fd" MOVE "11111101" TO BIT-VAL(I)
                   WHEN H"fe" MOVE "11111110" TO BIT-VAL(I)
                   WHEN H"ff" MOVE "11111111" TO BIT-VAL(I)
           END-PERFORM
           .
       FILL-BIT-TABLE-EXIT.
           EXIT
           .

       FILL-BYTE-TABLE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 256
               MOVE BIT-VAL(I) TO BYTE-KEY(I)
               MOVE BIT-KEY(I) TO BYTE-VAL(I);
           END-PERFORM
           .
       FILL-BYTE-TABLE-EXIT.
           EXIT
           .
