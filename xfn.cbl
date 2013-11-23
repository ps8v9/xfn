       IDENTIFICATION DIVISION.
       PROGRAM-ID. XFN.
      ******************************************************************
      * Test program for the XF4 and XF5 procedures, which mimic GNU   *
      * COBOL's X"F4" and X"F5" library routines. Some tests are run   *
      * to compare their relative performance.                         *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Working storage for the X"F4" and X"F5" library routines.
       01  A-BYTE               BINARY-CHAR.
       01  BYTE-ARRAY.
           02  BYTE-ELEMENT     BINARY-CHAR OCCURS 8.
       01  DISPLAY-BYTE         PIC ZZ9.
       01  DISPLAY-ARRAY.
           02  DISPLAY-ELEMENT  PIC 9 OCCURS 8.

      * Lookup table for bit patterns.
       01  BIT-TABLE.
           02 BIT-REC  OCCURS 256 TIMES
                       ASCENDING KEY IS BIT-REC-KEY
                       INDEXED BY BIT-IDX.
              03  BIT-REC-KEY  BINARY-SHORT.
              03  BIT-REC-VAL  PIC X(8).

      * Lookup table for byte values.
       01  BYTE-TABLE.
           02 BYTE-REC  OCCURS 256 TIMES
                        ASCENDING KEY IS BYTE-REC-KEY
                        INDEXED BY BYTE-IDX.
              03  BYTE-REC-KEY  PIC X(8).
              03  BYTE-REC-VAL  BINARY-SHORT.

      * Fields for lookups and lookup results.
       01  BIT-STRING  PIC X(8).
       01  SHORT-INT   BINARY-SHORT.

      * Loop indexes and limits.
       01  I      BINARY-LONG.
       01  J      BINARY-LONG.
       01  I-MAX  BINARY-LONG.
       01  J-MAX  BINARY-LONG.

      * Fields for performance testing.
       01  ROUTINE  PIC X(10).
       01  TIME1    PIC X(8).
       01  TIME2    PIC X(8).
       01  CNT      BINARY-LONG.

       PROCEDURE DIVISION.
           PERFORM FILL-BIT-TABLE THRU FILL-BIT-TABLE-EXIT
           PERFORM FILL-BYTE-TABLE THRU FILL-BYTE-TABLE-EXIT

      *    PERFORM SHOW-BIT-TABLE THRU SHOW-BIT-TABLE-EXIT
      *    PERFORM SHOW-BYTE-TABLE THRU SHOW-BYTE-TABLE-EXIT

           DISPLAY "Testing the XF5 procedure:"
           MOVE 42 TO SHORT-INT
           PERFORM XF5 THRU XF5-EXIT
           DISPLAY "  byte: " SHORT-INT
           DISPLAY "  bits: " BIT-STRING

           DISPLAY 'Testing CALL X"F5":'
           MOVE 42 TO A-BYTE
           CALL X"F5" USING A-BYTE, BYTE-ARRAY
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 8
               MOVE BYTE-ELEMENT(I)
                 TO DISPLAY-ELEMENT(I)
           END-PERFORM
           DISPLAY "  byte: " A-BYTE
           DISPLAY "  bits: " DISPLAY-ARRAY

           DISPLAY "Testing the XF4 procedure:"
           MOVE 0 TO SHORT-INT
           PERFORM XF4 THRU XF4-EXIT
           DISPLAY "  bits: " BIT-STRING
           DISPLAY "  byte: " SHORT-INT

           DISPLAY 'Testing CALL X"F4":'
           MOVE 0 TO A-BYTE
           CALL X"F4" USING A-BYTE, BYTE-ARRAY
           DISPLAY "  bits: " DISPLAY-ARRAY
           DISPLAY "  byte: " A-BYTE

           MOVE "XFN" TO ROUTINE
           PERFORM BIG-LOOP THRU BIG-LOOP-EXIT

           MOVE 'CALL X"FN"' TO ROUTINE
           PERFORM BIG-LOOP THRU BIG-LOOP-EXIT

           DISPLAY "Press ENTER to exit ..."
           ACCEPT OMITTED
           GOBACK
           .

      ******************************************************************
      * XF4: Looks up the byte value for a PIC X(8) bit pattern.       *
      ******************************************************************
       XF4.
           SEARCH ALL BYTE-REC
               WHEN BYTE-REC-KEY(BYTE-IDX) = BIT-STRING
                   MOVE BYTE-REC-VAL(BIT-IDX) TO SHORT-INT
           END-SEARCH
           .
       XF4-EXIT.
           EXIT
           .

      ******************************************************************
      * XF5: Looks up the PIC X(8) bit pattern for a byte value.       *
      ******************************************************************
       XF5.
           SEARCH ALL BIT-REC
               WHEN BIT-REC-KEY(BIT-IDX) = SHORT-INT
                   MOVE BIT-REC-VAL(BIT-IDX) TO BIT-STRING
           END-SEARCH
           .
       XF5-EXIT.
           EXIT
           .

      ******************************************************************
      * BIG-LOOP: Run the specified routine pair I-MAX x J-MAX times.  *
      ******************************************************************
       BIG-LOOP.
           MOVE 4096 TO I-MAX
           MOVE 256  TO J-MAX
           ACCEPT TIME1 FROM TIME

           EVALUATE ROUTINE
               WHEN = "XFN"
                   PERFORM VARYING I FROM 1 BY 1 UNTIL I > I-MAX
                       PERFORM VARYING J FROM 1 BY 1 UNTIL J > J-MAX
                           MOVE J TO SHORT-INT
                           PERFORM XF5 THRU XF5-EXIT
                           PERFORM XF4 THRU XF4-EXIT
                       END-PERFORM
                   END-PERFORM
               WHEN = 'CALL X"FN"'
                   PERFORM VARYING I FROM 1 BY 1 UNTIL I > I-MAX
                       PERFORM VARYING J FROM 1 BY 1 UNTIL J > J-MAX
                           MOVE J TO A-BYTE
                           CALL X"F5" USING A-BYTE, BYTE-ARRAY
                           CALL X"F4" USING A-BYTE, BYTE-ARRAY
                       END-PERFORM
                   END-PERFORM
               WHEN OTHER
                   DISPLAY "Invalid routine name: " ROUTINE
                   GO TO BIG-LOOP-EXIT
           END-EVALUATE

           ACCEPT TIME2 FROM TIME

           COMPUTE CNT = I-MAX * J-MAX
           DISPLAY "Start/end times for " CNT " round trips "
                   "through the " ROUTINE " routines:"
           DISPLAY " " TIME1(1:2) ":" TIME1(3:2) ":"
                       TIME1(5:2) "." TIME1(7:2)
           DISPLAY " " TIME2(1:2) ":" TIME2(3:2) ":"
                       TIME2(5:2) "." TIME2(7:2)
                  .
       BIG-LOOP-EXIT.
           EXIT
           .

       FILL-BIT-TABLE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 256
               COMPUTE BIT-REC-KEY(I) = I - 1
               EVALUATE BIT-REC-KEY(I)
                   WHEN H"00" MOVE "00000000" TO BIT-REC-VAL(I)
                   WHEN H"01" MOVE "00000001" TO BIT-REC-VAL(I)
                   WHEN H"02" MOVE "00000010" TO BIT-REC-VAL(I)
                   WHEN H"03" MOVE "00000011" TO BIT-REC-VAL(I)
                   WHEN H"04" MOVE "00000100" TO BIT-REC-VAL(I)
                   WHEN H"05" MOVE "00000101" TO BIT-REC-VAL(I)
                   WHEN H"06" MOVE "00000110" TO BIT-REC-VAL(I)
                   WHEN H"07" MOVE "00000111" TO BIT-REC-VAL(I)
                   WHEN H"08" MOVE "00001000" TO BIT-REC-VAL(I)
                   WHEN H"09" MOVE "00001001" TO BIT-REC-VAL(I)
                   WHEN H"0a" MOVE "00001010" TO BIT-REC-VAL(I)
                   WHEN H"0b" MOVE "00001011" TO BIT-REC-VAL(I)
                   WHEN H"0c" MOVE "00001100" TO BIT-REC-VAL(I)
                   WHEN H"0d" MOVE "00001101" TO BIT-REC-VAL(I)
                   WHEN H"0e" MOVE "00001110" TO BIT-REC-VAL(I)
                   WHEN H"0f" MOVE "00001111" TO BIT-REC-VAL(I)

                   WHEN H"10" MOVE "00010000" TO BIT-REC-VAL(I)
                   WHEN H"11" MOVE "00010001" TO BIT-REC-VAL(I)
                   WHEN H"12" MOVE "00010010" TO BIT-REC-VAL(I)
                   WHEN H"13" MOVE "00010011" TO BIT-REC-VAL(I)
                   WHEN H"14" MOVE "00010100" TO BIT-REC-VAL(I)
                   WHEN H"15" MOVE "00010101" TO BIT-REC-VAL(I)
                   WHEN H"16" MOVE "00010110" TO BIT-REC-VAL(I)
                   WHEN H"17" MOVE "00010111" TO BIT-REC-VAL(I)
                   WHEN H"18" MOVE "00011000" TO BIT-REC-VAL(I)
                   WHEN H"19" MOVE "00011001" TO BIT-REC-VAL(I)
                   WHEN H"1a" MOVE "00011010" TO BIT-REC-VAL(I)
                   WHEN H"1b" MOVE "00011011" TO BIT-REC-VAL(I)
                   WHEN H"1c" MOVE "00011100" TO BIT-REC-VAL(I)
                   WHEN H"1d" MOVE "00011101" TO BIT-REC-VAL(I)
                   WHEN H"1e" MOVE "00011110" TO BIT-REC-VAL(I)
                   WHEN H"1f" MOVE "00011111" TO BIT-REC-VAL(I)

                   WHEN H"20" MOVE "00100000" TO BIT-REC-VAL(I)
                   WHEN H"21" MOVE "00100001" TO BIT-REC-VAL(I)
                   WHEN H"22" MOVE "00100010" TO BIT-REC-VAL(I)
                   WHEN H"23" MOVE "00100011" TO BIT-REC-VAL(I)
                   WHEN H"24" MOVE "00100100" TO BIT-REC-VAL(I)
                   WHEN H"25" MOVE "00100101" TO BIT-REC-VAL(I)
                   WHEN H"26" MOVE "00100110" TO BIT-REC-VAL(I)
                   WHEN H"27" MOVE "00100111" TO BIT-REC-VAL(I)
                   WHEN H"28" MOVE "00101000" TO BIT-REC-VAL(I)
                   WHEN H"29" MOVE "00101001" TO BIT-REC-VAL(I)
                   WHEN H"2a" MOVE "00101010" TO BIT-REC-VAL(I)
                   WHEN H"2b" MOVE "00101011" TO BIT-REC-VAL(I)
                   WHEN H"2c" MOVE "00101100" TO BIT-REC-VAL(I)
                   WHEN H"2d" MOVE "00101101" TO BIT-REC-VAL(I)
                   WHEN H"2e" MOVE "00101110" TO BIT-REC-VAL(I)
                   WHEN H"2f" MOVE "00101111" TO BIT-REC-VAL(I)

                   WHEN H"30" MOVE "00110000" TO BIT-REC-VAL(I)
                   WHEN H"31" MOVE "00110001" TO BIT-REC-VAL(I)
                   WHEN H"32" MOVE "00110010" TO BIT-REC-VAL(I)
                   WHEN H"33" MOVE "00110011" TO BIT-REC-VAL(I)
                   WHEN H"34" MOVE "00110100" TO BIT-REC-VAL(I)
                   WHEN H"35" MOVE "00110101" TO BIT-REC-VAL(I)
                   WHEN H"36" MOVE "00110110" TO BIT-REC-VAL(I)
                   WHEN H"37" MOVE "00110111" TO BIT-REC-VAL(I)
                   WHEN H"38" MOVE "00111000" TO BIT-REC-VAL(I)
                   WHEN H"39" MOVE "00111001" TO BIT-REC-VAL(I)
                   WHEN H"3a" MOVE "00111010" TO BIT-REC-VAL(I)
                   WHEN H"3b" MOVE "00111011" TO BIT-REC-VAL(I)
                   WHEN H"3c" MOVE "00111100" TO BIT-REC-VAL(I)
                   WHEN H"3d" MOVE "00111101" TO BIT-REC-VAL(I)
                   WHEN H"3e" MOVE "00111110" TO BIT-REC-VAL(I)
                   WHEN H"3f" MOVE "00111111" TO BIT-REC-VAL(I)

                   WHEN H"40" MOVE "01000000" TO BIT-REC-VAL(I)
                   WHEN H"41" MOVE "01000001" TO BIT-REC-VAL(I)
                   WHEN H"42" MOVE "01000010" TO BIT-REC-VAL(I)
                   WHEN H"43" MOVE "01000011" TO BIT-REC-VAL(I)
                   WHEN H"44" MOVE "01000100" TO BIT-REC-VAL(I)
                   WHEN H"45" MOVE "01000101" TO BIT-REC-VAL(I)
                   WHEN H"46" MOVE "01000110" TO BIT-REC-VAL(I)
                   WHEN H"47" MOVE "01000111" TO BIT-REC-VAL(I)
                   WHEN H"48" MOVE "01001000" TO BIT-REC-VAL(I)
                   WHEN H"49" MOVE "01001001" TO BIT-REC-VAL(I)
                   WHEN H"4a" MOVE "01001010" TO BIT-REC-VAL(I)
                   WHEN H"4b" MOVE "01001011" TO BIT-REC-VAL(I)
                   WHEN H"4c" MOVE "01001100" TO BIT-REC-VAL(I)
                   WHEN H"4d" MOVE "01001101" TO BIT-REC-VAL(I)
                   WHEN H"4e" MOVE "01001110" TO BIT-REC-VAL(I)
                   WHEN H"4f" MOVE "01001111" TO BIT-REC-VAL(I)

                   WHEN H"50" MOVE "01010000" TO BIT-REC-VAL(I)
                   WHEN H"51" MOVE "01010001" TO BIT-REC-VAL(I)
                   WHEN H"52" MOVE "01010010" TO BIT-REC-VAL(I)
                   WHEN H"53" MOVE "01010011" TO BIT-REC-VAL(I)
                   WHEN H"54" MOVE "01010100" TO BIT-REC-VAL(I)
                   WHEN H"55" MOVE "01010101" TO BIT-REC-VAL(I)
                   WHEN H"56" MOVE "01010110" TO BIT-REC-VAL(I)
                   WHEN H"57" MOVE "01010111" TO BIT-REC-VAL(I)
                   WHEN H"58" MOVE "01011000" TO BIT-REC-VAL(I)
                   WHEN H"59" MOVE "01011001" TO BIT-REC-VAL(I)
                   WHEN H"5a" MOVE "01011010" TO BIT-REC-VAL(I)
                   WHEN H"5b" MOVE "01011011" TO BIT-REC-VAL(I)
                   WHEN H"5c" MOVE "01011100" TO BIT-REC-VAL(I)
                   WHEN H"5d" MOVE "01011101" TO BIT-REC-VAL(I)
                   WHEN H"5e" MOVE "01011110" TO BIT-REC-VAL(I)
                   WHEN H"5f" MOVE "01011111" TO BIT-REC-VAL(I)

                   WHEN H"60" MOVE "01100000" TO BIT-REC-VAL(I)
                   WHEN H"61" MOVE "01100001" TO BIT-REC-VAL(I)
                   WHEN H"62" MOVE "01100010" TO BIT-REC-VAL(I)
                   WHEN H"63" MOVE "01100011" TO BIT-REC-VAL(I)
                   WHEN H"64" MOVE "01100100" TO BIT-REC-VAL(I)
                   WHEN H"65" MOVE "01100101" TO BIT-REC-VAL(I)
                   WHEN H"66" MOVE "01100110" TO BIT-REC-VAL(I)
                   WHEN H"67" MOVE "01100111" TO BIT-REC-VAL(I)
                   WHEN H"68" MOVE "01101000" TO BIT-REC-VAL(I)
                   WHEN H"69" MOVE "01101001" TO BIT-REC-VAL(I)
                   WHEN H"6a" MOVE "01101010" TO BIT-REC-VAL(I)
                   WHEN H"6b" MOVE "01101011" TO BIT-REC-VAL(I)
                   WHEN H"6c" MOVE "01101100" TO BIT-REC-VAL(I)
                   WHEN H"6d" MOVE "01101101" TO BIT-REC-VAL(I)
                   WHEN H"6e" MOVE "01101110" TO BIT-REC-VAL(I)
                   WHEN H"6f" MOVE "01101111" TO BIT-REC-VAL(I)

                   WHEN H"70" MOVE "01110000" TO BIT-REC-VAL(I)
                   WHEN H"71" MOVE "01110001" TO BIT-REC-VAL(I)
                   WHEN H"72" MOVE "01110010" TO BIT-REC-VAL(I)
                   WHEN H"73" MOVE "01110011" TO BIT-REC-VAL(I)
                   WHEN H"74" MOVE "01110100" TO BIT-REC-VAL(I)
                   WHEN H"75" MOVE "01110101" TO BIT-REC-VAL(I)
                   WHEN H"76" MOVE "01110110" TO BIT-REC-VAL(I)
                   WHEN H"77" MOVE "01110111" TO BIT-REC-VAL(I)
                   WHEN H"78" MOVE "01111000" TO BIT-REC-VAL(I)
                   WHEN H"79" MOVE "01111001" TO BIT-REC-VAL(I)
                   WHEN H"7a" MOVE "01111010" TO BIT-REC-VAL(I)
                   WHEN H"7b" MOVE "01111011" TO BIT-REC-VAL(I)
                   WHEN H"7c" MOVE "01111100" TO BIT-REC-VAL(I)
                   WHEN H"7d" MOVE "01111101" TO BIT-REC-VAL(I)
                   WHEN H"7e" MOVE "01111110" TO BIT-REC-VAL(I)
                   WHEN H"7f" MOVE "01111111" TO BIT-REC-VAL(I)

                   WHEN H"80" MOVE "10000000" TO BIT-REC-VAL(I)
                   WHEN H"81" MOVE "10000001" TO BIT-REC-VAL(I)
                   WHEN H"82" MOVE "10000010" TO BIT-REC-VAL(I)
                   WHEN H"83" MOVE "10000011" TO BIT-REC-VAL(I)
                   WHEN H"84" MOVE "10000100" TO BIT-REC-VAL(I)
                   WHEN H"85" MOVE "10000101" TO BIT-REC-VAL(I)
                   WHEN H"86" MOVE "10000110" TO BIT-REC-VAL(I)
                   WHEN H"87" MOVE "10000111" TO BIT-REC-VAL(I)
                   WHEN H"88" MOVE "10001000" TO BIT-REC-VAL(I)
                   WHEN H"89" MOVE "10001001" TO BIT-REC-VAL(I)
                   WHEN H"8a" MOVE "10001010" TO BIT-REC-VAL(I)
                   WHEN H"8b" MOVE "10001011" TO BIT-REC-VAL(I)
                   WHEN H"8c" MOVE "10001100" TO BIT-REC-VAL(I)
                   WHEN H"8d" MOVE "10001101" TO BIT-REC-VAL(I)
                   WHEN H"8e" MOVE "10001110" TO BIT-REC-VAL(I)
                   WHEN H"8f" MOVE "10001111" TO BIT-REC-VAL(I)

                   WHEN H"90" MOVE "10010000" TO BIT-REC-VAL(I)
                   WHEN H"91" MOVE "10010001" TO BIT-REC-VAL(I)
                   WHEN H"92" MOVE "10010010" TO BIT-REC-VAL(I)
                   WHEN H"93" MOVE "10010011" TO BIT-REC-VAL(I)
                   WHEN H"94" MOVE "10010100" TO BIT-REC-VAL(I)
                   WHEN H"95" MOVE "10010101" TO BIT-REC-VAL(I)
                   WHEN H"96" MOVE "10010110" TO BIT-REC-VAL(I)
                   WHEN H"97" MOVE "10010111" TO BIT-REC-VAL(I)
                   WHEN H"98" MOVE "10011000" TO BIT-REC-VAL(I)
                   WHEN H"99" MOVE "10011001" TO BIT-REC-VAL(I)
                   WHEN H"9a" MOVE "10011010" TO BIT-REC-VAL(I)
                   WHEN H"9b" MOVE "10011011" TO BIT-REC-VAL(I)
                   WHEN H"9c" MOVE "10011100" TO BIT-REC-VAL(I)
                   WHEN H"9d" MOVE "10011101" TO BIT-REC-VAL(I)
                   WHEN H"9e" MOVE "10011110" TO BIT-REC-VAL(I)
                   WHEN H"9f" MOVE "10011111" TO BIT-REC-VAL(I)

                   WHEN H"a0" MOVE "10100000" TO BIT-REC-VAL(I)
                   WHEN H"a1" MOVE "10100001" TO BIT-REC-VAL(I)
                   WHEN H"a2" MOVE "10100010" TO BIT-REC-VAL(I)
                   WHEN H"a3" MOVE "10100011" TO BIT-REC-VAL(I)
                   WHEN H"a4" MOVE "10100100" TO BIT-REC-VAL(I)
                   WHEN H"a5" MOVE "10100101" TO BIT-REC-VAL(I)
                   WHEN H"a6" MOVE "10100110" TO BIT-REC-VAL(I)
                   WHEN H"a7" MOVE "10100111" TO BIT-REC-VAL(I)
                   WHEN H"a8" MOVE "10101000" TO BIT-REC-VAL(I)
                   WHEN H"a9" MOVE "10101001" TO BIT-REC-VAL(I)
                   WHEN H"aa" MOVE "10101010" TO BIT-REC-VAL(I)
                   WHEN H"ab" MOVE "10101011" TO BIT-REC-VAL(I)
                   WHEN H"ac" MOVE "10101100" TO BIT-REC-VAL(I)
                   WHEN H"ad" MOVE "10101101" TO BIT-REC-VAL(I)
                   WHEN H"ae" MOVE "10101110" TO BIT-REC-VAL(I)
                   WHEN H"af" MOVE "10101111" TO BIT-REC-VAL(I)

                   WHEN H"b0" MOVE "10110000" TO BIT-REC-VAL(I)
                   WHEN H"b1" MOVE "10110001" TO BIT-REC-VAL(I)
                   WHEN H"b2" MOVE "10110010" TO BIT-REC-VAL(I)
                   WHEN H"b3" MOVE "10110011" TO BIT-REC-VAL(I)
                   WHEN H"b4" MOVE "10110100" TO BIT-REC-VAL(I)
                   WHEN H"b5" MOVE "10110101" TO BIT-REC-VAL(I)
                   WHEN H"b6" MOVE "10110110" TO BIT-REC-VAL(I)
                   WHEN H"b7" MOVE "10110111" TO BIT-REC-VAL(I)
                   WHEN H"b8" MOVE "10111000" TO BIT-REC-VAL(I)
                   WHEN H"b9" MOVE "10111001" TO BIT-REC-VAL(I)
                   WHEN H"ba" MOVE "10111010" TO BIT-REC-VAL(I)
                   WHEN H"bb" MOVE "10111011" TO BIT-REC-VAL(I)
                   WHEN H"bc" MOVE "10111100" TO BIT-REC-VAL(I)
                   WHEN H"bd" MOVE "10111101" TO BIT-REC-VAL(I)
                   WHEN H"be" MOVE "10111110" TO BIT-REC-VAL(I)
                   WHEN H"bf" MOVE "10111111" TO BIT-REC-VAL(I)

                   WHEN H"c0" MOVE "11000000" TO BIT-REC-VAL(I)
                   WHEN H"c1" MOVE "11000001" TO BIT-REC-VAL(I)
                   WHEN H"c2" MOVE "11000010" TO BIT-REC-VAL(I)
                   WHEN H"c3" MOVE "11000011" TO BIT-REC-VAL(I)
                   WHEN H"c4" MOVE "11000100" TO BIT-REC-VAL(I)
                   WHEN H"c5" MOVE "11000101" TO BIT-REC-VAL(I)
                   WHEN H"c6" MOVE "11000110" TO BIT-REC-VAL(I)
                   WHEN H"c7" MOVE "11000111" TO BIT-REC-VAL(I)
                   WHEN H"c8" MOVE "11001000" TO BIT-REC-VAL(I)
                   WHEN H"c9" MOVE "11001001" TO BIT-REC-VAL(I)
                   WHEN H"ca" MOVE "11001010" TO BIT-REC-VAL(I)
                   WHEN H"cb" MOVE "11001011" TO BIT-REC-VAL(I)
                   WHEN H"cc" MOVE "11001100" TO BIT-REC-VAL(I)
                   WHEN H"cd" MOVE "11001101" TO BIT-REC-VAL(I)
                   WHEN H"ce" MOVE "11001110" TO BIT-REC-VAL(I)
                   WHEN H"cf" MOVE "11001111" TO BIT-REC-VAL(I)

                   WHEN H"d0" MOVE "11010000" TO BIT-REC-VAL(I)
                   WHEN H"d1" MOVE "11010001" TO BIT-REC-VAL(I)
                   WHEN H"d2" MOVE "11010010" TO BIT-REC-VAL(I)
                   WHEN H"d3" MOVE "11010011" TO BIT-REC-VAL(I)
                   WHEN H"d4" MOVE "11010100" TO BIT-REC-VAL(I)
                   WHEN H"d5" MOVE "11010101" TO BIT-REC-VAL(I)
                   WHEN H"d6" MOVE "11010110" TO BIT-REC-VAL(I)
                   WHEN H"d7" MOVE "11010111" TO BIT-REC-VAL(I)
                   WHEN H"d8" MOVE "11011000" TO BIT-REC-VAL(I)
                   WHEN H"d9" MOVE "11011001" TO BIT-REC-VAL(I)
                   WHEN H"da" MOVE "11011010" TO BIT-REC-VAL(I)
                   WHEN H"db" MOVE "11011011" TO BIT-REC-VAL(I)
                   WHEN H"dc" MOVE "11011100" TO BIT-REC-VAL(I)
                   WHEN H"dd" MOVE "11011101" TO BIT-REC-VAL(I)
                   WHEN H"de" MOVE "11011110" TO BIT-REC-VAL(I)
                   WHEN H"df" MOVE "11011111" TO BIT-REC-VAL(I)

                   WHEN H"e0" MOVE "11100000" TO BIT-REC-VAL(I)
                   WHEN H"e1" MOVE "11100001" TO BIT-REC-VAL(I)
                   WHEN H"e2" MOVE "11100010" TO BIT-REC-VAL(I)
                   WHEN H"e3" MOVE "11100011" TO BIT-REC-VAL(I)
                   WHEN H"e4" MOVE "11100100" TO BIT-REC-VAL(I)
                   WHEN H"e5" MOVE "11100101" TO BIT-REC-VAL(I)
                   WHEN H"e6" MOVE "11100110" TO BIT-REC-VAL(I)
                   WHEN H"e7" MOVE "11100111" TO BIT-REC-VAL(I)
                   WHEN H"e8" MOVE "11101000" TO BIT-REC-VAL(I)
                   WHEN H"e9" MOVE "11101001" TO BIT-REC-VAL(I)
                   WHEN H"ea" MOVE "11101010" TO BIT-REC-VAL(I)
                   WHEN H"eb" MOVE "11101011" TO BIT-REC-VAL(I)
                   WHEN H"ec" MOVE "11101100" TO BIT-REC-VAL(I)
                   WHEN H"ed" MOVE "11101101" TO BIT-REC-VAL(I)
                   WHEN H"ee" MOVE "11101110" TO BIT-REC-VAL(I)
                   WHEN H"ef" MOVE "11101111" TO BIT-REC-VAL(I)

                   WHEN H"f0" MOVE "11110000" TO BIT-REC-VAL(I)
                   WHEN H"f1" MOVE "11110001" TO BIT-REC-VAL(I)
                   WHEN H"f2" MOVE "11110010" TO BIT-REC-VAL(I)
                   WHEN H"f3" MOVE "11110011" TO BIT-REC-VAL(I)
                   WHEN H"f4" MOVE "11110100" TO BIT-REC-VAL(I)
                   WHEN H"f5" MOVE "11110101" TO BIT-REC-VAL(I)
                   WHEN H"f6" MOVE "11110110" TO BIT-REC-VAL(I)
                   WHEN H"f7" MOVE "11110111" TO BIT-REC-VAL(I)
                   WHEN H"f8" MOVE "11111000" TO BIT-REC-VAL(I)
                   WHEN H"f9" MOVE "11111001" TO BIT-REC-VAL(I)
                   WHEN H"fa" MOVE "11111010" TO BIT-REC-VAL(I)
                   WHEN H"fb" MOVE "11111011" TO BIT-REC-VAL(I)
                   WHEN H"fc" MOVE "11111100" TO BIT-REC-VAL(I)
                   WHEN H"fd" MOVE "11111101" TO BIT-REC-VAL(I)
                   WHEN H"fe" MOVE "11111110" TO BIT-REC-VAL(I)
                   WHEN H"ff" MOVE "11111111" TO BIT-REC-VAL(I)
           END-PERFORM
           .
       FILL-BIT-TABLE-EXIT.
           EXIT
           .

       FILL-BYTE-TABLE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 256
               MOVE BIT-REC-VAL(I) TO BYTE-REC-KEY(I)
               MOVE BIT-REC-KEY(I) TO BYTE-REC-VAL(I);
           END-PERFORM
           .
       FILL-BYTE-TABLE-EXIT.
           EXIT
           .

       SHOW-BIT-TABLE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 256
               DISPLAY BIT-REC-KEY(I) " " BIT-REC-VAL(I)
           END-PERFORM
           .
       SHOW-BIT-TABLE-EXIT.
           EXIT
           .

       SHOW-BYTE-TABLE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 256
               DISPLAY BYTE-REC-KEY(I) " " BYTE-REC-VAL(I)
           END-PERFORM
           .
       SHOW-BYTE-TABLE-EXIT.
           EXIT
           .
