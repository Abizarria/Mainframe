       IDENTIFICATION      DIVISION.                                    00010000
       PROGRAM-ID.         PGCICS00.                                    00020000
       AUTHOR.             ADILSON BIZARRIA.                            00030000
       INSTALLATION.       FUTURE SCHOOL.                               00040000
       DATE-WRITTEN.       11/01/2022.                                  00050000
       DATE-COMPILED.                                                   00060000
      *--------------------------------------------*                    00070000
      * CICS - CADASTRO DE ALUNOS (MENU PRINCIPAL) *                    00080000
      *--------------------------------------------*                    00090000
       ENVIRONMENT         DIVISION.                                    00100000
       CONFIGURATION       SECTION.                                     00110000
                                                                        00120000
       SOURCE-COMPUTER.    IBM-3090.                                    00130000
       OBJECT-COMPUTER.    IBM-3090.                                    00140000
                                                                        00150000
       SPECIAL-NAMES.                                                   00160000
                           DECIMAL-POINT IS COMMA.                      00170000
                                                                        00180000
       INPUT-OUTPUT        SECTION.                                     00190000
                                                                        00200000
       FILE-CONTROL.                                                    00210000
                                                                        00220000
       DATA                DIVISION.                                    00230000
       FILE                SECTION.                                     00240000
                                                                        00250000
       WORKING-STORAGE     SECTION.                                     00260000
                                                                        00270000
       77  WS-TIME         PIC S9(15)  COMP-3.                          00280000
       77  WS-DATA         PIC  X(10).                                  00290000
       77  WS-HORA         PIC  X(08).                                  00300000
       77  WS-MSGM         PIC  X(20).                                  00310000
       77  WS-FASE         PIC  9(01).                                  00320000
                                                                        00330000
       COPY   DFHAID.                                                   00340000
       COPY   DFHBMSCA.                                                 00350000
       COPY   MAPMENU.                                                  00360000
                                                                        00370000
       LINKAGE             SECTION.                                     00380000
                                                                        00390000
       01 DFHCOMMAREA.                                                  00400000
          03  LK-FASE      PIC X(01).                                   00410000
                                                                        00420000
       PROCEDURE            DIVISION.                                   00430000
                                                                        00440000
       000-INI-PROG.                                                    00450000
           IF  EIBCALEN   =   0                                         00460000
               PERFORM   100-INI-MAPA                                   00470000
           ELSE                                                         00480000
               PERFORM   200-REC-TELA                                   00490000
           END-IF.                                                      00500000
                                                                        00510000
       100-INI-MAPA.                                                    00520000
           MOVE    'ESCOLHA UMA OPCAO'    TO  TMSGMO                    00530000
                                                                        00540000
           MOVE     ZEROS     TO  TNOPCO                                00550000
           MOVE     SPACES    TO  TPCURO                                00560000
           MOVE     DFHBMFSE  TO  TNOPCA                                00570000
           MOVE     DFHBMPRF  TO  TMSGMA  TPCURA                        00580000
                                                                        00590000
           MOVE     -1        TO  TNOPCL                                00600000
           MOVE     0         TO  WS-FASE                               00610000
                                                                        00620000
           PERFORM  120-ATU-TELA.                                       00630000
                                                                        00640000
       110-ATU-DATA.                                                    00650000
           EXEC     CICS      ASKTIME                                   00660000
                              ABSTIME    (WS-TIME)                      00670000
           END-EXEC                                                     00680000
                                                                        00690000
           EXEC     CICS      FORMATTIME                                00700000
                              ABSTIME    (WS-TIME)                      00710000
                              DATESEP    ('/')                          00720000
                              DDMMYYYY   (WS-DATA)                      00730000
                              TIMESEP    (':')                          00740000
                              TIME       (WS-HORA)                      00750000
           END-EXEC                                                     00760000
                                                                        00770000
           MOVE     WS-DATA   TO  TDATAO                                00780000
           MOVE     WS-HORA   TO  THORAO                                00790000
           MOVE     DFHBMPRF  TO  TDATAA  THORAA.                       00800000
                                                                        00810000
       120-ATU-TELA.                                                    00820000
           PERFORM  110-ATU-DATA                                        00830000
                                                                        00840000
           MOVE     EIBTRMID  TO  TTERMO                                00850000
           MOVE     EIBTRNID  TO  TTRNSO                                00860000
           MOVE     DFHBMPRF  TO  TTRNSA  TTERMA                        00870000
                                                                        00880000
           EXEC     CICS      SEND     MAP       ('MAPMENU')            00890000
                                       MAPSET    ('MAPMENU')            00900000
                              FROM               (MAPMENUO)             00910000
                              ERASE    CURSOR                           00920000
           END-EXEC                                                     00930000
                                                                        00940000
           EXEC     CICS      RETURN   COMMAREA  (WS-FASE)              00950000
                                       TRANSID   ('ABS0')               00960000
                                                                        00970000
           END-EXEC.                                                    00980000
                                                                        00990000
       200-REC-TELA.                                                    01000000
           EXEC     CICS      RECEIVE  MAP       ('MAPMENU')            01010000
                                       MAPSET    ('MAPMENU')            01020000
                              INTO               (MAPMENUI)             01030000
           END-EXEC                                                     01040000
                                                                        01050000
           IF  EIBAID     =   DFHPF3                                    01060000
               PERFORM   900-FIM-PROG.                                  01070000
                                                                        01080000
           MOVE     LK-FASE   TO  WS-FASE                               01090000
                                                                        01100000
           IF  WS-FASE    =   1                                         01110000
               PERFORM   100-INI-MAPA.                                  01120000
                                                                        01130000
           EVALUATE  TNOPCI                                             01140000
             WHEN    1                                                  01150000
                     EXEC    CICS  XCTL                                 01160000
                                   PROGRAM ('PGCICS01')                 01170000
                     END-EXEC                                           01180000
             WHEN    2                                                  01190000
                     EXEC    CICS  XCTL                                 01200000
                                   PROGRAM ('PGCICS02')                 01210000
                     END-EXEC                                           01220000
             WHEN    3                                                  01230000
                     EXEC    CICS  XCTL                                 01240000
                                   PROGRAM ('PGCICS03')                 01250000
                     END-EXEC                                           01260000
             WHEN    4                                                  01270000
                     EXEC    CICS  XCTL                                 01280000
                                   PROGRAM ('PGCICS04')                 01290000
                     END-EXEC                                           01300000
             WHEN    5                                                  01310000
                     EXEC    CICS  XCTL                                 01320000
                                   PROGRAM ('PGCICS05')                 01330000
                     END-EXEC                                           01340000
             WHEN    6                                                  01350000
                     PERFORM  900-FIM-PROG                              01360000
             WHEN    OTHER                                              01370000
                     MOVE     'OPCAO INVALIDA'  TO  TMSGMO              01380000
                                                                        01390000
                     MOVE     DFHBMPRF  TO  TNOPCA                      01400000
                     MOVE     -1        TO  TPCURL                      01410000
                     MOVE     1         TO  WS-FASE                     01420000
                     PERFORM  120-ATU-TELA                              01430000
           END-EVALUATE.                                                01440000
                                                                        01450000
       900-FIM-PROG.                                                    01460000
           MOVE    'PROGRAMA ENCERRADO' TO  WS-MSGM                     01470000
                                                                        01480000
           EXEC     CICS      SEND                                      01490000
                              FROM     (WS-MSGM)                        01500000
                              ERASE                                     01510000
           END-EXEC                                                     01520000
                                                                        01530000
           EXEC     CICS      RETURN                                    01540000
           END-EXEC.                                                    01550000
