       IDENTIFICATION      DIVISION.                                    00010000
       PROGRAM-ID.         PGCICS04.                                    00020000
       AUTHOR.             ADILSON BIZARRIA.                            00030000
       INSTALLATION.       FUTURE SCHOOL.                               00040000
       DATE-WRITTEN.       11/05/2022.                                  00050000
       DATE-COMPILED.                                                   00060000
      *--------------------------------------*                          00070000
      * CICS - CADASTRO DE ALUNOS (CONSULTA) *                          00080000
      *--------------------------------------*                          00090000
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
       77  WS-FASE         PIC  9(01).                                  00310000
                                                                        00320000
       01  REG-ALU.                                                     00330000
           03  MTR-ALU     PIC  9(05).                                  00340000
           03  NOM-ALU     PIC  X(30).                                  00350000
           03  CUR-ALU     PIC  X(20).                                  00360000
           03  TEL-ALU     PIC  X(15).                                  00370000
           03  END-ALU     PIC  X(35).                                  00380000
           03  EML-ALU     PIC  X(30).                                  00390000
                                                                        00400000
       COPY    DFHAID.                                                  00410000
       COPY    DFHBMSCA.                                                00420000
       COPY    MAPOPER.                                                 00430000
                                                                        00440000
       LINKAGE             SECTION.                                     00450000
                                                                        00460000
       01  DFHCOMMAREA.                                                 00470000
           03  LK-FASE     PIC  9(01).                                  00480000
                                                                        00490000
       PROCEDURE       DIVISION.                                        00500000
                                                                        00510000
       000-INI-PROG.                                                    00520000
           IF  EIBCALEN   =   0                                         00530000
               PERFORM        100-INI-MAPA                              00540000
           ELSE                                                         00550000
               PERFORM        200-REC-TELA                              00560000
           END-IF.                                                      00570000
                                                                        00580000
       100-INI-MAPA.                                                    00590000
           MOVE    'INFORME A MATRICULA'    TO    TMSGMO                00600000
                                                                        00610000
           MOVE     ZEROS     TO  TMTRCO                                00620000
           MOVE     SPACES    TO  TNOMEO  TCURSO                        00630000
                                  TFONEO  TENDRO  TMAILO                00640000
                                  TCONFO  TRESPO  TPCURO                00650000
                                                                        00660000
           MOVE     DFHBMFSE  TO  TMTRCA                                00670000
           MOVE     DFHBMPRF  TO  TNOMEA  TCURSA  TFONEA                00680000
                                  TENDRA  TMAILA  TCONFA                00690000
                                  TRESPA  TMSGMA  TPCURA                00700000
                                                                        00710000
           MOVE     -1        TO  TMTRCL                                00720000
           MOVE     1         TO  WS-FASE                               00730000
                                                                        00740000
           PERFORM  120-ATU-TELA.                                       00750000
                                                                        00760000
       110-ATU-DATA.                                                    00770000
           EXEC     CICS      ASKTIME                                   00780000
                              ABSTIME    (WS-TIME)                      00790000
           END-EXEC                                                     00800000
                                                                        00810000
           EXEC     CICS      FORMATTIME                                00820000
                              ABSTIME    (WS-TIME)                      00830000
                              DATESEP    ('/')                          00840000
                              DDMMYYYY   (WS-DATA)                      00850000
                              TIMESEP    (':')                          00860000
                              TIME       (WS-HORA)                      00870000
           END-EXEC                                                     00880000
                                                                        00890000
           MOVE     WS-DATA   TO  TDATAO                                00900000
           MOVE     WS-HORA   TO  THORAO                                00910000
           MOVE     DFHBMPRF  TO  TDATAA  THORAA.                       00920000
                                                                        00930000
       120-ATU-TELA.                                                    00940000
           PERFORM  110-ATU-DATA                                        00950000
                                                                        00960000
           MOVE    'CONSULTA' TO  TOPERO                                00970000
           MOVE     EIBTRNID  TO  TTRNSO                                00980000
           MOVE     EIBTRMID  TO  TTERMO                                00990000
           MOVE     DFHBMPRF  TO  TOPERA  TTRNSA  TTERMA                01000000
                                                                        01010000
           EXEC     CICS      SEND     MAP       ('MAPOPER')            01020000
                                       MAPSET    ('MAPOPER')            01030000
                              FROM               (MAPOPERO)             01040000
                              ERASE    CURSOR                           01050000
           END-EXEC                                                     01060000
                                                                        01070000
           EXEC     CICS      RETURN   COMMAREA  (WS-FASE)              01080000
                                       TRANSID   ('ABS4')               01090000
           END-EXEC.                                                    01100000
                                                                        01110000
       200-REC-TELA.                                                    01120000
           EXEC     CICS      RECEIVE  MAP      ('MAPOPER')             01130000
                                       MAPSET   ('MAPOPER')             01140000
                              INTO              (MAPOPERI)              01150000
           END-EXEC                                                     01160000
                                                                        01170000
           IF  EIBAID    =    DFHPF3                                    01180000
               EXEC    CICS   XCTL                                      01190000
                              PROGRAM  ('PGCICS00')                     01200000
               END-EXEC.                                                01210000
                                                                        01220000
           MOVE     LK-FASE   TO  WS-FASE                               01230000
                                                                        01240000
           EVALUATE WS-FASE                                             01250000
             WHEN   1                                                   01260000
                    PERFORM   210-CHK-MATR                              01270000
             WHEN   2                                                   01280000
                    PERFORM   100-INI-MAPA                              01290000
           END-EVALUATE.                                                01300000
                                                                        01310000
       210-CHK-MATR.                                                    01320000
           IF  TMTRCI   NOT   NUMERIC                                   01330000
               MOVE    'MATRICULA INVALIDA'     TO  TMSGMO              01340000
           ELSE                                                         01350000
               PERFORM  220-LER-MATR                                    01360000
           END-IF                                                       01370000
                                                                        01380000
           MOVE     DFHBMPRF  TO  TMTRCA                                01390000
           MOVE     -1        TO  TPCURL                                01400000
           MOVE     2         TO  WS-FASE                               01410000
                                                                        01420000
           PERFORM  120-ATU-TELA.                                       01430000
                                                                        01440000
       220-LER-MATR.                                                    01450000
           MOVE     TMTRCI    TO  MTR-ALU                               01460000
                                                                        01470000
           EXEC     CICS      READ     FILE      ('CADALU')             01480000
                                       RIDFLD    (MTR-ALU)              01490000
                              INTO               (REG-ALU)              01500000
                              NOHANDLE                                  01510000
           END-EXEC                                                     01520000
                                                                        01530000
           IF  EIBRESP   =    DFHRESP(NORMAL)                           01540000
               MOVE    'CONSULTA BEM SUCEDIDA'  TO  TMSGMO              01550000
               MOVE     NOM-ALU   TO  TNOMEO                            01560000
               MOVE     CUR-ALU   TO  TCURSO                            01570000
               MOVE     TEL-ALU   TO  TFONEO                            01580000
               MOVE     END-ALU   TO  TENDRO                            01590000
               MOVE     EML-ALU   TO  TMAILO                            01600000
           ELSE                                                         01610000
               MOVE    'MATRICULA INEXISTENTE'  TO  TMSGMO              01620000
           END-IF.                                                      01630000
