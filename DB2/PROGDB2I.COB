       IDENTIFICATION      DIVISION.                                    00010000
       PROGRAM-ID.         PROGDB2I.                                    00020001
       AUTHOR.             ADILSON BIZARRIA.                            00030000
       INSTALLATION.       FUTURE SCHOOL.                               00040000
       DATE-WRITTEN.       11/14/2022.                                  00050000
       DATE-COMPILED.                                                   00060000
      *-------------------------------------*                           00070000
      * DB2 - CADASTRO DE ALUNOS (EXCLUSAO) *                           00080000
      *-------------------------------------*                           00090000
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
       COPY    MAPDB2O.                                                 00430000
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
                                                                        00920000
           MOVE     DFHBMPRF  TO  TDATAA  THORAA.                       00930000
                                                                        00940000
       120-ATU-TELA.                                                    00950000
           PERFORM  110-ATU-DATA                                        00960000
                                                                        00970000
           MOVE    'EXCLUSAO' TO  TOPERO                                00980000
           MOVE     EIBTRNID  TO  TTRNSO                                00990000
           MOVE     EIBTRMID  TO  TTERMO                                01000000
           MOVE     DFHBMPRF  TO  TOPERA  TTRNSA  TTERMA                01010000
                                                                        01020000
           EXEC     CICS      SEND     MAP       ('MAPDB2O')            01030000
                                       MAPSET    ('MAPDB2O')            01040000
                              FROM               (MAPDB2OO)             01050000
                              ERASE    CURSOR                           01060000
           END-EXEC                                                     01070000
                                                                        01080000
           EXEC     CICS      RETURN   COMMAREA  (WS-FASE)              01090000
                                       TRANSID   ('DB2I')               01100000
           END-EXEC.                                                    01110000
                                                                        01120000
       200-REC-TELA.                                                    01130000
           EXEC     CICS      RECEIVE  MAP       ('MAPDB2O')            01140000
                                       MAPSET    ('MAPDB2O')            01150000
                              INTO               (MAPDB2OI)             01160000
           END-EXEC                                                     01170000
                                                                        01180000
           IF  EIBAID    =    DFHPF3                                    01190000
               EXEC    CICS   XCTL                                      01200000
                              PROGRAM  ('PGCICS00')                     01210000
               END-EXEC.                                                01220000
                                                                        01230000
           MOVE     LK-FASE   TO  WS-FASE                               01240000
                                                                        01250000
           EVALUATE WS-FASE                                             01260000
             WHEN   1                                                   01270000
                    PERFORM   300-CHK-MATR                              01280000
             WHEN   2                                                   01290000
                    PERFORM   100-INI-MAPA                              01300000
             WHEN   3                                                   01310000
                    PERFORM   330-EXC-MATR                              01320000
           END-EVALUATE.                                                01330000
                                                                        01340000
       300-CHK-MATR.                                                    01350000
           IF  TMTRCI   NOT   NUMERIC                                   01360000
               MOVE    'MATRICULA INVALIDA'     TO  TMSGMO              01370000
           ELSE                                                         01380000
               PERFORM  310-LER-MATR                                    01390000
           END-IF                                                       01400000
                                                                        01410000
           MOVE     DFHBMPRF  TO  TMTRCA                                01420000
           MOVE     -1        TO  TPCURL                                01430000
           MOVE     2         TO  WS-FASE                               01440000
                                                                        01450000
           PERFORM  120-ATU-TELA.                                       01460000
                                                                        01470000
       310-LER-MATR.                                                    01480000
           MOVE     TMTRCI    TO  MTR-ALU                               01490000
                                                                        01500000
           EXEC     CICS      READ     FILE      ('CADALU')             01510000
                                       RIDFLD    (MTR-ALU)              01520000
                                       INTO      (REG-ALU)              01530000
                              NOHANDLE                                  01540001
           END-EXEC                                                     01550000
                                                                        01560000
           IF  EIBRESP   =    DFHRESP(NORMAL)                           01570000
               MOVE     NOM-ALU   TO  TNOMEO                            01580000
               MOVE     CUR-ALU   TO  TCURSO                            01590000
               MOVE     TEL-ALU   TO  TFONEO                            01600000
               MOVE     END-ALU   TO  TENDRO                            01610000
               MOVE     EML-ALU   TO  TMAILO                            01620000
               PERFORM  320-CHK-RESP                                    01630000
           ELSE                                                         01640000
               MOVE    'MATRICULA INEXISTENTE'      TO  TMSGMO          01650000
           END-IF.                                                      01660000
                                                                        01670000
       320-CHK-RESP.                                                    01680000
           MOVE    'CONFIRMA A EXCLUSAO?  (S/N)'    TO  TCONFO          01690000
                                                                        01700000
           MOVE     -1        TO  TRESPL                                01710000
           MOVE     DFHBMFSE  TO  TRESPA                                01720000
           MOVE     DFHBMPRF  TO  TMTRCA  TNOMEA  TCURSA                01730000
                                  TFONEA  TENDRA  TMAILA                01740000
           MOVE     3         TO  WS-FASE                               01750000
                                                                        01760000
           PERFORM  120-ATU-TELA.                                       01770000
                                                                        01780000
       330-EXC-MATR.                                                    01790000
           IF  TRESPI   =   'S'                                         01800000
               EXEC     CICS      DELETE   FILE    ('CADALU')           01810000
                                           RIDFLD  (MTR-ALU)            01820000
                                  NOHANDLE                              01830000
               END-EXEC                                                 01840000
                                                                        01850000
               IF  EIBRESP   =    DFHRESP(NORMAL)                       01860000
                   MOVE     'EXCLUSAO BEM SUCEDIDA'  TO  TMSGMO         01870000
               ELSE                                                     01880000
                   MOVE     'ERRO DURANTE EXCLUSAO'  TO  TMSGMO         01890000
               END-IF                                                   01900000
                                                                        01910000
               MOVE     SPACES    TO  TRESPO                            01920000
               MOVE     DFHBMPRF  TO  TMTRCA                            01930000
               MOVE     -1        TO  TPCURL                            01940000
               MOVE     2         TO  WS-FASE                           01950000
               PERFORM  120-ATU-TELA.                                   01960000
                                                                        01970000
           IF  TRESPI   =   'N'                                         01980000
               PERFORM  100-INI-MAPA.                                   01990000
                                                                        02000000
           MOVE    'RESPONDA APENAS COM S OU N'      TO  TMSGMO         02010000
           PERFORM  320-CHK-RESP.                                       02020000
