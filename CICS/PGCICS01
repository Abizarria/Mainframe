       IDENTIFICATION      DIVISION.                                    00010000
       PROGRAM-ID.         PGCICS01.                                    00020000
       AUTHOR.             ADILSON BIZARRIA.                            00030000
       INSTALLATION.       FUTURE SCHOOL.                               00040000
       DATE-WRITTEN.       11/01/2022.                                  00050000
       DATE-COMPILED.                                                   00060000
      *--------------------------------------*                          00070000
      * CICS - CADASTRO DE ALUNOS (INCLUSAO) *                          00080000
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
           MOVE    -1         TO  TMTRCL                                00720000
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
           MOVE    'INCLUSAO' TO  TOPERO                                00970000
           MOVE     EIBTRNID  TO  TTRNSO                                00980000
           MOVE     EIBTRMID  TO  TTERMO                                00990000
                                                                        01000000
           MOVE     DFHBMPRF  TO  TOPERA  TTRNSA  TTERMA                01010000
                                                                        01020000
           EXEC     CICS      SEND     MAP       ('MAPOPER')            01030000
                                       MAPSET    ('MAPOPER')            01040000
                              FROM               (MAPOPERO)             01050000
                              ERASE    CURSOR                           01060000
           END-EXEC                                                     01070000
                                                                        01080000
           EXEC     CICS      RETURN   COMMAREA  (WS-FASE)              01090000
                                       TRANSID   ('ABS1')               01100000
           END-EXEC.                                                    01110000
                                                                        01120000
       200-REC-TELA.                                                    01130000
           EXEC     CICS      RECEIVE  MAP       ('MAPOPER')            01140000
                                       MAPSET    ('MAPOPER')            01150000
                              INTO               (MAPOPERI)             01160000
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
                    PERFORM   310-CHK-ENTR                              01300000
             WHEN   3                                                   01310000
                    PERFORM   330-GRV-MATR                              01320000
             WHEN   4                                                   01330000
                    PERFORM   100-INI-MAPA                              01340000
           END-EVALUATE.                                                01350000
                                                                        01360000
       300-CHK-MATR.                                                    01370000
           IF  TMTRCI   NOT   NUMERIC  OR  TMTRCI  =  ZEROS             01380000
               MOVE    'MATRICULA INVALIDA'   TO  TMSGMO                01390000
               PERFORM  400-EXB-MSGM.                                   01400000
                                                                        01410000
           MOVE     TMTRCI    TO  MTR-ALU                               01420000
                                                                        01430000
           EXEC     CICS      READ     FILE      ('CADALU')             01440000
                                       RIDFLD    (MTR-ALU)              01450000
                                       INTO      (REG-ALU)              01460000
                              NOHANDLE                                  01470000
           END-EXEC                                                     01480000
                                                                        01490000
           IF  EIBRESP   =    DFHRESP(NORMAL)                           01500000
               MOVE    'MATRICULA EXISTENTE'  TO  TMSGMO                01510000
               MOVE     NOM-ALU   TO  TNOMEO                            01520000
               MOVE     CUR-ALU   TO  TCURSO                            01530000
               MOVE     TEL-ALU   TO  TFONEO                            01540000
               MOVE     END-ALU   TO  TENDRO                            01550000
               MOVE     EML-ALU   TO  TMAILO                            01560000
               PERFORM  400-EXB-MSGM.                                   01570000
                                                                        01580000
           MOVE    'INFORME O NOME'           TO  TMSGMO                01590000
           MOVE     DFHBMPRF  TO  TMTRCA                                01600000
           MOVE     DFHBMFSE  TO  TNOMEA  TCURSA                        01610000
                                  TFONEA  TENDRA  TMAILA                01620000
           MOVE     -1        TO  TNOMEL                                01630000
           MOVE     2         TO  WS-FASE                               01640000
           PERFORM  120-ATU-TELA.                                       01650000
                                                                        01660000
       310-CHK-ENTR.                                                    01670000
           MOVE     DFHBMPRF  TO  TMTRCA  TNOMEA  TCURSA                01680000
                                  TFONEA  TENDRA  TMAILA                01690000
                                                                        01700000
           IF  TNOMEI    =    SPACES  OR  LOW-VALUES                    01710000
               MOVE    'INFORME O NOME'       TO  TMSGMO                01720000
               MOVE     DFHBMFSE  TO  TNOMEA                            01730000
               MOVE     -1        TO  TNOMEL                            01740000
               PERFORM  120-ATU-TELA.                                   01750000
                                                                        01760000
           IF  TCURSI    =    SPACES  OR  LOW-VALUES                    01770000
               MOVE    'INFORME O CURSO'      TO  TMSGMO                01780000
               MOVE     DFHBMFSE  TO  TCURSA                            01790000
               MOVE     -1        TO  TCURSL                            01800000
               PERFORM  120-ATU-TELA.                                   01810000
                                                                        01820000
           IF  TFONEI    =    SPACES  OR  LOW-VALUES                    01830000
               MOVE    'INFORME O TELEFONE'   TO  TMSGMO                01840000
               MOVE     DFHBMFSE  TO  TFONEA                            01850000
               MOVE     -1        TO  TFONEL                            01860000
               PERFORM  120-ATU-TELA.                                   01870000
                                                                        01880000
           IF  TENDRI    =    SPACES  OR  LOW-VALUES                    01890000
               MOVE    'INFORME O ENDERECO'   TO  TMSGMO                01900000
               MOVE     DFHBMFSE  TO  TENDRA                            01910000
               MOVE     -1        TO  TENDRL                            01920000
               PERFORM  120-ATU-TELA.                                   01930000
                                                                        01940000
           IF  TMAILI    =    SPACES  OR  LOW-VALUES                    01950000
               MOVE    'INFORME O E-MAIL'     TO  TMSGMO                01960000
               MOVE     DFHBMFSE  TO  TMAILA                            01970000
               MOVE     -1        TO  TMAILL                            01980000
               PERFORM  120-ATU-TELA.                                   01990000
                                                                        02000000
           PERFORM  320-CHK-RESP.                                       02010000
                                                                        02020000
       320-CHK-RESP.                                                    02030000
           MOVE    'CONFIRMA A INCLUSAO?  (S/N)'  TO  TCONFO            02040000
                                                                        02050000
           MOVE     -1        TO  TRESPL                                02060000
           MOVE     DFHBMFSE  TO  TRESPA                                02070000
           MOVE     DFHBMPRF  TO  TMTRCA  TNOMEA  TCURSA                02080000
                                  TFONEA  TENDRA  TMAILA                02090000
           MOVE     3         TO  WS-FASE                               02100000
           PERFORM  120-ATU-TELA.                                       02110000
                                                                        02120000
       330-GRV-MATR.                                                    02130000
           IF  TRESPI   =   'S'                                         02140000
               MOVE     TMTRCI    TO  MTR-ALU                           02150000
               MOVE     TNOMEI    TO  NOM-ALU                           02160000
               MOVE     TCURSI    TO  CUR-ALU                           02170000
               MOVE     TFONEI    TO  TEL-ALU                           02180000
               MOVE     TENDRI    TO  END-ALU                           02190000
               MOVE     TMAILI    TO  EML-ALU                           02200000
                                                                        02210000
               EXEC     CICS      WRITE    FILE    ('CADALU')           02220000
                                           RIDFLD  (MTR-ALU)            02230000
                                  FROM             (REG-ALU)            02240000
                                  NOHANDLE                              02250000
               END-EXEC                                                 02260000
                                                                        02270000
               IF  EIBRESP   =    DFHRESP(NORMAL)                       02280000
                   MOVE     'INCLUSAO BEM SUCEDIDA'  TO  TMSGMO         02290000
               ELSE                                                     02300000
                   MOVE     'ERRO DURANTE INCLUSAO'  TO  TMSGMO         02310000
               END-IF                                                   02320000
                                                                        02330000
               MOVE     SPACES    TO  TRESPO                            02340000
               PERFORM  400-EXB-MSGM.                                   02350000
                                                                        02360000
           IF  TRESPI   =   'N'                                         02370000
               PERFORM  100-INI-MAPA.                                   02380000
                                                                        02390000
           MOVE    'RESPONDA APENAS COM S OU N'      TO  TMSGMO         02400000
           PERFORM  320-CHK-RESP.                                       02410000
                                                                        02420000
       400-EXB-MSGM.                                                    02430000
           MOVE     DFHBMPRF  TO  TMTRCA                                02440000
           MOVE     -1        TO  TPCURL                                02450000
           MOVE     4         TO  WS-FASE                               02460000
                                                                        02470000
           PERFORM  120-ATU-TELA.                                       02480000
