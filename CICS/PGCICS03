       IDENTIFICATION      DIVISION.                                    00010000
       PROGRAM-ID.         PGCICS03.                                    00020000
       AUTHOR.             ADILSON BIZARRIA.                            00030000
       INSTALLATION.       FUTURE SCHOOL.                               00040000
       DATE-WRITTEN.       11/06/2022.                                  00050000
       DATE-COMPILED.                                                   00060000
      *---------------------------------------*                         00070000
      * CICS - CADASTRO DE ALUNOS (ALTERACAO) *                         00080000
      *---------------------------------------*                         00090000
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
                              ABSTIME   (WS-TIME)                       00790000
           END-EXEC                                                     00800000
                                                                        00810000
           EXEC     CICS      FORMATTIME                                00820000
                              ABSTIME   (WS-TIME)                       00830000
                              DATESEP   ('/')                           00840000
                              DDMMYYYY  (WS-DATA)                       00850000
                              TIMESEP   (':')                           00860000
                              TIME      (WS-HORA)                       00870000
           END-EXEC                                                     00880000
                                                                        00890000
           MOVE     WS-DATA   TO  TDATAO                                00900000
           MOVE     WS-HORA   TO  THORAO                                00910000
           MOVE     DFHBMPRF  TO  TDATAA  THORAA.                       00920000
                                                                        00930000
       120-ATU-TELA.                                                    00940000
           PERFORM  110-ATU-DATA                                        00950000
                                                                        00960000
           MOVE    'ALTERACAO'    TO      TOPERO                        00970000
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
                              TRANSID            ('ABS3')               01100000
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
                    GO  TO    320-CHK-ENTR                              01300000
             WHEN   3                                                   01310000
                    PERFORM   340-GRV-ALTR                              01320000
             WHEN   4                                                   01330000
                    PERFORM   100-INI-MAPA                              01340000
           END-EVALUATE.                                                01350000
                                                                        01360000
       300-CHK-MATR.                                                    01370000
           IF  TMTRCI   NOT   NUMERIC                                   01380000
               MOVE    'MATRICULA INVALIDA'     TO  TMSGMO              01390000
           ELSE                                                         01400000
               PERFORM  310-LER-MATR                                    01410000
           END-IF                                                       01420000
                                                                        01430000
           MOVE     DFHBMPRF  TO  TMTRCA                                01440000
           MOVE     -1        TO  TPCURL                                01450000
           MOVE     4         TO  WS-FASE                               01460000
                                                                        01470000
           PERFORM  120-ATU-TELA.                                       01480000
                                                                        01490000
       310-LER-MATR.                                                    01500000
           MOVE     TMTRCI    TO  MTR-ALU                               01510000
                                                                        01520000
           EXEC     CICS      READ     FILE      ('CADALU')             01530000
                                       RIDFLD    (MTR-ALU)              01540000
                              INTO               (REG-ALU)              01550000
                              NOHANDLE                                  01560000
           END-EXEC                                                     01570000
                                                                        01580000
           IF  EIBRESP   =    DFHRESP(NORMAL)                           01590000
               MOVE     NOM-ALU   TO  TNOMEO                            01600000
               MOVE     CUR-ALU   TO  TCURSO                            01610000
               MOVE     TEL-ALU   TO  TFONEO                            01620000
               MOVE     END-ALU   TO  TENDRO                            01630000
               MOVE     EML-ALU   TO  TMAILO                            01640000
                                                                        01650000
               MOVE     2         TO  WS-FASE                           01660000
                                                                        01670000
               MOVE     SPACES    TO  TMSGMO                            01680000
               MOVE     -1        TO  TNOMEL                            01690000
                                                                        01700000
               MOVE     DFHBMPRF  TO  TMTRCA                            01710000
               MOVE     DFHBMFSE  TO  TNOMEA  TCURSA                    01720000
                                      TFONEA  TENDRA  TMAILA            01730000
               PERFORM  120-ATU-TELA.                                   01740000
                                                                        01750000
           MOVE    'MATRICULA INEXISTENTE'    TO  TMSGMO.               01760000
                                                                        01770000
       320-CHK-ENTR.                                                    01780000
           MOVE     DFHBMPRF  TO  TMTRCA                                01790000
           MOVE     DFHBMFSE  TO  TNOMEA  TCURSA                        01800000
                                  TFONEA  TENDRA  TMAILA                01810000
                                                                        01820000
           IF  TNOMEI    =    SPACES  OR  LOW-VALUES                    01830000
               MOVE    'INFORME O NOME'       TO  TMSGMO                01840000
               MOVE     -1        TO  TNOMEL                            01850000
               PERFORM  120-ATU-TELA.                                   01860000
                                                                        01870000
           IF  TCURSI    =    SPACES  OR  LOW-VALUES                    01880000
               MOVE    'INFORME O CURSO'      TO  TMSGMO                01890000
               MOVE     -1        TO  TCURSL                            01900000
               PERFORM  120-ATU-TELA.                                   01910000
                                                                        01920000
           IF  TFONEI    =    SPACES  OR  LOW-VALUES                    01930000
               MOVE    'INFORME O TELEFONE'   TO  TMSGMO                01940000
               MOVE     -1        TO  TFONEL                            01950000
               PERFORM  120-ATU-TELA.                                   01960000
                                                                        01970000
           IF  TENDRI    =    SPACES  OR  LOW-VALUES                    01980000
               MOVE    'INFORME O ENDERECO'   TO  TMSGMO                01990000
               MOVE     -1        TO  TENDRL                            02000000
               PERFORM  120-ATU-TELA.                                   02010000
                                                                        02020000
           IF  TMAILI    =    SPACES  OR  LOW-VALUES                    02030000
               MOVE    'INFORME O E-MAIL'     TO  TMSGMO                02040000
               MOVE     -1        TO  TMAILL                            02050000
               PERFORM  120-ATU-TELA.                                   02060000
                                                                        02070000
           MOVE     3         TO  WS-FASE.                              02080000
                                                                        02090000
       330-EXB-MSGM.                                                    02100000
           MOVE    'CONFIRMA A ALTERACAO? (S/N)'  TO  TCONFO            02110000
                                                                        02120000
           MOVE     -1        TO  TRESPL                                02130000
           MOVE     DFHBMFSE  TO  TRESPA                                02140000
           MOVE     DFHBMPRF  TO  TMTRCA  TNOMEA  TCURSA                02150000
                                  TFONEA  TENDRA  TMAILA                02160000
           PERFORM  120-ATU-TELA.                                       02170000
                                                                        02180000
       340-GRV-ALTR.                                                    02190000
           IF  TRESPI   =   'S'                                         02200000
               EXEC     CICS      READ     FILE    ('CADALU')           02210000
                                           RIDFLD  (MTR-ALU)            02220000
                                  UPDATE   INTO    (REG-ALU)            02230000
                                  NOHANDLE                              02240000
               END-EXEC                                                 02250000
                                                                        02260000
               MOVE     TMTRCI    TO  MTR-ALU                           02270000
               MOVE     TNOMEI    TO  NOM-ALU                           02280000
               MOVE     TCURSI    TO  CUR-ALU                           02290000
               MOVE     TFONEI    TO  TEL-ALU                           02300000
               MOVE     TENDRI    TO  END-ALU                           02310000
               MOVE     TMAILI    TO  EML-ALU                           02320000
                                                                        02330000
               EXEC     CICS      REWRITE  FILE    ('CADALU')           02340000
                                  FROM             (REG-ALU)            02350000
                                  NOHANDLE                              02360000
               END-EXEC                                                 02370000
                                                                        02380000
               IF  EIBRESP   =    DFHRESP(NORMAL)                       02390000
                   MOVE     'ALTERACAO BEM SUCEDIDA'  TO  TMSGMO        02400000
               ELSE                                                     02410000
                   MOVE     'ERRO DURANTE ALTERACAO'  TO  TMSGMO        02420000
               END-IF                                                   02430000
                                                                        02440000
               MOVE     SPACES    TO  TRESPO                            02450000
               MOVE     -1        TO  TPCURL                            02460000
               MOVE     4         TO  WS-FASE                           02470000
               PERFORM  120-ATU-TELA.                                   02480000
                                                                        02490000
           IF  TRESPI   =   'N'                                         02500000
               PERFORM  100-INI-MAPA.                                   02510000
                                                                        02520000
           MOVE    'RESPONDA APENAS COM S OU N'      TO  TMSGMO         02530000
           PERFORM  330-EXB-MSGM.                                       02540000
