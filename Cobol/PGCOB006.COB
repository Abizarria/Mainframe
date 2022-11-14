       IDENTIFICATION      DIVISION.                                    00010038
       PROGRAM-ID.         ABSPG006.                                    00020062
       AUTHOR.             ADILSON BIZARRIA.                            00030038
       INSTALLATION.       FUTURE SCHOOL.                               00040038
       DATE-WRITTEN.       11/10/2022.                                  00050062
       DATE-COMPILED.                                                   00060025
      *--------------------------------------------------------*        00070063
      * EXER-06 - GERAR RELATORIO ANALITICO DE PECAS COM TOTAL *        00080063
      *--------------------------------------------------------*        00090063
       ENVIRONMENT         DIVISION.                                    00100001
       CONFIGURATION       SECTION.                                     00110001
       SOURCE-COMPUTER.    IBM-3090.                                    00120029
       OBJECT-COMPUTER.    IBM-3090.                                    00130029
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.                      00140058
       INPUT-OUTPUT        SECTION.                                     00150001
                                                                        00160030
       FILE-CONTROL.                                                    00170001
           SELECT CADPEC   ASSIGN TO UT-S-CADPEC                        00180063
                  FILE     STATUS IS FST-PEC.                           00190063
                                                                        00200000
           SELECT RELATO   ASSIGN TO UT-S-RELATO                        00210061
                  FILE     STATUS IS FST-REL.                           00220058
                                                                        00230031
       DATA                DIVISION.                                    00240001
       FILE                SECTION.                                     00250008
                                                                        00260031
       FD  CADPEC          RECORD      CONTAINS 80 CHARACTERS           00270065
                           RECORDING   MODE     IS F                    00280001
                           LABEL       RECORD   IS STANDARD             00290001
                           DATA        RECORD   IS REG-PEC.             00300063
                                                                        00310031
       01  REG-PEC.                                                     00320063
           03  COD-PEC     PIC  9(05).                                  00330064
           03  NOM-PEC     PIC  X(30).                                  00340063
           03  QTD-PEC     PIC  9(05).                                  00350064
           03  MIN-PEC     PIC  9(05).                                  00360064
           03  MAX-PEC     PIC  9(05).                                  00370064
           03  FOR-PEC     PIC  9(05).                                  00380064
           03  PRU-PEC     PIC  9(07)V99.                               00390064
           03  PRD-PEC     PIC  9(07)V99.                               00400064
           03  TIP-PEC     PIC  X(01).                                  00410065
           03  FILLER      PIC  X(06).                                  00420064
                                                                        00430032
       FD  RELATO          RECORD      CONTAINS 104 CHARACTERS          00440064
                           RECORDING   MODE     IS  F                   00450064
                           LABEL       RECORD   IS  OMITTED             00460064
                           DATA        RECORD   IS  REG-REL.            00470064
                                                                        00480032
       01  REG-REL         PIC X(104).                                  00490064
                                                                        00500032
       WORKING-STORAGE SECTION.                                         00510001
                                                                        00520032
       77  FST-PEC         PIC  X(02).                                  00530063
       77  FST-REL         PIC  X(02).                                  00540060
                                                                        00550034
       77  FST-ERR         PIC  X(02).                                  00560034
       77  COD-ERR         PIC  9(01) VALUE ZEROS.                      00570063
       77  OPR-ERR         PIC  X(13).                                  00580005
       77  ARQ-ERR         PIC  X(08).                                  00590046
                                                                        00600033
       77  ACM-LIN         PIC  9(02) VALUE  99.                        00610059
       77  ACM-REG         PIC  9(04) VALUE  ZEROS.                     00620064
       77  ACM-PAG         PIC  9(04) VALUE  ZEROS.                     00630059
       77  ACM-LID         PIC  9(12) COMP-3 VALUE ZEROS.               00640033
       77  ACM-IMP         PIC  9(12) COMP-3 VALUE ZEROS.               00650059
                                                                        00660033
       77  MSK-LID         PIC  ZZZ.ZZZ.ZZZ.ZZ9.                        00670034
       77  MSK-IMP         PIC  ZZZ.ZZZ.ZZZ.ZZ9.                        00680059
                                                                        00690033
       01  DTA-SYS.                                                     00700059
           03  ANO-SYS     PIC  9(04).                                  00710059
           03  MES-SYS     PIC  9(02).                                  00720059
           03  DIA-SYS     PIC  9(02).                                  00730059
           03  HRA-SYS     PIC  9(02).                                  00740059
           03  MIN-SYS     PIC  9(02).                                  00750059
           03  FILLER      PIC  X(09).                                  00760059
                                                                        00770059
       01  CAB-001.                                                     00780058
           03  FILLER      PIC  X(93)  VALUE                            00790064
               'FUTURE SCHOOL CURSOS DE COMPUTACAO'.                    00800058
           03  FILLER      PIC  X(06)  VALUE  'PAG.:'.                  00810058
           03  PAG-CB1     PIC  Z.ZZ9.                                  00820058
                                                                        00830058
       01  CAB-002.                                                     00840058
           03  FILLER      PIC  X(82)  VALUE  'PROGRAMA = ABSPG006'.    00850064
           03  HRA-CB2     PIC  9(02).                                  00860058
           03  FILLER      PIC  X(01)  VALUE  ":".                      00870065
           03  MIN-CB2     PIC  9(02).                                  00880058
           03  FILLER      PIC  X(07)  VALUE  SPACES.                   00890064
           03  DIA-CB2     PIC  9(02).                                  00900058
           03  FILLER      PIC  X(01)  VALUE  "/".                      00910065
           03  MES-CB2     PIC  9(02).                                  00920058
           03  FILLER      PIC  X(01)  VALUE  "/".                      00930065
           03  ANO-CB2     PIC  9(04).                                  00940058
                                                                        00950058
       01  CAB-003.                                                     00960058
           03  FILLER      PIC  X(32)  VALUE  SPACES.                   00970064
           03  FILLER      PIC  X(72)  VALUE                            00980064
               'RELATORIO ANALITICO DO CADASTRO DE PECAS'.              00990064
                                                                        01000064
       01  CAB-004.                                                     01010064
           03  FILLER      PIC  X(62)  VALUE  SPACES.                   01020064
           03  FILLER      PIC  X(30)  VALUE  'VALOR'.                  01030064
           03  FILLER      PIC  X(12)  VALUE  'PERDA'.                  01040064
                                                                        01050058
       01  CAB-005.                                                     01060064
           03  FILLER      PIC  X(14)  VALUE  'CODIGO'.                 01070064
           03  FILLER      PIC  X(27)  VALUE  'NOME DA PECA'.           01080066
           03  FILLER      PIC  X(18)  VALUE  'QTDE   FORN.'.           01090064
           03  FILLER      PIC  X(13)  VALUE  'UNITARIO'.               01100064
           03  FILLER      PIC  X(17)  VALUE  'VALOR TOTAL'.            01110064
           03  FILLER      PIC  X(15)  VALUE  'PREVISTA   TIPO'.        01120064
                                                                        01130058
       01  DET-001.                                                     01140058
           03  COD-DT1     PIC  99.999.                                 01150064
           03  FILLER      PIC  X(02)  VALUE  SPACES.                   01160064
           03  NOM-DT1     PIC  X(32).                                  01170064
           03  QTD-DT1     PIC  ZZ.ZZ9.                                 01180064
           03  FILLER      PIC  X(02)  VALUE  SPACES.                   01190064
           03  FOR-DT1     PIC  9(05).                                  01200064
           03  FILLER      PIC  X(02)  VALUE  SPACES.                   01210064
           03  PRU-DT1     PIC  Z.ZZZ.ZZ9,99.                           01220064
           03  FILLER      PIC  X(02)  VALUE  SPACES.                   01230064
           03  PRT-DT1     PIC  ZZZ.ZZZ.ZZ9,99.                         01240064
           03  FILLER      PIC  X(02)  VALUE  SPACES.                   01250064
           03  PRD-DT1     PIC  Z.ZZZ.ZZ9,99.                           01260064
           03  FILLER      PIC  X(04)  VALUE  SPACES.                   01270067
           03  TIP-DT1     PIC  X(01).                                  01280064
                                                                        01290058
       01  TOT-001.                                                     01300064
           03  FILLER      PIC  X(27)  VALUE                            01310064
               'TOTAL DE REGISTROS LIDOS ='.                            01320064
           03  QTD-TT1     PIC  ZZZ.ZZ9.                                01330064
           03  FILLER      PIC  X(70)  VALUE  SPACES.                   01340064
                                                                        01350064
       PROCEDURE       DIVISION.                                        01360001
                                                                        01370033
       000-INICIO.                                                      01380001
           OPEN    INPUT   CADPEC                                       01390063
                   OUTPUT  RELATO.                                      01400058
                                                                        01410035
           IF  FST-PEC  =  '00' AND  FST-REL  =  '00'                   01420064
               PERFORM  100-IMP-REL  UNTIL  FST-PEC = '10'              01430064
               PERFORM  120-IMP-TOT                                     01440064
           ELSE                                                         01450003
               MOVE  1  TO  COD-ERR                                     01460064
               PERFORM  200-ERR-ARQ                                     01470064
           END-IF                                                       01480003
                                                                        01490035
           CLOSE     CADPEC   RELATO                                    01500063
                                                                        01510006
           IF  FST-PEC = '00' AND FST-REL  =  '00'                      01520063
               MOVE  ACM-LID  TO  MSK-LID                               01530006
               MOVE  ACM-IMP  TO  MSK-IMP                               01540059
                                                                        01550035
               DISPLAY '-> ABSPG006 <-> ESTATISTICA DE PROCESSAMENTO <-'01560062
               DISPLAY '|                                             |'01570006
               DISPLAY '| REGISTROS LIDOS (CADPECA)   ' MSK-LID ' |'    01580064
               DISPLAY '|                                             |'01590006
               DISPLAY '| REGISTROS IMPRESSOS (RELATO)' MSK-IMP ' |'    01600060
               DISPLAY '|                                             |'01610006
               DISPLAY '-----------------------------------------------'01620006
                                                                        01630035
               STOP  RUN                                                01640042
           ELSE                                                         01650042
               MOVE  4        TO  COD-ERR                               01660042
               PERFORM        200-ERR-ARQ                               01670042
           END-IF.                                                      01680042
                                                                        01690035
       100-IMP-REL.                                                     01700060
           READ      CADPEC                                             01710063
                                                                        01720005
           IF  FST-PEC  =   '00'                                        01730063
               ADD   1  TO  ACM-LID                                     01740054
                                                                        01750054
               IF  ACM-LIN  >   59                                      01760068
                   PERFORM  110-IMP-CAB                                 01770068
               END-IF                                                   01780068
                                                                        01790060
               MOVE     COD-PEC   TO  COD-DT1                           01800063
               MOVE     NOM-PEC   TO  NOM-DT1                           01810063
               MOVE     QTD-PEC   TO  QTD-DT1                           01820064
               MOVE     FOR-PEC   TO  FOR-DT1                           01830064
               MOVE     PRU-PEC   TO  PRU-DT1                           01840064
               COMPUTE  PRT-DT1   =   QTD-PEC  *   PRU-PEC              01850064
               MOVE     PRD-PEC   TO  PRD-DT1                           01860064
               MOVE     TIP-PEC   TO  TIP-DT1                           01870064
               WRITE    REG-REL  FROM DET-001  AFTER  1                 01880064
                                                                        01890060
               IF  FST-REL   =   '00'                                   01900060
                   ADD       1    TO  ACM-IMP                           01910060
                   ADD       1    TO  ACM-LIN                           01920060
               ELSE                                                     01930060
                   MOVE      3    TO  COD-ERR                           01940060
                   PERFORM        200-ERR-ARQ                           01950060
               END-IF                                                   01960060
           ELSE                                                         01970005
               IF  FST-PEC  NOT   =   '10'                              01980063
                   MOVE      2    TO  COD-ERR                           01990060
                   PERFORM        200-ERR-ARQ                           02000060
               END-IF                                                   02010043
           END-IF.                                                      02020005
                                                                        02030035
       110-IMP-CAB.                                                     02040060
           MOVE    FUNCTION     CURRENT-DATE  TO  DTA-SYS               02050060
           ADD     1        TO  ACM-PAG                                 02060067
           MOVE    ACM-PAG  TO  PAG-CB1                                 02070067
           MOVE    HRA-SYS  TO  HRA-CB2                                 02080060
           MOVE    MIN-SYS  TO  MIN-CB2                                 02090060
           MOVE    DIA-SYS  TO  DIA-CB2                                 02100060
           MOVE    MES-SYS  TO  MES-CB2                                 02110060
           MOVE    ANO-SYS  TO  ANO-CB2                                 02120060
                                                                        02130060
           WRITE   REG-REL FROM CAB-001  AFTER  PAGE                    02140064
           WRITE   REG-REL FROM CAB-002  AFTER  1                       02150064
           WRITE   REG-REL FROM CAB-003  AFTER  2                       02160064
           WRITE   REG-REL FROM CAB-004  AFTER  2                       02170064
           WRITE   REG-REL FROM CAB-005  AFTER  1                       02180064
           MOVE    SPACES   TO  REG-REL                                 02190060
           WRITE   REG-REL               AFTER  1                       02200064
           MOVE    8        TO  ACM-LIN.                                02210060
                                                                        02220060
       120-IMP-TOT.                                                     02230064
           MOVE    ACM-LID  TO  QTD-TT1                                 02240064
           WRITE   REG-REL FROM TOT-001  AFTER  2.                      02250064
                                                                        02260064
       200-ERR-ARQ.                                                     02270005
           IF  COD-ERR  =  1  AND  FST-PEC = '10'                       02280063
               DISPLAY '-----------> ABSPG006 <-----------'             02290062
               DISPLAY '|                                |'             02300005
               DISPLAY '|     ARQUIVO CADPECA VAZIO      |'             02310066
               DISPLAY '|                                |'             02320005
               DISPLAY '|      PROGRAMA ENCERRADO!       |'             02330066
               DISPLAY '|                                |'             02340005
               DISPLAY '----------------------------------'             02350005
           ELSE                                                         02360005
               EVALUATE COD-ERR                                         02370005
                   WHEN 1                                               02380005
                        MOVE  'NA ABERTURA'    TO  OPR-ERR              02390005
                   WHEN 2                                               02400005
                        MOVE  'NA LEITURA'     TO  OPR-ERR              02410005
                   WHEN 3                                               02420005
                        MOVE  'NA IMPRESSAO'   TO  OPR-ERR              02430060
                   WHEN 4                                               02440005
                        MOVE  'NO FECHAMENTO'  TO  OPR-ERR              02450005
               END-EVALUATE                                             02460020
                                                                        02470035
               IF  FST-PEC  >  '00' AND (COD-ERR =  1 OR 2 OR 4)        02480063
                   MOVE     FST-PEC  TO  FST-ERR                        02490063
                   MOVE     'CADPECA'    TO  ARQ-ERR                    02500066
               ELSE                                                     02510005
                   MOVE     FST-REL  TO  FST-ERR                        02520060
                   MOVE     'RELATO' TO  ARQ-ERR                        02530060
               END-IF                                                   02540020
                                                                        02550035
               DISPLAY '--------------> ABSPG006 <----------------'     02560062
               DISPLAY '|                                        |'     02570046
               DISPLAY '| ERRO ' OPR-ERR ' DO ARQUIVO ' ARQ-ERR ' |'    02580038
               DISPLAY '|                                        |'     02590046
               DISPLAY '|            FILE-STATUS: ' FST-ERR '           02600021
      -                '  |'                                            02610046
               DISPLAY '|                                        |'     02620046
               DISPLAY '|           PROGRAMA ENCERRADO!          |'     02630046
               DISPLAY '|                                        |'     02640046
               DISPLAY '------------------------------------------'     02650046
           END-IF.                                                      02660005
                                                                        02670035
           STOP    RUN.                                                 02680006
