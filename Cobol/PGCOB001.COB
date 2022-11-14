       IDENTIFICATION      DIVISION.                                    00010038
       PROGRAM-ID.         PGCOB001.                                    00020038
       AUTHOR.             ADILSON BIZARRIA.                            00030038
       INSTALLATION.       FUTURE SCHOOL.                               00040038
       DATE-WRITTEN.       04/10/2022.                                  00050038
       DATE-COMPILED.                                                   00060025
      *----------------------------------------------------------*      00070049
      * EXER-01 - CRIAR CADATU COM TODOS OS REGISTROS DO CADPECA *      00080049
      *----------------------------------------------------------*      00090049
       ENVIRONMENT         DIVISION.                                    00100001
       CONFIGURATION       SECTION.                                     00110001
       SOURCE-COMPUTER.    IBM-3090.                                    00120029
       OBJECT-COMPUTER.    IBM-3090.                                    00130029
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.                      00140017
       INPUT-OUTPUT        SECTION.                                     00150001
                                                                        00160030
       FILE-CONTROL.                                                    00170001
           SELECT CADPEC   ASSIGN  TO  UT-S-CADPEC                      00180047
                  FILE     STATUS  IS  FST-PEC.                         00190047
                                                                        00200000
           SELECT CADATU   ASSIGN  TO  UT-S-CADATU                      00210047
                  FILE     STATUS  IS  FST-ATU.                         00220047
                                                                        00230031
       DATA                DIVISION.                                    00240001
       FILE                SECTION.                                     00250008
                                                                        00260031
       FD  CADPEC          RECORD      CONTAINS 80 CHARACTERS           00270001
                           RECORDING   MODE     IS F                    00280001
                           LABEL       RECORD   IS STANDARD             00290001
                           DATA        RECORD   IS REG-PEC.             00300001
                                                                        00310031
       01  REG-PEC.                                                     00320001
           03  COD-PEC     PIC  9(05).                                  00330005
           03  NOM-PEC     PIC  X(30).                                  00340005
           03  QTD-PEC     PIC  9(05).                                  00350005
           03  MIN-PEC     PIC  9(05).                                  00360005
           03  MAX-PEC     PIC  9(05).                                  00370005
           03  FOR-PEC     PIC  X(05).                                  00380005
           03  PRU-PEC     PIC  9(07)V99.                               00390005
           03  PRD-PEC     PIC  9(07)V99.                               00400005
           03  TIP-PEC     PIC  X(01).                                  00410005
           03  FILLER      PIC  X(06).                                  00420005
                                                                        00430032
       FD  CADATU          RECORD      CONTAINS 80 CHARACTERS           00440001
                           RECORDING   MODE     IS F                    00450001
                           LABEL       RECORD   IS STANDARD             00460001
                           DATA        RECORD   IS REG-ATU.             00470001
                                                                        00480032
       01  REG-ATU.                                                     00490001
           03  COD-ATU     PIC  9(05).                                  00500005
           03  NOM-ATU     PIC  X(30).                                  00510005
           03  QTD-ATU     PIC  9(05).                                  00520005
           03  MIN-ATU     PIC  9(05).                                  00530005
           03  MAX-ATU     PIC  9(05).                                  00540005
           03  FOR-ATU     PIC  X(05).                                  00550005
           03  PRU-ATU     PIC  9(07)V99.                               00560005
           03  PRD-ATU     PIC  9(07)V99.                               00570005
           03  TIP-ATU     PIC  X(01).                                  00580005
           03  FILLER      PIC  X(06).                                  00590005
                                                                        00600032
       WORKING-STORAGE SECTION.                                         00610001
                                                                        00620032
       77  FST-PEC         PIC  X(02).                                  00630005
       77  FST-ATU         PIC  X(02).                                  00640005
                                                                        00650034
       77  FST-ERR         PIC  X(02).                                  00660034
       77  COD-ERR         PIC  9(01)  VALUE ZEROS.                     00670048
       77  OPR-ERR         PIC  X(13).                                  00680005
       77  ARQ-ERR         PIC  X(08).                                  00690046
                                                                        00700033
       77  ACM-LID         PIC  9(12)  COMP-3 VALUE ZEROS.              00710048
       77  ACM-GRV         PIC  9(12)  COMP-3 VALUE ZEROS.              00720048
                                                                        00730033
       77  MSK-LID         PIC  ZZZ.ZZZ.ZZZ.ZZ9.                        00740034
       77  MSK-GRV         PIC  ZZZ.ZZZ.ZZZ.ZZ9.                        00750034
                                                                        00760033
       PROCEDURE       DIVISION.                                        00770001
                                                                        00780033
       000-INICIO.                                                      00790001
           OPEN    INPUT   CADPEC                                       00800003
                   OUTPUT  CADATU.                                      00810003
                                                                        00820035
           IF  FST-PEC = '00' AND FST-ATU  = '00'                       00830006
               PERFORM        100-GRV-ARQ  UNTIL  FST-PEC = '10'        00840006
           ELSE                                                         00850003
               MOVE  1        TO  COD-ERR                               00860006
               PERFORM        200-ERR-ARQ                               00870006
           END-IF                                                       00880003
                                                                        00890035
           CLOSE     CADPEC   CADATU                                    00900006
                                                                        00910006
           IF  FST-PEC = '00' AND FST-ATU  =  '00'                      00920042
               MOVE  ACM-LID  TO  MSK-LID                               00930006
               MOVE  ACM-GRV  TO  MSK-GRV                               00940006
                                                                        00950035
               DISPLAY '-> ABSPG001 <-> ESTATISTICA DE PROCESSAMENTO <-'00960021
               DISPLAY '|                                             |'00970006
               DISPLAY '| REGISTROS LIDOS (CADPECA)   ' MSK-LID ' |'    00980045
               DISPLAY '|                                             |'00990006
               DISPLAY '| REGISTROS GRAVADOS (CADATU) ' MSK-GRV ' |'    01000045
               DISPLAY '|                                             |'01010006
               DISPLAY '-----------------------------------------------'01020006
                                                                        01030035
               STOP  RUN                                                01040042
           ELSE                                                         01050042
               MOVE  4        TO  COD-ERR                               01060042
               PERFORM        200-ERR-ARQ                               01070042
           END-IF.                                                      01080042
                                                                        01090035
       100-GRV-ARQ.                                                     01100005
           READ      CADPEC                                             01110003
                                                                        01120005
           IF  FST-PEC  =  '00'                                         01130005
               ADD      1        TO  ACM-LID                            01140005
               WRITE    REG-ATU FROM REG-PEC                            01150005
                                                                        01160035
               IF  FST-ATU  =   '00'                                    01170005
                   ADD      1    TO  ACM-GRV                            01180005
               ELSE                                                     01190005
                   MOVE     3    TO  COD-ERR                            01200005
                   PERFORM       200-ERR-ARQ                            01210005
               END-IF                                                   01220005
           ELSE                                                         01230005
               IF  FST-PEC  NOT  =   '10'                               01240042
                   MOVE  2       TO  COD-ERR                            01250042
                   PERFORM           200-ERR-ARQ                        01260043
               END-IF                                                   01270043
           END-IF.                                                      01280005
                                                                        01290035
       200-ERR-ARQ.                                                     01300005
           IF  COD-ERR  =  1  AND  FST-PEC = '10'                       01310005
               DISPLAY '-----------> ABSPG001 <-----------'             01320005
               DISPLAY '|                                |'             01330005
               DISPLAY '|      ARQUIVO CADPECA VAZIO     |'             01340005
               DISPLAY '|                                |'             01350005
               DISPLAY '|       PROGRAMA ENCERRADO!      |'             01360005
               DISPLAY '|                                |'             01370005
               DISPLAY '----------------------------------'             01380005
           ELSE                                                         01390005
               EVALUATE COD-ERR                                         01400005
                   WHEN 1                                               01410005
                        MOVE  'NA ABERTURA'    TO  OPR-ERR              01420005
                   WHEN 2                                               01430005
                        MOVE  'NA LEITURA'     TO  OPR-ERR              01440005
                   WHEN 3                                               01450005
                        MOVE  'NA GRAVACAO'    TO  OPR-ERR              01460005
                   WHEN 4                                               01470005
                        MOVE  'NO FECHAMENTO'  TO  OPR-ERR              01480005
               END-EVALUATE                                             01490020
                                                                        01500035
               IF  FST-PEC  >  '00'  AND (COD-ERR =  1 OR 2 OR 4)       01510019
                   MOVE     FST-PEC   TO  FST-ERR                       01520005
                   MOVE    'CADPECA'  TO  ARQ-ERR                       01530005
               ELSE                                                     01540005
                   MOVE     FST-ATU   TO  FST-ERR                       01550005
                   MOVE    'CADATU'   TO  ARQ-ERR                       01560005
               END-IF                                                   01570020
                                                                        01580035
               DISPLAY '---------------> ABSPG001 <---------------'     01590046
               DISPLAY '|                                        |'     01600046
               DISPLAY '| ERRO ' OPR-ERR ' DO ARQUIVO ' ARQ-ERR ' |'    01610038
               DISPLAY '|                                        |'     01620046
               DISPLAY '|            FILE-STATUS: ' FST-ERR '           01630021
      -                '  |'                                            01640046
               DISPLAY '|                                        |'     01650046
               DISPLAY '|           PROGRAMA ENCERRADO!          |'     01660046
               DISPLAY '|                                        |'     01670046
               DISPLAY '------------------------------------------'     01680046
           END-IF.                                                      01690005
                                                                        01700035
           STOP    RUN.                                                 01710006
