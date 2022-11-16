       IDENTIFICATION      DIVISION.                                    00010038
       PROGRAM-ID.         ABSPG003.                                    00020051
       AUTHOR.             ADILSON BIZARRIA.                            00030038
       INSTALLATION.       FUTURE SCHOOL.                               00040038
       DATE-WRITTEN.       10/10/2022.                                  00050055
       DATE-COMPILED.                                                   00060025
      *--------------------------------------------------------*        00070053
      * EXER-03 - CRIAR CADSEL A PARTIR DO CADPECA DESPREZANDO *        00080053
      *           REGISTROS COM CODIGOS ENTRE 10001 E 19999    *        00090053
      *--------------------------------------------------------*        00100053
       ENVIRONMENT         DIVISION.                                    00110001
       CONFIGURATION       SECTION.                                     00120001
       SOURCE-COMPUTER.    IBM-3090.                                    00130029
       OBJECT-COMPUTER.    IBM-3090.                                    00140029
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.                      00150017
       INPUT-OUTPUT        SECTION.                                     00160001
                                                                        00170030
       FILE-CONTROL.                                                    00180001
           SELECT CADPEC   ASSIGN  TO  UT-S-CADPEC                      00190056
                  FILE     STATUS  IS  FST-PEC.                         00200056
                                                                        00210000
           SELECT CADSEL   ASSIGN  TO  UT-S-CADSEL                      00220056
                  FILE     STATUS  IS  FST-SEL.                         00230056
                                                                        00240031
       DATA                DIVISION.                                    00250001
       FILE                SECTION.                                     00260008
                                                                        00270031
       FD  CADPEC          RECORD      CONTAINS 80 CHARACTERS           00280001
                           RECORDING   MODE     IS F                    00290001
                           LABEL       RECORD   IS STANDARD             00300001
                           DATA        RECORD   IS REG-PEC.             00310001
                                                                        00320031
       01  REG-PEC.                                                     00330001
           03  COD-PEC     PIC  9(05).                                  00340005
           03  NOM-PEC     PIC  X(30).                                  00350005
           03  QTD-PEC     PIC  9(05).                                  00360005
           03  MIN-PEC     PIC  9(05).                                  00370005
           03  MAX-PEC     PIC  9(05).                                  00380005
           03  FOR-PEC     PIC  X(05).                                  00390005
           03  PRU-PEC     PIC  9(07)V99.                               00400005
           03  PRD-PEC     PIC  9(07)V99.                               00410005
           03  TIP-PEC     PIC  X(01).                                  00420005
           03  FILLER      PIC  X(06).                                  00430005
                                                                        00440032
       FD  CADSEL          RECORD      CONTAINS 50 CHARACTERS           00450051
                           RECORDING   MODE     IS F                    00460001
                           LABEL       RECORD   IS STANDARD             00470001
                           DATA        RECORD   IS REG-SEL.             00480051
                                                                        00490032
       01  REG-SEL.                                                     00500051
           03  COD-SEL     PIC  9(05).                                  00510051
           03  NOM-SEL     PIC  X(30).                                  00520051
           03  QTD-SEL     PIC  9(05).                                  00530051
           03  PRU-SEL     PIC  9(07)V99.                               00540051
           03  FILLER      PIC  X(01).                                  00550051
                                                                        00560032
       WORKING-STORAGE SECTION.                                         00570001
                                                                        00580032
       77  FST-PEC         PIC  X(02).                                  00590005
       77  FST-SEL         PIC  X(02).                                  00600051
                                                                        00610034
       77  FST-ERR         PIC  X(02).                                  00620034
       77  COD-ERR         PIC  9(01)  VALUE ZEROS.                     00630057
       77  OPR-ERR         PIC  X(13).                                  00640005
       77  ARQ-ERR         PIC  X(08).                                  00650046
                                                                        00660033
       77  ACM-LID         PIC  9(12)  COMP-3 VALUE ZEROS.              00670057
       77  ACM-GRV         PIC  9(12)  COMP-3 VALUE ZEROS.              00680057
                                                                        00690033
       77  MSK-LID         PIC  ZZZ.ZZZ.ZZZ.ZZ9.                        00700034
       77  MSK-GRV         PIC  ZZZ.ZZZ.ZZZ.ZZ9.                        00710034
                                                                        00720033
       PROCEDURE       DIVISION.                                        00730001
                                                                        00740033
       000-INICIO.                                                      00750001
           OPEN    INPUT   CADPEC                                       00760003
                   OUTPUT  CADSEL.                                      00770052
                                                                        00780035
           IF  FST-PEC = '00' AND FST-SEL  = '00'                       00790051
               PERFORM        100-GRV-ARQ  UNTIL  FST-PEC = '10'        00800006
           ELSE                                                         00810003
               MOVE  1        TO  COD-ERR                               00820006
               PERFORM        200-ERR-ARQ                               00830006
           END-IF                                                       00840003
                                                                        00850035
           CLOSE     CADPEC   CADSEL                                    00860051
                                                                        00870006
           IF  FST-PEC = '00' AND FST-SEL  =  '00'                      00880051
               MOVE  ACM-LID  TO  MSK-LID                               00890006
               MOVE  ACM-GRV  TO  MSK-GRV                               00900006
                                                                        00910035
               DISPLAY '-> ABSPG003 <-> ESTATISTICA DE PROCESSAMENTO <-'00920051
               DISPLAY '|                                             |'00930006
               DISPLAY '| REGISTROS LIDOS (CADPECA)  ' MSK-LID '  |'    00940044
               DISPLAY '|                                             |'00950006
               DISPLAY '| REGISTROS GRAVADOS (CADSEL)' MSK-GRV '  |'    00960051
               DISPLAY '|                                             |'00970006
               DISPLAY '-----------------------------------------------'00980006
                                                                        00990035
               STOP  RUN                                                01000042
           ELSE                                                         01010042
               MOVE  4        TO  COD-ERR                               01020042
               PERFORM        200-ERR-ARQ                               01030042
           END-IF.                                                      01040042
                                                                        01050035
       100-GRV-ARQ.                                                     01060005
           READ      CADPEC                                             01070003
                                                                        01080005
           IF  FST-PEC  =   '00'                                        01090054
               ADD   1  TO  ACM-LID                                     01100054
                                                                        01110054
               IF  COD-PEC  <    10001   OR  COD-PEC >  19999           01120051
                   MOVE     SPACES   TO  REG-SEL                        01130051
                   MOVE     COD-PEC  TO  COD-SEL                        01140051
                   MOVE     NOM-PEC  TO  NOM-SEL                        01150051
                   MOVE     QTD-PEC  TO  QTD-SEL                        01160051
                   MOVE     PRU-PEC  TO  PRU-SEL                        01170051
                   WRITE    REG-SEL                                     01180051
                                                                        01190051
                   IF  FST-SEL  =   '00'                                01200053
                       ADD      1    TO  ACM-GRV                        01210053
                   ELSE                                                 01220053
                       MOVE     3    TO  COD-ERR                        01230053
                       PERFORM       200-ERR-ARQ                        01240053
                   END-IF                                               01250053
               END-IF                                                   01260053
           ELSE                                                         01270005
               IF  FST-PEC  NOT  =   '10'                               01280042
                   MOVE  2       TO  COD-ERR                            01290042
                   PERFORM           200-ERR-ARQ                        01300043
               END-IF                                                   01310043
           END-IF.                                                      01320005
                                                                        01330035
       200-ERR-ARQ.                                                     01340005
           IF  COD-ERR  =  1  AND  FST-PEC = '10'                       01350005
               DISPLAY '-----------> ABSPG003 <-----------'             01360051
               DISPLAY '|                                |'             01370005
               DISPLAY '|      ARQUIVO CADPECA VAZIO     |'             01380005
               DISPLAY '|                                |'             01390005
               DISPLAY '|       PROGRAMA ENCERRADO!      |'             01400005
               DISPLAY '|                                |'             01410005
               DISPLAY '----------------------------------'             01420005
           ELSE                                                         01430005
               EVALUATE COD-ERR                                         01440005
                   WHEN 1                                               01450005
                        MOVE  'NA ABERTURA'    TO  OPR-ERR              01460005
                   WHEN 2                                               01470005
                        MOVE  'NA LEITURA'     TO  OPR-ERR              01480005
                   WHEN 3                                               01490005
                        MOVE  'NA GRAVACAO'    TO  OPR-ERR              01500005
                   WHEN 4                                               01510005
                        MOVE  'NO FECHAMENTO'  TO  OPR-ERR              01520005
               END-EVALUATE                                             01530020
                                                                        01540035
               IF  FST-PEC  >  '00' AND (COD-ERR =  1 OR 2 OR 4)        01550047
                   MOVE     FST-PEC  TO  FST-ERR                        01560047
                   MOVE    'CADPECA' TO  ARQ-ERR                        01570047
               ELSE                                                     01580005
                   MOVE     FST-SEL  TO  FST-ERR                        01590051
                   MOVE    'CADSEL ' TO  ARQ-ERR                        01600051
               END-IF                                                   01610020
                                                                        01620035
               DISPLAY '--------------> ABSPG003 <----------------'     01630051
               DISPLAY '|                                        |'     01640046
               DISPLAY '| ERRO ' OPR-ERR ' DO ARQUIVO ' ARQ-ERR ' |'    01650038
               DISPLAY '|                                        |'     01660046
               DISPLAY '|            FILE-STATUS: ' FST-ERR '           01670021
      -                '  |'                                            01680046
               DISPLAY '|                                        |'     01690046
               DISPLAY '|           PROGRAMA ENCERRADO!          |'     01700046
               DISPLAY '|                                        |'     01710046
               DISPLAY '------------------------------------------'     01720046
           END-IF.                                                      01730005
                                                                        01740035
           STOP    RUN.                                                 01750006
