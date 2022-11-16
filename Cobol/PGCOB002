       IDENTIFICATION      DIVISION.                                    00010038
       PROGRAM-ID.         ABSPG002.                                    00020045
       AUTHOR.             ADILSON BIZARRIA.                            00030038
       INSTALLATION.       FUTURE SCHOOL.                               00040038
       DATE-WRITTEN.       07/10/2022.                                  00050045
       DATE-COMPILED.                                                   00060025
      *-----------------------------------------------------------*     00070053
      * EXER-02 - CRIAR CADATU COM CAMPOS SELECIONADOS DO CADPECA *     00080053
      *-----------------------------------------------------------*     00090053
       ENVIRONMENT         DIVISION.                                    00100001
       CONFIGURATION       SECTION.                                     00110001
       SOURCE-COMPUTER.    IBM-3090.                                    00120029
       OBJECT-COMPUTER.    IBM-3090.                                    00130029
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.                      00140017
       INPUT-OUTPUT        SECTION.                                     00150001
                                                                        00160030
       FILE-CONTROL.                                                    00170001
           SELECT CADPEC   ASSIGN  TO  UT-S-CADPEC                      00180051
                  FILE     STATUS  IS  FST-PEC.                         00190051
                                                                        00200000
           SELECT CADATU   ASSIGN  TO  UT-S-CADATU                      00210051
                  FILE     STATUS  IS  FST-ATU.                         00220051
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
       FD  CADATU          RECORD      CONTAINS 60 CHARACTERS           00440048
                           RECORDING   MODE     IS F                    00450001
                           LABEL       RECORD   IS STANDARD             00460001
                           DATA        RECORD   IS REG-ATU.             00470001
                                                                        00480032
       01  REG-ATU.                                                     00490001
           03  COD-ATU     PIC  9(05).                                  00500005
           03  NOM-ATU     PIC  X(30).                                  00510005
           03  QTD-ATU     PIC  9(05).                                  00520005
           03  PRU-ATU     PIC  9(07)V99.                               00530005
           03  AST-ATU     PIC  X(03).                                  00540046
           03  FILLER      PIC  X(08).                                  00550046
                                                                        00560032
       WORKING-STORAGE SECTION.                                         00570001
                                                                        00580032
       77  FST-PEC         PIC  X(02).                                  00590005
       77  FST-ATU         PIC  X(02).                                  00600005
                                                                        00610034
       77  FST-ERR         PIC  X(02).                                  00620034
       77  COD-ERR         PIC  9(01)  VALUE ZEROS.                     00630052
       77  OPR-ERR         PIC  X(13).                                  00640005
       77  ARQ-ERR         PIC  X(08).                                  00650046
                                                                        00660033
       77  ACM-LID         PIC  9(12)  COMP-3 VALUE ZEROS.              00670052
       77  ACM-GRV         PIC  9(12)  COMP-3 VALUE ZEROS.              00680052
                                                                        00690033
       77  MSK-LID         PIC  ZZZ.ZZZ.ZZZ.ZZ9.                        00700034
       77  MSK-GRV         PIC  ZZZ.ZZZ.ZZZ.ZZ9.                        00710034
                                                                        00720033
       PROCEDURE       DIVISION.                                        00730001
                                                                        00740033
       000-INICIO.                                                      00750001
           OPEN    INPUT   CADPEC                                       00760003
                   OUTPUT  CADATU.                                      00770003
                                                                        00780035
           IF  FST-PEC = '00' AND FST-ATU  = '00'                       00790006
               PERFORM        100-GRV-ARQ  UNTIL  FST-PEC = '10'        00800006
           ELSE                                                         00810003
               MOVE  1        TO  COD-ERR                               00820006
               PERFORM        200-ERR-ARQ                               00830006
           END-IF                                                       00840003
                                                                        00850035
           CLOSE     CADPEC   CADATU                                    00860006
                                                                        00870006
           IF  FST-PEC = '00' AND FST-ATU  =  '00'                      00880042
               MOVE  ACM-LID  TO  MSK-LID                               00890006
               MOVE  ACM-GRV  TO  MSK-GRV                               00900006
                                                                        00910035
               DISPLAY '-> ABSPG002 <-> ESTATISTICA DE PROCESSAMENTO <-'00920046
               DISPLAY '|                                             |'00930006
               DISPLAY '| REGISTROS LIDOS (CADPECA)  ' MSK-LID '  |'    00940044
               DISPLAY '|                                             |'00950006
               DISPLAY '| REGISTROS GRAVADOS (CADATU)' MSK-GRV '  |'    00960044
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
           IF  FST-PEC  =  '00'                                         01090005
               ADD      1        TO  ACM-LID                            01100005
               MOVE     SPACES   TO  REG-ATU                            01110050
               MOVE     COD-PEC  TO  COD-ATU                            01120046
               MOVE     NOM-PEC  TO  NOM-ATU                            01130046
               MOVE     QTD-PEC  TO  QTD-ATU                            01140046
               MOVE     PRU-PEC  TO  PRU-ATU                            01150046
               MOVE     '***'    TO  AST-ATU                            01160046
               WRITE    REG-ATU                                         01170046
                                                                        01180035
               IF  FST-ATU  =   '00'                                    01190005
                   ADD      1    TO  ACM-GRV                            01200005
               ELSE                                                     01210005
                   MOVE     3    TO  COD-ERR                            01220005
                   PERFORM       200-ERR-ARQ                            01230005
               END-IF                                                   01240005
           ELSE                                                         01250005
               IF  FST-PEC  NOT  =   '10'                               01260042
                   MOVE  2       TO  COD-ERR                            01270042
                   PERFORM           200-ERR-ARQ                        01280043
               END-IF                                                   01290043
           END-IF.                                                      01300005
                                                                        01310035
       200-ERR-ARQ.                                                     01320005
           IF  COD-ERR  =  1  AND  FST-PEC = '10'                       01330005
               DISPLAY '-----------> ABSPG002 <-----------'             01340046
               DISPLAY '|                                |'             01350005
               DISPLAY '|      ARQUIVO CADPECA VAZIO     |'             01360005
               DISPLAY '|                                |'             01370005
               DISPLAY '|       PROGRAMA ENCERRADO!      |'             01380005
               DISPLAY '|                                |'             01390005
               DISPLAY '----------------------------------'             01400005
           ELSE                                                         01410005
               EVALUATE COD-ERR                                         01420005
                   WHEN 1                                               01430005
                        MOVE  'NA ABERTURA'    TO  OPR-ERR              01440005
                   WHEN 2                                               01450005
                        MOVE  'NA LEITURA'     TO  OPR-ERR              01460005
                   WHEN 3                                               01470005
                        MOVE  'NA GRAVACAO'    TO  OPR-ERR              01480005
                   WHEN 4                                               01490005
                        MOVE  'NO FECHAMENTO'  TO  OPR-ERR              01500005
               END-EVALUATE                                             01510020
                                                                        01520035
               IF  FST-PEC  >  '00' AND (COD-ERR =  1 OR 2 OR 4)        01530047
                   MOVE     FST-PEC  TO  FST-ERR                        01540047
                   MOVE    'CADPECA' TO  ARQ-ERR                        01550047
               ELSE                                                     01560005
                   MOVE     FST-ATU  TO  FST-ERR                        01570047
                   MOVE    'CADATU'  TO  ARQ-ERR                        01580047
               END-IF                                                   01590020
                                                                        01600035
               DISPLAY '--------------> ABSPG002 <----------------'     01610046
               DISPLAY '|                                        |'     01620046
               DISPLAY '| ERRO ' OPR-ERR ' DO ARQUIVO ' ARQ-ERR ' |'    01630038
               DISPLAY '|                                        |'     01640046
               DISPLAY '|            FILE-STATUS: ' FST-ERR '           01650021
      -                '  |'                                            01660046
               DISPLAY '|                                        |'     01670046
               DISPLAY '|           PROGRAMA ENCERRADO!          |'     01680046
               DISPLAY '|                                        |'     01690046
               DISPLAY '------------------------------------------'     01700046
           END-IF.                                                      01710005
                                                                        01720035
           STOP    RUN.                                                 01730006
