       IDENTIFICATION      DIVISION.                                    00010038
       PROGRAM-ID.         ABSPG009.                                    00020059
       AUTHOR.             ADILSON BIZARRIA.                            00030038
       INSTALLATION.       FUTURE SCHOOL.                               00040038
       DATE-WRITTEN.       12/10/2022.                                  00050059
       DATE-COMPILED.                                                   00060025
      *-------------------------------------------------------*         00070059
      * EXER-09 - LER ARQUIVO CADCOTA, COMPARAR AS COTACOES,  *         00080060
      *           GRAVAR AS COTACOES SELECIONADAS NO CADSELE  *         00090059
      *           E GRAVAR AS COTACOES DESPREZADAS NO CADDESP *         00100059
      *-------------------------------------------------------*         00110059
       ENVIRONMENT         DIVISION.                                    00120001
       CONFIGURATION       SECTION.                                     00130001
       SOURCE-COMPUTER.    IBM-3090.                                    00140029
       OBJECT-COMPUTER.    IBM-3090.                                    00150029
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.                      00160017
       INPUT-OUTPUT        SECTION.                                     00170001
                                                                        00180030
       FILE-CONTROL.                                                    00190001
           SELECT  CADCOT  ASSIGN  TO  UT-S-CADCOT                      00200059
                   FILE    STATUS  IS  FST-COT.                         00210059
                                                                        00220000
           SELECT  CADSEL  ASSIGN  TO  UT-S-CADSEL                      00230059
                   FILE    STATUS  IS  FST-SEL.                         00240059
                                                                        00250031
           SELECT  CADDSP  ASSIGN  TO  UT-S-CADDSP                      00260059
                   FILE    STATUS  IS  FST-DSP.                         00270059
                                                                        00280059
       DATA                DIVISION.                                    00290001
       FILE                SECTION.                                     00300008
                                                                        00310031
       FD  CADCOT          RECORD      CONTAINS 80 CHARACTERS           00320059
                           RECORDING   MODE     IS F                    00330001
                           LABEL       RECORD   IS STANDARD             00340001
                           DATA        RECORD   IS REG-COT.             00350059
                                                                        00360031
       01  REG-COT.                                                     00370059
           03  COD-COT         PIC  9(05).                              00380059
           03  QTD-COT         PIC  9(05).                              00390059
           03  DTA-COT         PIC  9(06).                              00400059
           03  C01-COT.                                                 00410059
               05  FR1-COT     PIC  9(05).                              00420059
               05  VC1-COT     PIC  9(13)V99.                           00430059
           03  C02-COT.                                                 00440059
               05  FR2-COT     PIC  9(05).                              00450059
               05  VC2-COT     PIC  9(13)V99.                           00460059
           03  C03-COT.                                                 00470059
               05  FR3-COT     PIC  9(05).                              00480059
               05  VC3-COT     PIC  9(13)V99.                           00490059
           03  FILLER          PIC  X(04).                              00500059
                                                                        00510032
       FD  CADSEL          RECORD      CONTAINS 40 CHARACTERS           00520059
                           RECORDING   MODE     IS F                    00530001
                           LABEL       RECORD   IS STANDARD             00540001
                           DATA        RECORD   IS REG-SEL.             00550051
                                                                        00560032
       01  REG-SEL.                                                     00570051
           03  COD-SEL         PIC  9(05).                              00580059
           03  QTD-SEL         PIC  9(05).                              00590059
           03  DTA-SEL         PIC  9(06).                              00600059
           03  COT-SEL.                                                 00610059
               05  FOR-SEL     PIC  9(05).                              00620059
               05  VLC-SEL     PIC  9(13)V99.                           00630059
           03  FILLER          PIC  X(04).                              00640059
                                                                        00650059
       FD  CADDSP          RECORD      CONTAINS 40 CHARACTERS           00660059
                           RECORDING   MODE     IS F                    00670059
                           LABEL       RECORD   IS STANDARD             00680059
                           DATA        RECORD   IS REG-DSP.             00690059
                                                                        00700059
       01  REG-DSP.                                                     00710059
           03  COD-DSP         PIC  9(05).                              00720059
           03  QTD-DSP         PIC  9(05).                              00730059
           03  DTA-DSP         PIC  9(06).                              00740059
           03  COT-DSP.                                                 00750059
               05  FOR-DSP     PIC  9(05).                              00760059
               05  VLC-DSP     PIC  9(13)V99.                           00770059
           03  FILLER          PIC  X(04).                              00780059
                                                                        00790032
       WORKING-STORAGE SECTION.                                         00800001
                                                                        00810032
       77  IND                 PIC  9(01).                              00820062
       77  FST-COT             PIC  X(02).                              00830062
       77  FST-SEL             PIC  X(02).                              00840062
       77  FST-DSP             PIC  X(02).                              00850062
                                                                        00860034
       77  FST-ERR             PIC  X(02).                              00870062
       77  OPR-ERR             PIC  X(13).                              00880062
       77  ARQ-ERR             PIC  X(08).                              00890062
                                                                        00900033
       77  ACM-LID             PIC  9(12)  COMP-3 VALUE ZEROS.          00910062
       77  ACM-SEL             PIC  9(12)  COMP-3 VALUE ZEROS.          00920062
       77  ACM-DSP             PIC  9(12)  COMP-3 VALUE ZEROS.          00930062
                                                                        00940033
       77  MSK-LID             PIC  ZZZ.ZZZ.ZZZ.ZZ9.                    00950062
       77  MSK-SEL             PIC  ZZZ.ZZZ.ZZZ.ZZ9.                    00960062
       77  MSK-DSP             PIC  ZZZ.ZZZ.ZZZ.ZZ9.                    00970062
                                                                        00980033
       01  TAB-COT.                                                     00990062
           03  COT-TAB         OCCURS     04.                           01000062
               05  FOR-TAB     PIC  9(05).                              01010062
               05  VAL-TAB     PIC  9(13)V99.                           01020062
                                                                        01030059
       PROCEDURE       DIVISION.                                        01040001
                                                                        01050033
       000-INI-PRG.                                                     01060059
           OPEN     INPUT    CADCOT                                     01070059
                    OUTPUT   CADSEL   CADDSP.                           01080059
                                                                        01090035
           MOVE     'ABERTURA'   TO  OPR-ERR                            01100066
           PERFORM  500-CHK-ERR                                         01110059
           PERFORM  100-LER-COT  UNTIL  FST-COT = '10'                  01120059
                                                                        01130059
           CLOSE    CADCOT   CADSEL   CADDSP                            01140059
                                                                        01150059
           MOVE     'FECHAMENTO' TO  OPR-ERR                            01160066
           PERFORM  500-CHK-ERR                                         01170059
                                                                        01180059
           MOVE     ACM-LID  TO  MSK-LID                                01190059
           MOVE     ACM-SEL  TO  MSK-SEL                                01200059
           MOVE     ACM-DSP  TO  MSK-DSP                                01210059
                                                                        01220059
           DISPLAY '--> ABSPG009 <--> ESTATISTICA DE PROCESSAMENTO <--' 01230059
           DISPLAY '|                                                |' 01240059
           DISPLAY '|   REGISTROS LIDOS (CADCOTA)  ' MSK-LID '   |'     01250059
           DISPLAY '|                                                |' 01260059
           DISPLAY '|   COTACOES SELECIONADAS      ' MSK-SEL '   |'     01270059
           DISPLAY '|                                                |' 01280059
           DISPLAY '|   COTACOES DESPREZADAS       ' MSK-DSP '   |'     01290059
           DISPLAY '|                                                |' 01300059
           DISPLAY '--------------------------------------------------' 01310059
                                                                        01320059
           STOP  RUN.                                                   01330059
                                                                        01340059
       100-LER-COT.                                                     01350059
           READ     CADCOT                                              01360059
                                                                        01370059
           IF  FST-COT  =   '00'                                        01380059
               ADD      1        TO  ACM-LID                            01390059
               MOVE     C01-COT  TO  COT-TAB(1)                         01400062
               MOVE     C02-COT  TO  COT-TAB(2)                         01410062
               MOVE     C03-COT  TO  COT-TAB(3)                         01420062
                                                                        01430059
               PERFORM  210-ORD-COT  02  TIMES                          01440059
               PERFORM  310-GRV-SEL                                     01450059
               PERFORM  320-GRV-DSP  VARYING  IND  FROM  2  BY  1       01460059
                                     UNTIL    IND   >    3              01470059
           ELSE                                                         01480059
               IF  FST-COT  NOT  =  '10'                                01490059
                   MOVE     FST-COT  TO  FST-ERR                        01500066
                   MOVE    'LEITURA' TO  OPR-ERR                        01510066
                   MOVE    'CADCOTA' TO  ARQ-ERR                        01520059
                   PERFORM  510-ERR-ARQ                                 01530059
               END-IF                                                   01540059
           END-IF.                                                      01550059
                                                                        01560059
       210-ORD-COT.                                                     01570059
           PERFORM  220-ORD-COT  VARYING  IND  FROM  1  BY  1           01580059
                                 UNTIL    IND   >    2.                 01590063
       220-ORD-COT.                                                     01600059
           IF  VAL-TAB(IND)  >   VAL-TAB(IND + 1)                       01610059
               MOVE  VAL-TAB(IND)      TO  VAL-TAB(4)                   01620059
               MOVE  VAL-TAB(IND + 1)  TO  VAL-TAB(IND)                 01630059
               MOVE  VAL-TAB(4)        TO  VAL-TAB(IND + 1)             01640063
           END-IF                                                       01650066
                                                                        01660066
           MOVE     'GRAVACAO'   TO  OPR-ERR.                           01670066
                                                                        01680059
       310-GRV-SEL.                                                     01690059
           MOVE     SPACES   TO  REG-SEL                                01700059
           MOVE     COD-COT  TO  COD-SEL                                01710059
           MOVE     QTD-COT  TO  QTD-SEL                                01720059
           MOVE     DTA-COT  TO  DTA-SEL                                01730059
           MOVE     COT-TAB(1)   TO  COT-SEL                            01740059
           WRITE    REG-SEL                                             01750059
                                                                        01760059
           IF  FST-SEL  =   '00'                                        01770065
               ADD      1        TO  ACM-SEL                            01780065
           ELSE                                                         01790065
               MOVE     FST-SEL  TO  FST-ERR                            01800066
               MOVE    'CADSELE' TO  ARQ-ERR                            01810059
               PERFORM  510-ERR-ARQ                                     01820059
           END-IF.                                                      01830059
                                                                        01840059
       320-GRV-DSP.                                                     01850059
           MOVE     SPACES   TO  REG-DSP                                01860059
           MOVE     COD-COT  TO  COD-DSP                                01870059
           MOVE     QTD-COT  TO  QTD-DSP                                01880059
           MOVE     DTA-COT  TO  DTA-DSP                                01890059
           MOVE     COT-TAB(IND) TO  COT-DSP                            01900059
           WRITE    REG-DSP                                             01910059
                                                                        01920059
           IF  FST-DSP  =   '00'                                        01930065
               ADD      1        TO  ACM-DSP                            01940065
           ELSE                                                         01950065
               MOVE     FST-DSP  TO  FST-ERR                            01960066
               MOVE    'CADDESP' TO  ARQ-ERR                            01970059
               PERFORM  510-ERR-ARQ                                     01980059
           END-IF.                                                      01990059
                                                                        02000059
       500-CHK-ERR.                                                     02010060
           IF  FST-COT  >  '00'  OR  FST-SEL  >  '00'                   02020060
                                 OR  FST-DSP  >  '00'                   02030060
               EVALUATE TRUE                                            02040063
                 WHEN   FST-COT  >  '00'                                02050060
                        MOVE         FST-COT  TO  FST-ERR               02060060
                        MOVE        'CADCOTA' TO  ARQ-ERR               02070060
                 WHEN   FST-SEL  >  '00'                                02080060
                        MOVE         FST-SEL  TO  FST-ERR               02090063
                        MOVE        'CADSELE' TO  ARQ-ERR               02100060
                 WHEN   FST-DSP  >  '00'                                02110060
                        MOVE         FST-DSP  TO  FST-ERR               02120060
                        MOVE        'CADDESP' TO  ARQ-ERR               02130060
               END-EVALUATE                                             02140060
                                                                        02150060
               PERFORM  510-ERR-ARQ                                     02160060
           END-IF.                                                      02170060
                                                                        02180060
       510-ERR-ARQ.                                                     02190060
           IF  OPR-ERR  =  'ABERTURA'  AND  FST-COT = '10'              02200066
               DISPLAY '-----------> ABSPG009 <-----------'             02210060
               DISPLAY '|                                |'             02220060
               DISPLAY '|      ARQUIVO CADCOTA VAZIO     |'             02230060
               DISPLAY '|                                |'             02240060
               DISPLAY '|       PROGRAMA ENCERRADO!      |'             02250060
               DISPLAY '|                                |'             02260060
               DISPLAY '----------------------------------'             02270060
           ELSE                                                         02280060
               DISPLAY '---------------> ABSPG009 <----------------'    02290066
               DISPLAY '|                                         |'    02300066
               DISPLAY '|  ERRO ' OPR-ERR ' DO ARQUIVO ' ARQ-ERR '  |'  02310066
               DISPLAY '|                                         |'    02320066
               DISPLAY '|             FILE-STATUS: ' FST-ERR '          02330067
      -                '    |'                                          02340067
               DISPLAY '|                                         |'    02350066
               DISPLAY '|           PROGRAMA ENCERRADO!           |'    02360066
               DISPLAY '|                                         |'    02370066
               DISPLAY '-------------------------------------------'    02380066
           END-IF                                                       02390066
                                                                        02400060
           STOP    RUN.                                                 02410060
