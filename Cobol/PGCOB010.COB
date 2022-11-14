       IDENTIFICATION      DIVISION.                                    00010038
       PROGRAM-ID.         PGCOB010.                                    00020086
       AUTHOR.             ADILSON BIZARRIA.                            00030038
       INSTALLATION.       FUTURE SCHOOL.                               00040038
       DATE-WRITTEN.       13/10/2022.                                  00050086
       DATE-COMPILED.                                                   00060025
      *----------------------------------------------------------*      00070086
      * EXER-10 - EMITIR LISTAGEM DE ALUNOS A PARTIR DO CADALUNO *      00080086
      *           COM TOTAIS DE ALUNOS POR TURMA E TOTAL GERAL   *      00090086
      *----------------------------------------------------------*      00110086
       ENVIRONMENT         DIVISION.                                    00120001
       CONFIGURATION       SECTION.                                     00130001
       SOURCE-COMPUTER.    IBM-3090.                                    00140029
       OBJECT-COMPUTER.    IBM-3090.                                    00150029
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.                      00160017
       INPUT-OUTPUT        SECTION.                                     00170001
                                                                        00180030
       FILE-CONTROL.                                                    00190001
           SELECT  CADALU  ASSIGN  TO  UT-S-CADALU                      00200086
                   FILE    STATUS  IS  FST-ALU.                         00210086
                                                                        00220000
           SELECT  RELATO  ASSIGN  TO  UT-S-RELATO                      00230086
                   FILE    STATUS  IS  FST-REL.                         00240086
                                                                        00280059
       DATA                DIVISION.                                    00290001
       FILE                SECTION.                                     00300008
                                                                        00310031
       FD  CADALU          RECORD      CONTAINS  80  CHARACTERS         00320086
                           RECORDING   MODE      IS  F                  00330086
                           LABEL       RECORD    IS  STANDARD           00340086
                           DATA        RECORD    IS  REG-ALU.           00350086
       01  REG-ALU.                                                     00360086
           03  COD-ALU         PIC  9(04).                              00370088
           03  NOM-ALU         PIC  X(30).                              00380088
           03  TUR-ALU         PIC  9(03).                              00390088
           03  NT1-ALU         PIC  9(02)V99.                           00400088
           03  NT2-ALU         PIC  9(02)V99.                           00401088
           03  NT3-ALU         PIC  9(02)V99.                           00402088
           03  NT4-ALU         PIC  9(02)V99.                           00403088
           03  FILLER          PIC  X(07).                              00410088
                                                                        00500032
       FD  RELATO          RECORD      CONTAINS  60  CHARACTERS         00510088
                           RECORDING   MODE      IS  F                  00520086
                           LABEL       RECORD    IS  STANDARD           00530086
                           DATA        RECORD    IS  REG-REL.           00540086
                                                                        00541086
       01  REG-REL.        PIC  X(60).                                  00550088
                                                                        00630059
       WORKING-STORAGE SECTION.                                         00770001
                                                                        00780032
       77  FST-ALU             PIC  X(02).                              00800086
       77  FST-REL             PIC  X(02).                              00810086
                                                                        00830034
       77  FST-ERR             PIC  X(02).                              00840062
       77  OPR-ERR             PIC  X(06).                              00850072
       77  ARQ-ERR             PIC  X(08).                              00860062
                                                                        00870033
       77  TUR-ANT             PIC  9(03)  VALUE  ZEROS.                00871088
       77  ACM-LID             PIC  9(12)  COMP-3 VALUE ZEROS.          00880062
       77  ACM-IMP             PIC  9(12)  COMP-3 VALUE ZEROS.          00890086
                                                                        00910033
       77  MSK-LID             PIC  ZZZ.ZZZ.ZZZ.ZZ9.                    00920062
       77  MSK-IMP             PIC  ZZZ.ZZZ.ZZZ.ZZ9.                    00930086
                                                                        00950033
       01  DTA-SYS.                                                     00951088
           03  ANO-SYS     PIC  9(04).                                  00952088
           03  MES-SYS     PIC  9(02).                                  00953088
           03  DIA-SYS     PIC  9(02).                                  00953188
           03  HRA-SYS     PIC  9(02).                                  00954088
           03  MIN-SYS     PIC  9(02).                                  00955088
           03  FILLER      PIC  X(09).                                  00956088
                                                                        00958088
       01  CAB-001.                                                     00960088
            3  FILLER          PIC  X(39)  VALUE                        00970088
               'FUTURE SCHOOL CURSOS DE COMPUTACAO'.                    00980088
           03  HRA-CB1         PIC  9(02).                              00990088
           03  FILLER          PIC  X(01)  VALUE  ':'                   01000088
           03  MIN-CB1         PIC  9(02).                              01001088
           03  FILLER          PIC  X(11)  VALUE  '     PAG.:'          01002088
           03  PAG-CB1         PIC  Z.ZZ9.                              01003088
                                                                        01004088
       01  CAB-002.                                                     01005088
           03  FILLER          PIC  X(50)  VALUE  'PROGRAMA = PGCOB010'.01006088
           03  DIA-CB1         PIC  9(02).                              01007088
           03  FILLER          PIC  X(01)  VALUE  '/'.                  01008088
           03  MES-CB1         PIC  9(02).                              01009088
           03  FILLER          PIC  X(01)  VALUE  '/'.                  01009188
           03  ANO-CB1         PIC  9(04).                              01009288
                                                                        01009388
       01  CAB-003.                                                     01009488
           03  FILLER          PIC  X(09)  VALUE  SPACES.               01009588
           03  FILLER          PIC  X(51)  VALUE                        01009688
               'RELATORIO DE ALUNOS MATRICULADOS POR TURMA'.            01009788
                                                                        01009888
       01  CAB-004             PIC  X(60)  VALUE  ALL '='.              01009988
                                                                        01010088
       01  CAB-005.                                                     01010188
           03  FILLER          PIC  X(19)  VALUE  'CODIGO'.             01010288
           03  FILLER          PIC  X(36)  VALUE  'NOME DO ALUNO'.      01010388
           03  FILLER          PIC  X(05)  VALUE  'TURMA'.              01010488
                                                                        01010588
       01  DET-001.                                                     01010688
           03  FILLER          PIC  X(01)  VALUE  SPACES.               01010788
           03  COD-DT1         PIC  9(04).                              01010888
           03  FILLER          PIC  X(11)  VALUE  SPACES.               01010988
           03  NOM-DT1         PIC  X(40).                              01011088
           03  TUR-DT1         PIC  9(03).                              01011188
           03  FILLER          PIC  X(01)  VALUE  SPACES.               01011288
                                                                        01011388
       01  TOT-001.                                                     01011488
           03  FILLER          PIC  X(12)  VALUE  '***** TURMA'         01011588
           03  TUR-TT1         PIC  9(03).                              01011688
           03  FILLER          PIC  X(25)  VALUE                        01011788
               '    TOTAL DE ALUNOS =>'.                                01011888
           03  QTD-TT1         PIC  Z.ZZ9.                              01011988
           03  FILLER          PIC  X(14)  VALUE  SPACES.               01012088
                                                                        01012188
       01  TOT-002.                                                     01012288
           03  FILLER          PIC  X(39)  VALUE                        01012388
               '***** TOTAL DE ALUNOS LIDOS        =>'.                 01012488
           03  QTD-TT2         PIC  ZZ.ZZ9.                             01012588
           03  FILLER          PIC  X(15)  VALUE  SPACES.               01012688
                                                                        01012788
       PROCEDURE       DIVISION.                                        01013001
                                                                        01020033
       000-INI-PRG.                                                     01030059
           OPEN     INPUT    CADALU                                     01040086
                    OUTPUT   CADREL.                                    01050086
                                                                        01060076
           MOVE     'ABRIR'  TO  OPR-ERR                                01070072
           PERFORM  500-CHK-ERR                                         01080059
           PERFORM  100-LER-ALU  UNTIL  FST-ALU = '10'                  01090086
                                                                        01100059
           CLOSE    CADALU   CADREL                                     01110086
                                                                        01120079
           MOVE     'FECHAR' TO  OPR-ERR                                01130072
           PERFORM  500-CHK-ERR                                         01140059
                                                                        01150059
           MOVE     ACM-LID  TO  MSK-LID                                01160059
           MOVE     ACM-IMP  TO  MSK-IMP                                01170086
           MOVE     ACM-DSP  TO  MSK-DSP                                01180059
                                                                        01190059
           DISPLAY '--> PGCOB010 <--> ESTATISTICA DE PROCESSAMENTO <--' 01200086
           DISPLAY '|                                                |' 01210059
           DISPLAY '|   REGISTROS LIDOS (CADALUNO) ' MSK-LID '   |'     01220088
           DISPLAY '|                                                |' 01230059
           DISPLAY '|   REGISTROS IMPRESSOS        ' MSK-IMP '   |'     01240088
           DISPLAY '|                                                |' 01270059
           DISPLAY '--------------------------------------------------' 01280059
                                                                        01290059
           STOP  RUN.                                                   01300059
                                                                        01310059
       100-LER-ALU.                                                     01320086
           READ     CADALU                                              01330086
                                                                        01340081
           IF  FST-ALU  =   '00'                                        01350086
               ADD      1        TO  ACM-LID                            01360059
               MOVE     C01-ALU  TO  COT-TAB(1)                         01370086
               MOVE     C02-ALU  TO  COT-TAB(2)                         01380086
               MOVE     C03-ALU  TO  COT-TAB(3)                         01390086
                                                                        01400085
               PERFORM  210-ORD-ALU  02  TIMES                          01410086
                                                                        01420085
               MOVE     SPACES   TO  REG-SEL  REG-DSP                   01430085
                                                                        01440085
               PERFORM  310-GRV-SEL                                     01450059
               PERFORM  320-GRV-DSP  VARYING  IND  FROM  2  BY  1       01460059
                                     UNTIL    IND   >    3              01470059
           ELSE                                                         01480059
               IF  FST-ALU  NOT  =  '10'                                01490086
                   MOVE     FST-ALU  TO  FST-ERR                        01500086
                   MOVE    'LER'     TO  OPR-ERR                        01510072
                   MOVE    'CADALUA' TO  ARQ-ERR                        01520086
                   PERFORM  510-ERR-ARQ                                 01530059
               END-IF                                                   01540059
           END-IF.                                                      01550059
                                                                        01560059
        110-IMP-CAB.                                                    01561089
            MOVE    FUNCTION     CURRENT-DATE  TO  DTA-SYS              01562089
            ADD     1        TO  ACM-PAG                                01563089
            MOVE    ACM-PAG  TO  PAG-CB1                                01564089
            MOVE    HRA-SYS  TO  HRA-CB1                                01565089
            MOVE    MIN-SYS  TO  MIN-CB1                                01566089
            MOVE    DIA-SYS  TO  DIA-CB2                                01567089
            MOVE    MES-SYS  TO  MES-CB2                                01568089
            MOVE    ANO-SYS  TO  ANO-CB2                                01569089
                                                                        01569189
            WRITE   REG-REL FROM CAB-001  AFTER  PAGE                   01569289
            WRITE   REG-REL FROM CAB-002  AFTER  1                      01569389
            WRITE   REG-REL FROM CAB-003  AFTER  2                      01569489
            WRITE   REG-REL FROM CAB-004  AFTER  2                      01569589
       210-ORD-ALU.                                                     01570086
           PERFORM  220-ORD-ALU  VARYING  IND  FROM  1  BY  1           01580086
                                 UNTIL    IND   >    2.                 01590063
                                                                        01600084
           MOVE     'GRAVAR' TO  OPR-ERR.                               01610084
                                                                        01620084
       220-ORD-ALU.                                                     01630086
           IF  VAL-TAB(IND)  >   VAL-TAB(IND + 1)                       01640059
               MOVE  VAL-TAB(IND)      TO  VAL-TAB(4)                   01650059
               MOVE  VAL-TAB(IND + 1)  TO  VAL-TAB(IND)                 01660059
               MOVE  VAL-TAB(4)        TO  VAL-TAB(IND + 1)             01670063
           END-IF.                                                      01680084
                                                                        01690059
       310-GRV-SEL.                                                     01700059
           MOVE     COD-ALU  TO  COD-SEL                                01710086
           MOVE     QTD-ALU  TO  QTD-SEL                                01720086
           MOVE     DTA-ALU  TO  DTA-SEL                                01730086
           MOVE     COT-TAB(1)   TO  COT-SEL                            01740059
           WRITE    REG-SEL                                             01750059
                                                                        01760082
           IF  FST-SEL  =   '00'                                        01770065
               ADD      1        TO  ACM-SEL                            01780065
           ELSE                                                         01790065
               MOVE     FST-SEL  TO  FST-ERR                            01800066
               MOVE    'CADSELE' TO  ARQ-ERR                            01810059
               PERFORM  510-ERR-ARQ                                     01820059
           END-IF.                                                      01830059
                                                                        01840059
       320-GRV-DSP.                                                     01850059
           MOVE     COD-ALU  TO  COD-DSP                                01860086
           MOVE     QTD-ALU  TO  QTD-DSP                                01870086
           MOVE     DTA-ALU  TO  DTA-DSP                                01880086
           MOVE     COT-TAB(IND) TO  COT-DSP                            01890059
           WRITE    REG-DSP                                             01900059
                                                                        01910083
           IF  FST-DSP  =   '00'                                        01920065
               ADD      1        TO  ACM-DSP                            01930065
           ELSE                                                         01940065
               MOVE     FST-DSP  TO  FST-ERR                            01950066
               MOVE    'CADDESP' TO  ARQ-ERR                            01960059
               PERFORM  510-ERR-ARQ                                     01970059
           END-IF.                                                      01980059
                                                                        01990059
       500-CHK-ERR.                                                     02000060
           IF  FST-ALU  >  '00'  OR  FST-REL  >  '00'                   02010088
               EVALUATE TRUE                                            02030063
                 WHEN   FST-ALU  >  '00'                                02040086
                        MOVE         FST-ALU   TO  FST-ERR              02050088
                        MOVE        'CADALUNO' TO  ARQ-ERR              02060088
                 WHEN   FST-REL  >  '00'                                02070088
                        MOVE         FST-REL   TO  FST-ERR              02080088
                        MOVE        'RELATO'   TO  ARQ-ERR              02090088
               END-EVALUATE                                             02130060
                                                                        02140060
               PERFORM  510-ERR-ARQ                                     02150060
           END-IF.                                                      02160060
                                                                        02170060
       510-ERR-ARQ.                                                     02180060
           IF  OPR-ERR  =  'ABRIR'  AND  FST-ALU  =  '10'               02190086
               DISPLAY '-----------> PGCOB010 <-----------'             02200086
               DISPLAY '|                                |'             02210060
               DISPLAY '|     ARQUIVO CADALUNO VAZIO     |'             02220088
               DISPLAY '|                                |'             02230060
               DISPLAY '|       PROGRAMA ENCERRADO!      |'             02240060
               DISPLAY '|                                |'             02250060
               DISPLAY '----------------------------------'             02260060
           ELSE                                                         02270072
               DISPLAY '---------------> PGCOB010 <------------'        02280086
               DISPLAY '|                                     |'        02290072
               DISPLAY '|  ERRO AO ' OPR-ERR ' O ARQUIVO ' ARQ-ERR '  |'02300072
               DISPLAY '|                                     |'        02310072
               DISPLAY '|           FILE-STATUS: ' FST-ERR '           |02320072
      -                ''                                               02330072
               DISPLAY '|                                     |'        02340072
               DISPLAY '|         PROGRAMA ENCERRADO!         |'        02350072
               DISPLAY '|                                     |'        02360072
               DISPLAY '---------------------------------------'        02370072
           END-IF                                                       02380066
                                                                        02390060
           STOP    RUN.                                                 02400060
