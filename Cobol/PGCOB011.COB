       IDENTIFICATION      DIVISION.                                    00010038
       PROGRAM-ID.         PG011SRT                                   00020086
       AUTHOR.             ADILSON BIZARRIA.                            00030038
       INSTALLATION.       FUTURE SCHOOL.                               00040038
       DATE-WRITTEN.       14/10/2022.                                  00050086
       DATE-COMPILED.                                                   00060025
      *-----------------------------------------------------------*     00070086
      * EXER-11 - EMITIR LISTAGEM DE MATERIAIS A PARTIR DO CADMAT *     00080086
      *           COM SUB-TOTAIS POR CONTA E TOTAL GERAL AO FINAL *     00090086
      *           DO RELATORIO. GRAVAR UM REGISTRO NO CADCONTA A  *
      *           A CADA SUB-TOTAL DE CONTA                       *
      *-----------------------------------------------------------*     00110086
       ENVIRONMENT         DIVISION.                                    00120001
       CONFIGURATION       SECTION.                                     00130001
       SOURCE-COMPUTER.    IBM-3090.                                    00140029
       OBJECT-COMPUTER.    IBM-3090.                                    00150029
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.                      00160017
       INPUT-OUTPUT        SECTION.                                     00170001
                                                                        00180030
       FILE-CONTROL.                                                    00190001
           SELECT  CADMAT  ASSIGN  TO  UT-S-CADMAT                      00200086
                   FILE    STATUS  IS  FST-MAT.                         00210086
                                                                        00220000
           SELECT  CADSRT  ASSING  TO  UT-S-CADSTR.

           SELECT  CADCTA  ASSIGN  TO  UT-S-CADCTA                      00200086
                   FILE    STATUS  IS  FST-CTA.                         00210086
                                                                        00220000
           SELECT  RELATO  ASSIGN  TO  UT-S-RELATO                      00230086
                   FILE    STATUS  IS  FST-REL.                         00240086
                                                                        00280059
       DATA                DIVISION.                                    00290001
       FILE                SECTION.                                     00300008
                                                                        00310031
       FD  CADMAT          RECORD      CONTAINS  50  CHARACTERS         00320086
                           RECORDING   MODE      IS  F                  00330086
                           LABEL       RECORD    IS  STANDARD           00340086
                           DATA        RECORD    IS  REG-MAT.           00350086
      *COPY  CADMAT.

       01  REG-MAT.                                                     00360086
           03  COD-MAT         PIC  9(05).                              00370088
           03  DSC-MAT         PIC  X(20).                              00380088
           03  QTD-MAT         PIC  9(05).                              00390088
           03  VAL-MAT         PIC  9(05)V99.                           00400088
           03  CTA-MAT         PIC  X(10).                              00401088
           03  FILLER          PIC  X(03).                              00410088
                                                                        00500032
       SD  CADSTR          DATA        RECORD    IS  REG-SRT.

       01  REG-SRT.
           03  CTA-SRT         PIC  X(10).
           03  MAT-SRT         PIC  9(05).
           03  DSC-SRT         PIC  X(20).
           03  QTD-SRT         PIC  9(05).
           03  VAL-STR         PIC  9(05)V99.

       FD  CADCTA          RECORD      CONTAINS  50  CHARACTERS         00320086
                           RECORDING   MODE      IS  F                  00330086
                           LABEL       RECORD    IS  STANDARD           00340086
                           DATA        RECORD    IS  REG-CTA.           00350086
      *COPY  CADCONTA.

       01  REG-CTA.                                                     00360086
           03  COD-CTA         PIC  X(10).                              00370088
           03  QTD-CTA         PIC  9(06).                              00380088
           03  VAL-CTA         PIC  9(11)V99.                           00390088
           03  FILLER          PIC  X(01).                              00400088

       FD  RELATO          RECORD      CONTAINS  80  CHARACTERS         00510088
                           RECORDING   MODE      IS  F                  00520086
                           LABEL       RECORD    IS  STANDARD           00530086
                           DATA        RECORD    IS  REG-REL.           00540086
                                                                        00541086
       01  REG-REL.        PIC  X(80).                                  00550088
                                                                        00630059
       WORKING-STORAGE SECTION.                                         00770001
                                                                        00780032
       77  FST-MAT             PIC  X(02).                              00800086
       77  FST-CTA             PIC  X(02).                              00810086
       77  FST-SRT             PIC  X(02)     VALUE  '00'.              00810086
                                                                        00830034
       77  FST-ERR             PIC  X(02).                              00840062
       77  OPR-ERR             PIC  X(06).                              00850072
       77  ARQ-ERR             PIC  X(08).                              00860062
                                                                        00870033
       77  CTA-ANT             PIC  X(10)     VALUE  SPACES.

       77  ACM-LIN             PIC  9(02)     VALUE  99.
       77  ACM-PAG             PIC  9(04)     VALUE  ZEROS.
       77  ACM-QTD             PIC  9(06)     VALUE  ZEROS.
       77  TOT-QTD             PIC  9(07)     VALUE  ZEROS.
       77  ACM-VAL             PIC  9(11)V99  VALUE  ZEROS.
       77  TOT-VAL             PIC  9(12)V99  VALUE  ZEROS. 

       01  DTA-SYS.                                                     00951088
           03  ANO-SYS         PIC  9(04).                              00952088
           03  MES-SYS         PIC  9(02).                              00953088
           03  DIA-SYS         PIC  9(02).                              00953188
           03  HRA-SYS         PIC  9(02).                              00954088
           03  MIN-SYS         PIC  9(02).                              00955088
           03  FILLER          PIC  X(09).                              00956088
                                                                        00958088
       01  CAB-001.                                                     00960088
           03  FILLER          PIC  X(56)  VALUE                        00970088
               'FUTURE SCHOOL CURSOS DE COMPUTACAO'.                    00980088
           03  HRA-CB1         PIC  9(02).                              00990088
           03  FILLER          PIC  X(01)  VALUE  ':'                   01000088
           03  MIN-CB1         PIC  9(02).                              01001088
           03  FILLER          PIC  X(14)  VALUE  '        PAG.:'.      01002088
           03  PAG-CB1         PIC  Z.ZZ9.                              01003088
                                                                        01004088
       01  CAB-002.                                                     01005088
           03  FILLER          PIC  X(70)  VALUE  'PROGRAMA = PG011SRT'.
           03  DIA-CB1         PIC  9(02).                              01007088
           03  FILLER          PIC  X(01)  VALUE  '/'.                  01008088
           03  MES-CB1         PIC  9(02).                              01009088
           03  FILLER          PIC  X(01)  VALUE  '/'.                  01009188
           03  ANO-CB1         PIC  9(04).                              01009288
                                                                        01009388
       01  CAB-003.                                                     01009488
           03  FILLER          PIC  X(18)  VALUE  SPACES.               01009588
           03  FILLER          PIC  X(62)  VALUE                        01009688
               'RELATORIO ANALITICO DO CADASTRO DE MATERIAIS'.          01009788
                                                                        01009888
       01  CAB-004             PIC  X(80)  VALUE  ALL '='.              01009988
       
       01  CAB-005.
           03  FILLER          PIC  X(47)  VALUE ' CODIGO'.
           03  FILLER          PIC  X(33)  VALUE 'VALOR'.
                                                                        01010088
       01  CAB-006.                                                     01010188
           03  FILLER          PIC  X(12)  VALUE  'MATERIAL'.           01010288
           03  FILLER          PIC  X(20)  VALUE  'NOME DO MATERIAL'.   01010388
           03  FILLER          PIC  X(12)  VALUE  'QUANTIDADE'.
           03  FILLER          PIC  X(12)  VALUE  'UNITARIO'.
           03  FILLER          PIC  X(24)  VALUE  'VALOR TOTAL   CONTA'.
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
           03  FILLER          PIC  X(18)  VALUE  ' *  TOTAL DA CONTA'. 01011588
           03  CTA-TT1         PIC  X(14).                              01011688
           03  QTD-TT1         PIC  ZZZ.ZZ9.                            01011988
           03  FILLER          PIC  X(10)  VALUE  SPACES.               01012088
           03  VAL-TT1         PIC  ZZ.ZZZ.ZZZ.ZZ9,99.
                                                                        01012188
       01  TOT-002.                                                     01012288
           03  FILLER          PIC  X(32)  VALUE  ' ** TOTAL GERAL'.    01012388
           03  QTD-TT2         PIC  Z.ZZZ.ZZ9.                          01012588
           03  FILLER          PIC  X(09)  VALUE  SPACES.               01012688
           03  VAL-TT2         PIC  ZZZ.ZZZ.ZZZ.ZZ9,99.
                                                                        01012788
       PROCEDURE       DIVISION.                                        01013001
                                                                        01020033
       000-INI-PRG.                                                     01030059
           OPEN     INPUT    CADMAT                                     01040086
                    OUTPUT   CADCTA   RELATO.                           01050086
                                                                        01060076
           MOVE     'ABRIR'  TO  OPR-ERR                                01070072
           PERFORM  500-CHK-ERR                                         01080059

           SORT     CADSRT   ON  ASCENDING  KEY  CTA-SRT
                    INPUT        PROCEDURE  IS   100-ENT-SRT
                    OUTPUT       PROCEDURE  IS   200-SAI-SRT 
                                                                        01100059
           CLOSE    CADMAT   CADCTA   RELATO                            01110086
                                                                        01120079
           MOVE     'FECHAR' TO  OPR-ERR                                01130072
           PERFORM  500-CHK-ERR                                         01140059
                                                                        01150059
           MOVE     ACM-LID  TO  MSK-LID                                01160059
           MOVE     ACM-IMP  TO  MSK-IMP                                01170086
           MOVE     ACM-DSP  TO  MSK-DSP                                01180059
                                                                        01190059
           DISPLAY '-------------> PG011SRT <-------------'             01200086
           DISPLAY '|                                    |'             01210059
           DISPLAY '|   PROGRAMA ENCERRADO COM SUCESSO   |'             01220088
           DISPLAY '|                                    |'             01270059
           DISPLAY '--------------------------------------'             01280059
                                                                        01290059
           STOP  RUN.                                                   01300059
                                                                        01310059
       100-ENT-SRT      SECTION.

       110-LER-MAT.                                                     01320086
           PERFORM  120-GRV-SRT  UNTIL  FST-MAT  =  '10'.

       120-GRV-SRT.
           READ     CADALU                                              01330086
                                                                        01340081
           IF  FST-MAT  =   '00'                                        01350086
               MOVE     CTA-MAT  TO  CTA-SRT                            01370086
               MOVE     COD-MAT  TO  MAT-SRT                            01380086
               MOVE     DSC-MAT  TO  DSC-SRT                            01390086
               MOVE     QTD-MAT  TO  QTD-SRT                            01380086
               MOVE     VAL-MAT  TO  VAL-SRT                            01390086

               RELEASE  REG-SRT
           ELSE                                                         01480059
               IF  FST-MAT  NOT  =  '10'                                01490086
                   MOVE     FST-MAT  TO  FST-ERR                        01500086
                   MOVE    'LER'     TO  OPR-ERR                        01510072
                   MOVE    'CADMAT'  TO  ARQ-ERR                        01520086
                   PERFORM  520-MSG-ERR                                 01530059
               END-IF                                                   01540059
           END-IF.                                                      01550059

       200-SAI-SRT     SECTION.

       210-LER-SRT.
           PERFORM  220-IMP-REL  UNTIL  FST-SRT  =  '10'
           PERFORM  210-IMP-TOT

           MOVE     TOT-QTD  TO  QTD-TT2
           MOVE     TOT-VAL  TO  VAL-TT2
           WRITE    REG-REL FROM TOT-002  AFTER  2

           IF  FST-REL  >   '00' 
               PERFORM  510-ERR-IMP THRU 520-MSG-ERR
           END-IF.

       220-IMP-REL
           RETURN   CADSRT  AT  END
                    MOVE   '10'  TO  FST-SRT.

           IF  FST-SRT  =  '00'
               IF  ACM-LIN  >   59
                   PERFORM      300-IMP-CAB
               END-IF

               IF  CTA-SRT  >   CTA-ANT
                   PERFORM      310-IMP-TOT
               END-IF

               MOVE    COD-SRT  TO  MAT-DT1
               MOVE    DSC-SRT  TO  DSC-DT1
               MOVE    QTD-SRT  TO  QTD-DT1
               MOVE    VAL-SRT  TO  VAL-DT1
               COMPUTE TOT-DT1  =   QTD-SRT  *  VAL-SRT
               MOVE    CTA-SRT  TO  CTA-DT1

               WRITE   REG-REL FROM DET-001  AFTER  1

               IF  FST-REL  =  "00"
                   ADD      1        TO  ACM-LIN
                   ADD      QTD-SRT  TO  ACM-QTD
                   COMPUTE  ACM-VAL  =   ACM-VAL  
                                     +  (QTD-SRT  *  VAL-SRT)
               ELSE
                   PERFORM  510-ERR-IMP  THRU    520-MSG-ERR
               END-IF
           END-IF.

       230-FIM-SRT     SECTION.

       300-IMP-CAB.
           MOVE    FUNCTION     CURRENT-DATE  TO  DTA-SYS
           ADD     1        TO  ACM-PAG                  
           MOVE    ACM-PAG  TO  PAG-CB1                  
           MOVE    HRA-SYS  TO  HRA-CB1                  
           MOVE    MIN-SYS  TO  MIN-CB1                  
           MOVE    DIA-SYS  TO  DIA-CB2                  
           MOVE    MES-SYS  TO  MES-CB2                  
           MOVE    ANO-SYS  TO  ANO-CB2                  

           IF  ACM-PAG  =   1
               WRITE   REG-REL FROM CAB-001  AFTER  0
           ELSE
               WRITE   REG-REL FROM CAB-001  AFTER  PAGE
           END-IF

           WRITE   REG-REL FROM CAB-002  AFTER  1
           WRITE   REG-REL FROM CAB-003  AFTER  2                    
           WRITE   REG-REL FROM CAB-004  AFTER  2
           WRITE   REG-REL FROM CAB-005  AFTER  1
           WRITE   REG-REL FROM CAB-006  AFTER  1
           WRITE   REG-REL FROM CAB-004  AFTER  1
           MOVE    SPACES   TO  REG-REL
           WRITE   REG-REL               AFTER  1

           IF  FST-REL  =  '00'
               MOVE     10  TO  ACM-LIN
           ELSE
               PERFORM  510-ERR-IMP  THRU     520-MSG-ERR
           END IF.

       310-IMP-TOT. 
           IF  CTA-ANT  NOT  =   SPACES
               MOVE     SPACES   TO  REG-CTA
               MOVE     CTA-ANT  TO  CTA-TT1  COD-CTA
               MOVE     ACM-QTD  TO  QTD-TT1  QTD-CTA
               MOVE     ACM-VAL  TO  VAL-TT1  VAL-CTA
               WRITE    REG-REL FROM TOT-001  AFTER  2

               IF  FST-REL  =   '00'
                   ADD      1    TO  ACM-LIN
               ELSE
                   PERFORM  510-ERR-IMP THRU  520-MSG-ERR
               END-IF

               WRITE    REG-CTA
               
               IF  FST-CTA  >   '00'
                   MOVE     FST-CTA      TO   FST-ERR
                   MOVE    'GRAVAR'      TO   OPR-ERR
                   MOVE    'CADCONTA'    TO   QRD-ERR
                   PERFORM  520-MSG-ERR
               END-IF
           END-IF

           ADD     ACM-QTD  TO  TOT-QTD
           ADD     ACM-VAL  TO  TOT-VAL
           MOVE    ZEROS    TO  ACM-QTD
           MOVE    ZEROS    TO  ACM-VAL
           MOVE    CTA-SRT  TO  CTA-ANT.
 
       500-CHK-ERR.                                                     02000060
           IF  FST-MAT  >  '00'  OR  FST-CTA  >  '00'
                                 OR  FST-REL  >  '00'
               EVALUATE TRUE                                            02030063
                 WHEN   FST-MAT  >  '00'                                02040086
                        MOVE         FST-MAT   TO  FST-ERR              02050088
                        MOVE        'CADMAT'   TO  ARQ-ERR              02060088
                 WHEN   FST-CTA  >  '00'                                02040086
                        MOVE         FST-CTA   TO  FST-ERR              02050088
                        MOVE        'CADCONTA' TO  ARQ-ERR              02060088
                 WHEN   FST-REL  >  '00'                                02070088
                        MOVE         FST-REL   TO  FST-ERR              02080088
                        MOVE        'RELATO'   TO  ARQ-ERR              02090088
               END-EVALUATE                                             02130060
                                                                        02140060
               PERFORM  520-MSG-ERR                                     02150060
           END-IF.                                                      02160060
                                                                        02170060
       510-ERR-IMP.
           MOVE     FST-REL  TO  FST-ERR
           MOVE    'GRAVAR'  TO  OPR-ERR
           MOVE    'RELATO'  TO  ARQ-ERR.

	 520-MSG-ERR.                                                     02180060
           IF  OPR-ERR  =  'ABRIR'  AND  FST-MAT  =  '10'               02190086
               DISPLAY '-----------> PG011SRT <-----------'             02200086
               DISPLAY '|                                |'             02210060
               DISPLAY '|      ARQUIVO CADMAT VAZIO      |'             02220088
               DISPLAY '|                                |'             02230060
               DISPLAY '|       PROGRAMA ENCERRADO!      |'             02240060
               DISPLAY '|                                |'             02250060
               DISPLAY '----------------------------------'             02260060
           ELSE                                                         02270072
               DISPLAY '-------------> PG011SRT <--------------'        02280086
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
