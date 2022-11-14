       IDENTIFICATION      DIVISION.                                    00010038
       PROGRAM-ID.         ABSPG007.                                    00020072
       AUTHOR.             ADILSON BIZARRIA.                            00030038
       INSTALLATION.       FUTURE SCHOOL.                               00040038
       DATE-WRITTEN.       11/10/2022.                                  00050072
       DATE-COMPILED.                                                   00060025
      *-------------------------------------------------------------*   00070072
      * EXER-07 - GERAR RELACAO DE ALUNOS COM MEDIA SUPERIOR A 6.99 *   00080072
      *-------------------------------------------------------------*   00090072
       ENVIRONMENT         DIVISION.                                    00100001
       CONFIGURATION       SECTION.                                     00110001
       SOURCE-COMPUTER.    IBM-3090.                                    00120029
       OBJECT-COMPUTER.    IBM-3090.                                    00130029
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.                      00140058
       INPUT-OUTPUT        SECTION.                                     00150001
                                                                        00160030
       FILE-CONTROL.                                                    00170001
           SELECT CADALU   ASSIGN  TO  UT-S-CADALU                      00180065
                  FILE     STATUS  IS  FST-ALU.                         00190065
                                                                        00200000
           SELECT RELATO   ASSIGN  TO  UT-S-RELATO                      00210065
                  FILE     STATUS  IS  FST-REL.                         00220065
                                                                        00230031
       DATA                DIVISION.                                    00240001
       FILE                SECTION.                                     00250008
                                                                        00260031
       FD  CADALU          RECORD      CONTAINS 60 CHARACTERS           00270058
                           RECORDING   MODE     IS F                    00280001
                           LABEL       RECORD   IS STANDARD             00290001
                           DATA        RECORD   IS REG-ALU.             00300058
                                                                        00310031
       01  REG-ALU.                                                     00320058
           03  COD-ALU     PIC  9(04).                                  00330058
           03  NOM-ALU     PIC  X(30).                                  00340058
           03  TUR-ALU     PIC  9(03).                                  00350058
           03  NT1-ALU     PIC  9(02)V99.                               00360058
           03  NT2-ALU     PIC  9(02)V99.                               00370058
           03  NT3-ALU     PIC  9(02)V99.                               00380058
           03  NT4-ALU     PIC  9(02)V99.                               00390058
           03  FILLER      PIC  X(07).                                  00400058
                                                                        00410032
       FD  RELATO          RECORD      CONTAINS 60 CHARACTERS           00420072
                           RECORDING   MODE     IS F                    00430001
                           LABEL       RECORD   IS OMITTED              00440058
                           DATA        RECORD   IS REG-REL.             00450058
                                                                        00460032
       01  REG-REL         PIC  X(60).                                  00470072
                                                                        00480032
       WORKING-STORAGE SECTION.                                         00490001
                                                                        00500032
       77  FST-ALU         PIC  X(02).                                  00510060
       77  FST-REL         PIC  X(02).                                  00520060
                                                                        00530034
       77  FST-ERR         PIC  X(02).                                  00540034
       77  COD-ERR         PIC  9(01)  VALUE ZEROS.                     00550066
       77  OPR-ERR         PIC  X(13).                                  00560005
       77  ARQ-ERR         PIC  X(08).                                  00570046
                                                                        00580033
       77  ACM-LIN         PIC  9(02)  VALUE  99.                       00590066
       77  ACM-PAG         PIC  9(04)  VALUE  ZEROS.                    00600066
       77  ACM-LID         PIC  9(06)  VALUE  ZEROS.                    00610072
       77  ACM-IMP         PIC  9(06)  VALUE  ZEROS.                    00620072
       77  MED-ALU         PIC  9(02)V99.                               00630072
                                                                        00640033
       01  DTA-SYS.                                                     00650059
           03  ANO-SYS     PIC  9(04).                                  00660059
           03  MES-SYS     PIC  9(02).                                  00670059
           03  DIA-SYS     PIC  9(02).                                  00680059
           03  FILLER      PIC  X(13).                                  00690072
                                                                        00700059
       01  CAB-001.                                                     00710058
           03  FILLER      PIC  X(49)  VALUE                            00720072
               'FUTURE SCHOOL CURSOS DE COMPUTACAO'.                    00730058
           03  FILLER      PIC  X(06)  VALUE  'PAG.:'.                  00740058
           03  PAG-CB1     PIC  Z.ZZ9.                                  00750058
                                                                        00760058
       01  CAB-002.                                                     00770058
           03  FILLER      PIC  X(50)  VALUE  'PROGRAMA = ABSPG007'.    00780072
           03  DIA-CB2     PIC  9(02).                                  00790058
           03  FILLER      PIC  X(01)  VALUE  "/".                      00800062
           03  MES-CB2     PIC  9(02).                                  00810058
           03  FILLER      PIC  X(01)  VALUE  "/".                      00820062
           03  ANO-CB2     PIC  9(04).                                  00830058
                                                                        00840058
       01  CAB-003.                                                     00850058
           03  FILLER      PIC  X(15)  VALUE  SPACES.                   00860072
           03  FILLER      PIC  X(45)  VALUE                            00870072
               'LISTAGEM DOS ALUNOS APROVADOS'.                         00880072
                                                                        00890058
       01  CAB-004.                                                     00900058
           03  FILLER      PIC  X(14)  VALUE  'CODIGO'.                 00910058
           03  FILLER      PIC  X(31)  VALUE  'NOME DO ALUNO'.          00920072
           03  FILLER      PIC  X(15)  VALUE  'MEDIA     TURMA'.        00930072
                                                                        00940058
       01  DET-001.                                                     00950058
           03  FILLER      PIC  X(01)  VALUE  SPACES.                   00960058
           03  COD-DT1     PIC  9(04).                                  00970058
           03  FILLER      PIC  X(05)  VALUE  SPACES.                   00980077
           03  NOM-DT1     PIC  X(35).                                  00990072
           03  MED-DT1     PIC  Z9,99.                                  01000072
           03  FILLER      PIC  X(06)  VALUE  SPACES.                   01010077
           03  TUR-DT1     PIC  9(03).                                  01020072
                                                                        01030058
       01  TOT-001.                                                     01040072
           03  FILLER      PIC  X(28)  VALUE                            01050072
               'TOTAL DE ALUNOS LIDOS     :'.                           01060072
           03  LID-TT1     PIC  999.999.                                01070072
                                                                        01080072
       01  TOT-002.                                                     01090072
           03  FILLER      PIC  X(28)  VALUE                            01100072
               'TOTAL DE ALUNOS APROVADOS :'.                           01110072
           03  IMP-TT2     PIC  999.999.                                01120074
                                                                        01130072
       PROCEDURE        DIVISION.                                       01140064
                                                                        01150033
       000-INICIO.                                                      01160001
           OPEN    INPUT      CADALU                                    01170064
                   OUTPUT     RELATO.                                   01180064
                                                                        01190068
           IF  FST-ALU  =  '00' AND  FST-REL  =  '00'                   01200071
               PERFORM  100-IMP-REL  UNTIL  FST-ALU = '10'              01210071
               PERFORM  120-IMP-TOT                                     01220072
           ELSE                                                         01230003
               MOVE  1  TO  COD-ERR                                     01240071
               PERFORM  200-ERR-ARQ                                     01250071
           END-IF                                                       01260003
                                                                        01270035
           CLOSE    CADALU   RELATO                                     01280071
                                                                        01290006
           IF  FST-ALU  =   '00'  AND  FST-REL  =  '00'                 01300072
               STOP  RUN                                                01310042
           ELSE                                                         01320042
               MOVE  4  TO  COD-ERR                                     01330063
               PERFORM  200-ERR-ARQ                                     01340063
           END-IF.                                                      01350042
                                                                        01360035
       100-IMP-REL.                                                     01370060
           READ      CADALU                                             01380060
           IF  FST-ALU  =   '00'                                        01390060
               ADD   1  TO  ACM-LID                                     01400054
                                                                        01410054
               COMPUTE  MED-ALU  = (NT1-ALU  +  NT2-ALU   +             01420072
                                    NT3-ALU  +  NT4-ALU)  /  4          01430072
               IF  MED-ALU   >  6,99                                    01440072
                   IF  ACM-LIN  >   59                                  01450072
                       PERFORM  110-IMP-CAB                             01460072
                   END-IF                                               01470072
                                                                        01480063
                   MOVE     COD-ALU  TO  COD-DT1                        01490072
                   MOVE     NOM-ALU  TO  NOM-DT1                        01500072
                   MOVE     MED-ALU  TO  MED-DT1                        01510072
                   MOVE     TUR-ALU  TO  TUR-DT1                        01520072
                   WRITE    REG-REL FROM DET-001  AFTER  1              01530072
                                                                        01540060
                   IF  FST-REL  =  '00'                                 01550072
                       ADD   1  TO  ACM-IMP                             01560072
                       ADD   1  TO  ACM-LIN                             01570072
                   ELSE                                                 01580072
                       MOVE  3  TO  COD-ERR                             01590072
                       PERFORM  200-ERR-ARQ                             01600072
                   END-IF                                               01610072
               END-IF                                                   01620072
           ELSE                                                         01630072
               IF  FST-ALU  NOT  =   '10'                               01640072
                   MOVE  2   TO  COD-ERR                                01650072
                   PERFORM   200-ERR-ARQ                                01660072
               END-IF                                                   01670072
           END-IF.                                                      01680005
                                                                        01690035
       110-IMP-CAB.                                                     01700060
           MOVE    FUNCTION     CURRENT-DATE  TO  DTA-SYS               01710060
                                                                        01720064
           ADD     1        TO  ACM-PAG                                 01730064
           MOVE    ACM-PAG  TO  PAG-CB1                                 01740064
           MOVE    DIA-SYS  TO  DIA-CB2                                 01750060
           MOVE    MES-SYS  TO  MES-CB2                                 01760060
           MOVE    ANO-SYS  TO  ANO-CB2                                 01770060
                                                                        01780060
           IF  ACM-PAG  =   1                                           01790064
               WRITE   REG-REL FROM CAB-001  AFTER  0                   01800071
           ELSE                                                         01810064
               WRITE   REG-REL FROM CAB-001  AFTER  PAGE                01820071
           END-IF                                                       01830064
                                                                        01840064
           WRITE   REG-REL FROM CAB-002  AFTER  1                       01850071
           WRITE   REG-REL FROM CAB-003  AFTER  2                       01860071
           WRITE   REG-REL FROM CAB-004  AFTER  2                       01870071
           MOVE    SPACES   TO  REG-REL                                 01880060
           WRITE   REG-REL               AFTER  1                       01890071
           MOVE    7        TO  ACM-LIN.                                01900072
                                                                        01910072
       120-IMP-TOT.                                                     01920072
           MOVE    ACM-LID  TO  LID-TT1                                 01930072
           MOVE    ACM-IMP  TO  IMP-TT2                                 01940072
           WRITE   REG-REL FROM TOT-001  AFTER  2                       01950072
           WRITE   REG-REL FROM TOT-002  AFTER  1.                      01960072
                                                                        01970060
       200-ERR-ARQ.                                                     01980005
           IF  COD-ERR  =  1  AND  FST-ALU = '10'                       01990060
               DISPLAY '-----------> ABSPG007 <-----------'             02000072
               DISPLAY '|                                |'             02010005
               DISPLAY '|     ARQUIVO CADALUNO VAZIO     |'             02020060
               DISPLAY '|                                |'             02030005
               DISPLAY '|       PROGRAMA ENCERRADO!      |'             02040005
               DISPLAY '|                                |'             02050005
               DISPLAY '----------------------------------'             02060005
           ELSE                                                         02070005
               EVALUATE COD-ERR                                         02080005
                   WHEN 1                                               02090005
                        MOVE  'NA ABERTURA'    TO  OPR-ERR              02100005
                   WHEN 2                                               02110005
                        MOVE  'NA LEITURA'     TO  OPR-ERR              02120005
                   WHEN 3                                               02130005
                        MOVE  'NA IMPRESSAO'   TO  OPR-ERR              02140060
                   WHEN 4                                               02150005
                        MOVE  'NO FECHAMENTO'  TO  OPR-ERR              02160005
               END-EVALUATE                                             02170020
                                                                        02180035
               IF  FST-ALU  >  '00' AND (COD-ERR =  1 OR 2 OR 4)        02190060
                   MOVE     FST-ALU  TO  FST-ERR                        02200060
                   MOVE     'CADALUNO'   TO  ARQ-ERR                    02210060
               ELSE                                                     02220005
                   MOVE     FST-REL  TO  FST-ERR                        02230060
                   MOVE     'RELATO' TO  ARQ-ERR                        02240060
               END-IF                                                   02250020
           END-IF                                                       02260072
                                                                        02270072
           STOP    RUN.                                                 02280006
