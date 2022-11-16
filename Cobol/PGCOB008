       IDENTIFICATION      DIVISION.                                    00010038
       PROGRAM-ID.         ABSPG008.                                    00020070
       AUTHOR.             ADILSON BIZARRIA.                            00030038
       INSTALLATION.       FUTURE SCHOOL.                               00040038
       DATE-WRITTEN.       12/10/2022.                                  00050070
       DATE-COMPILED.                                                   00060025
      *-------------------------------------------------------------*   00070070
      * EXER-08 - GERAR LISTA DE MATERIAS E CALCULAR PRECO DE VENDA *   00080070
      *-------------------------------------------------------------*   00090070
       ENVIRONMENT         DIVISION.                                    00100001
       CONFIGURATION       SECTION.                                     00110001
       SOURCE-COMPUTER.    IBM-3090.                                    00120029
       OBJECT-COMPUTER.    IBM-3090.                                    00130029
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.                      00140058
       INPUT-OUTPUT        SECTION.                                     00150001
                                                                        00160030
       FILE-CONTROL.                                                    00170001
           SELECT CADMAT   ASSIGN TO UT-S-CADMAT                        00180070
                  FILE     STATUS IS FST-MAT.                           00190070
                                                                        00200000
           SELECT RELATO   ASSIGN TO UT-S-RELATO                        00210061
                  FILE     STATUS IS FST-REL.                           00220058
                                                                        00230031
       DATA                DIVISION.                                    00240001
       FILE                SECTION.                                     00250008
                                                                        00260031
       FD  CADMAT          RECORD      CONTAINS 40 CHARACTERS           00270071
                           RECORDING   MODE     IS F                    00280001
                           LABEL       RECORD   IS STANDARD             00290001
                           DATA        RECORD   IS REG-MAT.             00300070
                                                                        00310031
       01  REG-MAT.                                                     00320070
           03  COD-MAT     PIC  9(05).                                  00330070
           03  NOM-MAT     PIC  X(20).                                  00340071
           03  QTD-MAT     PIC  9(05).                                  00350070
           03  PRC-MAT     PIC  9(05)V99.                               00360071
           03  FILLER      PIC  X(03).                                  00370071
                                                                        00380032
       FD  RELATO          RECORD      CONTAINS 82  CHARACTERS          00390071
                           RECORDING   MODE     IS  F                   00400064
                           LABEL       RECORD   IS  OMITTED             00410064
                           DATA        RECORD   IS  REG-REL.            00420064
                                                                        00430032
       01  REG-REL         PIC  X(82).                                  00440071
                                                                        00450032
       WORKING-STORAGE SECTION.                                         00460001
                                                                        00470032
       77  FST-MAT         PIC  X(02).                                  00480070
       77  FST-REL         PIC  X(02).                                  00490060
                                                                        00500034
       77  FST-ERR         PIC  X(02).                                  00510034
       77  COD-ERR         PIC  9(01) VALUE ZEROS.                      00520063
       77  OPR-ERR         PIC  X(13).                                  00530005
       77  ARQ-ERR         PIC  X(08).                                  00540046
                                                                        00550033
       77  ACM-VAL         PIC  9(06)V99.                               00560071
       77  ACM-LIN         PIC  9(02) VALUE  99.                        00570059
       77  ACM-PAG         PIC  9(04) VALUE  ZEROS.                     00580059
       77  ACM-LID         PIC  9(12) COMP-3 VALUE ZEROS.               00590033
       77  ACM-IMP         PIC  9(12) COMP-3 VALUE ZEROS.               00600059
                                                                        00610033
       77  MSK-LID         PIC  ZZZ.ZZZ.ZZZ.ZZ9.                        00620034
       77  MSK-IMP         PIC  ZZZ.ZZZ.ZZZ.ZZ9.                        00630059
                                                                        00640033
       01  DTA-SYS.                                                     00650059
           03  ANO-SYS     PIC  9(04).                                  00660059
           03  MES-SYS     PIC  9(02).                                  00670059
           03  DIA-SYS     PIC  9(02).                                  00680059
           03  HRA-SYS     PIC  9(02).                                  00690059
           03  MIN-SYS     PIC  9(02).                                  00700059
           03  FILLER      PIC  X(09).                                  00710059
                                                                        00720059
       01  CAB-001.                                                     00730058
           03  FILLER      PIC  X(53)  VALUE                            00740071
               'FUTURE SCHOOL CURSOS DE COMPUTACAO'.                    00750058
           03  HRA-CB1     PIC  9(02).                                  00760071
           03  FILLER      PIC  X(01)  VALUE  ":".                      00770071
           03  MIN-CB1     PIC  9(02).                                  00780071
           03  FILLER      PIC  X(12)  VALUE  SPACES.                   00790071
           03  FILLER      PIC  X(06)  VALUE  'PAG.:'.                  00800058
           03  PAG-CB1     PIC  Z.ZZ9.                                  00810058
                                                                        00820058
       01  CAB-002.                                                     00830058
           03  FILLER      PIC  X(82)  VALUE  'PROGRAMA = ABSPG008'.    00840070
                                                                        00850058
       01  CAB-003.                                                     00860058
           03  FILLER      PIC  X(19)  VALUE  SPACES.                   00870071
           03  FILLER      PIC  X(35)  VALUE                            00880075
               'LISTAGEM ANALITICA DE MATERIAIS EM'.                    00890075
           03  DIA-CB3     PIC  9(02).                                  00900072
           03  FILLER      PIC  X(01)  VALUE  "/".                      00910071
           03  MES-CB3     PIC  9(02).                                  00920072
           03  FILLER      PIC  X(01)  VALUE  "/".                      00930071
           03  ANO-CB3     PIC  9(04).                                  00940072
           03  FILLER      PIC  X(18)  VALUE  SPACES.                   00950075
                                                                        00960064
       01  CAB-004.                                                     00970064
           03  FILLER      PIC  X(35)  VALUE  ' CODIGO'.                00980071
           03  FILLER      PIC  X(47)  VALUE  'QUANTIDADE     PRECO'.   00990071
                                                                        01000058
       01  CAB-005.                                                     01010064
           03  FILLER      PIC  X(14)  VALUE  'MATERIAL'.               01020071
           03  FILLER      PIC  X(21)  VALUE  'NOME DO MATERIAL'.       01030075
           03  FILLER      PIC  X(23)  VALUE  'EM ESTOQUE   UNITARIO'.  01040075
           03  FILLER      PIC  X(13)  VALUE  'VALOR TOTAL'.            01050071
           03  FILLER      PIC  X(11)  VALUE  'VALOR VENDA'.            01060071
                                                                        01070058
       01  DET-001.                                                     01080058
           03  FILLER      PIC  X(01)  VALUE  SPACES.                   01090071
           03  COD-DT1     PIC  99.999.                                 01100064
           03  FILLER      PIC  X(05)  VALUE  SPACES.                   01110074
           03  NOM-DT1     PIC  X(25).                                  01120074
           03  QTD-DT1     PIC  ZZ.ZZ9.                                 01130064
           03  FILLER      PIC  X(05)  VALUE  SPACES.                   01140074
           03  PRC-DT1     PIC  Z.ZZ9,99.                               01150071
           03  FILLER      PIC  X(03)  VALUE  SPACES.                   01160074
           03  TOT-DT1     PIC  ZZZ.ZZ9,99.                             01170071
           03  FILLER      PIC  X(03)  VALUE  SPACES.                   01180074
           03  VDA-DT1     PIC  ZZZ.ZZ9,99.                             01190071
                                                                        01200058
       PROCEDURE       DIVISION.                                        01210001
                                                                        01220033
       000-INICIO.                                                      01230001
           OPEN    INPUT   CADMAT                                       01240070
                   OUTPUT  RELATO.                                      01250058
                                                                        01260035
           IF  FST-MAT  =  '00' AND  FST-REL  =  '00'                   01270070
               PERFORM  100-IMP-REL  UNTIL  FST-MAT = '10'              01280070
           ELSE                                                         01290003
               MOVE  1  TO  COD-ERR                                     01300064
               PERFORM  200-ERR-ARQ                                     01310064
           END-IF                                                       01320003
                                                                        01330035
           CLOSE     CADMAT   RELATO                                    01340070
                                                                        01350006
           IF  FST-MAT = '00' AND FST-REL  =  '00'                      01360070
               MOVE  ACM-LID  TO  MSK-LID                               01370006
               MOVE  ACM-IMP  TO  MSK-IMP                               01380059
                                                                        01390035
               DISPLAY '-> ABSPG008 <-> ESTATISTICA DE PROCESSAMENTO <-'01400070
               DISPLAY '|                                             |'01410006
               DISPLAY '| REGISTROS LIDOS (CADMAT)    ' MSK-LID ' |'    01420071
               DISPLAY '|                                             |'01430006
               DISPLAY '| REGISTROS IMPRESSOS (RELATO)' MSK-IMP ' |'    01440060
               DISPLAY '|                                             |'01450006
               DISPLAY '-----------------------------------------------'01460006
                                                                        01470035
               STOP  RUN                                                01480042
           ELSE                                                         01490042
               MOVE  4        TO  COD-ERR                               01500042
               PERFORM        200-ERR-ARQ                               01510042
           END-IF.                                                      01520042
                                                                        01530035
       100-IMP-REL.                                                     01540060
           READ      CADMAT                                             01550070
                                                                        01560005
           IF  FST-MAT  =   '00'                                        01570070
               ADD   1  TO  ACM-LID                                     01580054
                                                                        01590054
               IF  ACM-LIN  >   59                                      01600068
                   PERFORM  110-IMP-CAB                                 01610068
               END-IF                                                   01620068
                                                                        01630060
               MOVE     COD-MAT  TO  COD-DT1                            01640073
               MOVE     NOM-MAT  TO  NOM-DT1                            01650073
               MOVE     QTD-MAT  TO  QTD-DT1                            01660073
               MOVE     PRC-MAT  TO  PRC-DT1                            01670073
               COMPUTE  ACM-VAL  =   QTD-MAT  *   PRC-MAT               01680073
               MOVE     ACM-VAL  TO  TOT-DT1                            01690073
                                                                        01700071
               EVALUATE COD-MAT                                         01710071
                  WHEN   00000  THRU 09999                              01720076
                        COMPUTE      VDA-DT1  =  ACM-VAL  *  1,10       01730076
                  WHEN   10000  THRU 19999                              01740076
                        COMPUTE      VDA-DT1  =  ACM-VAL  *  1,15       01750076
                  WHEN   20000  THRU 29999                              01760076
                        COMPUTE      VDA-DT1  =  ACM-VAL  *  1,22       01770076
                  WHEN   OTHER                                          01780076
                        COMPUTE      VDA-DT1  =  ACM-VAL  *  1,30       01790076
               END-EVALUATE                                             01800071
                                                                        01810071
               WRITE    REG-REL FROM DET-001  AFTER  1                  01820076
                                                                        01830060
               IF  FST-REL   =  '00'                                    01840076
                   ADD       1   TO  ACM-IMP                            01850076
                   ADD       1   TO  ACM-LIN                            01860076
               ELSE                                                     01870060
                   MOVE      3   TO  COD-ERR                            01880076
                   PERFORM       200-ERR-ARQ                            01890076
               END-IF                                                   01900060
           ELSE                                                         01910005
               IF  FST-MAT  NOT  =   '10'                               01920076
                   MOVE      2   TO  COD-ERR                            01930076
                   PERFORM       200-ERR-ARQ                            01940076
               END-IF                                                   01950043
           END-IF.                                                      01960005
                                                                        01970035
       110-IMP-CAB.                                                     01980060
           MOVE    FUNCTION     CURRENT-DATE  TO  DTA-SYS               01990060
           ADD     1        TO  ACM-PAG                                 02000067
           MOVE    ACM-PAG  TO  PAG-CB1                                 02010067
           MOVE    HRA-SYS  TO  HRA-CB1                                 02020071
           MOVE    MIN-SYS  TO  MIN-CB1                                 02030071
           MOVE    DIA-SYS  TO  DIA-CB3                                 02040071
           MOVE    MES-SYS  TO  MES-CB3                                 02050071
           MOVE    ANO-SYS  TO  ANO-CB3                                 02060071
                                                                        02070060
           IF  ACM-PAG = 1                                              02080071
               WRITE   REG-REL FROM CAB-001  AFTER  0                   02090071
           ELSE                                                         02100071
               WRITE   REG-REL FROM CAB-001  AFTER  PAGE                02110071
           END-IF                                                       02120071
                                                                        02130071
           WRITE   REG-REL FROM CAB-002  AFTER  1                       02140064
           WRITE   REG-REL FROM CAB-003  AFTER  2                       02150064
           WRITE   REG-REL FROM CAB-004  AFTER  2                       02160064
           WRITE   REG-REL FROM CAB-005  AFTER  1                       02170064
           MOVE    SPACES   TO  REG-REL                                 02180060
           WRITE   REG-REL               AFTER  1                       02190064
           MOVE    8        TO  ACM-LIN.                                02200060
                                                                        02210060
       200-ERR-ARQ.                                                     02220005
           IF  COD-ERR  =  1  AND  FST-MAT = '10'                       02230070
               DISPLAY '-----------> ABSPG008 <-----------'             02240070
               DISPLAY '|                                |'             02250005
               DISPLAY '|     ARQUIVO CADMAT VAZIO       |'             02260071
               DISPLAY '|                                |'             02270005
               DISPLAY '|      PROGRAMA ENCERRADO!       |'             02280066
               DISPLAY '|                                |'             02290005
               DISPLAY '----------------------------------'             02300005
           ELSE                                                         02310005
               EVALUATE COD-ERR                                         02320005
                   WHEN 1                                               02330005
                        MOVE  'NA ABERTURA'    TO  OPR-ERR              02340005
                   WHEN 2                                               02350005
                        MOVE  'NA LEITURA'     TO  OPR-ERR              02360005
                   WHEN 3                                               02370005
                        MOVE  'NA IMPRESSAO'   TO  OPR-ERR              02380060
                   WHEN 4                                               02390005
                        MOVE  'NO FECHAMENTO'  TO  OPR-ERR              02400005
               END-EVALUATE                                             02410020
                                                                        02420035
               IF  FST-MAT  >  '00' AND (COD-ERR =  1 OR 2 OR 4)        02430070
                   MOVE     FST-MAT  TO  FST-ERR                        02440070
                   MOVE     'CADMAT' TO  ARQ-ERR                        02450071
               ELSE                                                     02460005
                   MOVE     FST-REL  TO  FST-ERR                        02470060
                   MOVE     'RELATO' TO  ARQ-ERR                        02480060
               END-IF                                                   02490020
                                                                        02500035
               DISPLAY '--------------> ABSPG008 <----------------'     02510070
               DISPLAY '|                                        |'     02520046
               DISPLAY '| ERRO ' OPR-ERR ' DO ARQUIVO ' ARQ-ERR ' |'    02530038
               DISPLAY '|                                        |'     02540046
               DISPLAY '|            FILE-STATUS: ' FST-ERR '           02550021
      -                '  |'                                            02560046
               DISPLAY '|                                        |'     02570046
               DISPLAY '|           PROGRAMA ENCERRADO!          |'     02580046
               DISPLAY '|                                        |'     02590046
               DISPLAY '------------------------------------------'     02600046
           END-IF.                                                      02610005
                                                                        02620035
           STOP    RUN.                                                 02630006
