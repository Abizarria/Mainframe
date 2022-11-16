       IDENTIFICATION        DIVISION.                                  00010000
       PROGRAM-ID.           PGCICS05.                                  00020000
       AUTHOR.               ADILSON BIZARRIA                           00030000
       INSTALLATION.         FUTURE SCHOOL.                             00040000
       DATE-WRITTEN.         11/07/2022.                                00050000
       DATE-COMPILED.                                                   00060000
      *---------------------------------------*                         00070000
      * CICS - CADASTRO DE ALUNOS (RELATORIO) *                         00080000
      *---------------------------------------*                         00090000
       ENVIRONMENT           DIVISION.                                  00100000
       CONFIGURATION         SECTION.                                   00110000
                                                                        00120000
       SOURCE-COMPUTER.      IBM-3090.                                  00130000
       OBJECT-COMPUTER.      IBM-3090.                                  00140000
                                                                        00150000
       SPECIAL-NAMES.                                                   00160000
                             DECIMAL-POINT IS COMMA.                    00170000
                                                                        00180000
       INPUT-OUTPUT          SECTION.                                   00190000
                                                                        00200000
       FILE-CONTROL.                                                    00210000
                                                                        00220000
       DATA                  DIVISION.                                  00230000
       FILE                  SECTION.                                   00240000
                                                                        00250000
       WORKING-STORAGE       SECTION.                                   00260000
                                                                        00270000
       77  WS-TIME           PIC S9(015)  COMP-3.                       00280019
       77  WS-DATA           PIC  X(010).                               00290019
       77  WS-HORA           PIC  X(008).                               00300019
       77  WS-MSGM           PIC  X(030).                               00310019
       77  WS-LADO           PIC  9(001)  VALUE  0.                     00320099
       77  WS-IND1           PIC  9(002).                               00330019
       77  WS-IND2           PIC  9(002).                               00340019
                                                                        00350000
       01  REG-ALU.                                                     00360000
           03  MTR-ALU       PIC  9(005)  VALUE  ZEROS.                 00370099
           03  NOM-ALU       PIC  X(030).                               00380019
           03  CUR-ALU       PIC  X(020).                               00390019
           03  TEL-ALU       PIC  X(015).                               00400019
           03  END-ALU       PIC  X(035).                               00410019
           03  EML-ALU       PIC  X(030).                               00420019
                                                                        00430000
       01  TAB-REG           PIC X(1890).                               00440019
       01  RED-REG           REDEFINES    TAB-REG.                      00450019
           03  REG-TAB       PIC  X(135)  OCCURS  14.                   00460019
                                                                        00470019
       01  CAB-001.                                                     00480010
           03  FILLER        PIC  X(008)  VALUE  'MATRI'.               00490019
           03  FILLER        PIC  X(032)  VALUE  'NOME DO ALUNO'.       00500019
           03  FILLER        PIC  X(022)  VALUE  'CURSO'.               00510019
           03  FILLER        PIC  X(015)  VALUE  'TELEFONE'.            00520019
                                                                        00530010
       01  CAB-002.                                                     00540010
           03  FILLER        PIC  X(047)  VALUE  'MATRI   ENDERECO'.    00550019
           03  FILLER        PIC  X(030)  VALUE  'E-MAIL'.              00560019
                                                                        00570010
       01  DET-001.                                                     00580010
           03  MTR-DT1       PIC  9(005).                               00590019
           03  FILLER        PIC  X(003)  VALUE  SPACES.                00600019
           03  NOM-DT1       PIC  X(030).                               00610019
           03  FILLER        PIC  X(002)  VALUE  SPACES.                00620019
           03  CUR-DT1       PIC  X(020).                               00630019
           03  FILLER        PIC  X(002)  VALUE  SPACES.                00640019
           03  TEL-DT1       PIC  X(015).                               00650019
                                                                        00660010
       01  DET-002.                                                     00670010
           03  MTR-DT2       PIC  9(005).                               00680019
           03  FILLER        PIC  X(003)  VALUE  SPACES.                00690019
           03  END-DT2       PIC  X(035).                               00700019
           03  FILLER        PIC  X(004)  VALUE  SPACES.                00710019
           03  EML-DT2       PIC  X(030).                               00720019
                                                                        00730010
       COPY    DFHAID.                                                  00740000
       COPY    DFHBMSCA.                                                00750000
       COPY    MAPLIST.                                                 00760000
                                                                        00770000
       LINKAGE               SECTION.                                   00780000
                                                                        00790000
       01  DFHCOMMAREA.                                                 00800019
           03  LK-COMM       PIC  9(005).                               00810019
                                                                        00820000
       PROCEDURE             DIVISION.                                  00830000
                                                                        00840000
       000-INI-PROG.                                                    00850000
           IF  EIBCALEN   =   0                                         00860000
               PERFORM        100-EXB-TELA                              00870099
           ELSE                                                         00880000
               PERFORM        300-REC-TELA                              00890000
           END-IF.                                                      00900000
                                                                        00910000
       100-EXB-TELA.                                                    00920019
           PERFORM  110-ATU-DATA                                        00930000
                                                                        00940000
           MOVE    'RELATORIO'     TO        TOPERO                     00950061
           MOVE     EIBTRNID  TO   TTRNSO                               00960000
           MOVE     EIBTRMID  TO   TTERMO                               00970000
                                                                        00980019
           IF  WS-LADO   =    0                                         00990019
               MOVE      CAB-001   TO        THEADO                     01000019
           ELSE                                                         01010019
               MOVE      CAB-002   TO        THEADO                     01020019
           END-IF                                                       01030019
                                                                        01040019
           MOVE     DFHBMPRF  TO   TOPERA    TTRNSA                     01050019
                                   TTERMA    THEADA                     01060019
                                                                        01070000
           EXEC     CICS  SEND     MAP      ('CABLIST')                 01080019
                                   MAPSET   ('MAPLIST')                 01090019
                          FROM              (CABLISTO)                  01100019
                          ERASE    ACCUM     CURSOR                     01110019
           END-EXEC                                                     01120000
                                                                        01130000
           PERFORM  200-LIST-000                                        01140019
           PERFORM  210-EXB-MSGM                                        01150099
                                                                        01160099
           EXEC     CICS  SEND     PAGE      END-EXEC                   01170001
                                                                        01180000
           EXEC     CICS  RETURN   COMMAREA (MTR-ALU)                   01190099
                                   TRANSID  ('ABS5')                    01200019
           END-EXEC.                                                    01210000
                                                                        01220000
       110-ATU-DATA.                                                    01230000
           EXEC     CICS  ASKTIME                                       01240000
                          ABSTIME     (WS-TIME)                         01250000
           END-EXEC                                                     01260000
                                                                        01270000
           EXEC     CICS  FORMATTIME                                    01280000
                          ABSTIME     (WS-TIME)                         01290000
                          DATESEP     ('/')                             01300000
                          DDMMYYYY    (WS-DATA)                         01310000
                          TIMESEP     (':')                             01320000
                          TIME        (WS-HORA)                         01330000
           END-EXEC                                                     01340000
                                                                        01350000
           MOVE     WS-DATA   TO   TDATAO                               01360000
           MOVE     WS-HORA   TO   THORAO                               01370000
           MOVE     DFHBMPRF  TO   TDATAA    THORAA.                    01380019
                                                                        01390000
       200-LIST-000.                                                    01400019
           MOVE     SPACES    TO   TAB-REG                              01410099
           MOVE     ZEROS     TO   WS-IND1   WS-IND2                    01420048
                                                                        01430019
           EXEC     CICS  STARTBR  FILE     ('CADALU')                  01440019
                                   RIDFLD   (MTR-ALU)                   01450019
                          GTEQ     NOHANDLE                             01460099
           END-EXEC                                                     01470019
                                                                        01480000
           PERFORM  200-LIST-010   UNTIL     EIBRESP  >  0              01490099
                                   OR        WS-IND1  >  13             01500099
           PERFORM  200-LIST-020   UNTIL     WS-IND2  =  WS-IND1.       01510099
                                                                        01520019
       200-LIST-010.                                                    01530019
           EXEC     CICS  READNEXT FILE     ('CADALU')                  01540092
                                   RIDFLD   (MTR-ALU)                   01550019
                                   INTO     (REG-ALU)                   01560019
                          NOHANDLE                                      01570091
           END-EXEC                                                     01580000
                                                                        01590010
           IF  EIBRESP  =     0                                         01600099
               ADD      1          TO  WS-IND1                          01610019
               MOVE     REG-ALU    TO  REG-TAB (WS-IND1).               01620019
                                                                        01630019
       200-LIST-020.                                                    01640019
           ADD      1         TO   WS-IND2                              01650019
           MOVE     REG-TAB (WS-IND2)  TO   REG-ALU                     01660020
                                                                        01670019
           EVALUATE WS-LADO                                             01680019
              WHEN  0                                                   01690019
                    MOVE     MTR-ALU   TO   MTR-DT1                     01700020
                    MOVE     NOM-ALU   TO   NOM-DT1                     01710020
                    MOVE     CUR-ALU   TO   CUR-DT1                     01720020
                    MOVE     TEL-ALU   TO   TEL-DT1                     01730020
                                                                        01740019
                    MOVE     DET-001   TO   TINFOO                      01750020
              WHEN  1                                                   01760019
                    MOVE     MTR-ALU   TO   MTR-DT2                     01770020
                    MOVE     END-ALU   TO   END-DT2                     01780020
                    MOVE     EML-ALU   TO   EML-DT2                     01790020
                                                                        01800019
                    MOVE     DET-002   TO   TINFOO                      01810020
           END-EVALUATE                                                 01820019
                                                                        01830099
           MOVE     DFHBMPRF  TO   TINFOA                               01840019
                                                                        01850019
           EXEC     CICS  SEND     MAP     ('DETLIST')                  01860019
                                   MAPSET  ('MAPLIST')                  01870019
                          FROM             (DETLISTO)                   01880019
                          ACCUM    CURSOR                               01890019
           END-EXEC.                                                    01900019
                                                                        01910000
       210-EXB-MSGM.                                                    01920099
           IF  WS-IND2  >   0                                           01930099
               MOVE  'PF3-MENU, PF7-ACIMA, PF8-ABAIXO, PF10-ESQUERDA, PF01940099
      -              '11-DIREITA'      TO    TMSGMO                     01950099
           ELSE                                                         01960099
               MOVE  'CADASTRO VAZIO'  TO    TMSGMO                     01970099
               MOVE   ZEROS            TO    MTR-ALU                    01980099
           END-IF                                                       01990099
                                                                        02000099
           MOVE     SPACES    TO   TPCURO                               02010099
           MOVE     DFHBMPRF  TO   TMSGMA    TPCURA                     02020099
           MOVE     -1        TO   TPCURL                               02030099
                                                                        02040099
           EXEC     CICS  SEND     MAP      ('RODLIST')                 02050099
                                   MAPSET   ('MAPLIST')                 02060099
                          FROM              (RODLISTO)                  02070099
                          ACCUM    CURSOR                               02080099
           END-EXEC.                                                    02090099
                                                                        02100099
       300-REC-TELA.                                                    02110000
           MOVE     LK-COMM   TO   MTR-ALU                              02120099
                                                                        02130099
           IF  MTR-ALU   =    ZEROS                                     02140099
               PERFORM        430-RET-MENU.                             02150099
                                                                        02160099
           EVALUATE EIBAID                                              02170062
              WHEN  DFHPF3                                              02180062
                    PERFORM   430-RET-MENU                              02190099
              WHEN  DFHPF7                                              02200062
                    PERFORM   400-PF07-000                              02210062
              WHEN  DFHPF8                                              02220062
                    PERFORM   410-PF08-000                              02230099
              WHEN  DFHPF10                                             02240062
                    MOVE      0             TO  WS-LADO                 02250099
                    MOVE      REG-TAB (01)  TO  REG-ALU                 02260062
                    PERFORM   100-EXB-TELA                              02270062
              WHEN  DFHPF11                                             02280062
                    MOVE      1             TO  WS-LADO                 02290062
                    MOVE      REG-TAB (01)  TO  REG-ALU                 02300062
                    PERFORM   100-EXB-TELA                              02310062
              WHEN  OTHER                                               02320099
                    MOVE      REG-TAB (01)  TO  REG-ALU                 02330099
                    PERFORM   100-EXB-TELA                              02340099
           END-EVALUATE.                                                02350063
                                                                        02360099
       400-PF07-000.                                                    02370081
           MOVE     ZEROS     TO   WS-IND1                              02380099
           MOVE     REG-TAB  (01)  TO        REG-ALU                    02390099
                                                                        02400099
           EXEC     CICS  STARTBR  FILE     ('CADALU')                  02410039
                                   RIDFLD   (MTR-ALU)                   02420039
                          GTEQ     NOHANDLE                             02430039
           END-EXEC                                                     02440039
                                                                        02450062
           PERFORM  400-PF07-010   UNTIL     EIBRESP  >  0              02460058
                                   OR        WS-IND1  >  14             02470099
           PERFORM  420-FIM-STRT.                                       02480099
                                                                        02490021
       400-PF07-010.                                                    02500039
           EXEC     CICS  READPREV FILE     ('CADALU')                  02510056
                                   RIDFLD   (MTR-ALU)                   02520056
                                   INTO     (REG-ALU)                   02530056
                          NOHANDLE                                      02540099
           END-EXEC                                                     02550056
                                                                        02560045
           IF  EIBRESP    =   0                                         02570099
               ADD        1        TO        WS-IND1.                   02580099
                                                                        02590099
       410-PF08-000.                                                    02600099
           COMPUTE  MTR-ALU   =    MTR-ALU   +  1                       02610099
                                                                        02620099
           EXEC     CICS  STARTBR  FILE     ('CADALU')                  02630099
                                   RIDFLD   (MTR-ALU)                   02640099
                          GTEQ     NOHANDLE                             02650099
           END-EXEC                                                     02660099
                                                                        02670099
           IF  EIBRESP    =   0                                         02680099
               PERFORM    420-FIM-STRT                                  02690099
           ELSE                                                         02700099
               MOVE       REG-TAB (01)   TO  REG-ALU                    02710099
               PERFORM    100-EXB-TELA                                  02720099
           END-IF.                                                      02730099
                                                                        02740099
       420-FIM-STRT.                                                    02750099
           EXEC     CICS  ENDBR    FILE     ('CADALU')                  02760099
           END-EXEC                                                     02770099
                                                                        02780099
           PERFORM  100-EXB-TELA.                                       02790099
                                                                        02800099
       430-RET-MENU.                                                    02810099
           EXEC      CICS     XCTL                                      02820099
                     PROGRAM ('PGCICS00')                               02830099
           END-EXEC.                                                    02840099
