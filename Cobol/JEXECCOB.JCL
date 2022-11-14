//JEXPG001 JOB  IBMUSERA,MSGCLASS=A,CLASS=A,NOTIFY=USERNAME,TIME=(0,01) 00010090
//JOBLIB   DD   DISP=SHR,DSN=FUTURE.LMODUL.COBOL                        00020084
//*                                                                     00030075
//* SUBSTITUIR USERNAME, FILENAME E PROGNAME CONFORME OBJETIVO          00032090
//*                                                                     00033090
//* SUBSTITUIR ARQENT E ARQSAI CONFORME NOME DO ARQUIVO NA FD           00034090
//*                                                                     00035090
//STEP1    EXEC PGM=ABSPG002                                            00040087
//*                                                                     00050090
//ARQENT   DD  DSN=FUTURE.ARQUIVOS.ARQENT,                              00080090
//             DISP=SHR,                                                00090084
//             VOL=SER=PR39C7,                                          00100084
//             UNIT=3390                                                00110084
//*                                                                     00111084
//* INCLUIR 'ARQSAI DD DUMMY' PARA NAO GERAR SAIDA                      00112090
//*                                                                     00113090
//* INFORMAR O TAMANHO DO REGISTRO EM LRECL                             00114090
//*                                                                     00115090
//ARQSAI   DD  DSN=.ABS.AULAS.ARQUIVOS.FILENAME.PROGNAME,               00120090
//             DISP=(NEW,CATLG,DELETE),                                 00130089
//*            DISP=SHR,                                                00140090
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=800),                     00150084
//             SPACE=(TRK,(3,3),RLSE),                                  00160084
//             UNIT=3390                                                00170084
//SYSPRINT DD  SYSOUT=*                                                 00180084
//SYSOUT   DD  SYSOUT=*                                                 00190084
//SYSUDUMP DD  SYSOUT=*                                                 00191085
//SYSABEND DD  SYSOUT=*                                                 00192085
//*                                                                     00200075
