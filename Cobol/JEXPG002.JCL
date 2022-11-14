//JEXPG002 JOB IBMUSERA,MSGCLASS=A,CLASS=A,NOTIFY=ALU0407,TIME=(0,01)   00010099
//JOBLIB   DD  DISP=SHR,DSN=FUTURE.LMODUL.COBOL                         00020099
//*                                                                     00030075
//STEP1   EXEC PGM=ABSPG002                                             00040099
//*-----------------------------------------------------------*         00050099
//* EXER-02 - CRIAR CADATU COM CAMPOS SELECIONADOS DO CADPECA *         00060099
//*-----------------------------------------------------------*         00070099
//*                                                                     00080099
//CADPEC   DD  DSN=FUTURE.ARQUIVOS.CADPECA,                             00090099
//             DISP=SHR,VOL=SER=PR39C7,UNIT=3390                        00100099
//*                                                                     00110084
//CADATU   DD  DSN=ABS.AULAS.CADATU.PG002,                              00120099
//             DISP=(NEW,CATLG,DELETE),                                 00130099
//*            DISP=SHR,                                                00140099
//             DCB=(RECFM=FB,LRECL=060,BLKSIZE=0600),                   00150099
//             SPACE=(TRK,(3,3),RLSE),UNIT=3390                         00160099
//*                                                                     00170099
//SYSPRINT DD  SYSOUT=*                                                 00180099
//SYSOUT   DD  SYSOUT=*                                                 00190099
//SYSUDUMP DD  SYSOUT=Y                                                 00200099
//SYSABEND DD  SYSOUT=*                                                 00210099
//*                                                                     00220075
