//JEXPG004 JOB IBMUSERA,MSGCLASS=A,CLASS=A,NOTIFY=ALU0407,TIME=(0,01)   00010099
//JOBLIB   DD  DISP=SHR,DSN=FUTURE.LMODUL.COBOL                         00020099
//*                                                                     00030075
//STEP1   EXEC PGM=ABSPG004                                             00040099
//*-------------------------------------------------------*             00050099
//* EXER-04 - CRIAR CADSEL A PARTIR DO CADPECA APENAS COM *             00060099
//*           REGISTROS COM COD-PEC < 10001 E QTD-PEC > 0 *             00070099
//*-------------------------------------------------------*             00080099
//*                                                                     00090099
//CADPEC   DD  DSN=FUTURE.ARQUIVOS.CADPECA,                             00100099
//             DISP=SHR,VOL=SER=PR39C7,UNIT=3390                        00110099
//*                                                                     00120084
//CADSEL   DD  DSN=ABS.AULAS.CADSEL.PG004,                              00130099
//*            DISP=(NEW,CATLG,DELETE),                                 00140099
//             DISP=SHR,                                                00150099
//             DCB=(RECFM=FB,LRECL=050,BLKSIZE=0500),                   00160099
//             SPACE=(TRK,(3,3),RLSE),UNIT=3390                         00170099
//*                                                                     00210099
//SYSPRINT DD  SYSOUT=*                                                 00220099
//SYSOUT   DD  SYSOUT=*                                                 00230099
//SYSUDUMP DD  SYSOUT=Y                                                 00240099
//SYSABEND DD  SYSOUT=*                                                 00250099
//*                                                                     00260075
