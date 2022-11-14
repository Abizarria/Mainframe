//JSEQVSAM JOB IBMUSERC,MSGCLASS=A,CLASS=C,NOTIFY=ALU0407               00010000
//JOBLIB   DD  DSN=FUTURE.LMODUL.COBOL,DISP=SHR                         00020000
//*                                                                     00030000
//* COPIA ARQUIVO SENQUENCIAL PARA ARQUIVO VSAM (CARGA)                 00040000
//*                                                                     00050000
//REPRO1   EXEC PGM=IDCAMS,REGION=0M                                    00060000
//DDIN     DD   DSN=ABS.ARQUIVOS.CADXXX,DISP=SHR                        00070000
//*                                                                     00080000
//DDOUT    DD   DSN=ABS.VSAM.CADXXX.CLUSTER,DISP=SHR                    00090000
//*                                                                     00100000
//SYSPRINT DD SYSOUT=*                                                  00110000
//SYSIN    DD *                                                         00120000
   REPRO INFILE(DDIN) OUTFILE(DDOUT)                                    00130000
//                                                                      00140000
