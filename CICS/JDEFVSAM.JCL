//JDEFVSAM JOB IBMUSERC,MSGCLASS=A,CLASS=C,NOTIFY=ALU0407               00010004
//*                                                                     00020000
//*SYSOUT   DD SYSOUT=*                                                 00030000
//*                                                                     00040000
//*-------------------------------------------------------*             00050004
//* CRIA ARQUIVO VSAM - (O NOME NAO PODE SER DSN ALOCADO) *             00060004
//*-------------------------------------------------------*             00070004
//DEDEF1   EXEC PGM=IDCAMS,REGION=1M                                    00080000
//DDFILE   DD   UNIT=3390,VOL=SER=PR39D7,DISP=SHR                       00090000
//SYSPRINT DD SYSOUT=*                                                  00100000
//SYSIN    DD *                                                         00110000
   DELETE      (ABS.VSAM.CADALU.CLUSTER)                                00120003
   DEFINE CLUSTER -                                                     00130000
          (NAME(ABS.VSAM.CADALU.CLUSTER) -                              00140003
           RECORDSIZE(135 135) -                                        00150001
           VOLUME(PR39D7)    -                                          00160000
           KEY(5 0)          -                                          00170000
           INDEXED           -                                          00180000
           CYL(01 01)        -                                          00190000
           FREESPACE(10 10)  -                                          00200000
           SHAREOPTIONS(2 3) -                                          00210000
          CONTROLINTERVALSIZE(4096) -                                   00220000
           UNIQUE -                                                     00230000
           IMBED -                                                      00240000
           REPLICATE) -                                                 00250000
         DATA -                                                         00260000
         (NAME(ABS.VSAM.CADALU.DATA)) -                                 00270003
         INDEX -                                                        00280000
         (NAME(ABS.VSAM.CADALU.INDEX))                                  00290003
//*                                                                     00300000