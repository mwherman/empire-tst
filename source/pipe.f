Ccc   * $Author: Capote $
Ccc   * $Date: 2009-08-02 23:58:34 $
Ccc   * $Id: pipe.f,v 1.8 2009-08-02 23:58:34 Capote Exp $
C/*
C/* Note, RCN, July 2009
C/* This routine is only used by WINDOWS implementation
C/* The LINUX implementation uses the routine pipe.c
C/*               R.Capote, 01/99                      */
C/*                                                    */
C/* Subroutine to execute command line by FORTRAN code */
C/* FORTRAN declaration:                               */
C/* INTEGER*4 PIPE,IWIN                                */
C/* FORTRAN USE:                                       */
C/* IWIN=PIPE("command")                               */
C/* command = VALID SYSTEM COMMAND LINE with less than */
C/*             75 characters                          */
C/* IWIN =  0 for valid command execution              */
C/*      <> 0 otherwise                                */
C/*                                                    */
C/* The calling FORTRAN program must declare           */
C/* PIPE as INTEGER*4 funtion to avoid memory problems */
C/* Do not use default declaration please, they are    */
C/* compiler and system dependent !!!!                 */
C
      INTEGER*4 FUNCTION PIPE(Stringp)
C     for MSFORTRAN
C     USE PORTLIB
     
      CHARACTER*(*) STRINGP

C     for MSFORTRAN
C     pipe = SYSTEM(stringp)

C     for LAHEY, ifort, etc:
      pipe=0
      CALL SYSTEM(STRINGP)
      RETURN
      END