Ccc   * $Author: herman $ 
Ccc   * $Date: 2006-01-02 06:18:32 $
Ccc   * $Id: pipe.f,v 1.4 2006-01-02 06:18:32 herman Exp $
            
C/* pipe.f
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
C     MSFORTRAN
      USE PORTLIB
      CHARACTER*(*) STRINGP
C     LAHEY FORTRAN
C     pipe=0
C     CALL SYSTEM(STRINP)
C     MSFORTRAN
      pipe = SYSTEM (stringp)
      RETURN
      END
