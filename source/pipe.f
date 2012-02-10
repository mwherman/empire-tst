Ccc   * $Rev: 2537 $
Ccc   * $Author: rcapote $
Ccc   * $Date: 2012-02-10 14:07:34 +0100 (Fr, 10 Feb 2012) $
C/*                                                    */
C/* Subroutine to execute command line by FORTRAN code */
C/* FORTRAN declaration:                               */
C/* INTEGER*4 PIPE,IWIN                                */
C/* FORTRAN USE:                                       */
C/* IWIN=PIPE("command")                               */
C/* command = VALID SYSTEM COMMAND LINE with less than */
C/*             75 characters                          */
C/*                                                    */
C/* The calling FORTRAN program must declare           */
C/* PIPE as INTEGER*4 funtion to avoid memory problems */
C/* Do not use default declaration please, they are    */
C/* compiler and system dependent !!!!                 */
C
      INTEGER*4 FUNCTION PIPE(Stringp)
      CHARACTER*(*) STRINGP
      pipe=0
      CALL SYSTEM(STRINGP)
      RETURN
      END



