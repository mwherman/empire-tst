Ccc   * $Rev: 2526 $
Ccc   * $Author: shoblit $
Ccc   * $Date: 2012-02-09 21:34:11 +0100 (Do, 09 Feb 2012) $
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
      FUNCTION PIPE(Stringp)
C
C*** Start of declarations rewritten by SPAG
C
C Dummy arguments
C
      CHARACTER(*) :: Stringp
      INTEGER*4 :: PIPE
C
C*** End of declarations rewritten by SPAG
C
      PIPE = 0
      CALL SYSTEM(Stringp)
      RETURN
      END FUNCTION PIPE
 
 
 
