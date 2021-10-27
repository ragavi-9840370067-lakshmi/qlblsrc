000100200302/*********************************************************************/
000200200409/* TITLE        : ISC COMMISSION PAID BY ONTARIO RESIDENTS           */ 
000300200302/* SYSTEM       : L&T Financial Services Technology Inc.             */
000400200302/* PROGRAM TYPE : CL                                                 */
000500200302/* PROGRAMMER   : Priya Nivedha                                      */
000600200302/* PROGRAM      : JBACCCOMM                                          */
000700200302/* DESCRIPTION  : Report of ISC commission paid details              */
000800200302/* DATE CREATED : 2020/02/28                                         */
000900200302/* RFS NUMBER   : RFS185636                                          */
001000200325/* CALLS PROGRAM: SQACCCOMM,ACCCOMMB,ACCCOMMS                        */
001100200302/*********************************************************************/
001200200302/*                C H A N G E   H I S T O R Y                        */
001300200302/*********************************************************************/
001400200302/*********************************************************************/
001500200302/* PROGRAMMER  *DATE OF CHANGE* DESCRIPTION OF CHANGE                */
001600200302/* Priya N     * 2020/02/28   * RFS185636 - QTP Rewrite ACCCOMM      */
001700200302/*********************************************************************/
001800200302       PGM
001900200302         QTP        AUTO(*LIBL/PTACCCOMM)
002000200302         QUIZ       AUTO(*LIBL/PZACCCOMMB)
002100200302         QUIZ       AUTO(*LIBL/PZACCCOMMS)
002200200302       ENDPGM
