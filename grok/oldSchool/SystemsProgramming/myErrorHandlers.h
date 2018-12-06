/* Header file for Error and Logging functions
 *
 * Purpose: Keep handling erros and loging
 *          activity to one line in programs
 *
 */

#ifndef __MY_ERROR_HANDLERS_H
#define __MY_ERROR_HANDLERS_H

#define MAXLINE 4096

void err_quit(const char *, ...);
void err_sys(const char *, ...);

#endif /* __MYERROR_HANDLERS_H  */
