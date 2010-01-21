/* 
 * LIM h/w communication program.
 *
 * This process converts between Erlang-style h/w signals through stdin/stdout
 * and our specific LIM interface through a tty whose name is passed as a
 * parameter. It also handles the buffering while waiting for an echo from
 * a signal sent to the LIM.
 *
 * An Erlang h/w signal consists of a 2 byte signal length (MSB first) and
 * then the bytes in the signal. When reading a signal from Erlang we make it
 * easy for ourselves by doing 2 "read"s, one to get the length, and the
 * other for the signal bytes. In each case we make sure we get the required
 * # of bytes as the connection is probably a pipe (socket).
 *
 * The buffering is done while waiting for an echo is done by simply not
 * doing any input from Erlang and hoping that Erlang will not block.
 *
 * If the preprocessor variable DEBUG is set (cc -DDEBUG ...) then each line
 * to/from the LIM will be written to stderr.
 */

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <sgtty.h>
#include <string.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/time.h>

#define MAXMSG 1024		/* Maximum message size */

typedef unsigned char byte;	/* Signals data is all unsigned bytes */

/* Hw terminal data. */
static struct sgttyb ttyhw = {
    B4800, B4800,		/* input, output speed */
    0177, 025,			/* erase, kill characters */
    ANYP			/* any parity, cooked mode */
  };

#define NULLFDS ((struct fd_set *) 0)
#define NULLTV ((struct timeval *) 0)

static char *progname;		/* Start name of program */

static byte sbuf[MAXMSG+2];

static read_signal();
static write_signal();

main(argc, argv)
int argc;
char **argv;
{
    int limfd;

    progname = argv[0];		/* Save start name of program */

    /* Open and set the LIM h/w port. */
    if (argc <= 1) {
	(void) fprintf(stderr, "%s: need device name\n", progname);
	return;
    }
    if ((limfd = open(argv[1], O_RDWR)) < 0 || stty(limfd, &ttyhw) < 0) {
	(void) fprintf(stderr, "%s: opening/setting device\n", progname);
	return;
    }
    wait_message(limfd);
}

/*
 * Wait for a message from either LIM or stdin.
 * Exit if end-of-file from Erlang or read error o Erlang fd.
 */
wait_message(limfd)
{
    fd_set readfds;
    int stdfd, maxfd;
    int wait_echo;		/* Are we waiting for an echo? */
    int len, i;

    /* Initialize flags, fd sets, pointers, etc. */
    wait_echo = 0;
    FD_ZERO(&readfds);
    stdfd = fileno(stdin);
    maxfd = (stdfd > limfd) ? stdfd : limfd;

    while (1) {
	/* Set the read fd bits, and calculate maximum fd. */
	if (wait_echo)
	  FD_CLR(stdfd, &readfds);
	else
	  FD_SET(stdfd, &readfds);
	FD_SET(limfd, &readfds);

	/* Wait for something to happen. */
	if (select(maxfd+1, &readfds, NULLFDS, NULLFDS, NULLTV) < 0) {
	    fprintf(stderr, "%s: error occured waiting for input\n", progname);
	    return;
	}

	/* Test if there was h/w input, if so send it on. */
	if (FD_ISSET(limfd, &readfds)) {
	    FD_CLR(limfd, &readfds);
	    len = read_signal(limfd, sbuf+2);
	    if (len > 0) {
		/* Write length in 2 first bytes (MSB first). */
		put_int16(len, sbuf);
		write(fileno(stdout), sbuf, len+2);
	    }
	    else if (len == 0)
	      wait_echo = 0;
	    else		/* This is a read error */
	      return;
	}

	/* Test if there was input on stdin, send complete messages to lim. */
	if (FD_ISSET(stdfd, &readfds)) {
	    FD_CLR(stdfd, &readfds);
	    /* First read the the 2 length bytes (MSB first), then the data. */
	    if (read_fill(stdfd, sbuf, 2) != 2)
	      return;
	    len = get_int16(sbuf);
	    if (read_fill(stdfd, sbuf, len) != len)
	      return;
	    if (write_signal(limfd, sbuf, len) < 0)
	      return;
	    wait_echo = 1;
	}
    }
}

/* Fill buffer, return buffer length, 0 for EOF, < 0 for error. */
read_fill(fd, buf, len)
char *buf;
{
    int i, got = 0;

    do {
	if ((i = read(fd, buf+got, len-got)) <= 0)
	  return (i);
	got += i;
    } while (got < len);
    return (len);
}	

/* Procedures for putting and getting integers to/from strings. */
static get_int16(s)
byte *s;
{
    return ((*s << 8) | s[1]);
}

static put_int16(i, s)
byte *s;
{
    *s = (i >> 8) & 0xff;
    s[1] = i & 0xff;
}

/*
 * LIM specific description:
 *
 * The communication used is a tty connected to the 'operator' line of
 * the LIM in which the central cpu program has been modified to send
 * signals in an ascii form through the operator line.
 * (implemented by Per Hedeland).
 *
 * The syntax is:
 *
 *  To:		<hex> <hex> .....; RET
 *
 *  Echo:	*; RET LF
 *  From:	<hex> <hex> .....; RET LF
 *
 * The bytes in the signal are converted to/from a line of hex digits
 * sent/received through the tty. The tty is opened in UNIX 'cooked' mode
 * so we automatically read a line terminated by LF. No conversion is done
 * on CR.
 */

#define MAXLIMMSG MAXMSG*2+4

/* Needed for reading and writing signal bytes. */
static char tbuf[MAXLIMMSG];

/*
 * Read signal from the LIM and convert it to bytes. Returns the # of bytes
 * in the signal, 0 if echo or -1 for error.
 */
static read_signal(fd, bytes)
int fd;
byte *bytes;
{
    int len;
    char *s;
    byte *b;

    /* Read in next signal from LIM. */
    if ((len = read(fd, tbuf, MAXLIMMSG)) < 0)
      return (-1);
    s = tbuf;

#ifdef DEBUG
    tbuf[len] = 0;		/* Need NULL terminated string */
    fputs(tbuf, stderr);
#endif

    /* Check for 'echo'. */
    if (*s == '*')
      return (0);

    /* Convert hex string to list of bytes. */
    for (b = bytes; *s != ';'; b++) {
	*b = (*s >= 'A' ? *s - 'A' + 10 : *s - '0');
	s++;
	*b = (*s >= 'A' ? *s - 'A' + 10 : *s - '0') | *b << 4;
	s++;
    }
    /* Return the # of data bytes read. */
    return (b - bytes);
}

/*
 * Take a signal, convert it to a line of hex digits + ';' + RET and
 * send it to the lim. Return number chars written, -1 for error.
 */
static write_signal(fd, bytes, len)
int fd;
byte *bytes;
{
    char *s;
    byte *b;
    static char hexdigit[] = "0123456789ABCDEF";

    /* Convert bytes to hex string. */
    for (b = bytes, s = tbuf; len-- > 0; b++) {
	*s++ = hexdigit[(*b & 0xf0) >> 4]; /* High nibble */
	*s++ = hexdigit[(*b & 0x0f)]; /* Low nibble */
    }

    /* Terminate the command line. */
    *s++ = ';';
    *s++ = '\r';

#ifdef DEBUG
    tbuf[s-tbuf] = 0;		/* Need NULL terminated string */
    fputs(tbuf, stderr);
#endif

    /* Write the signal to the LIM. */
    return (write(fd, tbuf, s - tbuf));
}
