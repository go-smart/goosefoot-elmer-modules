#include <stdio.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

int s;

void clearerrno_()
{
    errno = 0;
}

int geterrno_()
{
    return errno;
}

void printerr_()
{
    printf("ERR: %s\n", strerror(errno));
}

int make_connection_()
{
    s = socket(AF_UNIX, SOCK_STREAM, 0);

    struct sockaddr_un perc;
    perc.sun_family  = AF_UNIX;
    strcpy(perc.sun_path, "percentage.sock");
    connect(s, (struct sockaddr *)&perc, sizeof(perc));

    return 0;
}

int output_percentage_(char* status, double p)
{
    char percstr[330];

    sprintf(percstr, "%lf||%s\n", p, status);
    clearerrno_();
    printf("STR: %s\n", percstr);
    send(s, percstr, strlen(percstr), 0);
    printerr_();
    return 0;
}
