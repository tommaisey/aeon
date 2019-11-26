#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>

/* returns true if there are bytes available to be read from the socket identified by fd. */
int bytes_ready (int fd, int timeOutMs)
{
    fd_set fileDescriptorSet;
    struct timeval timeout;

    FD_ZERO (&fileDescriptorSet); /* clear the set */
    FD_SET (fd, &fileDescriptorSet); /* add file descriptor to the set */

    timeout.tv_sec = timeOutMs / 1000;
    timeout.tv_usec = (int) (timeOutMs - (1000 * timeout.tv_sec)) * 1000;

    return 0 < select (fd + 1, &fileDescriptorSet, NULL, NULL, &timeout);  // Ready
}
