/* webserver_helper.c — thin POSIX socket wrappers for the Lux web server */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

/* Create, bind, and listen on a TCP socket.  Returns fd or -1. */
int lux_server_create(int port) {
    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) return -1;

    int opt = 1;
    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(addr));
    addr.sin_family      = AF_INET;
    addr.sin_addr.s_addr = INADDR_ANY;
    addr.sin_port        = htons((unsigned short)port);

    if (bind(fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) { close(fd); return -1; }
    if (listen(fd, 16) < 0)                                   { close(fd); return -1; }
    return fd;
}

/* Block until a client connects.  Returns client fd or -1. */
int lux_server_accept(int server_fd) {
    return accept(server_fd, NULL, NULL);
}

/* Read up to (size-1) bytes, NUL-terminate, return byte count. */
int lux_conn_read(int fd, char *buf, int size) {
    int n = (int)read(fd, buf, (size_t)(size - 1));
    if (n > 0) buf[n] = '\0';
    return n;
}

/* Write exactly size bytes; return bytes written. */
int lux_conn_write(int fd, char *buf, int size) {
    return (int)write(fd, buf, (size_t)size);
}

/* Close the connection fd. */
int lux_conn_close(int fd) {
    return close(fd);
}

/*
 * Extract the URL path from the first line of an HTTP request.
 * e.g. "GET /hello HTTP/1.1\r\n..." -> "/hello"
 * Writes into out (NUL-terminated), returns path length.
 */
int lux_parse_path(char *req, char *out, int maxlen) {
    char *sp1 = strchr(req, ' ');
    if (!sp1) { strncpy(out, "/", (size_t)maxlen); out[maxlen-1] = '\0'; return 1; }
    char *start = sp1 + 1;
    char *sp2   = strchr(start, ' ');
    if (!sp2) sp2 = start + strlen(start);
    int len = (int)(sp2 - start);
    if (len >= maxlen) len = maxlen - 1;
    strncpy(out, start, (size_t)len);
    out[len] = '\0';
    return len;
}

/* Case-sensitive string equality: 1 = equal, 0 = different. */
int lux_streq(char *a, char *b) {
    return strcmp(a, b) == 0 ? 1 : 0;
}

/*
 * Build a complete HTTP/1.1 response with proper CRLF line endings and
 * write it to fd.  Lux strings don't support \r, so we do it here in C.
 */
void lux_send_response(int fd, char *status, char *ctype, char *body) {
    char header[512];
    int blen = (int)strlen(body);
    int hlen = snprintf(header, sizeof(header),
        "HTTP/1.1 %s\r\n"
        "Content-Type: %s\r\n"
        "Content-Length: %d\r\n"
        "Connection: close\r\n"
        "\r\n",
        status, ctype, blen);
    write(fd, header, (size_t)hlen);
    write(fd, body,   (size_t)blen);
}
